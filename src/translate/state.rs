use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;
use translate::Builder;

#[derive(Copy, Clone)]
pub enum StateKind {
    Coroutine,
    Internal,
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn coroutine_state(&self) -> CoroutineState {
        let blocks = &self.resume_blocks;

        let mut variants = Vec::with_capacity(blocks.len());
        let mut seen_ty_param_ids = HashSet::new();
        let mut ty_param_ids = vec![];
        let mut arms = Vec::with_capacity(blocks.len());
        
        for &block in blocks.iter() {
            let (variant, tp) = self.state_variant(block);
            variants.push(variant);

            // It's possible for a declaration to be created but not actually get used in the state
            // variables, so we only create a type parameter for a declaration if it's actually
            // used.
            for ty_param_id in tp {
                if !seen_ty_param_ids.contains(&ty_param_id) {
                    seen_ty_param_ids.insert(ty_param_id);
                    ty_param_ids.push(ty_param_id);
                }
            }

            arms.push(self.coroutine_arm(block));
        }

        // Build up a match expression.
        let expr = quote_expr!(self.cx,
            match coroutine_state {
                $arms
                CoroutineState::Illegal => { panic!("illegal state") }
            }
        );

        let generics = self.ast_builder.generics()
            .with_ty_param_ids(ty_param_ids.iter())
            .build();

        let enum_item = self.ast_builder.item().enum_("CoroutineState")
            .generics().with(generics.clone()).build()
            .id("Illegal")
            .with_variants(variants)
            .build();

        let state_path = self.ast_builder
            .path()
                .segment("CoroutineState")
                .with_tys(
                    ty_param_ids.iter().map(|variable| self.ast_builder.ty().id(variable))
                )
                .build()
            .build();

        let default_item = quote_item!(self.cx,
            impl $generics ::std::default::Default for $state_path {
                fn default() -> Self {
                    CoroutineState::Illegal
                }
            }
        ).expect("state default item");

        CoroutineState {
            enum_item: enum_item,
            default_item: default_item,
            expr: expr,
        }
    }

    pub fn coroutine_state_expr(&self, block: BasicBlock) -> P<ast::Expr> {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);
        
        let locals = self.get_incoming_scope_decls(block);
        let ids = locals.into_iter()
            .map(|(scope, _)| ast_builder.id(format!("scope{}", scope.index())))
            .collect::<Vec<_>>();

        let path = self.state_path(block, StateKind::Coroutine);
        ast_builder.expr().call()
            .build_path(path)
            .with_args(ids.into_iter().map(|id| ast_builder.expr().id(id)))
            .build()
    }

    pub fn internal_state(&self) -> InternalState {
        let blocks = self.mir.basic_blocks();

        let mut ty_param_ids = Vec::new();
        let mut seen_ty_param_ids = HashSet::new();
        let mut variants = Vec::with_capacity(blocks.len());
        let mut arms = Vec::with_capacity(blocks.len());

        for block in blocks.indices() {
            let (variant, tp) = self.state_variant(block);
            variants.push(variant);

            // It's possible for a declaration to be created but not actually get used in the state
            // variables, so we only create a type parameter for a declaration if it's actually
            // used.
            for ty_param_id in tp {
                if !seen_ty_param_ids.contains(&ty_param_id) {
                    seen_ty_param_ids.insert(ty_param_id);
                    ty_param_ids.push(ty_param_id);
                }
            }

            arms.push(self.internal_arm(block));
        }

        let expr = quote_expr!(self.cx,
            loop {
                match state {
                    $arms
                }
            }
        );

        let generics = self.ast_builder.generics()
            .with_ty_param_ids(ty_param_ids.iter())
            .build();

        let enum_name = self.ast_builder.id("InternalState");

        let enum_item = self.ast_builder.item().enum_(enum_name)
            .generics().with(generics.clone()).build()
            .id("Illegal")
            .with_variants(variants)
            .build();

        let state_path = self.ast_builder.path()
            .segment(enum_name)
                .with_tys(
                    ty_param_ids.iter()
                        .map(|variable| self.ast_builder.ty().id(variable))
                )
            .build()
            .build();

        InternalState {
            enum_item: enum_item,
            expr: expr,
        }
    }

    pub fn internal_state_expr(&self, block: BasicBlock) -> P<ast::Expr> {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(block, StateKind::Internal);
        let incoming_scope_decls = self.get_incoming_scope_decls(block);

        if incoming_scope_decls.is_empty() {
            ast_builder.expr().path()
                .build(state_path)
        } else {
            let exprs = incoming_scope_decls.iter()
                .map(|&(_, ref decls)| {
                    ast_builder.expr().tuple()
                        .with_exprs(
                            decls.iter().map(|&(_, ident)| ast_builder.expr().id(ident))
                        )
                        .build()
                });

            ast_builder.expr().call()
                .build_path(state_path)
                .with_args(exprs)
                .build()
        }
    }

    fn get_incoming_scope_decls(
        &self,
        block: BasicBlock
    ) -> Vec<(VisibilityScope, Vec<(Local, ast::Ident)>)> {
        let mut map = BTreeMap::new();

        for &local in self.assignments.on_entry(block) {
            let local_data = &self.mir.local_decls[local];
            let decls = map.entry(local_data.source_info.scope)
                .or_insert_with(Vec::new);

            decls.push((local, local_data.name));
        }

        map.into_iter().collect::<Vec<_>>()
    }

    fn get_shadowed_decls(&self, decls: &mut Vec<(Local, ast::Ident)>, local: Local) {
        if let Some(decl) = self.mir.local_decls[local].shadowed_decl {
            debug!("get_shadowed_decl: {:?} {:?}", decl, self.mir.local_decls[decl]);

            decls.push((decl, self.shadowed_ident(decl)));

            self.get_shadowed_decls(decls, decl);
        }
    }

    fn state_pat(&self,
                 block: BasicBlock,
                 kind: StateKind,
                 ids: &[ast::Ident]) -> P<ast::Pat> {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(block, kind);

        ast_builder.pat().enum_().build(state_path)
            .with_ids(ids)
            .build()
    }

    /// Build up an `ast::Arm` for a coroutine state variant. This arm's role is to lift up the
    /// coroutine arguments into the state machine, which is simply generating a conversion like
    /// this:
    ///
    /// ```rust
    /// CoroutineInternal::State1(scope1, scope2) => {
    ///     InternalState::State1(scope1, scope2, args)
    /// }
    /// ```
    fn coroutine_arm(&self, block: BasicBlock) -> ast::Arm {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let locals = self.get_incoming_scope_decls(block);
        let ids = locals.into_iter()
            .map(|(scope, _)| ast_builder.id(format!("scope{}", scope.index())))
            .collect::<Vec<_>>();

        let coroutine_path = self.state_path(block, StateKind::Coroutine);
        let coroutine_pat = ast_builder.pat().enum_().build(coroutine_path)
            .with_ids(&ids)
            .build();

        let internal_path = self.state_path(block, StateKind::Internal);
        let internal_expr = ast_builder.expr().call()
            .build_path(internal_path)
            .with_args(ids.into_iter().map(|id| ast_builder.expr().id(id)))
            .arg().id("args")
            .build();

        ast_builder.arm()
            .with_pat(coroutine_pat)
            .body().build(internal_expr)
    }

    /// Build up an `ast::Arm` for an internal state variant.
    fn internal_arm(&self, block: BasicBlock) -> ast::Arm {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(block, StateKind::Internal);
        let scope_decls = self.get_incoming_scope_decls(block);

        let pat = if scope_decls.is_empty() {
            ast_builder.pat().build_path(state_path)
        } else {
            let pats = scope_decls.iter().map(|&(scope, _)| {
                ast_builder.pat().id(format!("scope{}", scope.index()))
            });

            ast_builder.pat().enum_().build(state_path)
                .with_pats(pats)
                .build()
        };

        // First, setup the blocks.
        let mut body = ast_builder.block()
            .with_stmts(self.block(block))
            .build();

        for &(scope, ref decls) in scope_decls.iter().rev() {
            let decl_pat = ast_builder.pat()
                .tuple()
                .with_pats(
                    decls.iter().map(|&(local, name)| {
                        match self.mir.local_decls[local].mutability {
                            ast::Mutability::Immutable => ast_builder.pat().id(name),
                            ast::Mutability::Mutable => ast_builder.pat().mut_id(name),
                        }
                    })
                )
                .build();

            let stmt = ast_builder.stmt()
                .let_().build(decl_pat)
                .expr().id(format!("scope{}", scope.index()));

            body = ast_builder.block()
                .stmt().build(stmt)
                .expr().build_block(body);
        }

        ast_builder.arm()
            .with_pat(pat)
            .body().build_block(body)
    }

    fn state_id(&self, block: BasicBlock) -> ast::Ident {
        match self.mir[block].name {
            Some(name) => {
                self.ast_builder.id(format!("State{}{}", block.index(), name))
            }
            None => {
                self.ast_builder.id(format!("State{}", block.index()))
            }
        }
    }

    fn state_path(&self, block: BasicBlock, kind: StateKind) -> ast::Path {
        let enum_name = match kind {
            StateKind::Coroutine => "CoroutineState",
            StateKind::Internal => "InternalState",
        };

        self.ast_builder
            .path()
            .span(self.mir.span)
            .id(enum_name)
            .id(self.state_id(block))
            .build()
    }

    /*
    fn state_pat(&self, block: BasicBlock, kind: StateKind) -> P<ast::Pat> {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(block, kind);

        let scope_decls = self.get_incoming_scope_decls(block);

        if scope_decls.is_empty() {
            ast_builder.pat().build_path(state_path)
        } else {
            let scope_ids = scope_decls.iter()
                .map(|&(scope, _)| ast_builder.id(format!("scope{}", scope.index())))
                .collect::<Vec<_>>();

            ast_builder.pat().enum_().build(state_path)
                .with_ids(scope_ids)
                .build()
        }
    }
    */

    fn state_variant(&self, block: BasicBlock) -> (ast::Variant, Vec<ast::Ident>) {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let state_id = self.state_id(block);
        let incoming_scope_decls = self.get_incoming_scope_decls(block);

        let ty_param_ids = incoming_scope_decls.iter()
            .flat_map(|&(_, ref decls)| {
                decls.iter().map(|&(decl, _)| {
                    ast_builder.id(format!("T{}", decl.index()))
                })
            })
            .collect::<Vec<_>>();

        let variant = if incoming_scope_decls.is_empty() {
            ast_builder.variant(state_id).unit()
        } else {
            let mut tys = incoming_scope_decls.iter()
                .map(|&(_, ref decls)| {
                    ast_builder.ty().tuple()
                        .with_tys(
                            decls.iter().map(|&(decl, _)| {
                                ast_builder.ty().id(format!("T{}", decl.index()))
                            })
                        )
                        .build()
                });

            let ty = tys.next().unwrap();

            ast_builder.variant(state_id).tuple().ty().build(ty)
                .with_fields(
                    tys.map(|ty| ast_builder.tuple_field().ty().build(ty))
                )
                .build()
        };

        (variant, ty_param_ids)
    }

}

pub struct CoroutineState {
    pub enum_item: P<ast::Item>,
    pub default_item: P<ast::Item>,
    pub expr: P<ast::Expr>,
}

pub struct InternalState {
    pub enum_item: P<ast::Item>,
    pub expr: P<ast::Expr>,
}
