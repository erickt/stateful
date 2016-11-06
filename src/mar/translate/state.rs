use mar::repr::*;
use mar::indexed_vec::Idx;
use mar::translate::Builder;
use std::collections::HashSet;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    fn state_id(&self, block: BasicBlock) -> ast::Ident {
        match self.mar[block].name {
            Some(name) => {
                self.ast_builder.id(format!("State{}{}", block.index(), name))
            }
            None => {
                self.ast_builder.id(format!("State{}", block.index()))
            }
        }
    }

    pub fn state_path(&self, block: BasicBlock) -> ast::Path {
        self.ast_builder.path()
            .span(self.mar.span)
            .id("State")
            .id(self.state_id(block))
            .build()
    }

    fn get_incoming_scope_decls(
        &self,
        block: BasicBlock
    ) -> Vec<(VisibilityScope, Vec<(Local, ast::Ident)>)> {
        let mut scope_decls = vec![];

        for (scope, live_decls) in self.mar[block].live_decls.iter() {
            let mut decls = vec![];

            for live_decl in live_decls {
                // Only add active decls to the state.
                let local = match *live_decl {
                    LiveDecl::Active(local) => {
                        decls.push((local, self.mar.local_decls[local].ident));
                        local
                    }
                    LiveDecl::Moved(local) => local,
                };

                self.get_shadowed_decls(&mut decls, local);
            }

            scope_decls.push((*scope, decls));
        }

        scope_decls
    }

    fn get_shadowed_decls(&self, decls: &mut Vec<(Local, ast::Ident)>, local: Local) {
        if let Some(decl) = self.mar.local_decls[local].shadowed_decl {
            debug!("get_shadowed_decl: {:?} {:?}", decl, self.mar.local_decls[decl]);

            decls.push((decl, self.shadowed_ident(decl)));

            self.get_shadowed_decls(decls, decl);
        }
    }

    pub fn state_expr(&self, span: Span, block: BasicBlock) -> P<ast::Expr> {
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(block);
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


    pub fn state_enum_default_and_arms(&self) -> (P<ast::Item>, P<ast::Item>, Vec<ast::Arm>) {
        let all_basic_blocks = self.mar.basic_blocks();

        let mut ty_param_ids = Vec::new();
        let mut seen_ty_param_ids = HashSet::new();
        let mut state_variants = Vec::with_capacity(all_basic_blocks.len());
        let mut state_arms = Vec::with_capacity(all_basic_blocks.len());

        for (block, _) in self.mar.basic_blocks().iter_enumerated() {
            let (variant, tp) = self.state_variant(block);

            // It's possible for a declaration to be created but not actually get used in the state
            // variables, so we only create a type parameter for a declaration if it's actually
            // used.
            for ty_param_id in tp {
                if !seen_ty_param_ids.contains(&ty_param_id) {
                    seen_ty_param_ids.insert(ty_param_id);
                    ty_param_ids.push(ty_param_id);
                }
            }

            state_variants.push(variant);

            let arm = self.state_arm(block);
            state_arms.push(arm);
        }

        let generics = self.ast_builder.generics()
            .with_ty_param_ids(ty_param_ids.iter())
            .build();

        let state_enum = self.ast_builder.item().enum_("State")
            .generics().with(generics.clone()).build()
            .id("Illegal")
            .with_variants(state_variants)
            .build();

        let state_path = self.ast_builder.path()
            .segment("State")
                .with_tys(
                    ty_param_ids.iter()
                        .map(|variable| self.ast_builder.ty().id(variable))
                )
            .build()
            .build();

        let state_default = quote_item!(self.cx,
            impl $generics ::std::default::Default for $state_path {
                fn default() -> Self {
                    State::Illegal
                }
            }
        ).expect("state default item");

        (state_enum, state_default, state_arms)
    }

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

    fn state_arm(&self, block: BasicBlock) -> ast::Arm {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(block);
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
                    decls.iter().map(|&(_, name)| ast_builder.pat().id(name))
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

    /*
    fn state_pat(&self,
                 block: BasicBlock,
                 scope_decls: &[(VisibilityScope, Vec<(Local, ast::Ident)>)]) -> P<ast::Pat> {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);


        if scope_decls.is_empty() {
            ast_builder.pat().build_path(state_path)
        } else {
            let pats = scope_decls.iter().map(|&(scope, _)| {
                ast_builder.pat().id(format!("scope{}", scope.index()))
            });


            /*
            let pats = incoming_scope_decls.iter().map(|scope| {
                ast_builder.pat().tuple()
                    .with_pats(
                        scope.iter().map(|&(decl, ident)| {
                            let decl_data = self.mar.local_decl_data(decl);

                            match decl_data.mutability {
                                Mutability::Immutable => ast_builder.pat().id(ident),
                                Mutability::Mutable => ast_builder.pat().mut_id(ident),
                            }
                        })
                        )
                    .build()
                });
                */

            ast_builder.pat().enum_().build(state_path)
                .with_pats(pats)
                .build()
        }
    }
    */
}
