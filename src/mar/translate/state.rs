use mar::repr::*;
use mar::indexed_vec::Idx;
use mar::translate::Builder;
use std::collections::HashSet;
use syntax::ast::{self, Mutability};
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

    fn get_incoming_decls(&self, block: BasicBlock) -> Vec<(Var, ast::Ident)> {
        let mut decls = vec![];

        for live_decl in self.mar[block].decls() {
            // Only add active decls to the state.
            let var = match *live_decl {
                LiveDecl::Active(var) => {
                    decls.push((var, self.mar.var_decls[var].ident));
                    var
                }
                LiveDecl::Forward(var) | LiveDecl::Moved(var) => var,
            };

            self.get_shadowed_decls(&mut decls, var);
        }

        //debug!("decls: {:?} {:?}", block, decls);

        decls
    }

    fn get_shadowed_decls(&self, decls: &mut Vec<(Var, ast::Ident)>, var: Var) {
        if let Some(decl) = self.mar.var_decls[var].shadowed_decl {
            debug!("get_shadowed_decl: {:?} {:?}", decl, self.mar.var_decls[decl]);

            decls.push((decl, self.shadowed_ident(decl)));

            self.get_shadowed_decls(decls, decl);
        }
    }

    pub fn state_expr(&self, span: Span, block: BasicBlock) -> P<ast::Expr> {
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(block);
        let incoming_decls = self.get_incoming_decls(block);

        if incoming_decls.is_empty() {
            ast_builder.expr().path()
                .build(state_path)
        } else {
            let id_exprs = incoming_decls.iter()
                .map(|&(_, ident)| {
                    (ident, ast_builder.expr().span(self.mar.span).id(ident))
                });

            ast_builder.expr().struct_path(state_path)
                .with_id_exprs(id_exprs)
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
        let state_id = self.state_id(block);
        let incoming_decls = self.get_incoming_decls(block);
        let ty_param_ids = incoming_decls.iter()
            .map(|&(decl, _)| {
                self.ast_builder.id(format!("T{}", decl.index()))
            })
            .collect::<Vec<_>>();

        let variant = if incoming_decls.is_empty() {
            self.ast_builder.variant(state_id).unit()
        } else {
            let fields = incoming_decls.iter()
                .zip(ty_param_ids.iter())
                .map(|(&(_, ident), ty_param)| {
                    self.ast_builder.struct_field(ident).ty().id(ty_param)
                });

            self.ast_builder.variant(state_id)
                .struct_()
                    .with_fields(fields)
                .build()
        };

        (variant, ty_param_ids)
    }

    fn state_arm(&self, block: BasicBlock) -> ast::Arm {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let body = ast_builder.block()
            .with_stmts(self.block(block))
            .build();

        ast_builder.arm()
            .with_pat(self.state_pat(block))
            .body().build_block(body)
    }

    fn state_pat(&self, block: BasicBlock) -> P<ast::Pat> {
        let state_path = self.state_path(block);
        let incoming_decls = self.get_incoming_decls(block);

        if incoming_decls.is_empty() {
            self.ast_builder.pat().build_path(state_path)
        } else {
            let mut struct_pat_builder = self.ast_builder.pat().struct_()
                .build(state_path);

            for &(decl, ident) in &incoming_decls {
                let decl_data = self.mar.var_decl_data(decl);

                struct_pat_builder = match decl_data.mutability {
                    Mutability::Immutable => struct_pat_builder.id(ident),
                    Mutability::Mutable => struct_pat_builder.mut_id(ident),
                };
            }

            struct_pat_builder.build()
        }
    }
}
