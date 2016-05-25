use mar::repr::*;
use mar::trans::Builder;
use std::collections::HashSet;
use syntax::ast::{self, Mutability};
use syntax::ptr::P;

impl<'a> Builder<'a> {
    fn state_id(&self, block: BasicBlock) -> ast::Ident {
        match self.mar.basic_block_data(block).name {
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
            .id("State")
            .id(self.state_id(block))
            .build()
    }

    fn get_incoming_decls(&self, block: BasicBlock) -> Vec<(VarDecl, ast::Ident)> {
        let block_data = self.mar.basic_block_data(block);

        let mut incoming_blocks = block_data.incoming_blocks.iter();

        let mut decls = match incoming_blocks.next() {
            Some(block) => {
                let block_data = self.mar.basic_block_data(*block);
                let decls = block_data.live_decls.clone();

                // Make sure the rest of the incoming blocks have identical decls.
                for block in incoming_blocks {
                    if decls != self.mar.basic_block_data(*block).live_decls {
                        self.cx.bug("block has different incoming decls");
                    }
                }

                decls
            }
            None => {
                vec![]
            }
        };

        decls.extend(
            block_data.new_decls.iter()
                .map(|decl| {
                    let ident = self.mar.var_decl_data(*decl).ident;
                    (*decl, ident)
                })
        );

        decls
    }

    pub fn state_expr(&self, block: BasicBlock) -> P<ast::Expr> {
        let state_path = self.state_path(block);
        let incoming_decls = self.get_incoming_decls(block);

        if incoming_decls.is_empty() {
            self.ast_builder.expr().path()
                .build(state_path)
        } else {
            let id_exprs = incoming_decls.iter()
                .map(|&(_, ident)| {
                    (ident, self.ast_builder.expr().id(ident))
                });

            self.ast_builder.expr().struct_path(state_path)
                .with_id_exprs(id_exprs)
                .build()
        }

    }

    pub fn state_enum_default_and_arms(&self) -> (P<ast::Item>, P<ast::Item>, Vec<ast::Arm>) {
        let all_basic_blocks = self.mar.all_basic_blocks();

        let mut ty_param_ids = Vec::new();
        let mut seen_ty_param_ids = HashSet::new();
        let mut state_variants = Vec::with_capacity(all_basic_blocks.len());
        let mut state_arms = Vec::with_capacity(all_basic_blocks.len());

        for block in all_basic_blocks {
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

        let end_expr = self.ast_builder.block()
            .expr().build(self.state_expr(END_BLOCK));

        let state_default = quote_item!(self.cx,
            impl $generics Default for $state_path {
                fn default() -> Self {
                    $end_expr
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
        let body = self.ast_builder.block()
            .with_stmts(self.block(block))
            .build();

        self.ast_builder.arm()
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

            for &(decl, ident) in incoming_decls.iter() {
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
