use mar::repr::*;
use mar::trans::Builder;
use syntax::ast;
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

    fn get_incoming_decls(&self, block: BasicBlock) -> &[(VarDecl, ast::Ident)] {
        let block_data = self.mar.basic_block_data(block);

        match block_data.incoming_blocks.first() {
            Some(block) => {
                let block_data = self.mar.basic_block_data(*block);
                &block_data.live_decls
            }
            None => &[]
        }
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
        let mut state_variants = Vec::with_capacity(all_basic_blocks.len());
        let mut state_arms = Vec::with_capacity(all_basic_blocks.len());

        for block in all_basic_blocks {
            let variant = self.state_variant(block);
            state_variants.push(variant);

            let arm = self.state_arm(block);
            state_arms.push(arm);
        }

        let state_variables = (0..self.mar.var_decls.len())
            .map(|index| format!("T{}", index))
            .collect::<Vec<_>>();

        let generics = self.ast_builder.generics()
            .with_ty_param_ids(state_variables.iter())
            .build();

        let state_enum = self.ast_builder.item().enum_("State")
            .generics().with(generics.clone()).build()
            .with_variants(state_variants)
            .build();

        let state_path = self.ast_builder.path()
            .segment("State")
                .with_tys(
                    state_variables.iter()
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

    fn state_variant(&self, block: BasicBlock) -> P<ast::Variant> {
        let state_id = self.state_id(block);
        let incoming_decls = self.get_incoming_decls(block);

        if incoming_decls.is_empty() {
            self.ast_builder.variant(state_id).unit()
        } else {
            let fields = incoming_decls.iter()
                .map(|&(decl, ident)| {
                    self.ast_builder.struct_field(ident)
                        .ty().id(format!("T{}", decl.index()))
                });

            self.ast_builder.variant(state_id)
                .struct_()
                    .with_fields(fields)
                .build()
        }
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
            self.ast_builder.pat().enum_().build(state_path)
                .build()
        } else {
            let field_pats = incoming_decls.iter()
                .map(|&(decl, ident)| {
                    let decl_data = self.mar.var_decl_data(decl);

                    let pat = match decl_data.mutability {
                        ast::MutImmutable => self.ast_builder.pat().id(ident),
                        ast::MutMutable => self.ast_builder.pat().mut_id(ident),
                    };

                    (ident, pat)
                });

            self.ast_builder.pat().struct_().build(state_path)
                .with_pats(field_pats)
                .build()
        }
    }
}
