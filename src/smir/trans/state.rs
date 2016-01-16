use smir::repr::*;
use smir::trans::Builder;
use syntax::ast;
use syntax::ptr::P;

impl<'a> Builder<'a> {
    fn state_id(&self, block: BasicBlock) -> ast::Ident {
        match self.smir.basic_block_data(block).name {
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

    pub fn state_expr(&self, block: BasicBlock) -> P<ast::Expr> {
        let state_path = self.state_path(block);

        self.ast_builder.expr().path().build(state_path)
    }

    pub fn state_enum_default_and_arms(&self) -> (P<ast::Item>, P<ast::Item>, Vec<ast::Arm>) {
        let all_basic_blocks = self.smir.all_basic_blocks();
        let mut state_variants = Vec::with_capacity(all_basic_blocks.len());
        let mut state_arms = Vec::with_capacity(all_basic_blocks.len());

        for block in all_basic_blocks {
            let variant = self.state_variant(block);
            state_variants.push(variant);

            let arm = self.state_arm(block);
            state_arms.push(arm);
        }

        let state_enum = self.ast_builder.item().enum_("State")
            .with_variants(state_variants)
            .build();

        let state_path = self.ast_builder.path()
            .segment("State").build()
            .build();

        let end_expr = self.ast_builder.block()
            .with_stmts(self.block(END_BLOCK))
            .build();

        let state_default = quote_item!(self.cx,
            impl Default for $state_path {
                fn default() -> Self {
                    $end_expr
                }
            }
        ).expect("state default item");

        (state_enum, state_default, state_arms)
    }

    fn state_variant(&self, block: BasicBlock) -> P<ast::Variant> {
        let state_id = self.state_id(block);

        self.ast_builder.variant(state_id)
            .unit()
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

        self.ast_builder.pat().enum_().build(state_path)
            .build()
    }
}
