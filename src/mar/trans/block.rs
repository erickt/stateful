use mar::repr::*;
use mar::trans::Builder;
use syntax::ast;

impl<'a> Builder<'a> {
    pub fn block(&self, block: BasicBlock) -> Vec<ast::Stmt> {
        let block_data = self.mar.basic_block_data(block);

        assert!(block_data.terminator.is_some(),
                "block does not have a terminator");

        block_data.statements.iter()
            .flat_map(|statement| self.stmt(block, statement))
            .chain(
                block_data.terminator.iter()
                    .flat_map(|terminator| self.terminator(terminator))
            )
            .collect()
    }

    fn terminator(&self, terminator: &Terminator) -> Vec<ast::Stmt> {
        match *terminator {
            Terminator::Goto { target } => {
                self.goto(target)
            }
            Terminator::If { ref cond, targets: (then_block, else_block) } => {
                let then_block = self.ast_builder.block()
                    .with_stmts(self.goto(then_block))
                    .build();

                let else_block = self.ast_builder.block()
                    .with_stmts(self.goto(else_block))
                    .build();

                vec![
                    self.ast_builder.stmt().expr().if_()
                        .build(cond.clone())
                        .build_then(then_block)
                        .build_else(else_block),
                ]
            }
            Terminator::Match { ref discr, ref targets } => {
                let arms = targets.iter()
                    .map(|target| {
                        let block = self.ast_builder.block()
                            .with_stmts(self.goto(target.block))
                            .build();

                        self.ast_builder.arm()
                            .with_pats(target.pats.iter().cloned())
                            .with_guard(target.guard.clone())
                            .body().build_block(block)
                    });

                vec![
                    self.ast_builder.stmt().expr().match_()
                        .build(discr.clone())
                        .with_arms(arms)
                        .build()
                ]
            }
            Terminator::Yield { ref expr, target } => {
                let next_state = self.state_expr(target);

                vec![
                    self.ast_builder.stmt().semi().return_expr()
                        .tuple()
                            .expr().some().build(expr.clone())
                            .expr().build(next_state)
                            .build()
                ]
            }
            Terminator::Return => {
                let next_state = self.state_expr(END_BLOCK);

                vec![
                    self.ast_builder.stmt().semi().return_expr().tuple()
                        .expr().none()
                        .expr().build(next_state)
                        .build()
                ]
            }
        }
    }

    fn goto(&self, target: BasicBlock) -> Vec<ast::Stmt> {
        let next_state = self.state_expr(target);

        vec![
            self.ast_builder.stmt().expr().assign()
                .id("state")
                .build(next_state),
            self.ast_builder.stmt().expr().continue_(),
        ]
    }
}
