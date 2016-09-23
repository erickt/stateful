use mar::repr::*;
use mar::translate::Builder;
use syntax::ast;
use syntax::codemap::Span;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn block(&self, block: BasicBlock) -> Vec<ast::Stmt> {
        let block_data = &self.mar[block];

        assert!(block_data.terminator.is_some(),
                "block does not have a terminator");

        block_data.statements().iter()
            .flat_map(|statement| self.stmt(block, statement))
            .chain(
                block_data.terminator.iter()
                    .flat_map(|terminator| self.terminator(terminator))
            )
            .collect()
    }

    fn terminator(&self, terminator: &Terminator) -> Vec<ast::Stmt> {
        let ast_builder = self.ast_builder.span(terminator.span);

        match terminator.kind {
            TerminatorKind::Goto { target, .. } => {
                self.goto(terminator.span, target)
            }
            TerminatorKind::If { ref cond, targets: (then_block, else_block) } => {
                let then_block = ast_builder
                    .span(self.block_span(then_block))
                    .block()
                    .with_stmts(self.goto(terminator.span, then_block))
                    .build();

                let else_block = ast_builder
                    .span(self.block_span(else_block))
                    .block()
                    .with_stmts(self.goto(terminator.span, else_block))
                    .build();

                vec![
                    ast_builder.stmt().expr().if_()
                        .build(cond.clone())
                        .build_then(then_block)
                        .build_else(else_block),
                ]
            }
            TerminatorKind::Match { ref discr, ref targets } => {
                let arms = targets.iter()
                    .map(|target| {
                        let ast_builder = ast_builder.span(self.block_span(target.block));

                        let block = ast_builder.block()
                            .span(self.mar.span)
                            .with_stmts(self.goto(terminator.span, target.block))
                            .build();

                        ast_builder.arm()
                            .with_pats(target.pats.iter().cloned())
                            .with_guard(target.guard.clone())
                            .body().build_block(block)
                    });

                vec![
                    ast_builder.stmt().expr().match_()
                        .build(discr.clone())
                        .with_arms(arms)
                        .build()
                ]
            }
            TerminatorKind::Return => {
                let next_state = ast_builder.expr().path()
                    .span(self.mar.span)
                    .ids(&["State", "Illegal"])
                    .build();

                match self.mar.state_machine_kind {
                    StateMachineKind::Generator => {
                        vec![
                            ast_builder.stmt().semi().call()
                                    .path()
                                        .global()
                                        .ids(&["std", "mem", "drop"])
                                        .build()
                                .arg().id("return_")
                                .build(),
                            ast_builder.stmt().semi().return_expr().tuple()
                                .expr().none()
                                .expr().build(next_state)
                                .build()
                        ]
                    }
                    StateMachineKind::Async => {
                        let return_expr = ast_builder.expr().id("return_");
                        let ready_expr = ast_builder.expr().call()
                            .path()
                                .global()
                                .ids(&["futures", "Async", "Ready"])
                                .build()
                            .with_arg(return_expr)
                            .build();

                        vec![
                            ast_builder.stmt().semi().return_expr().ok().tuple()
                                .expr().build(ready_expr)
                                .expr().build(next_state)
                                .build()
                        ]
                    }
                }
            }
            TerminatorKind::Await { target } => {
                let awaited_expr = ast_builder.expr().path()
                    .global()
                    .ids(&["futures", "Async", "NotReady"])
                    .build();
                let next_state = self.state_expr(terminator.span, target);

                let tuple = ast_builder.expr().ok().tuple()
                    .expr().build(awaited_expr)
                    .expr().build(next_state)
                    .build();

                vec![
                    ast_builder.stmt().semi()
                        .return_expr()
                        .build(tuple)
                ]
            }
            TerminatorKind::Suspend { ref rvalue, target } => {
                let ast_builder = ast_builder.span(rvalue.span);
                let next_state = self.state_expr(terminator.span, target);

                let tuple = ast_builder.expr().tuple()
                    .expr().build(rvalue.clone())
                    .expr().build(next_state)
                    .build();

                vec![
                    ast_builder.stmt().semi()
                        .return_expr()
                        .build(tuple)
                ]
            }
        }
    }

    fn goto(&self, span: Span, target: BasicBlock) -> Vec<ast::Stmt> {
        let ast_builder = self.ast_builder.span(span);
        let next_state = self.state_expr(span, target);
        let next_expr = ast_builder.expr()
            .assign()
            .id("state")
            .build(next_state);

        vec![
            ast_builder.stmt().semi().build(next_expr),
            ast_builder.stmt().semi().continue_(),
        ]
    }

    pub fn block_span(&self, block: BasicBlock) -> Span {
        self.mar[block].span
    }
}
