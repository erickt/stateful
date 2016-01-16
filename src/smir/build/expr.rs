use smir::build::Builder;
use smir::build::scope::LoopScope;
use smir::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a> Builder<'a> {
    pub fn expr(&mut self,
                extent: CodeExtent,
                block: BasicBlock,
                expr: &P<ast::Expr>) -> BasicBlock {
        match expr.node {
            ast::ExprBlock(ref ast_block) => {
                self.ast_block(extent, block, ast_block)
            }
            ast::ExprAgain(label) => {
                self.break_or_continue(expr.span,
                                       label.map(|label| label.node),
                                       block,
                                       |loop_scope| loop_scope.continue_block)
            }
            ast::ExprBreak(label) => {
                self.break_or_continue(expr.span,
                                       label.map(|label| label.node),
                                       block,
                                       |loop_scope| loop_scope.break_block)
            }
            ast::ExprRet(Some(_)) => {
                self.cx.span_bug(expr.span, "cannot return a value");
            }
            ast::ExprRet(None) => {
                self.exit_scope(expr.span, extent, block, END_BLOCK);
                self.cfg.start_new_block(Some("AfterReturn"))
            }
            ast::ExprIf(ref cond_expr, ref then_expr, ref else_expr) => {
                // FIXME: This does not handle the `cond_expr` containing a transition yet.
                
                let mut then_block = self.cfg.start_new_block(Some("Then"));
                let mut else_block = self.cfg.start_new_block(Some("Else"));

                self.cfg.terminate(block, Terminator::If {
                    cond: cond_expr.clone(),
                    targets: (then_block, else_block),
                });

                then_block = self.into(extent, then_block, then_expr);
                else_block = self.into(extent, else_block, else_expr);

                let join_block = self.cfg.start_new_block(Some("Join"));
                self.cfg.terminate(then_block, Terminator::Goto { target: join_block });
                self.cfg.terminate(else_block, Terminator::Goto { target: join_block });

                join_block
            }
            ast::ExprLoop(ref body, label) => {
                self.expr_loop(extent, block, None, body, label)
            }
            ast::ExprWhile(ref cond_expr, ref body, label) => {
                self.expr_loop(extent, block, Some(cond_expr), body, label)
            }
            _ => {
                self.into(extent, block, expr.clone())
            }
        }
    }

    fn expr_loop(&mut self,
                 extent: CodeExtent,
                 block: BasicBlock,
                 condition: Option<&P<ast::Expr>>,
                 body: &P<ast::Block>,
                 label: Option<ast::Ident>) -> BasicBlock {
        // [block] --> [loop_block] ~~> [loop_block_end] -1-> [exit_block]
        //                  ^                  |
        //                  |                  0
        //                  |                  |
        //                  |                  v
        //           [body_block_end] <~~~ [body_block]
        //
        // If `opt_cond_expr` is `None`, then the graph is somewhat simplified:
        //
        // [block] --> [loop_block / body_block ] ~~> [body_block_end]    [exit_block]
        //                         ^                          |
        //                         |                          |
        //                         +--------------------------+

        let loop_block = self.cfg.start_new_block(Some("Loop"));
        let exit_block = self.cfg.start_new_block(Some("LoopExit"));

        // start the loop
        self.cfg.terminate(block, Terminator::Goto { target: loop_block });

        self.in_loop_scope(extent, label, loop_block, exit_block, |this| {
            // conduct the test, if necessary
            let body_block;
            if let Some(cond_expr) = condition {
                // FIXME: This does not yet handle the expr having a transition.
                
                body_block = this.cfg.start_new_block(Some("LoopBody"));

                this.cfg.terminate(loop_block, Terminator::If {
                    cond: cond_expr.clone(),
                    targets: (body_block, exit_block),
                });
            } else {
                body_block = loop_block;
            }

            // execute the body, branching back to the test
            let body_block_end = this.into(extent, body_block, body);
            this.cfg.terminate(body_block_end, Terminator::Goto { target: loop_block });

            // final point is exit_block
            exit_block
        })
    }

    fn break_or_continue<F>(&mut self,
                            span: Span,
                            label: Option<ast::Ident>,
                            block: BasicBlock,
                            exit_selector: F)
                            -> BasicBlock
        where F: FnOnce(&LoopScope) -> BasicBlock
    {
        let loop_scope = self.find_loop_scope(span, label);
        let exit_block = exit_selector(&loop_scope);
        self.exit_scope(span, loop_scope.extent, block, exit_block);
        self.cfg.start_new_block(Some("AfterBreakOrContinue"))
    }
}
