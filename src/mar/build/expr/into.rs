use aster::AstBuilder;
use aster::ident::ToIdent;
use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::build::expr::category::{Category, RvalueFunc};
use mar::build::scope::LoopScope;
use mar::repr::*;
use syntax::ast::{self, ExprKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn into_expr(&mut self,
                     destination: Lvalue,
                     mut block: BasicBlock,
                     expr: &P<ast::Expr>) -> BlockAnd<()> {
        let expr_span = expr.span;

        match expr.node {
            ExprKind::Block(ref ast_block) => {
                self.ast_block(destination, block, ast_block)
            }
            ExprKind::If(ref cond_expr, ref then_expr, ref else_expr) => {
                self.expr_if(
                    destination,
                    block,
                    expr.span,
                    cond_expr,
                    then_expr,
                    else_expr)
            }
            ExprKind::Match(ref discriminant, ref arms) => {
                self.expr_match(
                    destination,
                    expr.span,
                    block,
                    discriminant.clone(),
                    arms)
            }
            ExprKind::Loop(ref body, label) => {
                self.expr_loop(
                    destination,
                    block,
                    expr.span,
                    None,
                    body,
                    label)
            }
            ExprKind::While(ref cond_expr, ref body, label) => {
                self.expr_loop(
                    destination,
                    block,
                    expr.span,
                    Some(&cond_expr),
                    body,
                    label)
            }
            ExprKind::ForLoop(..) |
            ExprKind::IfLet(..)   |
            ExprKind::WhileLet(..) => {
                panic!("{:?} Should never reach this point - `preapare` should have desugared this.", expr);
            }
            ExprKind::Mac(ref mac) => {
                match self.expr_mac(destination.clone(), block, mac) {
                    Some(block) => block,
                    None => self.into(destination, block, expr.clone()),
                }
            }
            ExprKind::Call(ref fun, ref args) => {
                let fun = unpack!(block = self.as_operand(block, fun));
                let fun = self.operand_to_expr(&fun);

                let call_expr = {
                    let args = args.into_iter()
                        .map(|arg| {
                            let arg = unpack!(block = self.as_operand(block, arg));
                            self.operand_to_expr(&arg)
                        });

                    AstBuilder::new().span(expr_span).expr()
                        .call().build(fun)
                        .with_args(args)
                        .build()
                };

                self.cfg.push_assign(block, destination, call_expr);
                block.unit()
            }
            ExprKind::MethodCall(ref name, ref tys, ref args) => {
                let method_call_expr = {
                    let mut args = args.into_iter()
                        .map(|arg| {
                            let arg = unpack!(block = self.as_operand(block, arg));
                            self.operand_to_expr(&arg)
                        });

                    AstBuilder::new().expr()
                        .span(name.span).method_call(name.node)
                        .span(expr_span).build(args.next().unwrap())
                        .with_tys(tys.clone())
                        .with_args(args)
                        .build()
                };

                self.cfg.push_assign(block, destination, method_call_expr);
                block.unit()
            }

            // These cases don't actually need a destination
            ExprKind::Assign(..) |
            ExprKind::AssignOp(..) |
            ExprKind::Continue(..) |
            ExprKind::Break(..) |
            ExprKind::Ret(..) => {
                self.stmt_expr(block, expr)
            }

            // these are the cases that are more naturally handled by some other mode
            ExprKind::Unary(..) |
            ExprKind::Binary(..) |
            ExprKind::Box(..) |
            ExprKind::Cast(..) |
            ExprKind::Repeat(..) |
            ExprKind::Path(..) |
            ExprKind::Vec(..) |
            ExprKind::Tup(..) |
            ExprKind::Struct(..) |
            ExprKind::Range(..) |
            ExprKind::Closure(..) |
            ExprKind::Index(..) |
            ExprKind::AddrOf(..) |
            ExprKind::Lit(..) |
            ExprKind::InlineAsm(..) |
            ExprKind::Field(..) |
            ExprKind::TupField(..) => {
                debug_assert!(match Category::of(&expr.node).unwrap() {
                    Category::Rvalue(RvalueFunc::Into) => false,
                    _ => true,
                });

                let rvalue = unpack!(block = self.as_rvalue(block, expr));
                self.cfg.push_assign(block, destination, rvalue);
                block.unit()
            }

            ExprKind::InPlace(..) |
            ExprKind::Type(..) |
            ExprKind::Try(..) |
            ExprKind::Paren(..) => {
                self.cx.span_bug(expr_span,
                                 &format!("not yet supported: {:?}", expr));
            }
        }
    }

    fn expr_if(&mut self,
               destination: Lvalue,
               mut block: BasicBlock,
               span: Span,
               cond_expr: &P<ast::Expr>,
               then_expr: &P<ast::Block>,
               else_expr: &Option<P<ast::Expr>>) -> BlockAnd<()> {
        let operand = unpack!(block = self.as_operand(block, cond_expr));

        let mut then_block = self.start_new_block(span, Some("Then"));
        let mut else_block = self.start_new_block(span, Some("Else"));
        self.terminate(span, block, TerminatorKind::If {
            cond: operand,
            targets: (then_block, else_block),
        });

        self.in_conditional_scope(span, |this| {
            this.next_conditional_scope();

            then_block = unpack!(this.ast_block(destination.clone(), then_block, then_expr));

            this.next_conditional_scope();

            else_block = if let Some(ref else_expr) = *else_expr {
                unpack!(this.into(destination, else_block, else_expr))
            } else {
                // Body of the `if` expression without an `else` clause must return `()`, thus
                // we implicitly generate a `else {}` if it is not specified.
                this.assign_lvalue_unit(span, else_block, destination);
                else_block
            };
        });

        let join_block = self.start_new_block(span, Some("IfJoin"));

        self.terminate(
            span,
            then_block,
            TerminatorKind::Goto {
                target: join_block,
                end_scope: true,
            });

        self.terminate(
            span,
            else_block,
            TerminatorKind::Goto {
                target: join_block,
                end_scope: true,
            });

        join_block.unit()
    }

    fn expr_loop(&mut self,
                 destination: Lvalue,
                 block: BasicBlock,
                 span: Span,
                 condition: Option<&P<ast::Expr>>,
                 body: &P<ast::Block>,
                 label: Option<ast::SpannedIdent>) -> BlockAnd<()> {
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

        // `loop { ... }` has a type of `()`.
        self.assign_lvalue_unit(span, block, destination);

        let loop_block = self.start_new_block(body.span, Some("Loop"));
        let exit_block = self.start_new_block(body.span, Some("LoopExit"));

        // start the loop
        self.terminate(
            body.span,
            block,
            TerminatorKind::Goto {
                target: loop_block,
                end_scope: false,
            });

        self.in_loop_scope(label, loop_block, exit_block, |this| {
            // conduct the test, if necessary
            let body_block;
            if let Some(cond_expr) = condition {
                // FIXME: This does not yet handle the expr having a transition.

                let loop_block_end;
                let cond = unpack!(loop_block_end = this.as_operand(loop_block, cond_expr));
                body_block = this.start_new_block(cond_expr.span, Some("LoopBody"));

                this.terminate(
                    cond_expr.span,
                    loop_block_end,
                    TerminatorKind::If {
                        cond: cond,
                        targets: (body_block, exit_block),
                    });
            } else {
                body_block = loop_block;
            }

            // execute the body, branching back to the test
            let body_block_end = unpack!(
                this.in_scope(body.span, body_block, |this| {
                    let lvalue = this.declare_temp_lvalue(body.span, "temp_loop");
                    this.ast_block(lvalue, body_block, body)
                })
            );

            this.terminate(
                body.span,
                body_block_end,
                TerminatorKind::Goto {
                    target: loop_block,
                    end_scope: true,
                });
        });

        let live_decls = self.find_live_decls();
        self.cfg.block_data_mut(exit_block).decls = live_decls.clone();

        exit_block.unit()
    }

    fn break_or_continue<F>(&mut self,
                            destination: Lvalue,
                            span: Span,
                            label: Option<ast::Ident>,
                            block: BasicBlock,
                            exit_selector: F)
                            -> BasicBlock
        where F: FnOnce(&LoopScope) -> BasicBlock
    {
        if !self.is_in_loop() {
            self.cx.span_err(span, "cannot break outside of a loop");
        }

        // `break` or `continue` has a type of `()`.
        self.assign_lvalue_unit(span, block, destination);

        let loop_scope = self.find_loop_scope(span, label);
        let exit_block = exit_selector(&loop_scope);
        self.exit_scope(span, loop_scope.extent, block, exit_block);

        // Even though we've exited `block`, there could be code following the break/continue. To
        // keep rust happy, we'll create a new block that has an edge to `block`, even though
        // control will never actually flow into this block.
        self.start_new_block(span, Some("AfterBreakOrContinue"))
    }
}
