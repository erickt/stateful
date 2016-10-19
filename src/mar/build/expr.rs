use aster::AstBuilder;
use aster::ident::ToIdent;
use mar::build::Builder;
use mar::build::scope::LoopScope;
use mar::repr::*;
use syntax::ast::{self, ExprKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn expr(&mut self,
                destination: Lvalue,
                extent: CodeExtent,
                block: BasicBlock,
                expr: &P<ast::Expr>) -> BasicBlock {
        let expr = self.expand_moved(expr);

        // There's no reason for us to transform expressions if they don't contain any transitions.
        if !self.contains_transition(&expr) {
            return self.into(destination, extent, block, expr);
        }

        match expr.node {
            ExprKind::Block(ref ast_block) => {
                self.into(destination, extent, block, ast_block)
            }
            ExprKind::Continue(label) => {
                self.break_or_continue(expr.span,
                                       label.map(|label| label.node),
                                       block,
                                       |loop_scope| loop_scope.continue_block)
            }
            ExprKind::Break(label) => {
                self.break_or_continue(expr.span,
                                       label.map(|label| label.node),
                                       block,
                                       |loop_scope| loop_scope.break_block)
            }
            ExprKind::Ret(ref returned_expr) => {
                self.expr_ret(
                    destination,
                    extent,
                    block,
                    expr.span,
                    returned_expr)
            }
            ExprKind::If(ref cond_expr, ref then_expr, ref else_expr) => {
                self.expr_if(
                    destination,
                    extent,
                    block,
                    expr.span,
                    cond_expr,
                    then_expr,
                    else_expr)
            }
            ExprKind::Match(ref discriminant, ref arms) => {
                let (block, discriminant) = self.expr_temp(
                    extent,
                    block,
                    discriminant,
                    "match_cond");

                self.match_expr(
                    destination,
                    extent,
                    expr.span,
                    block,
                    discriminant.clone(),
                    arms)
            }
            ExprKind::Loop(ref body, label) => {
                let block = self.expr_loop(extent, block, None, body, label);

                // `loop { ... }` has a type of `()`.
                self.assign_lvalue_unit(expr.span, block, destination);

                block
            }
            ExprKind::While(ref cond_expr, ref body, label) => {
                let (block, cond_expr) = self.expr_temp(
                    extent,
                    block,
                    cond_expr,
                    "while_cond");

                let block = self.expr_loop(extent, block, Some(&cond_expr), body, label);

                // `while $expr { ... }` has a type of `()`.
                self.assign_lvalue_unit(expr.span, block, destination);

                block
            }
            ExprKind::ForLoop(..) |
            ExprKind::IfLet(..)   |
            ExprKind::WhileLet(..) => {
                panic!("{:?} Should never reach this point - `preapare` should have desugared this.", expr);
            }
            ExprKind::Assign(ref lvalue, ref rvalue) => {
                // FIXME: We don't try to expand the lvalue, which even though it's an expression,
                // it's only allowed to have certain expressions. This means that `x[0] = 1` is
                // valid, but `{ x[0] } = 1` is an error.
                //
                // However, it's possible the lvalue could have a subexpression, allowing us to
                // write something silly like `x[{ yield_!(1); 0 }] = 1`. Since stateful doesn't
                // yet have the ability to distinguish between valid and invalid lvalues, nor a
                // way to skip generating a temporary lvalue when it's unnecessary, we're just not
                // going to expand lvalue for the moment.
                let (block, rvalue) = self.expr_temp(extent, block, rvalue, "rvalue");

                let lvalue = Lvalue::Var {
                    span: lvalue.span,
                    decl: self.find_lvalue(lvalue),
                };

                self.expr(lvalue, extent, block, &rvalue)
            }
            ExprKind::Mac(ref mac) => {
                match self.expr_mac(destination.clone(), extent, block, mac) {
                    Some(block) => block,
                    None => self.into(destination, extent, block, expr.clone()),
                }
            }
            _ => {
                self.cx.span_bug(expr.span,
                                 &format!("don't know how to handle {:#?} yet", expr))
            }
        }
    }

    /// Expand an expression into a new temporary value. Note that this temporary value is marked
    /// as "moved", so make sure to use it before another potential yield point.
    fn expr_temp<T>(&mut self,
                    extent: CodeExtent,
                    block: BasicBlock,
                    expr: &P<ast::Expr>,
                    name: T) -> (BasicBlock, P<ast::Expr>)
        where T: ToIdent,
    {
        let temp_decl = self.declare_temp(expr.span, name);
        self.schedule_move(temp_decl);

        let temp_lvalue = Lvalue::Var {
            span: expr.span,
            decl: temp_decl,
        };

        let block = self.expr(temp_lvalue, extent, block, expr);

        let temp_expr = AstBuilder::new().span(expr.span).expr()
            .id(self.var_decls[temp_decl].ident);

        (block, temp_expr)
    }

    /// Compile `return $expr` into:
    ///
    /// ```
    /// 'block:
    ///     $return pointer = $expr;
    ///     goto 'exit;
    ///
    /// 'after_return:
    ///     ...
    /// ```
    fn expr_ret(&mut self,
                destination: Lvalue,
                extent: CodeExtent,
                mut block: BasicBlock,
                span: Span,
                returned_expr: &Option<P<ast::Expr>>) -> BasicBlock {
        // Assign the return pointer.
        let return_pointer = self.var_decls[RETURN_POINTER].ident;

        let expr = if let Some(ref returned_expr) = *returned_expr {
            quote_expr!(self.cx, $return_pointer = $returned_expr)
        } else {
            quote_expr!(self.cx, $return_pointer = ())
        };

        block = self.expr(destination, extent, block, &expr);

        // Exit our scope.
        let return_block = self.return_block();
        self.exit_scope(span, extent, block, return_block);

        // We need to start a new block after this one since there might be trailing expressions
        // that we need to type check.
        self.start_new_block(span, Some("AfterReturn"))
    }

    fn expr_if(&mut self,
               destination: Lvalue,
               extent: CodeExtent,
               block: BasicBlock,
               span: Span,
               cond_expr: &P<ast::Expr>,
               then_expr: &P<ast::Block>,
               else_expr: &Option<P<ast::Expr>>) -> BasicBlock {
        self.in_conditional_scope(extent, |this| {
            let (block, cond_expr) = this.expr_temp(
                extent,
                block,
                cond_expr,
                "if_cond");

            let mut then_block = this.start_new_block(span, Some("Then"));
            let mut else_block = this.start_new_block(span, Some("Else"));

            this.terminate(span, block, TerminatorKind::If {
                cond: cond_expr.clone(),
                targets: (then_block, else_block),
            });

            then_block = this.into(destination.clone(), extent, then_block, then_expr);
            else_block = this.into(destination, extent, else_block, else_expr);

            let join_block = this.start_new_block(span, Some("IfJoin"));

            this.terminate(
                then_expr.span,
                then_block,
                TerminatorKind::Goto {
                    target: join_block,
                    end_scope: true,
                });

            this.terminate(
                match *else_expr {
                    Some(ref expr) => expr.span,
                    None => span,
                },
                else_block,
                TerminatorKind::Goto {
                    target: join_block,
                    end_scope: true,
                });

            join_block
        })
    }

    fn expr_loop(&mut self,
                 extent: CodeExtent,
                 block: BasicBlock,
                 condition: Option<&P<ast::Expr>>,
                 body: &P<ast::Block>,
                 label: Option<ast::SpannedIdent>) -> BasicBlock {
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

        self.in_loop_scope(extent, label, loop_block, exit_block, |this| {
            // conduct the test, if necessary
            let body_block;
            if let Some(cond_expr) = condition {
                // FIXME: This does not yet handle the expr having a transition.

                body_block = this.start_new_block(cond_expr.span, Some("LoopBody"));

                this.terminate(
                    cond_expr.span,
                    loop_block,
                    TerminatorKind::If {
                        cond: cond_expr.clone(),
                        targets: (body_block, exit_block),
                    });
            } else {
                body_block = loop_block;
            }

            // execute the body, branching back to the test
            let lvalue = this.declare_temp_lvalue(body.span, "_loop_result_temp");
            let body_block_end = this.into(lvalue, extent, body_block, body);

            this.terminate(
                body.span,
                body_block_end,
                TerminatorKind::Goto {
                    target: loop_block,
                    end_scope: true,
                });

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
        if !self.is_in_loop() {
            self.cx.span_err(span, "cannot break outside of a loop");
        }

        let loop_scope = self.find_loop_scope(span, label);
        let exit_block = exit_selector(&loop_scope);
        self.exit_scope(span, loop_scope.extent, block, exit_block);

        // Even though we've exited `block`, there could be code following the break/continue. To
        // keep rust happy, we'll create a new block that has an edge to `block`, even though
        // control will never actually flow into this block.
        self.start_new_block(span, Some("AfterBreakOrContinue"))
    }

    fn find_lvalue(&mut self, expr: &P<ast::Expr>) -> Var {
        match expr.node {
            ExprKind::Path(None, ref path) => {
                match self.get_decl_from_path(path) {
                    Some(decl) => decl,
                    None => {
                        self.cx.span_bug(
                            expr.span,
                            &format!("Path is not a decl: {:?}", path))
                    }
                }
            }
            _ => {
                self.cx.span_bug(
                    expr.span,
                    &format!("Cannot handle `{:?}` as an lvalue yet", expr))
            }
        }
    }
}
