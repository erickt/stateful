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
                self.break_or_continue(destination,
                                       expr.span,
                                       label.map(|label| label.node),
                                       block,
                                       |loop_scope| loop_scope.continue_block)
            }
            ExprKind::Break(label) => {
                self.break_or_continue(destination,
                                       expr.span,
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
                self.expr_match(
                    destination,
                    extent,
                    expr.span,
                    block,
                    discriminant.clone(),
                    arms)
            }
            ExprKind::Loop(ref body, label) => {
                self.expr_loop(
                    destination,
                    extent,
                    block,
                    expr.span,
                    None,
                    body,
                    label)
            }
            ExprKind::While(ref cond_expr, ref body, label) => {
                let (block, _temp_var, cond_expr) = self.expr_temp(
                    extent,
                    block,
                    cond_expr,
                    "temp_while_cond");

                self.expr_loop(
                    destination,
                    extent,
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
            ExprKind::Assign(ref lvalue, ref rvalue) => {
                // `$rvalue = $lvalue` has a type of `()`.
                self.assign_lvalue_unit(expr.span, block, destination);

                // FIXME: We don't try to expand the lvalue, which even though it's an expression,
                // it's only allowed to have certain expressions. This means that `x[0] = 1` is
                // valid, but `{ x[0] } = 1` is an error.
                //
                // However, it's possible the lvalue could have a subexpression, allowing us to
                // write something silly like `x[{ yield_!(1); 0 }] = 1`. Since stateful doesn't
                // yet have the ability to distinguish between valid and invalid lvalues, nor a
                // way to skip generating a temporary lvalue when it's unnecessary, we're just not
                // going to expand lvalue for the moment.
                let (mut block, temp_var, rvalue) = self.expr_temp(
                    extent,
                    block,
                    rvalue,
                    "temp_rvalue");

                let lvalue = Lvalue::Var {
                    span: lvalue.span,
                    decl: self.find_lvalue(lvalue),
                };

                block = self.expr(lvalue, extent, block, &rvalue);

                // We've assigned the rvalue, so mark the temporary moved.
                self.schedule_move(expr.span, temp_var);

                block
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

    /// Expand an expression into a new temporary value.
    pub fn expr_temp<T>(&mut self,
                        extent: CodeExtent,
                        block: BasicBlock,
                        expr: &P<ast::Expr>,
                        name: T) -> (BasicBlock, Var, P<ast::Expr>)
        where T: ToIdent,
    {
        let temp_var = self.declare_temp(expr.span, name);

        let temp_lvalue = Lvalue::Var {
            span: expr.span,
            decl: temp_var,
        };

        let block = self.expr(temp_lvalue, extent, block, expr);

        let temp_expr = AstBuilder::new().span(expr.span).expr()
            .id(self.var_decls[temp_var].ident);

        (block, temp_var, temp_expr)
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
        let (block, temp_var, cond_expr) = self.expr_temp(
            extent,
            block,
            cond_expr,
            "if_cond");

        let mut then_block = self.start_new_block(span, Some("Then"));
        let mut else_block = self.start_new_block(span, Some("Else"));

        self.schedule_move(cond_expr.span, temp_var);

        self.terminate(span, block, TerminatorKind::If {
            cond: cond_expr,
            targets: (then_block, else_block),
        });

        self.in_conditional_scope(span, extent, |this| {
            this.next_conditional_scope();

            then_block = this.into(destination.clone(), extent, then_block, then_expr);

            this.next_conditional_scope();

            else_block = match *else_expr {
                Some(ref else_expr) => this.into(destination, extent, else_block, else_expr),
                None => {
                    this.assign_lvalue_unit(span, else_block, destination);
                    else_block
                }
            };
        });

        let join_block = self.start_new_block(span, Some("IfJoin"));

        self.terminate(
            then_expr.span,
            then_block,
            TerminatorKind::Goto {
                target: join_block,
                end_scope: true,
            });

        self.terminate(
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
    }

    fn expr_loop(&mut self,
                 destination: Lvalue,
                 extent: CodeExtent,
                 block: BasicBlock,
                 span: Span,
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

        let block = self.in_loop_scope(extent, label, loop_block, exit_block, |this| {
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
            let body_block_end = this.in_scope(extent, body.span, body_block, |this| {
                let lvalue = this.declare_temp_lvalue(body.span, "temp_loop");
                this.into(lvalue, extent, body_block, body)
            });

            this.terminate(
                body.span,
                body_block_end,
                TerminatorKind::Goto {
                    target: loop_block,
                    end_scope: true,
                });

            // final point is exit_block
            exit_block
        });

        // `loop { ... }` has a type of `()`.
        self.assign_lvalue_unit(span, block, destination);

        block
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

        let loop_scope = self.find_loop_scope(span, label);
        let exit_block = exit_selector(&loop_scope);
        self.exit_scope(span, loop_scope.extent, block, exit_block);

        // Even though we've exited `block`, there could be code following the break/continue. To
        // keep rust happy, we'll create a new block that has an edge to `block`, even though
        // control will never actually flow into this block.
        let block = self.start_new_block(span, Some("AfterBreakOrContinue"));

        // `break` or `continue` has a type of `()`.
        self.assign_lvalue_unit(span, block, destination);

        block
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
