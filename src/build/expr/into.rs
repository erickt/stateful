use build::{BlockAnd, BlockAndExtension, Builder};
use build::expr::category::{Category, RvalueFunc};
use mir::*;
use syntax::ast::{self, ExprKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn into_expr(&mut self,
                     destination: Lvalue,
                     mut block: BasicBlock,
                     expr: &P<ast::Expr>) -> BlockAnd<()> {
        debug!("into_expr(destination={:?}, block={:?}, expr={:?})", destination, block, expr);

        let this = self;
        let expr_span = expr.span;
        let source_info = this.source_info(expr_span);

        match expr.node {
            ExprKind::Block(ref ast_block) => {
                this.ast_block(destination, block, ast_block)
            }
            ExprKind::If(ref cond_expr, ref then_expr, ref else_expr) => {
                this.expr_if(
                    destination,
                    block,
                    expr.span,
                    cond_expr,
                    then_expr,
                    else_expr)
            }
            ExprKind::Match(ref discriminant, ref arms) => {
                this.match_expr(
                    destination,
                    expr.span,
                    block,
                    discriminant.clone(),
                    arms)
            }
            ExprKind::Loop(ref body, label) => {
                this.expr_loop(
                    destination,
                    block,
                    source_info,
                    None,
                    body,
                    label)
            }
            ExprKind::While(ref cond_expr, ref body, label) => {
                this.expr_loop(
                    destination,
                    block,
                    source_info,
                    Some(&cond_expr),
                    body,
                    label)
            }
            ExprKind::ForLoop(..) |
            ExprKind::IfLet(..)   |
            ExprKind::WhileLet(..) => {
                panic!("{:?} Should never reach this point - `desugar` should have desugared this.", expr);
            }
            ExprKind::Mac(ref mac) => {
                match this.expr_mac(destination.clone(), block, mac) {
                    Some(block) => block,
                    None => {
                        let rvalue = unpack!(block = this.as_rvalue(block, expr));
                        this.cfg.push_assign(block, source_info, &destination, rvalue);
                        block.unit()
                    }
                }
            }
            ExprKind::Call(ref func, ref args) => {
                let func = unpack!(block = this.as_operand(block, func));
                let args = args.into_iter()
                    .map(|arg| unpack!(block = this.as_rvalue(block, arg)))
                    .collect::<Vec<_>>();

                this.cfg.push(block, Statement {
                    source_info: source_info,
                    kind: StatementKind::Call {
                        destination: destination,
                        func: func,
                        args: args,
                    },
                });
                block.unit()
            }
            ExprKind::MethodCall(ref ident, ref tys, ref args) => {
                let mut args = args.into_iter();

                let self_ = args.next().unwrap();
                let self_ = unpack!(block = this.as_operand(block, self_));

                let args = args
                    .map(|arg| unpack!(block = this.as_rvalue(block, arg)))
                    .collect::<Vec<_>>();

                this.cfg.push(block, Statement {
                    source_info: source_info,
                    kind: StatementKind::MethodCall {
                        destination: destination,
                        ident: *ident,
                        tys: tys.clone(),
                        self_: self_,
                        args: args,
                    }
                });
                block.unit()
            }

            // These cases don't actually need a destination
            ExprKind::Assign(..) |
            ExprKind::AssignOp(..) |
            ExprKind::Continue(..) |
            ExprKind::Break(..) |
            ExprKind::Ret(..) => {
                // As a difference from MIR, we need to make sure the destination is actually
                // assigned.
                this.cfg.push_assign_unit(block, source_info, &destination);
                this.stmt_expr(block, expr)
            }

            // these are the cases that are more naturally handled by some other mode
            ExprKind::Unary(..) |
            ExprKind::Binary(..) |
            ExprKind::Box(..) |
            ExprKind::Cast(..) |
            ExprKind::Repeat(..) |
            ExprKind::Path(..) |
            ExprKind::Array(..) |
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

                let rvalue = unpack!(block = this.as_rvalue(block, expr));
                this.cfg.push_assign(block, source_info, &destination, rvalue);
                block.unit()
            }

            ExprKind::InPlace(..) |
            ExprKind::Type(..) |
            ExprKind::Try(..) |
            ExprKind::Paren(..) => {
                this.cx.span_bug(expr_span,
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
        let source_info = self.source_info(span);

        let operand = unpack!(block = self.as_operand(block, cond_expr));

        let mut then_block = self.cfg.start_new_block(span, Some("Then"));
        let mut else_block = self.cfg.start_new_block(span, Some("Else"));
        self.terminate(span, block, TerminatorKind::If {
            cond: operand,
            targets: (then_block, else_block),
        });

        then_block = unpack!(self.ast_block(destination.clone(), then_block, then_expr));

        else_block = if let Some(ref else_expr) = *else_expr {
            unpack!(self.into(destination, else_block, else_expr))
        } else {
            // Body of the `if` expression without an `else` clause must return `()`, thus
            // we implicitly generate a `else {}` if it is not specified.
            self.cfg.push_assign_unit(else_block, source_info, &destination);
            else_block
        };

        let join_block = self.cfg.start_new_block(span, Some("IfJoin"));

        self.terminate(span, then_block, TerminatorKind::Goto { target: join_block });
        self.terminate(span, else_block, TerminatorKind::Goto { target: join_block });

        join_block.unit()
    }

    fn expr_loop(&mut self,
                 destination: Lvalue,
                 block: BasicBlock,
                 source_info: SourceInfo,
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

        let this = self;

        let loop_block = this.cfg.start_new_block(source_info.span, Some("Loop"));
        let exit_block = this.cfg.start_new_block(source_info.span, Some("LoopExit"));

        // start the loop
        this.cfg.terminate(block, source_info,
                           TerminatorKind::Goto { target: loop_block });

        this.in_loop_scope(
            label, loop_block, exit_block, destination.clone(),
            move |this| {
                // conduct the test, if necessary
                let body_block;
                if let Some(cond_expr) = condition {
                    let loop_block_end;
                    let cond = unpack!(
                        loop_block_end = this.as_operand(loop_block, cond_expr));
                    body_block = this.cfg.start_new_block(cond_expr.span, Some("LoopBody"));
                    this.cfg.terminate(loop_block_end, source_info,
                                       TerminatorKind::If {
                                           cond: cond,
                                           targets: (body_block, exit_block)
                                       });

                    // if the test is false, there's no `break` to assign `destination`, so
                    // we have to do it; this overwrites any `break`-assigned value but it's
                    // always `()` anyway
                    this.cfg.push_assign_unit(exit_block, source_info, &destination);
                } else {
                    body_block = loop_block;
                }

                // The "return" value of the loop body must always be an unit. We therefore
                // introduce a unit temporary as the destination for the loop body.
                let tmp = this.temp(body_block, body.span, "temp_loop");
                // Execute the body, branching back to the test.
                let body_block_end = unpack!(this.into(tmp, body_block, body));
                this.cfg.terminate(body_block_end, source_info,
                                   TerminatorKind::Goto { target: loop_block });
            }
        );
        exit_block.unit()

        /*
        let might_break = this.in_loop_scope(
            label, loop_block, exit_block,
            move |this| {
                // conduct the test, if necessary
                let body_block;
                if let Some(cond_expr) = condition {
                    // This loop has a condition, ergo its exit_block is reachable.
                    this.find_loop_scope(source_info.span, None).might_break = true;

                    let loop_block_end;
                    let cond = unpack!(loop_block_end = this.as_operand(loop_block, cond_expr));
                    body_block = this.cfg.start_new_block(cond_expr.span, Some("LoopBody"));

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

                /*
                let extent = this.start_new_extent();
                let body_block_end = unpack!(this.in_scope(extent, source_info.span, loop_block, |this| {
                    // conduct the test, if necessary
                    let body_block;
                    if let Some(cond_expr) = condition {
                        // This loop has a condition, ergo its exit_block is reachable.
                        this.find_loop_scope(source_info.span, None).might_break = true;

                        let loop_block_end;
                        let cond = unpack!(loop_block_end = this.as_operand(loop_block, cond_expr));
                        body_block = this.cfg.start_new_block(cond_expr.span, Some("LoopBody"));

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

                    // The “return” value of the loop body must always be an unit, but we cannot
                    // reuse that as a “return” value of the whole loop expressions, because some
                    // loops are diverging (e.g. `loop {}`). Thus, we introduce a unit temporary as
                    // the destination for the loop body and assign the loop’s own “return” value
                    // immediately after the iteration is finished.
                    let tmp = this.temp(body_block, body.span, "temp_loop");
                    // Execute the body, branching back to the test.
                    this.ast_block(tmp, body_block, body)
                }));

                this.terminate(body.span, body_block_end, TerminatorKind::Goto { target: loop_block });
                */
            }
        );

        // If the loop may reach its exit_block, we assign an empty tuple to the
        // destination to keep the MIR well-formed.
        if might_break {
            this.cfg.push_assign_unit(exit_block, source_info, &destination);
        }
        exit_block.unit()
        */
    }
}
