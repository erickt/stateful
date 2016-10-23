use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Generate code to suspend the coroutine.
    pub fn expr_suspend(&mut self,
                        destination: Lvalue,
                        _extent: CodeExtent,
                        block: BasicBlock,
                        expr: P<ast::Expr>) -> BasicBlock {
        let next_block = self.start_new_block(expr.span, Some("AfterSuspend"));

        // We don't yet support receiving values into the coroutine yet, so just store a `()` in
        // the destination.
        self.assign_lvalue_unit(expr.span, next_block, destination);

        self.terminate(expr.span, block, TerminatorKind::Suspend {
            rvalue: expr,
            target: next_block,
        });

        next_block
    }

    pub fn expr_suspend_unit(&mut self,
                             extent: CodeExtent,
                             block: BasicBlock,
                             expr: P<ast::Expr>) -> BasicBlock {
        let lvalue = self.declare_temp_lvalue(expr.span, "suspend");
        self.expr_suspend(lvalue, extent, block, expr)
    }

    /// Compile `yield_!($expr)` into:
    ///
    /// ```
    /// 'before: {
    ///     ...
    ///     let () = suspend!(Some($expr));
    ///     goto!('after);
    /// }
    ///
    /// 'after: {
    ///     ...
    /// }
    /// ```
    pub fn expr_yield(&mut self,
                      destination: Lvalue,
                      extent: CodeExtent,
                      block: BasicBlock,
                      expr: P<ast::Expr>) -> BasicBlock {
        let expr = quote_expr!(
            self.cx,
            suspend!(Some($expr))
        );

        self.expr(destination, extent, block, &expr)

        /*
        let expr = AstBuilder::new().span(expr.span).expr()
            .some()
            .build(expr);

        self.expr_suspend(destination, block, expr)
        */
    }

    /// Compile `$result = await!($expr)` into:
    ///
    /// ```
    /// 'before: {
    ///     ...
    ///     let future = $expr;
    ///     goto!('await_loop);
    /// }
    ///
    /// let result;
    ///
    /// loop {
    ///     match future.poll() {
    ///         Ok(Async::NotReady) => {
    ///             let () = suspend!(Ok(Async::NotReady));
    ///             goto!('await_loop);
    ///         }
    ///         Ok(Async::Ready(result)) => {
    ///             $result = Ok(result);
    ///             goto!('await_exit);
    ///         }
    ///         Err(err) => {
    ///             $result = Err(err);
    ///             goto!('await_exit);
    ///         }
    ///     }
    /// }
    ///
    /// 'await_exit: {
    ///     ...
    /// }
    /// ```
    pub fn expr_await(&mut self,
                      destination: Lvalue,
                      extent: CodeExtent,
                      block: BasicBlock,
                      future_expr: P<ast::Expr>) -> BasicBlock {
        self.in_scope(extent, future_expr.span, block, |this| {
            let (block, _temp_var, future_expr) = this.expr_temp(
                extent,
                block,
                &future_expr,
                "temp_future");

            let expr = quote_expr!(this.cx,
                {
                    let result;

                    //loop {
                        let thing2 = match ::futures::Future::poll(&mut $future_expr) {
                            /*
                            ::std::result::Result::Ok(::futures::Async::NotReady) => {
                                let thing = suspend!(::futures::Async::NotReady);
                            }
                            */
                            ::std::result::Result::Ok(::futures::Async::Ready(ok)) => {
                                result = ::std::result::Result::Ok(ok);
                                //break;
                            }

                            /*
                            ::std::result::Result::Err(err) => {
                                result = ::std::result::Result::Err(moved!(err));
                                break;
                            }
                            */
                        };
                    //}

                    result
                }
            );

            this.expr(destination, extent, block, &expr)
        })

        /*
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

        self.in_loop_scope(extent, None, loop_block, exit_block, |this| {
            // execute the body, branching back to the test
            let body_block_end = this.into(lvalue, extent, body_block, body);

            this.terminate(
                body.span,
                body_block_end,
                TerminatorKind::Goto {
                    target: loop_block,
                    end_scope: true,
                });

            exit_block
        })
    }

    fn expr_await_loop(&mut self,
                       destination: Lvalue,
                       extent: CodeExtent,
                       block: BasicBlock,
                       future_expr: P<ast::Expr>) -> BasicBlock {
        let future_expr = self.expand_moved(&future_expr);
        let span = future_expr.span;
        let builder = AstBuilder::new().span(span);

        let (block, future_expr) = self.expr_temp(
            extent,
            block,
            &future_expr,
            "future");

        // Next, construct a match that polls the future until it's ready:
        //
        // match try!(sleeper.poll()) {
        //     Async::Ready(result) => {
        //         goto $next_state with result
        //     }
        //     Async::NotReady => {
        //         return Async::NotReady
        //     }
        // }

        // Handle the not ready arm.
        let not_ready_path = builder.path()
            .global()
            .ids(&["futures", "Async", "NotReady"])
            .build();

        let not_ready_pat = builder.pat().path()
            .build(not_ready_path);

        let not_ready_arm = Arm {
            pats: vec![not_ready_pat],
            guard: None,
            block: self.start_new_block(span, Some("AwaitNotReady")),
        };

        // Handle the ready arm.
        let ready_ident = builder.id("result");

        let ready_path = builder.path()
            .global()
            .ids(&["futures", "Async", "Ready"])
            .build();

        let ready_pat = builder.pat().enum_()
            .build(ready_path)
            .id(ready_ident)
            .build();

        let ready_arm = Arm {
            pats: vec![ready_pat],
            guard: None,
            block: self.start_new_block(span, Some("AwaitReady")),
        };

        let mut arm_blocks = vec![];

        self.in_conditional_scope(extent, |this| {
            this.next_conditional_scope();

            let arm_block = this.in_scope(extent, span, block, |this| {
                this.expr(
                    destination.clone(),
                    extent,
                    not_ready_arm.block, 
                    &builder.expr().unit())
            });

            arm_blocks.push(arm_block);

            ///////////////////////////////////////////////////////////////////

            this.next_conditional_scope();

            let arm_block = this.in_scope(extent, span, block, |this| {
                this.expr(
                    destination.clone(),
                    extent,
                    ready_arm.block, 
                    &builder.expr().unit())
            });

            arm_blocks.push(arm_block);
        });

        let join_block = self.start_new_block(span, Some("MatchJoin"));

        for arm_block in arm_blocks {
            self.terminate(
                span,
                arm_block,
                TerminatorKind::Goto { target: join_block, end_scope: true });
        }

        // Finally, handle the match.
        let poll_path = builder.path()
            .global()
            .ids(&["futures", "Future", "poll"])
            .build();

        let future_poll_expr = builder.expr()
            .try()
            .call()
            .path().build(poll_path)
            .arg()
            .mut_ref().build(future_expr)
            .build();

        self.terminate(span, block, TerminatorKind::Match {
            discr: future_poll_expr,
            targets: vec![not_ready_arm, ready_arm],
        });

        join_block


        /*
        let loop_block = self.start_new_block(span, Some("AwaitLoop"));
        let exit_block = self.start_new_block(span, Some("AwaitExit"));

        let mut not_ready_block = self.start_new_block(span, Some("AwaitNotReady"));
        let mut ready_block = self.start_new_block(span, Some("AwaitReady"));

        self.terminate(span, block, TerminatorKind::Goto {
            target: loop_block,
            end_scope: true,
        });

        // Handle the not ready arm.
        let not_ready_path = builder.path()
            .global()
            .ids(&["futures", "Async", "NotReady"])
            .build();

        let not_ready_pat = builder.pat().path()
            .build(not_ready_path);


        self.terminate(span, not_ready_arm.block, TerminatorKind::Await {
            target: loop_block,
        });

        // Handle the ready arm.
        let ready_ident = builder.id("result");

        let ready_path = builder.path()
            .global()
            .ids(&["futures", "Async", "Ready"])
            .build();

        let ready_pat = builder.pat().enum_()
            .build(ready_path)
            .id(ready_ident)
            .build();

        let ready_arm_block = self.in_scope(extent, span, block, |this| {
            this.add_decls_from_pats(ready_arm.block, ready_arm.pats.iter());

            // Don't try to store the result if we're just writing into a temporary.
            if destination.is_temp() {
                ready_arm.block
            } else {
                let ready_expr = builder.expr().id(ready_ident);
                this.expr(destination, extent, ready_arm.block, &ready_expr)
            }
        });

        self.terminate(span, ready_arm_block, TerminatorKind::Goto {
            target: exit_block,
            end_scope: true,
        });

        // Finally, handle the match.
        let poll_path = builder.path()
            .global()
            .ids(&["futures", "Future", "poll"])
            .build();

        let future_poll_expr = builder.expr()
            .try()
            .call()
            .path().build(poll_path)
            .arg()
            .mut_ref().id(future_ident)
            .build();

        self.terminate(span, loop_block, TerminatorKind::Match {
            discr: future_poll_expr,
            targets: vec![ready_arm, not_ready_arm],
        });

        exit_block
        */
        */
    }
}
