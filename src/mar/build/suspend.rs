use aster::AstBuilder;
use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Generate code to suspend the coroutine.
    pub fn expr_suspend(&mut self,
                        destination: Lvalue,
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
                             block: BasicBlock,
                             expr: P<ast::Expr>) -> BasicBlock {
        let lvalue = self.cfg.temp_lvalue(expr.span, Some("suspend"));
        self.expr_suspend(lvalue, block, expr)
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
                      block: BasicBlock,
                      expr: P<ast::Expr>) -> BasicBlock {
        let expr = self.expand_moved(&expr);
        let expr = AstBuilder::new().span(expr.span).expr()
            .some()
            .build(expr);

        self.expr_suspend(destination, block, expr)
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
    /// 'await_loop: {
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
                      mut block: BasicBlock,
                      expr: P<ast::Expr>) -> BasicBlock {
        let expr = self.expand_moved(&expr);
        let span = expr.span;
        let builder = AstBuilder::new().span(span);

        // Declare a variable which will store the awaited future.
        let future_ident = builder.id("future");

        let future_stmt = builder.stmt()
            .let_().mut_id(future_ident).build_expr(expr);

        block = self.stmt(extent, block, &future_stmt);

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

        let loop_block = self.start_new_block(span, Some("AwaitLoop"));
        let exit_block = self.start_new_block(span, Some("AwaitExit"));

        self.terminate(span, block, TerminatorKind::Goto {
            target: loop_block,
            end_scope: true,
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

        let ready_arm = Arm {
            pats: vec![ready_pat],
            guard: None,
            block: self.start_new_block(span, Some("AwaitReady")),
        };

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

        self.terminate(span, not_ready_arm.block, TerminatorKind::Await {
            target: loop_block,
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
    }
}
