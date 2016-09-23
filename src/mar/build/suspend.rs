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
        let expr = AstBuilder::new().span(expr.span).expr()
            .some()
            .build(expr);

        self.expr_suspend(destination, block, expr)
    }
}
