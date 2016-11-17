use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::repr::*;
use syntax::ast;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Generate code to suspend the coroutine.
    pub fn expr_suspend(&mut self,
                        destination: Lvalue,
                        mut block: BasicBlock,
                        rvalue: P<ast::Expr>) -> BlockAnd<()> {
        let rvalue_span = rvalue.span;

        let rvalue = unpack!(block = self.as_rvalue(block, &rvalue));
        let next_block = self.start_new_block(rvalue_span, Some("AfterSuspend"));

        self.terminate(rvalue_span, block, TerminatorKind::Suspend {
            rvalue: rvalue,
            target: next_block,
        });

        // We don't yet support receiving values into the coroutine yet, so just store a `()` in
        // the destination.
        self.push_assign_unit(rvalue_span, next_block, &destination);

        next_block.unit()
    }
}
