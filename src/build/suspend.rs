use build::{BlockAnd, BlockAndExtension, Builder};
use mir::*;
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
        let next_block = self.start_new_block(rvalue_span, Some("Suspend"));

        self.terminate(rvalue_span, block, TerminatorKind::Suspend {
            destination: (destination, next_block),
            rvalue: rvalue,
        });

        // We don't yet support receiving values into the coroutine yet, so just store a `()` in
        // the destination.
        self.push_assign_unit(rvalue_span, block, &destination);

        next_block.unit()
    }
}
