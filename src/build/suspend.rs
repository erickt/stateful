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
        let next_block = self.cfg.start_new_block(rvalue_span, Some("Resume"));

        self.terminate(rvalue_span, block, TerminatorKind::Suspend {
            destination: (destination.clone(), next_block),
            rvalue: rvalue,
        });

        let coroutine_args_lvalue = Lvalue::Local(COROUTINE_ARGS);
        let coroutine_args_operand = Operand::Consume(coroutine_args_lvalue);
        let coroutine_args_rvalue = Rvalue::Use(coroutine_args_operand);
        self.push_assign(next_block, rvalue_span, &destination, coroutine_args_rvalue);

        next_block.unit()
    }
}
