use build::{BlockAnd, BlockAndExtension, Builder};
use mir::*;
use syntax::ast;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Generate code to suspend the coroutine.
    pub fn expr_suspend(&mut self,
                        destination: Lvalue,
                        mut block: BasicBlock,
                        arg: P<ast::Expr>) -> BlockAnd<()> {
        let arg_span = arg.span;

        let arg = unpack!(block = self.as_operand(block, &arg));
        let next_block = self.cfg.start_new_block(arg_span, Some("Resume"));

        self.terminate(arg_span, block, TerminatorKind::Suspend {
            destination: (destination.clone(), next_block),
            arg: arg,
        });

        let coroutine_args_lvalue = Lvalue::Local(COROUTINE_ARGS);
        let coroutine_args_operand = Operand::Consume(coroutine_args_lvalue);
        let coroutine_args_rvalue = Rvalue::Use(coroutine_args_operand);

        self.push_assign(next_block, arg_span, &destination, coroutine_args_rvalue);

        next_block.unit()
    }
}
