use mar::build::{Builder, transition};
use mar::repr::*;
use syntax::ast;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn expr_mac(&mut self,
                    destination: Lvalue,
                    extent: CodeExtent,
                    block: BasicBlock,
                    mac: &ast::Mac) -> Option<BasicBlock> {
        match (self.state_machine_kind, transition::parse_mac_transition(self.cx, mac)) {
            (StateMachineKind::Generator, Some(transition::Transition::Yield(expr))) => {
                let expr = self.expand_moved(&expr);

                Some(self.expr_yield(destination, extent, block, expr))
            }
            (StateMachineKind::Async, Some(transition::Transition::Await(expr))) => {
                let expr = self.expand_moved(&expr);

                Some(self.expr_await(destination, extent, block, expr))
            }
            (_, Some(transition::Transition::Suspend(expr))) => {
                let expr = self.expand_moved(&expr);

                Some(self.expr_suspend(destination, extent, block, expr))
            }
            _ => None,
        }
    }
}
