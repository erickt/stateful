use mar::build::{Builder, BlockAnd};
use mar::repr::*;
use syntax::ast;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn as_temp(&mut self, _block: BasicBlock, expr: &P<ast::Expr>) -> BlockAnd<Lvalue> {
        match expr.node {
            _ => {
                self.cx.span_bug(expr.span,
                                 &format!("don't know how to handle {:#?} yet", expr))
            }
        }
    }
}
