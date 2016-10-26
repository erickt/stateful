use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::repr::*;
use syntax::ast;
use syntax::ptr::P;

pub trait EvalInto {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 block: BasicBlock) -> BlockAnd<()>;
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn into<E>(&mut self,
                   destination: Lvalue,
                   block: BasicBlock,
                   expr: E) -> BlockAnd<()>
        where E: EvalInto
    {
        expr.eval_into(self, destination, block)
    }
}

impl<'a> EvalInto for &'a P<ast::Expr> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 block: BasicBlock) -> BlockAnd<()> {
        builder.into_expr(destination, block, self)
    }
}

impl EvalInto for P<ast::Expr> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 block: BasicBlock) -> BlockAnd<()> {
        builder.into_expr(destination, block, &self)
    }
}
