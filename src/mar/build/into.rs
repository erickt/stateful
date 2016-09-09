use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::ptr::P;

pub trait EvalInto {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock;
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn into<E>(&mut self,
                   destination: Lvalue,
                   extent: CodeExtent,
                   block: BasicBlock,
                   expr: E) -> BasicBlock
        where E: EvalInto
    {
        expr.eval_into(self, destination, extent, block)
    }
}

impl<'a> EvalInto for &'a P<ast::Block> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        builder.ast_block(destination, extent, block, self)
    }
}

impl<'a> EvalInto for &'a P<ast::Expr> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        self.clone().eval_into(builder, destination, extent, block)
    }
}

impl EvalInto for P<ast::Expr> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 _extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        builder.assign_lvalue(block, destination, self);
        block
    }
}

impl<'a> EvalInto for &'a Option<P<ast::Expr>> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        if let Some(ref expr) = *self {
            builder.expr(destination, extent, block, expr)
        } else {
            block
        }
    }
}

impl EvalInto for Option<P<ast::Expr>> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        if let Some(ref expr) = self {
            builder.expr(destination, extent, block, expr)
        } else {
            block
        }
    }
}
