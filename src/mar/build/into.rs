use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::respan;
use syntax::ptr::P;

pub trait EvalInto {
    fn eval_into(self,
                 builder: &mut Builder,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock;
}

impl<'a> Builder<'a> {
    pub fn into<E>(&mut self,
                   extent: CodeExtent,
                   block: BasicBlock,
                   expr: E) -> BasicBlock
        where E: EvalInto
    {
        expr.eval_into(self, extent, block)
    }
}

impl<'a> EvalInto for &'a P<ast::Block> {
    fn eval_into(self,
                 builder: &mut Builder,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        builder.ast_block(extent, block, self)
    }
}

impl EvalInto for P<ast::Stmt> {
    fn eval_into(self,
                 builder: &mut Builder,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        builder.into_stmt(extent, block, self);
        block
    }
}

impl<'a> EvalInto for &'a P<ast::Expr> {
    fn eval_into(self,
                 builder: &mut Builder,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        self.clone().eval_into(builder, extent, block)
    }
}

impl EvalInto for P<ast::Expr> {
    fn eval_into(self,
                 builder: &mut Builder,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        let stmt = P(respan(self.span, ast::StmtSemi(self, ast::DUMMY_NODE_ID)));
        builder.into(extent, block, stmt)
    }
}

impl<'a> EvalInto for &'a Option<P<ast::Expr>> {
    fn eval_into(self,
                 builder: &mut Builder,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        if let Some(ref expr) = *self {
            expr.eval_into(builder, extent, block)
        } else {
            block
        }
    }
}

impl EvalInto for Option<P<ast::Expr>> {
    fn eval_into(self,
                 builder: &mut Builder,
                 extent: CodeExtent,
                 block: BasicBlock) -> BasicBlock {
        if let Some(expr) = self {
            expr.eval_into(builder, extent, block)
        } else {
            block
        }
    }
}
