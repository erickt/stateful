use mar::build::Builder;
use mar::repr::*;
use syntax::ast;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn ast_block(&mut self,
                     extent: CodeExtent,
                     block: BasicBlock,
                     ast_block: &ast::Block) -> BasicBlock {
        self.in_scope(extent, block, |this| {
            // FIXME: handle trailing exprs
            this.stmts(extent, block, &ast_block.stmts[..])
        })
    }
}
