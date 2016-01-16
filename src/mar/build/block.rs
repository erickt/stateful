use mar::build::Builder;
use mar::repr::*;
use syntax::ast;

impl<'a> Builder<'a> {
    pub fn ast_block(&mut self,
                     extent: CodeExtent,
                     mut block: BasicBlock,
                     ast_block: &ast::Block) -> BasicBlock {
        self.in_scope(extent, block, |this| {
            block = this.stmts(extent, block, &ast_block.stmts[..]);

            if let Some(ref expr) = ast_block.expr {
                this.cx.span_bug(expr.span, "cannot handle block trailing exprs yet");
            }

            block
        })
    }
}
