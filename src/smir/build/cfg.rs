use smir::build::CFG;
use smir::repr::*;
use syntax::ast;
use syntax::codemap::Span;

impl CFG {
    pub fn block_data(&self, block: BasicBlock) -> &BasicBlockData {
        &self.basic_blocks[block.index()]
    }

    pub fn block_data_mut(&mut self, block: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks[block.index()]
    }

    pub fn start_new_block(&mut self, name: Option<&'static str>) -> BasicBlock {
        let node_index = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlockData::new(name, None));
        BasicBlock::new(node_index)
    }

    pub fn push(&mut self, block: BasicBlock, statement: Statement) {
        self.block_data_mut(block).statements.push(statement);
    }

    pub fn push_drop(&mut self, block: BasicBlock, span: Span, id: ast::Ident) {
        self.block_data_mut(block).statements.push(Statement::Drop(span, id));
    }

    pub fn terminate(&mut self, block: BasicBlock, terminator: Terminator) {
        assert!(self.block_data(block).terminator.is_none(),
                "terminate: block {:?} already has a terminator set", block);
        self.block_data_mut(block).terminator = Some(terminator);
    }
}
