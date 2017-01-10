use build::CFG;
use mir::*;
use syntax::codemap::Span;

impl CFG {
    pub fn block_data_mut(&mut self, block: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks[block]
    }

    pub fn start_new_block(&mut self, span: Span, name: Option<&'static str>) -> BasicBlock {
        debug!("start_new_block(name={:?})", name);
        let block = self.basic_blocks.push(BasicBlockData::new(span, name));
        debug!("start_new_block: block={:?}", block);

        block
    }

    pub fn push(&mut self, block: BasicBlock, statement: Statement) {
        self.block_data_mut(block).statements.push(statement);
    }

    pub fn push_assign(&mut self,
                       block: BasicBlock,
                       source_info: SourceInfo,
                       lvalue: &Lvalue,
                       rvalue: Rvalue) {
        self.push(block, Statement {
            source_info: source_info,
            kind: StatementKind::Assign(lvalue.clone(), rvalue)
        });
    }

    pub fn push_assign_unit(&mut self,
                            block: BasicBlock,
                            source_info: SourceInfo,
                            lvalue: &Lvalue) {
        let rvalue = Rvalue::Tuple(vec![]);
        self.push_assign(block, source_info, lvalue, rvalue);
    }

    pub fn terminate(&mut self,
                     block: BasicBlock,
                     source_info: SourceInfo,
                     kind: TerminatorKind) {
        debug!("terminate(block={:?}, kind={:?}", block, kind);

        let block_data = self.block_data_mut(block);

        assert!(block_data.terminator.is_none(),
                "terminate: block {:?} already has a terminator set: {:?}", block, block_data.terminator);

        block_data.terminator = Some(Terminator {
            source_info: source_info,
            kind: kind,
        });
    }
}
