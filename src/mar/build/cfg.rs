use mar::build::CFG;
use mar::repr::*;
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

    pub fn push_drop(&mut self,
                     block: BasicBlock,
                     span: Span,
                     decl: VarDecl,
                     alias: Option<Alias>) {
        let lvalue = self.var_decl_data(decl).ident;
        self.block_data_mut(block).statements.push(Statement::Drop {
            span: span,
            lvalue: lvalue,
            alias: alias,
        });
    }

    pub fn terminate(&mut self,
                     block: BasicBlock,
                     live_decls: Vec<(VarDecl, ast::Ident)>,
                     terminator: Terminator) {
        assert!(self.block_data(block).terminator.is_none(),
                "terminate: block {:?} already has a terminator set", block);
        self.add_incoming_block(&terminator, block);
        let block_data = self.block_data_mut(block);
        block_data.terminator = Some(terminator);
        block_data.live_decls = live_decls;
    }

    fn add_incoming_block(&mut self, terminator: &Terminator, block: BasicBlock) {
        match *terminator {
            Terminator::Goto { target } => {
                self.block_data_mut(target).incoming_blocks.push(block);
            }
            Terminator::Yield { target, .. } => {
                self.block_data_mut(target).incoming_blocks.push(block);
            }
            Terminator::If { targets: (then_block, else_block), .. } => {
                self.block_data_mut(then_block).incoming_blocks.push(block);
                self.block_data_mut(else_block).incoming_blocks.push(block);
            }
            Terminator::Match { ref targets, .. } => {
                for target in targets.iter() {
                    self.block_data_mut(target.block).incoming_blocks.push(block);
                }
            }
            Terminator::Return => {
                self.block_data_mut(END_BLOCK).incoming_blocks.push(block);
            }
        }
    }

    pub fn var_decl_data(&self, decl: VarDecl) -> &VarDeclData {
        &self.var_decls[decl.index()]
    }

    pub fn var_decl_data_mut(&mut self, decl: VarDecl) -> &mut VarDeclData {
        &mut self.var_decls[decl.index()]
    }

    pub fn push_decl(&mut self,
                     mutability: ast::Mutability,
                     ident: ast::Ident) -> VarDecl {
        let decl = VarDecl::new(self.var_decls.len());
        let decl_data = VarDeclData::new(mutability, ident);
        self.var_decls.push(decl_data);
        decl
    }
}
