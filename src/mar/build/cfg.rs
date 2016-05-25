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

    pub fn start_new_block(&mut self,
                           name: Option<&'static str>,
                           decls: Vec<(VarDecl, ast::Ident)>) -> BasicBlock {
        let node_index = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlockData::new(name, decls, None));
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

    pub fn terminate(&mut self, block: BasicBlock, terminator: Terminator) {
        assert!(self.block_data(block).terminator.is_none(),
                "terminate: block {:?} already has a terminator set", block);
        let block_data = self.block_data_mut(block);
        block_data.terminator = Some(terminator);
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
