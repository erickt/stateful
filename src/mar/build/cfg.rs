use mar::build::CFG;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;

impl<'a> CFG<'a> {
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

    pub fn terminate(&mut self, block: BasicBlock, terminator: Terminator) {
        assert!(self.block_data(block).terminator.is_none(),
                "terminate: block {:?} already has a terminator set", block);

        let live_decls = (0..self.var_decls.len())
            .map(|decl| VarDecl::new(decl))
            .collect::<Vec<_>>();

        // Push the live decls into the terminating block.
        match terminator {
            Terminator::Goto { target } => {
                self.set_live_decls(target, live_decls);
            }
            Terminator::Yield { target, .. } => {
                self.set_live_decls(target, live_decls);
            }
            Terminator::If { targets: (then_block, else_block), .. } => {
                self.set_live_decls(then_block, live_decls.clone());
                self.set_live_decls(else_block, live_decls);
            }
            Terminator::Return => {
            }
        }

        let block_data = self.block_data_mut(block);
        block_data.terminator = Some(terminator);
    }

    fn set_live_decls(&mut self, block: BasicBlock, live_decls: Vec<VarDecl>) {
        if block == END_BLOCK {
            return;
        }

        let block_data = self.block_data_mut(block);

        // It's possible another block created an edge to this block. If this is the case, make
        // sure we're passing in the same idents.
        if !block_data.live_decls.is_empty() {
            if live_decls != block_data.live_decls {
                self.cx.bug("Block has different live declarations");
            }

            return;
        }

        block_data.live_decls = live_decls;
    }

    pub fn var_decl_data(&self, decl: VarDecl) -> &VarDeclData {
        &self.var_decls[decl.index()]
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
