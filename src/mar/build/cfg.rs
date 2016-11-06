use mar::build::CFG;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl CFG {
    pub fn block_data(&self, block: BasicBlock) -> &BasicBlockData {
        &self.basic_blocks[block]
    }

    pub fn block_data_mut(&mut self, block: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks[block]
    }

    pub fn start_new_block(&mut self,
                           span: Span,
                           name: Option<&'static str>,
                           live_decls: LiveDeclMap) -> BasicBlock {
        self.basic_blocks.push(BasicBlockData::new(span, name, live_decls))
    }

    pub fn push(&mut self, block: BasicBlock, statement: Statement) {
        self.block_data_mut(block).statements.push(statement);
    }

    pub fn push_drop(&mut self, block: BasicBlock, lvalue: Local, moved: bool) {
        self.push(block, Statement::Drop {
            lvalue: lvalue,
            moved: moved,
        });
    }

    pub fn push_declare(&mut self, block: BasicBlock, local: Local) {
        self.push(block, Statement::Declare(local));
    }

    pub fn push_assign(&mut self,
                       block: BasicBlock,
                       span: Span,
                       lvalue: &Lvalue,
                       rvalue: Rvalue) {
        self.push(block, Statement::Assign {
            span: span,
            lvalue: lvalue.clone(),
            rvalue: rvalue,
        });
    }

    pub fn push_call(&mut self,
                     block: BasicBlock,
                     span: Span,
                     lvalue: Lvalue,
                     fun: Operand,
                     args: Vec<Operand>) {
        self.push(block, Statement::Call {
            span: span,
            lvalue: lvalue,
            fun: fun,
            args: args,
        });
    }

    pub fn push_method_call(&mut self,
                            block: BasicBlock,
                            span: Span,
                            lvalue: Lvalue,
                            ident: ast::SpannedIdent,
                            tys: Vec<P<ast::Ty>>,
                            self_: Lvalue,
                            args: Vec<Operand>) {
        self.push(block, Statement::MethodCall {
            span: span,
            lvalue: lvalue,
            ident: ident,
            tys: tys,
            self_: self_,
            args: args,
        });
    }

    pub fn terminate(&mut self,
                     block: BasicBlock,
                     source_info: SourceInfo,
                     kind: TerminatorKind) {
        assert!(self.block_data(block).terminator.is_none(),
                "terminate: block {:?} already has a terminator set", block);

        let block_data = self.block_data_mut(block);
        block_data.terminator = Some(Terminator {
            source_info: source_info,
            kind: kind,
        });
    }
}
