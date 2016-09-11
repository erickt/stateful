use mar::build::CFG;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl CFG {
    pub fn block_data(&self, block: BasicBlock) -> &BasicBlockData {
        &self.basic_blocks[block.index()]
    }

    pub fn block_data_mut(&mut self, block: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks[block.index()]
    }

    pub fn start_new_block(&mut self,
                           span: Span,
                           name: Option<&'static str>,
                           decls: Vec<(VarDecl, ast::Ident)>) -> BasicBlock {
        let node_index = self.basic_blocks.len();
        self.basic_blocks.push(BasicBlockData::new(span, name, decls, None));
        BasicBlock::new(node_index)
    }

    pub fn push(&mut self, block: BasicBlock, statement: Statement) {
        self.block_data_mut(block).statements.push(statement);
    }

    pub fn push_drop(&mut self,
                     block: BasicBlock,
                     span: Span,
                     decl: VarDecl) {
        self.block_data_mut(block).statements.push(Statement::Drop {
            span: span,
            lvalue: decl,
        });
    }

    pub fn push_unshadow(&mut self,
                         block: BasicBlock,
                         span: Span,
                         shadow: ShadowedDecl) {
        self.block_data_mut(block).statements.push(Statement::Unshadow {
            span: span,
            shadow: shadow,
        });
    }

    pub fn push_declare_decl(&mut self,
                             block: BasicBlock,
                             span: Span,
                             decl: VarDecl,
                             ty: Option<P<ast::Ty>>) {
        self.push(block, Statement::Declare {
            span: span,
            decl: decl,
            ty: ty,
        });
    }

    pub fn push_assign(&mut self,
                       block: BasicBlock,
                       lvalue: Lvalue,
                       rvalue: P<ast::Expr>) {
        self.push(block, Statement::Assign {
            lvalue: lvalue,
            rvalue: rvalue,
        });
    }

    pub fn terminate(&mut self,
                     span: Span,
                     block: BasicBlock,
                     kind: TerminatorKind) {
        assert!(self.block_data(block).terminator.is_none(),
                "terminate: block {:?} already has a terminator set", block);

        let block_data = self.block_data_mut(block);
        block_data.terminator = Some(Terminator {
            span: span,
            kind: kind,
        });
    }

    pub fn var_decl_data(&self, decl: VarDecl) -> &VarDeclData {
        &self.var_decls[decl.index()]
    }

    pub fn var_decl_data_mut(&mut self, decl: VarDecl) -> &mut VarDeclData {
        &mut self.var_decls[decl.index()]
    }

    pub fn push_decl(&mut self,
                     mutability: ast::Mutability,
                     ident: ast::Ident,
                     ty: Option<P<ast::Ty>>) -> VarDecl {
        let decl = VarDecl::new(self.var_decls.len());
        let decl_data = VarDeclData::new(mutability, ident, ty);
        self.var_decls.push(decl_data);
        decl
    }

    pub fn temp_lvalue(&mut self, span: Span) -> Lvalue {
        Lvalue::Temp {
            span: span,
        }
    }
}
