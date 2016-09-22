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
                           decls: Vec<(Var, ast::Ident)>) -> BasicBlock {
        self.basic_blocks.push(BasicBlockData::new(span, name, decls, None))
    }

    pub fn push(&mut self, block: BasicBlock, statement: Statement) {
        self.block_data_mut(block).statements.push(statement);
    }

    pub fn push_drop(&mut self,
                     block: BasicBlock,
                     span: Span,
                     decl: Var) {
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
                             decl: Var,
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

    pub fn temp_lvalue(&mut self, span: Span, name: Option<&'static str>) -> Lvalue {
        Lvalue::Temp {
            span: span,
            name: name,
        }
    }
}
