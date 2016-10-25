use aster::AstBuilder;
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
                           decls: Vec<LiveDecl>) -> BasicBlock {
        self.basic_blocks.push(BasicBlockData::new(span, name, decls, None))
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

    pub fn push_declare_decl(&mut self, block: BasicBlock, local: Local) {
        self.push(block, Statement::Declare {
            local: local,
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

    pub fn temp_lvalue_and_expr(&mut self,
                                span: Span,
                                name: &'static str) -> (Lvalue, P<ast::Expr>) {
        let lvalue = self.temp_lvalue(span, Some(name));
        let expr = AstBuilder::new().span(span).expr().id(name);

        (lvalue, expr)
    }
}
