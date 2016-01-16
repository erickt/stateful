use mar::repr::*;
use mar::trans::Builder;
use syntax::ast;
use syntax::ptr::P;

impl<'a> Builder<'a> {
    pub fn stmt(&self, _block: BasicBlock, stmt: &Statement) -> Vec<P<ast::Stmt>> {
        match *stmt {
            Statement::Expr(ref stmt) => vec![stmt.clone()],
            Statement::Drop(span, id) => {
                vec![
                    self.ast_builder
                        .span(span)
                        .stmt().semi().call()
                            .path()
                                .global()
                                .ids(&["std", "mem", "drop"])
                                .build()
                        .arg().id(id)
                        .build()
                ]
            }
        }
    }
}
