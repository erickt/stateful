use mar::repr::*;
use mar::translate::Builder;
use syntax::ast;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt(&self, _block: BasicBlock, stmt: &Statement) -> Vec<ast::Stmt> {
        match *stmt {
            Statement::Expr(ref stmt) => vec![stmt.clone()],
            Statement::Let { span, ref pat, ref ty, ref init } => {
                vec![
                    self.ast_builder.span(span).stmt()
                        .build_let(pat.clone(), ty.clone(), init.clone())
                ]

            }
            Statement::Drop { span, ref lvalue, ref alias } => {
                // We need an explicit drop here to make sure we drop variables as they go out of
                // a block scope. Otherwise, they won't be dropped until the next yield point,
                // which wouldn't match the Rust semantics.
                let mut stmts = vec![
                    self.ast_builder
                        .span(span)
                        .stmt().semi().call()
                            .path()
                                .global()
                                .ids(&["std", "mem", "drop"])
                                .build()
                        .arg().id(lvalue)
                        .build()
                ];

                if let Some(ref alias) = *alias {
                    let (mode, ident) = {
                        let decl = self.mar.var_decl_data(alias.decl);
                        let mode = ast::BindingMode::ByValue(decl.mutability);
                        (mode, decl.ident)
                    };

                    stmts.push(self.ast_builder.span(span).stmt()
                        .let_().build_id(mode, ident, None)
                        .expr().id(alias.lvalue)
                    );
                }

                stmts
            }
        }
    }
}
