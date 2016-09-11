use mar::repr::*;
use mar::translate::Builder;
use syntax::ast;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt(&self, _block: BasicBlock, stmt: &Statement) -> Vec<ast::Stmt> {
        match *stmt {
            Statement::Expr(ref stmt) => vec![stmt.clone()],
            Statement::Declare { span, decl, ref ty } => {
                let id = self.mar.var_decl_data(decl).ident;

                vec![
                    self.ast_builder.span(span).stmt().build_let(
                        self.ast_builder.pat().id(id),
                        ty.clone(),
                        None)
                ]
            }
            Statement::Let { span, ref pat, ref ty, ref init } => {
                vec![
                    self.ast_builder.span(span).stmt()
                        .build_let(pat.clone(), ty.clone(), init.clone())
                ]

            }
            Statement::Assign { ref lvalue, ref rvalue } => {
                match *lvalue {
                    Lvalue::Var { span, decl, .. } => {
                        let id = self.mar.var_decl_data(decl).ident;

                        vec![
                            self.ast_builder.span(span).stmt().semi()
                                .assign().id(id)
                                .build(rvalue.clone())
                        ]
                    }
                    Lvalue::Temp { span } => {
                        vec![
                            self.ast_builder.span(span).stmt().semi()
                                .build(rvalue.clone())
                        ]
                    }
                    Lvalue::ReturnPointer { span } => {
                        vec![
                            self.ast_builder.span(span).stmt().expr()
                                .build(rvalue.clone())
                        ]
                    }
                }
            }
            Statement::Drop { span, lvalue } => {
                let id = self.mar.var_decl_data(lvalue).ident;

                // We need an explicit drop here to make sure we drop variables as they go out of
                // a block scope. Otherwise, they won't be dropped until the next yield point,
                // which wouldn't match the Rust semantics.
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
            Statement::Unshadow { span, ref shadow } => {
                let (mode, ident) = {
                    let decl = self.mar.var_decl_data(shadow.decl);
                    let mode = ast::BindingMode::ByValue(decl.mutability);
                    (mode, decl.ident)
                };

                vec![
                    self.ast_builder.span(span).stmt()
                        .let_().build_id(mode, ident, None)
                        .expr().id(shadow.lvalue)
                ]
            }
        }
    }
}
