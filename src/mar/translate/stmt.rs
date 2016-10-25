use mar::repr::*;
use mar::translate::Builder;
use syntax::ast;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt(&self, _block: BasicBlock, stmt: &Statement) -> Vec<ast::Stmt> {
        match *stmt {
            Statement::Expr(ref stmt) => vec![stmt.clone()],
            Statement::Declare { local } => {
                let local_decl = self.mar.local_decl_data(local);
                let ast_builder = self.ast_builder.span(local_decl.span);

                let mut stmts = vec![];

                if let Some(shadowed_decl) = local_decl.shadowed_decl {
                    let shadowed_ident = self.shadowed_ident(shadowed_decl);

                    stmts.push(
                        ast_builder.stmt().let_id(shadowed_ident)
                        .expr().id(local_decl.ident)
                    );
                }

                let stmt_builder = match local_decl.mutability {
                    ast::Mutability::Mutable => {
                        ast_builder.stmt().let_().mut_id(local_decl.ident)
                    }
                    ast::Mutability::Immutable => {
                        ast_builder.stmt().let_().id(local_decl.ident)
                    }
                };

                stmts.push(
                    stmt_builder
                        .build_option_ty(local_decl.ty.clone())
                        .build()
                );

                stmts
            }
            Statement::Assign { ref lvalue, ref rvalue } => {
                match *lvalue {
                    Lvalue::Local { span, decl, .. } => {
                        let id = self.mar.local_decl_data(decl).ident;

                        vec![
                            self.ast_builder.span(span).stmt().semi()
                                .assign().id(id)
                                .build(rvalue.clone())
                        ]
                    }
                    Lvalue::Temp { span, name } => {
                        if let Some(name) = name {
                            vec![
                                self.ast_builder.span(span).stmt().let_id(name)
                                    .expr().build(rvalue.clone())
                            ]
                        } else {
                            vec![
                                self.ast_builder.span(span).stmt().semi()
                                    .build(rvalue.clone())
                            ]
                        }
                    }
                    Lvalue::ReturnPointer { span } => {
                        vec![
                            self.ast_builder.span(span).stmt().expr()
                                .build(rvalue.clone())
                        ]
                    }
                }
            }
            Statement::Drop { lvalue, moved } => {
                let decl = self.mar.local_decl_data(lvalue);
                let ast_builder = self.ast_builder.span(decl.span);

                // We need an explicit drop here to make sure we drop variables as they go out of
                // a block scope. Otherwise, they won't be dropped until the next yield point,
                // which wouldn't match the Rust semantics.
                let mut stmts = vec![];

                // Only drop if we were not moved.
                if !moved {
                    stmts.push(
                        ast_builder.stmt().semi().call()
                            .path()
                                .global()
                                .ids(&["std", "mem", "drop"])
                                .build()
                            .arg().id(decl.ident)
                            .build()
                    );
                }

                if let Some(shadowed_decl) = decl.shadowed_decl {
                    let shadowed_ident = self.shadowed_ident(shadowed_decl);

                    stmts.push(
                        ast_builder.stmt().let_id(decl.ident).expr().id(shadowed_ident)
                    );
                }

                stmts
            }
        }
    }
}
