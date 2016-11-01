use mar::repr::*;
use mar::translate::Builder;
use syntax::ast;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt(&self, _block: BasicBlock, stmt: &Statement) -> Vec<ast::Stmt> {
        match *stmt {
            Statement::Expr(ref stmt) => vec![stmt.clone()],
            Statement::Declare(local) => {
                let local_decl = self.mar.local_decl_data(local);
                let ast_builder = self.ast_builder.span(local_decl.source_info.span);

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
            Statement::Assign { span, ref lvalue, ref rvalue } => {
                let lvalue = lvalue.to_expr(&self.mar.local_decls);
                let rvalue = rvalue.to_expr(&self.mar.local_decls);

                vec![
                    self.ast_builder.span(span).stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ]
            }
            Statement::Call { span, ref lvalue, ref fun, ref args } => {
                let lvalue = lvalue.to_expr(&self.mar.local_decls);

                let fun = fun.to_expr(&self.mar.local_decls);
                let args = args.iter()
                    .map(|arg| arg.to_expr(&self.mar.local_decls));

                let rvalue = self.ast_builder.span(span).expr()
                    .call().build(fun)
                    .with_args(args)
                    .build();

                vec![
                    self.ast_builder.span(span).stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ]
            }
            Statement::MethodCall { span, ref lvalue, ident, ref tys, ref args } => {
                let lvalue = lvalue.to_expr(&self.mar.local_decls);

                let mut args = args.iter()
                    .map(|arg| arg.to_expr(&self.mar.local_decls));

                let rvalue = self.ast_builder.expr()
                    .span(ident.span).method_call(ident.node)
                    .span(span).build(args.next().unwrap())
                    .with_tys(tys.clone())
                    .with_args(args)
                    .build();

                vec![
                    self.ast_builder.span(span).stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ]
            }
            Statement::Drop { lvalue, moved } => {
                let decl = self.mar.local_decl_data(lvalue);
                let ast_builder = self.ast_builder.span(decl.source_info.span);

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
