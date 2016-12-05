use mir::*;
use syntax::ast;
use translate::Builder;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt(&self, _block: BasicBlock, stmt: &Statement) -> Vec<ast::Stmt> {
        match stmt.kind {
            StatementKind::Expr(ref stmt) => vec![stmt.clone()],
            StatementKind::Declare(local) => {
                let mut stmts = self.rename_shadowed_local(local).into_iter()
                    .collect::<Vec<_>>();

                let local_decl = self.mir.local_decl_data(local);
                let ast_builder = self.ast_builder.span(local_decl.source_info.span);

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
            StatementKind::Let { span, ref pat, ref lvalues, ref ty, ref rvalue } => {
                let rvalue = rvalue.to_expr(&self.mir.local_decls);

                // Rename shadowed variables.
                let mut stmts = lvalues.iter()
                    .filter_map(|&local| self.rename_shadowed_local(local))
                    .collect::<Vec<_>>();

                stmts.push(
                    self.ast_builder.span(span).stmt().let_()
                        .build(pat.clone())
                        .build_option_ty(ty.clone())
                        .expr().build(rvalue)
                );

                stmts
            }
            StatementKind::Assign { span, ref lvalue, ref rvalue } => {
                let lvalue = lvalue.to_expr(&self.mir.local_decls);
                let rvalue = rvalue.to_expr(&self.mir.local_decls);

                vec![
                    self.ast_builder.span(span).stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ]
            }
            StatementKind::Call { span, ref lvalue, ref fun, ref args } => {
                let lvalue = lvalue.to_expr(&self.mir.local_decls);

                let fun = fun.to_expr(&self.mir.local_decls);
                let args = args.iter()
                    .map(|arg| arg.to_expr(&self.mir.local_decls));

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
            StatementKind::MethodCall { span, ref lvalue, ident, ref tys, ref self_, ref args } => {
                let lvalue = lvalue.to_expr(&self.mir.local_decls);
                let self_ = self_.to_expr(&self.mir.local_decls);

                let args = args.iter()
                    .map(|arg| arg.to_expr(&self.mir.local_decls));

                let rvalue = self.ast_builder.expr()
                    .span(ident.span).method_call(ident.node)
                    .span(span).build(self_)
                    .with_tys(tys.clone())
                    .with_args(args)
                    .build();

                vec![
                    self.ast_builder.span(span).stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ]
            }
            StatementKind::Drop { lvalue, moved } => {
                let decl = self.mir.local_decl_data(lvalue);
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
            /*
            StatementKind::StorageLive(_) |
            StatementKind::StorageDead(_) |
            StatementKind::Nop => {
                vec![]
            }
            */
        }
    }

    fn rename_shadowed_local(&self, local: Local) -> Option<ast::Stmt> {
        let local_decl = self.mir.local_decl_data(local);
        let ast_builder = self.ast_builder.span(local_decl.source_info.span);

        if let Some(shadowed_decl) = local_decl.shadowed_decl {
            let shadowed_ident = self.shadowed_ident(shadowed_decl);

            Some(ast_builder.stmt().let_id(shadowed_ident)
                .expr().id(local_decl.ident)
            )
        } else {
            None
        }
    }
}
