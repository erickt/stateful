use aster::AstBuilder;
use mir::*;
use syntax::ast;
use translate::Builder;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt(&self, _block: BasicBlock, stmt: &Statement) -> Vec<ast::Stmt> {
        let ast_builder = self.ast_builder.span(stmt.source_info.span);

        match stmt.kind {
            StatementKind::Expr(ref stmt) => vec![stmt.clone()],
            StatementKind::Declare(local) => {
                let mut stmts = self.rename_shadowed_local(&ast_builder, local).into_iter()
                    .collect::<Vec<_>>();

                let local_decl = self.mir.local_decl_data(local);

                let stmt_builder = match local_decl.mutability {
                    ast::Mutability::Mutable => {
                        ast_builder.stmt().let_().mut_id(local_decl.name)
                    }
                    ast::Mutability::Immutable => {
                        ast_builder.stmt().let_().id(local_decl.name)
                    }
                };

                stmts.push(
                    stmt_builder
                        .build_option_ty(local_decl.ty.clone())
                        .build()
                );

                stmts
            }
            StatementKind::Let { ref pat, ref lvalues, ref ty, ref rvalue } => {
                let rvalue = rvalue.to_expr(&self.mir.local_decls);

                // Rename shadowed variables.
                let mut stmts = lvalues.iter()
                    .filter_map(|&local| self.rename_shadowed_local(&ast_builder, local))
                    .collect::<Vec<_>>();

                stmts.push(
                    ast_builder.stmt().let_()
                        .build(pat.clone())
                        .build_option_ty(ty.clone())
                        .expr().build(rvalue)
                );

                stmts
            }
            StatementKind::Assign(ref lvalue, ref rvalue) => {
                let lvalue = lvalue.to_expr(&self.mir.local_decls);
                let rvalue = rvalue.to_expr(&self.mir.local_decls);

                vec![
                    ast_builder.stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ]
            }
            /*
            StatementKind::Call { ref destination, ref func, ref args } => {
                let lvalue = destination.to_expr(&self.mir.local_decls);

                let func = func.to_expr(&self.mir.local_decls);
                let args = args.iter()
                    .map(|arg| arg.to_expr(&self.mir.local_decls));

                let rvalue = ast_builder.expr()
                    .call().build(func)
                    .with_args(args)
                    .build();

                vec![
                    ast_builder.stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ]
            }
            StatementKind::MethodCall { ref destination, ident, ref tys, ref self_, ref args } => {
                let lvalue = destination.to_expr(&self.mir.local_decls);
                let self_ = self_.to_expr(&self.mir.local_decls);

                let args = args.iter()
                    .map(|arg| arg.to_expr(&self.mir.local_decls));

                let rvalue = ast_builder.expr()
                    .span(ident.span).method_call(ident.node)
                    .span(stmt.source_info.span).build(self_)
                    .with_tys(tys.clone())
                    .with_args(args)
                    .build();

                vec![
                    ast_builder.stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ]
            }
            StatementKind::Drop { ref location, moved } => {
                match *location {
                    Lvalue::Local(local) => {
                        let decl = self.mir.local_decl_data(local);

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
                                    .arg().id(decl.name)
                                    .build()
                            );
                        }

                        if let Some(shadowed_decl) = decl.shadowed_decl {
                            let shadowed_ident = self.shadowed_ident(shadowed_decl);

                            stmts.push(
                                ast_builder.stmt().let_id(decl.name).expr().id(shadowed_ident)
                            );
                        }

                        stmts
                    }
                    _ => vec![]
                }
            }
            */
            /*
            StatementKind::StorageLive(_) |
            StatementKind::StorageDead(_) |
            StatementKind::Nop => {
                vec![]
            }
            */
        }
    }

    fn rename_shadowed_local(&self, ast_builder: &AstBuilder, local: Local) -> Option<ast::Stmt> {
        let local_decl = self.mir.local_decl_data(local);

        if let Some(shadowed_decl) = local_decl.shadowed_decl {
            let shadowed_ident = self.shadowed_ident(shadowed_decl);

            Some(ast_builder.stmt().let_id(shadowed_ident)
                .expr().id(local_decl.name)
            )
        } else {
            None
        }
    }
}
