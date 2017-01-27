use mir::*;
use super::builder::Builder;
use super::local_stack::LocalStack;
use syntax::ast;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt(&mut self,
                _block: BasicBlock,
                local_stack: &mut LocalStack,
                stmt: &Statement) -> Vec<ast::Stmt> {
        let stmt_span = stmt.source_info.span;
        let ast_builder = self.ast_builder.span(stmt_span);

        match stmt.kind {
            StatementKind::Stmt(ref stmt) => vec![stmt.clone()],
            StatementKind::Let { ref pat, ref lvalues, ref ty, ref rvalue } => {
                let rvalue = rvalue.to_expr(&self.mir.local_decls);

                // Rename shadowed variables.
                let mut stmts = lvalues.iter()
                    .filter_map(|lvalue| {
                        if let Lvalue::Local(local) = *lvalue {
                            local_stack.push(local)
                        } else {
                            None
                        }
                    })
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
            StatementKind::Call {
                ref destination,
                ref func,
                ref args,
            } => {
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
            StatementKind::MethodCall {
                ref destination,
                ident,
                ref tys,
                ref self_,
                ref args,
            } => {
                let lvalue = destination.to_expr(&self.mir.local_decls);

                let self_ = self_.to_expr(&self.mir.local_decls);

                let args = args.iter()
                    .map(|arg| arg.to_expr(&self.mir.local_decls));

                let rvalue = ast_builder.expr()
                    .span(ident.span).method_call(ident.node)
                    .span(stmt_span).build(self_)
                    .with_tys(tys.clone())
                    .with_args(args)
                    .build();

                vec![
                    ast_builder.stmt().semi().assign()
                        .build(lvalue)
                        .build(rvalue)
                ]
            }
            StatementKind::StorageLive(_) |
            StatementKind::StorageDead(_) => {
            //StatementKind::Nop => {
                vec![]
            }
        }
    }
}
