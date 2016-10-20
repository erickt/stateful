use mar::build::Builder;
use mar::repr::*;
use syntax::ast::{self, StmtKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn ast_block(&mut self,
                     destination: Lvalue,
                     extent: CodeExtent,
                     mut block: BasicBlock,
                     ast_block: &ast::Block) -> BasicBlock {
        self.in_scope(extent, ast_block.span, block, |this| {
            let (stmts, expr) = split_stmts(&ast_block.stmts[..]);

            for stmt in stmts {
                block = this.stmt(extent, block, stmt);
            }

            if let Some(expr) = expr {
                this.expr(destination, extent, block, &expr)
            } else {
                this.assign_lvalue_unit(ast_block.span, block, destination);
                block
            }
        })
    }

    pub fn stmt(&mut self,
                extent: CodeExtent,
                block: BasicBlock,
                stmt: &ast::Stmt) -> BasicBlock {
        match stmt.node {
            StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => {
                let destination = self.declare_temp_lvalue(stmt.span, "_stmt_result_temp");
                self.expr(destination, extent, block, expr)
            }
            StmtKind::Local(ref local) => {
                self.local(extent, block, stmt.span, local)
            }
            StmtKind::Item(..) => {
                self.cx.span_bug(stmt.span, "Cannot handle item declarations yet");
            }
            StmtKind::Mac(ref mac) => {
                let (ref mac, _, _) = **mac;
                let destination = self.declare_temp_lvalue(stmt.span, "_stmt_result_temp");

                match self.expr_mac(destination.clone(), extent, block, mac) {
                    Some(block) => block,
                    None => {
                        self.assign_lvalue_unit(stmt.span, block, destination);
                        self.cfg.push(block, Statement::Expr(stmt.clone()));
                        block
                    }
                }
            }
        }
    }

    pub fn local(&mut self,
                 extent: CodeExtent,
                 mut block: BasicBlock,
                 span: Span,
                 local: &P<ast::Local>) -> BasicBlock {
        let decls = self.get_decls_from_pat(&local.pat, local.ty.clone());

        if decls.is_empty() {
            self.cx.span_bug(span, "No decls found?")
        } else if decls.len() == 1 {
            let decl = decls[0];
            self.var_decls[decl].ty = local.ty.clone();

            if let Some(ref init) = local.init {
                let destination = Lvalue::Var {
                    span: span,
                    decl: decl,
                };

                block = self.expr(destination, extent, block, init);
            }

            block
        } else {
            self.cx.span_bug(span, "Cannot handle multiple decls at the moment?")
        }
    }
}

fn split_stmts(stmts: &[ast::Stmt]) -> (&[ast::Stmt], Option<P<ast::Expr>>) {
    if let Some((last, remainder)) = stmts.split_last() {
        if let StmtKind::Expr(ref expr) = last.node {
            return (remainder, Some(expr.clone()));
        }
    }

    (stmts, None)
}
