use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::repr::*;
use syntax::ast::{self, StmtKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn ast_block(&mut self,
                     destination: Lvalue,
                     mut block: BasicBlock,
                     ast_block: &ast::Block) -> BlockAnd<()> {
        self.in_scope(ast_block.span, block, |this| {
            let (stmts, expr) = split_stmts(&ast_block.stmts[..]);

            for stmt in stmts {
                block = unpack!(this.stmt(block, stmt));
            }

            if let Some(expr) = expr {
                this.into(destination, block, &expr)
            } else {
                this.push_assign_unit(ast_block.span, block, destination);
                block.unit()
            }
        })
    }

    pub fn stmt(&mut self,
                block: BasicBlock,
                stmt: &ast::Stmt) -> BlockAnd<()> {
        match stmt.node {
            StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => {
                self.stmt_expr(block, expr)
            }
            StmtKind::Local(ref local) => {
                self.local(block, stmt.span, local)
            }
            StmtKind::Item(..) => {
                self.cx.span_bug(stmt.span, "Cannot handle item declarations yet");
            }
            StmtKind::Mac(ref mac) => {
                let (ref mac, _, _) = **mac;
                let destination = self.declare_temp_lvalue(stmt.span, "temp_stmt_mac");

                match self.expr_mac(destination.clone(), block, mac) {
                    Some(block) => block,
                    None => {
                        self.cfg.push(block, Statement::Expr(stmt.clone()));

                        self.push_assign_unit(stmt.span, block, destination);
                        block.unit()
                    }
                }
            }
        }
    }

    pub fn local(&mut self,
                 mut block: BasicBlock,
                 span: Span,
                 local: &P<ast::Local>) -> BlockAnd<()> {
        let decls = self.get_decls_from_pat(&local.pat, local.ty.clone());

        if decls.is_empty() {
            self.cx.span_bug(span, "No decls found?")
        } else if decls.len() == 1 {
            let decl = decls[0];
            self.local_decls[decl].ty = local.ty.clone();

            if let Some(ref init) = local.init {
                block = unpack!(self.into(Lvalue::Local(decl), block, init));
            }

            block.unit()
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
