use aster::AstBuilder;
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

    fn stmt(&mut self,
            extent: CodeExtent,
            block: BasicBlock,
            stmt: &ast::Stmt) -> BasicBlock {
        match stmt.node {
            StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => {
                let destination = self.cfg.temp_lvalue(stmt.span);
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
                match self.stmt_mac(block, mac) {
                    Some(block) => block,
                    None => {
                        self.cfg.push(block, Statement::Expr(stmt.clone()));
                        block
                    }
                }
            }
        }
    }

    fn local(&mut self,
             extent: CodeExtent,
             mut block: BasicBlock,
             span: Span,
             local: &P<ast::Local>) -> BasicBlock {
        let mut decls = vec![];

        for (decl, _) in self.get_decls_from_pat(&local.pat) {
            let lvalue = self.cfg.var_decl_data(decl).ident;

            let shadow = self.find_decl(lvalue).map(|decl| {
                self.shadow(block, extent, span, decl)
            });

            decls.push((decl, shadow));
        }

        if decls.is_empty() {
            self.cx.span_bug(span, "No decls found?")
        } else if decls.len() == 1 {
            let (decl, alias) = decls[0];

            self.schedule_forward_decl(span, decl, local.ty.clone(), alias);

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

    fn shadow(&mut self,
              block: BasicBlock,
              extent: CodeExtent,
              span: Span,
              decl: VarDecl) -> ShadowedDecl {
        let lvalue = self.cfg.var_decl_data(decl).ident;

        let ast_builder = AstBuilder::new().span(span);
        let alias = ast_builder.id(format!("{}_shadowed_{}", lvalue, decl.index()));
        let alias_decl = self.cfg.push_decl(ast::Mutability::Immutable, alias, None);

        self.cfg.push_declare_decl(
            block,
            span,
            alias_decl,
            None);

        let destination = Lvalue::Var {
            span: span,
            decl: alias_decl,
        };

        self.into(destination, extent, block, ast_builder.expr().id(lvalue));

        ShadowedDecl {
            lvalue: alias,
            decl: decl,
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
