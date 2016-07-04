use aster::AstBuilder;
use mar::build::Builder;
use mar::repr::*;
use syntax::ast::{self, StmtKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmts(&mut self,
                 extent: CodeExtent,
                 mut block: BasicBlock,
                 stmts: &[ast::Stmt]) -> BasicBlock {
        for stmt in stmts {
            block = self.stmt(extent, block, stmt);
        }

        block
    }

    pub fn stmt(&mut self,
                extent: CodeExtent,
                block: BasicBlock,
                stmt: &ast::Stmt) -> BasicBlock {
        match stmt.node {
            StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => {
                // Ignore empty statements.
                if expr_is_empty(expr) {
                    block
                } else {
                    self.expr(extent, block, expr)
                }
            }
            StmtKind::Local(ref local) => {
                self.local(extent, block, stmt.span, local)
                
            }
            StmtKind::Item(..) => {
                self.cx.span_bug(stmt.span, "Cannot handle item declarations yet");
            }
            StmtKind::Mac(ref mac) => {
                let (ref mac, _, _) = **mac;
                match self.mac(block, mac) {
                    Some(block) => block,
                    None => self.into(extent, block, stmt.clone()),
                }
            }
        }
    }

    fn local(&mut self,
             extent: CodeExtent,
             block: BasicBlock,
             span: Span,
             local: &P<ast::Local>) -> BasicBlock {
        if local.init.is_none() {
            self.cx.span_bug(span, &format!("Local variables need initializers at the moment"));
        }

        for (decl, _) in self.get_decls_from_pat(&local.pat) {
            let lvalue = self.cfg.var_decl_data(decl).ident;
            let alias = self.find_decl(lvalue).map(|alias| {
                self.alias(block, span, alias)
            });

            self.schedule_drop(span, extent, decl, alias);
        }

        self.cfg.push(block, Statement::Let {
            span: span,
            pat: local.pat.clone(),
            ty: local.ty.clone(),
            init: local.init.clone(),
        });

        block
    }

    fn alias(&mut self,
             block: BasicBlock,
             span: Span,
             decl: VarDecl) -> Alias {
        let lvalue = self.cfg.var_decl_data(decl).ident;

        let ast_builder = AstBuilder::new();
        let alias = ast_builder.id(format!("{}_shadowed_{}", lvalue, decl.index()));

        self.cfg.push(block, Statement::Let {
            span: span,
            pat: ast_builder.pat().id(alias),
            ty: None,
            init: Some(ast_builder.expr().id(lvalue)),
        });

        Alias {
            lvalue: alias,
            decl: decl,
        }
    }

    pub fn into_stmt(&mut self,
                     _extent: CodeExtent,
                     block: BasicBlock,
                     stmt: ast::Stmt) -> BasicBlock {
        self.cfg.push(block, Statement::Expr(stmt));
        block
    }
}

fn stmt_is_empty(stmt: &ast::Stmt) -> bool {
    match stmt.node {
        ast::StmtKind::Expr(ref e) | ast::StmtKind::Semi(ref e) => expr_is_empty(e),
        _ => false
    }
}

fn expr_is_empty(expr: &ast::Expr) -> bool {
    match expr.node {
        ast::ExprKind::Block(ref block) => {
            for stmt in block.stmts.iter() {
                if !stmt_is_empty(stmt) {
                    return false;
                }
            }

            true
        }
        _ => {
            false
        }
    }
}
