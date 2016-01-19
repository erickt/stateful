use aster::AstBuilder;
use mar::build::Builder;
use mar::repr::*;
use std::ascii::AsciiExt;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;
use syntax::visit;

impl<'a> Builder<'a> {
    pub fn stmts(&mut self,
                 extent: CodeExtent,
                 mut block: BasicBlock,
                 stmts: &[P<ast::Stmt>]) -> BasicBlock {
        for stmt in stmts {
            block = self.stmt(extent, block, stmt);
        }

        block
    }

    pub fn stmt(&mut self,
                extent: CodeExtent,
                block: BasicBlock,
                stmt: &P<ast::Stmt>) -> BasicBlock {
        match stmt.node {
            ast::StmtExpr(ref expr, _) | ast::StmtSemi(ref expr, _) => {
                self.expr(extent, block, expr)
            }
            ast::StmtDecl(ref decl, _) => {
                match decl.node {
                    ast::DeclLocal(ref local) => {
                        self.local(extent, block, stmt.span, local)
                        
                    }
                    ast::DeclItem(..) => {
                        self.cx.span_bug(stmt.span, "Cannot handle item declarations yet");
                    }
                }
            }
            ast::StmtMac(ref mac, _, _) => {
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

        for (_, lvalue) in self.get_idents_from_pat(&local.pat) {
            let alias = if self.lvalue_exists(lvalue) {
                Some(self.alias(block, span, lvalue))
            } else {
                None
            };

            self.schedule_drop(span, extent, lvalue, alias);
        }

        self.cfg.push(block, Statement::Let {
            span: span,
            pat: local.pat.clone(),
            ty: local.ty.clone(),
            init: local.init.clone(),
        });

        block
    }

    fn alias(&mut self, block: BasicBlock, span: Span, lvalue: ast::Ident) -> ast::Ident {
        let ast_builder = AstBuilder::new();
        let alias = ast_builder.id(format!("{}_mar_{}", lvalue, self.aliases));
        self.aliases += 1;

        self.cfg.push(block, Statement::Let {
            span: span,
            pat: ast_builder.pat().id(alias),
            ty: None,
            init: Some(ast_builder.expr().id(lvalue)),
        });

        alias
    }

    fn get_idents_from_pat(&self, pat: &ast::Pat) -> Vec<(ast::Mutability, ast::Ident)> {
        struct Visitor<'a> {
            cx: &'a ExtCtxt<'a>,
            idents: Vec<(ast::Mutability, ast::Ident)>,
        }

        impl<'a, 'b> visit::Visitor<'a> for Visitor<'b> {
            fn visit_pat(&mut self, pat: &ast::Pat) {
                match pat.node {
                    ast::PatIdent(ast::BindingMode::ByValue(mutability), id, _) => {
                        // Consider only lower case identities as a variable.
                        let id_str = id.node.name.as_str();
                        let first_char = id_str.chars().next().unwrap();

                        if first_char == first_char.to_ascii_lowercase() {
                            self.idents.push((mutability, id.node));
                        }
                    }
                    ast::PatIdent(..) => {
                        self.cx.span_bug(pat.span,
                                         &format!("Canot handle pat {:?}", pat))
                    }
                    _ => { }
                }

                visit::walk_pat(self, pat);
            }

            fn visit_mac(&mut self, _mac: &ast::Mac) { }
        }

        let mut visitor = Visitor {
            cx: self.cx,
            idents: Vec::new(),
        };

        visit::Visitor::visit_pat(&mut visitor, pat);

        visitor.idents
    }

    pub fn into_stmt(&mut self,
                     extent: CodeExtent,
                     block: BasicBlock,
                     stmt: P<ast::Stmt>) -> BasicBlock {
        self.cfg.push(block, Statement::Expr(stmt));
        block
    }
}
