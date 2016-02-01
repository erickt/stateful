use aster::AstBuilder;
use mar::build::Builder;
use mar::repr::*;
use std::ascii::AsciiExt;
use syntax::ast;
use syntax::codemap::Span;
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

        for decl in self.get_idents_from_pat(&local.pat) {
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

    fn get_idents_from_pat(&mut self, pat: &ast::Pat) -> Vec<VarDecl> {
        struct Visitor<'a, 'b: 'a> {
            builder: &'a mut Builder<'b>,
            var_decls: Vec<VarDecl>,
        }

        impl<'a, 'b, 'c> visit::Visitor<'a> for Visitor<'b, 'c> {
            fn visit_pat(&mut self, pat: &ast::Pat) {
                match pat.node {
                    ast::PatIdent(ast::BindingMode::ByValue(mutability), id, _) => {
                        // Consider only lower case identities as a variable.
                        let id_str = id.node.name.as_str();
                        let first_char = id_str.chars().next().unwrap();

                        if first_char == first_char.to_ascii_lowercase() {
                            let decl = self.builder.cfg.push_decl(mutability, id.node);
                            self.var_decls.push(decl);
                        }
                    }
                    ast::PatIdent(..) => {
                        self.builder.cx.span_bug(pat.span,
                                                 &format!("Canot handle pat {:?}", pat))
                    }
                    _ => { }
                }

                visit::walk_pat(self, pat);
            }

            fn visit_mac(&mut self, _mac: &ast::Mac) { }
        }

        let mut visitor = Visitor {
            builder: self,
            var_decls: Vec::new(),
        };

        visit::Visitor::visit_pat(&mut visitor, pat);

        visitor.var_decls
    }

    pub fn into_stmt(&mut self,
                     _extent: CodeExtent,
                     block: BasicBlock,
                     stmt: P<ast::Stmt>) -> BasicBlock {
        self.cfg.push(block, Statement::Expr(stmt));
        block
    }
}
