use aster::AstBuilder;
use mar::build::Builder;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::ext::tt::transcribe::new_tt_reader;
use syntax::fold::{self, Folder};
use syntax::parse::parser::Parser;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn expand_moved(&mut self, expr: &P<ast::Expr>) -> P<ast::Expr> {
        let mut expander = ExpandMac {
            cx: self.cx,
            moved_idents: vec![],
        };

        let expr = expander.fold_expr(expr.clone());

        for moved_ident in expander.moved_idents {
            if let Some(decl) = self.find_decl(moved_ident) {
                self.schedule_move(decl);
            } else {
                self.cx.span_bug(
                    expr.span,
                    &format!("ident {:?} not in scope", moved_ident));
            }
        }

        expr
    }
}

struct ExpandMac<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    moved_idents: Vec<ast::Ident>,
}

impl<'a, 'b> ExpandMac<'a, 'b> {
    fn parse_mac_moved(&mut self, mac: &ast::Mac) -> P<ast::Expr> {
        let rdr = new_tt_reader(
            &self.cx.parse_sess().span_diagnostic,
            None,
            None,
            mac.node.tts.clone());

        let mut parser = Parser::new(
            self.cx.parse_sess(),
            self.cx.cfg(),
            Box::new(rdr));

        let ident = panictry!(parser.parse_ident());
        self.moved_idents.push(ident);

        AstBuilder::new().expr()
            .span(mac.span)
            .id(ident)
    }
}

impl<'a, 'b> Folder for ExpandMac<'a, 'b> {
    fn fold_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        match expr.node {
            ast::ExprKind::Mac(ref mac) if is_moved_path(&mac.node.path) => {
                return self.parse_mac_moved(mac);
            }
            _ => {}
        }

        expr.map(|expr| fold::noop_fold_expr(expr, self))
    }

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
        fold::noop_fold_mac(mac, self)
    }
}

fn is_moved_path(path: &ast::Path) -> bool {
    let builder = AstBuilder::new();
    let yield_ = builder.path()
        .id("moved")
        .build();

    !path.global && path.segments == yield_.segments
}
