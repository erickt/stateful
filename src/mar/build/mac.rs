use aster::AstBuilder;
use mar::build::{BlockAnd, Builder, transition};
use mar::repr::*;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::ext::tt::transcribe::new_tt_reader;
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn expr_mac(&mut self,
                    destination: Lvalue,
                    block: BasicBlock,
                    mac: &ast::Mac) -> Option<BlockAnd<()>> {
        match transition::parse_mac_transition(self.cx, mac) {
            Some(transition::Transition::Suspend(expr)) => {
                Some(self.expr_suspend(destination, block, expr))
            }
            _ => {
                if is_path(&mac.node.path, "moved") {
                    let expr = parse_mac(self.cx, mac);

                    Some(self.into(destination, block, &expr))
                } else {
                    None
                }
            }
        }
    }
}

pub fn parse_mac(cx: &ExtCtxt, mac: &ast::Mac) -> P<ast::Expr> {
    let rdr = new_tt_reader(
        &cx.parse_sess().span_diagnostic,
        None,
        mac.node.tts.clone());

    let mut parser = Parser::new(
        cx.parse_sess(),
        cx.cfg().clone(),
        Box::new(rdr.clone()));

    let expr = panictry!(parser.parse_expr());
    panictry!(parser.expect(&Token::Eof));

    expr
}

pub fn is_path(path: &ast::Path, name: &str) -> bool {
    !path.global && path.segments == AstBuilder::new()
        .path().id(name)
        .build().segments
}
