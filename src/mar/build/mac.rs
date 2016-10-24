use aster::AstBuilder;
use mar::build::{Builder, transition};
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
                    extent: CodeExtent,
                    block: BasicBlock,
                    mac: &ast::Mac) -> Option<BasicBlock> {
        match (self.state_machine_kind, transition::parse_mac_transition(self.cx, mac)) {
            (StateMachineKind::Generator, Some(transition::Transition::Yield(expr))) => {
                let expr = self.expand_moved(&expr);

                Some(self.expr_yield(destination, extent, block, expr))
            }
            (StateMachineKind::Async, Some(transition::Transition::Await(expr))) => {
                let expr = self.expand_moved(&expr);

                Some(self.expr_await(destination, extent, block, expr))
            }
            (_, Some(transition::Transition::Suspend(expr))) => {
                let expr = self.expand_moved(&expr);

                Some(self.expr_suspend(destination, extent, block, expr))
            }
            _ => {
                if is_path(&mac.node.path, "moved") {
                    let expr = parse_mac(self.cx, mac);
                    let expr = self.expand_moved(&expr);

                    Some(self.expr(destination, extent, block, &expr))
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
        None,
        mac.node.tts.clone());

    let mut parser = Parser::new(cx.parse_sess(), cx.cfg(), Box::new(rdr.clone()));
    let expr = panictry!(parser.parse_expr());
    panictry!(parser.expect(&Token::Eof));

    expr
}

pub fn is_path(path: &ast::Path, name: &str) -> bool {
    !path.global && path.segments == AstBuilder::new()
        .path().id(name)
        .build().segments
}
