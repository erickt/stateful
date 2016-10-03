use mar::build::{Builder, transition};
use mar::repr::*;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::ext::tt::transcribe::new_tt_reader;
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt_mac(&mut self,
                    extent: CodeExtent,
                    block: BasicBlock,
                    mac: &ast::Mac) -> Option<BasicBlock> {
        match self.state_machine_kind {
            StateMachineKind::Generator => {
                if transition::is_yield_path(&mac.node.path) {
                    let destination = self.cfg.temp_lvalue(mac.span, Some("_yield_temp"));

                    Some(self.mac_yield(destination, block, mac))
                } else {
                    None
                }
            }
            StateMachineKind::Async => {
                if transition::is_await_path(&mac.node.path) {
                    let destination = self.cfg.temp_lvalue(mac.span, Some("_async_result_temp"));

                    Some(self.mac_await(destination, extent, block, mac))
                } else {
                    None
                }
            }
        }
    }

    pub fn expr_mac(&mut self,
                    destination: Lvalue,
                    extent: CodeExtent,
                    mut block: BasicBlock,
                    mac: &ast::Mac) -> BasicBlock {
        match self.state_machine_kind {
            StateMachineKind::Generator => {
                if transition::is_yield_path(&mac.node.path) {
                    block = self.mac_yield(destination, block, mac);
                }
            }
            StateMachineKind::Async => {
                if transition::is_await_path(&mac.node.path) {
                    block = self.mac_await(destination, extent, block, mac);
                }
            }
        }

        block
    }

    fn mac_yield(&mut self,
                 destination: Lvalue,
                 block: BasicBlock,
                 mac: &ast::Mac) -> BasicBlock {
        let expr = parse_mac_yield(self.cx, mac);
        let expr = self.expand_moved(&expr);

        self.expr_yield(destination, block, expr)
    }

    fn mac_await(&mut self,
                 destination: Lvalue,
                 extent: CodeExtent,
                 block: BasicBlock,
                 mac: &ast::Mac) -> BasicBlock {
        let expr = parse_mac_await(self.cx, mac);
        let expr = self.expand_moved(&expr);

        self.expr_await(destination, extent, block, expr)
    }
}

fn parse_mac_yield(cx: &ExtCtxt, mac: &ast::Mac) -> P<ast::Expr> {
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

fn parse_mac_await(cx: &ExtCtxt, mac: &ast::Mac) -> P<ast::Expr> {
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
