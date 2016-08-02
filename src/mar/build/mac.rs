use mar::build::{Builder, transition};
use mar::repr::*;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::ext::tt::transcribe::new_tt_reader;
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn mac(&mut self, block: BasicBlock, mac: &ast::Mac) -> Option<BasicBlock> {
        match self.state_machine_kind {
            StateMachineKind::Generator => {
                if transition::is_yield_path(&mac.node.path) {
                    Some(self.mac_yield(block, mac))
                } else {
                    None
                }
            }
            StateMachineKind::Async => {
                /*
                if transition::is_await_path(&mac.node.path) {
                    Some(self.mac_await(block, mac))
                } else
                */
                if transition::is_suspend_path(&mac.node.path) {
                    Some(self.mac_suspend(block, mac))
                } else {
                    None
                }
            }
        }
    }

    fn mac_yield(&mut self, block: BasicBlock, mac: &ast::Mac) -> BasicBlock {
        let expr = parse_mac_yield(self.cx, mac);
        let expr = self.expand_moved(&expr);

        let next_block = self.start_new_block(mac.span, Some("AfterYield"));

        self.terminate(mac.span, block, TerminatorKind::Yield {
            expr: expr.clone(),
            target: next_block,
        });

        next_block
    }

    /*
    fn mac_await(&mut self, block: BasicBlock, mac: &ast::Mac) -> BasicBlock {
        let expr = parse_mac_await(self.cx, mac);
        let expr = self.expand_moved(&expr);

        let next_block = self.start_new_block(mac.span, Some("AfterAwait"));

        self.terminate(mac.span, block, TerminatorKind::Resume {
            expr: expr,
            target: next_block,
        });

        next_block
    }
    */

    fn mac_suspend(&mut self, block: BasicBlock, mac: &ast::Mac) -> BasicBlock {
        parse_mac_suspend(self.cx, mac);

        let next_block = self.start_new_block(mac.span, Some("AfterSuspend"));

        self.terminate(mac.span, block, TerminatorKind::Suspend {
            target: next_block,
        });

        next_block
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

/*
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
*/

fn parse_mac_suspend(cx: &ExtCtxt, mac: &ast::Mac) {
    let rdr = new_tt_reader(
        &cx.parse_sess().span_diagnostic,
        None,
        None,
        mac.node.tts.clone());

    let mut parser = Parser::new(cx.parse_sess(), cx.cfg(), Box::new(rdr.clone()));
    panictry!(parser.expect(&Token::Eof))
}
