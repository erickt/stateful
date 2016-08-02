use mar::build::Builder;
use mar::build::transition;
use mar::repr::*;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::ext::tt::transcribe::new_tt_reader;
use syntax::parse::common::SeqSep;
use syntax::parse::lexer::{Reader, TokenAndSpan};
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn mac(&mut self, block: BasicBlock, mac: &ast::Mac) -> Option<BasicBlock> {
        if transition::is_yield_path(&mac.node.path) {
            Some(self.mac_yield(block, mac))
        } else {
            None
        }
    }

    fn mac_yield(&mut self, block: BasicBlock, mac: &ast::Mac) -> BasicBlock {
        let (expr, idents) = parse_mac_yield(self.cx, mac);
        assert!(idents.is_empty());

        let expr = self.expand_moved(&expr);

        let next_block = self.start_new_block(mac.span, Some("AfterYield"));

        self.terminate(mac.span, block, TerminatorKind::Yield {
            expr: expr.clone(),
            target: next_block,
        });

        next_block
    }
}

fn parse_mac_yield(cx: &ExtCtxt, mac: &ast::Mac) -> (P<ast::Expr>, Vec<ast::Ident>) {
    let mut rdr = new_tt_reader(&cx.parse_sess().span_diagnostic,
                                None,
                                None,
                                mac.node.tts.clone());

    let mut parser = Parser::new(cx.parse_sess(), cx.cfg(), Box::new(rdr.clone()));

    let expr = panictry!(parser.parse_expr());

    for _ in 0..parser.tokens_consumed {
        let _ = rdr.next_token();
    }

    let TokenAndSpan { tok, sp } = rdr.peek();

    let idents = match tok {
        Token::Eof => Vec::new(),
        Token::Semi => {
            parser.bump();

            let seq_sep = SeqSep::trailing_allowed(Token::Comma);
            let idents = panictry!(parser.parse_seq_to_end(&Token::Eof,
                                                           seq_sep,
                                                           |p| p.parse_ident()));

            if idents.is_empty() {
                cx.span_fatal(sp, "unexpected end of macro");
            }

            idents
        }
        _ => {
            let token_str = parser.this_token_to_string();
            cx.span_fatal(sp, &format!("expected ident, found `{}`", token_str));
        }
    };

    (expr, idents)
}
