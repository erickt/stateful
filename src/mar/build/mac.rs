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
                 mut block: BasicBlock,
                 mac: &ast::Mac) -> BasicBlock {
        let span = mac.span;
        let builder = AstBuilder::new().span(span);

        // First, parse and expand the macro.
        let expr = parse_mac_await(self.cx, mac);
        let expr = self.expand_moved(&expr);

        // Next, declare a variable which will store the awaited future.
        let future_ident = builder.id("future");

        let future_stmt = builder.stmt()
            .let_().mut_id(future_ident).build_expr(expr);

        block = self.stmt(extent, block, &future_stmt);

        // Next, construct a match that polls the future until it's ready:
        //
        // match try!(sleeper.poll()) {
        //     Async::Ready(result) => {
        //         goto $next_state with result
        //     }
        //     Async::NotReady => {
        //         return Async::NotReady
        //     }
        // }

        let loop_block = self.start_new_block(span, Some("AwaitLoop"));
        let exit_block = self.start_new_block(span, Some("AwaitExit"));

        self.terminate(span, block, TerminatorKind::Goto {
            target: loop_block,
            end_scope: true,
        });

        // Handle the ready arm.
        let ready_ident = builder.id("result");

        let ready_path = builder.path()
            .global()
            .ids(&["futures", "Async", "Ready"])
            .build();

        let ready_pat = builder.pat().enum_()
            .build(ready_path)
            .id(ready_ident)
            .build();

        let ready_arm = Arm {
            pats: vec![ready_pat],
            guard: None,
            block: self.start_new_block(span, Some("AwaitReady")),
        };

        let ready_arm_block = self.in_scope(extent, span, block, |this| {
            this.add_decls_from_pats(ready_arm.block, ready_arm.pats.iter());

            // Don't try to store the result if we're just writing into a temporary.
            if destination.is_temp() {
                ready_arm.block
            } else {
                let ready_expr = builder.expr().id(ready_ident);
                this.expr(destination, extent, ready_arm.block, &ready_expr)
            }
        });

        self.terminate(span, ready_arm_block, TerminatorKind::Goto {
            target: exit_block,
            end_scope: true,
        });

        // Handle the not ready arm.
        let not_ready_path = builder.path()
            .global()
            .ids(&["futures", "Async", "NotReady"])
            .build();

        let not_ready_pat = builder.pat().path()
            .build(not_ready_path);

        let not_ready_arm = Arm {
            pats: vec![not_ready_pat],
            guard: None,
            block: self.start_new_block(span, Some("AwaitNotReady")),
        };

        self.terminate(span, not_ready_arm.block, TerminatorKind::Await {
            target: loop_block,
        });

        // Finally, handle the match.
        let poll_path = builder.path()
            .global()
            .ids(&["futures", "Future", "poll"])
            .build();

        let future_poll_expr = builder.expr()
            .try()
            .call()
            .path().build(poll_path)
            .arg()
            .mut_ref().id(future_ident)
            .build();

        self.terminate(span, loop_block, TerminatorKind::Match {
            discr: future_poll_expr,
            targets: vec![ready_arm, not_ready_arm],
        });

        exit_block
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
