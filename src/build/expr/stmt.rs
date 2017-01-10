// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use build::mac::{is_mac, parse_mac};
use build::scope::LoopScope;
use build::{BlockAnd, BlockAndExtension, Builder};
use mir::*;
use syntax::ast::{self, ExprKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt_expr(&mut self,
                     mut block: BasicBlock,
                     expr: &P<ast::Expr>) -> BlockAnd<()> {
        debug!("stmt_expr(block={:?}, expr={:?})", block, expr);

        let this = self;
        let expr_span = expr.span;
        let source_info = this.source_info(expr.span);
        // Handle a number of expressions that don't need a destination at all. This
        // avoids needing a mountain of temporary `()` variables.
        match expr.node {
            ExprKind::Continue(label) => {
                if !this.is_in_loop() {
                    span_err!(this.cx, expr_span, "cannot continue outside of a loop");
                }

                let LoopScope { continue_block, extent, .. } =
                    *this.find_loop_scope(expr_span, label);
                let after_block = this.cfg.start_new_block(expr_span, Some("AfterContinue"));
                this.exit_scope(expr_span, extent, block, continue_block, after_block);
                after_block.unit()
            }
            ExprKind::Break(label, ref value) => {
                if !this.is_in_loop() {
                    span_err!(this.cx, expr_span, "cannot break outside of a loop");
                }

                let (break_block, extent, destination) = {
                    let LoopScope {
                        break_block,
                        extent,
                        ref break_destination,
                        ..
                    } = *this.find_loop_scope(expr_span, label);
                    (break_block, extent, break_destination.clone())
                };
                if let Some(ref value) = *value {
                    unpack!(block = this.into(destination, block, value));
                } else {
                    this.cfg.push_assign_unit(block, source_info, &destination);
                }
                let after_block = this.cfg.start_new_block(expr_span, Some("AfterBreak"));
                this.exit_scope(expr_span, extent, block, break_block, after_block);
                after_block.unit()

            }
            ExprKind::Assign(ref lhs, ref rhs) => {
                // Note: we evaluate assignments right-to-left. This
                // is better for borrowck interaction with overloaded
                // operators like x[j] = x[i].

                let rhs = unpack!(block = this.as_rvalue(block, rhs));
                let lhs = unpack!(block = this.as_lvalue(block, lhs));

                this.push_assign(block, expr_span, &lhs, rhs);

                block.unit()
            }
            ExprKind::AssignOp(op, ref lhs, ref rhs) => {
                // Note: we evaluate assignments right-to-left. This
                // is better for borrowck interaction with overloaded
                // operators like x[j] = x[i].

                let rhs = unpack!(block = this.as_operand(block, rhs));
                let lhs = unpack!(block = this.as_lvalue(block, lhs));
                let result = unpack!(
                    block = this.build_binary_op(
                        block,
                        op,
                        Operand::Consume(lhs.clone()),
                        rhs));

                this.push_assign(block, expr_span, &lhs, result);

                block.unit()
            }
            ExprKind::Ret(ref returned_expr) => {
                this.expr_ret(block, expr.span, returned_expr)
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "moved") => {
                let expr = parse_mac(this.cx, mac);
                this.stmt_expr(block, &expr)
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "copied") => {
                let expr = parse_mac(this.cx, mac);
                this.stmt_expr(block, &expr)
            }

            _ => {
                let temp = this.temp(block, expr_span, "temp_stmt_expr");
                unpack!(block = this.into(temp, block, expr));
                block.unit()
            }
        }
    }

    /// Compile `return $expr` into:
    ///
    /// ```
    /// 'block:
    ///     $return pointer = $expr;
    ///     goto 'exit;
    ///
    /// 'after_return:
    ///     ...
    /// ```
    fn expr_ret(&mut self,
                mut block: BasicBlock,
                span: Span,
                value: &Option<P<ast::Expr>>) -> BlockAnd<()> {
        block = match *value {
            Some(ref value) => {
                unpack!(self.into(Lvalue::Local(RETURN_POINTER), block, value))
            }
            None => {
                self.push_assign_unit(span, block, &Lvalue::Local(RETURN_POINTER));
                block
            }
        };
        let extent = self.extent_of_return_scope();
        let return_block = self.return_block();

        // We need to start a new block after this one since there might be trailing expressions
        // that we need to type check.
        let after_block = self.cfg.start_new_block(span, Some("AfterReturn"));

        self.exit_scope(span, extent, block, return_block, after_block);

        after_block.unit()
    }
}
