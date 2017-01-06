// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use build::expr::category::Category;
use build::mac::{is_mac, parse_mac};
use build::{BlockAndExtension, Builder, BlockAnd};
use mir::*;
use syntax::ast::{self, ExprKind};
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn as_operand(&mut self,
                      mut block: BasicBlock,
                      expr: &P<ast::Expr>) -> BlockAnd<Operand> {
        debug!("expr_as_operand(block={:?}, expr={:?})", block, expr);
        let this = self;

        match expr.node {
            ExprKind::Path(..) => {
                // Path operands don't need a temporary.
                let operand = unpack!(block = this.as_lvalue(block, expr));
                block.and(Operand::Consume(operand))
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "moved") => {
                let expr = parse_mac(this.cx, mac);
                this.as_operand(block, &expr)
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "copied") => {
                let expr = parse_mac(this.cx, mac);
                this.as_operand(block, &expr)
            }

            /*
            ExprKind::AddrOf(..) => {
                // `&x` operands don't need a temporary.
                let operand = unpack!(block = this.as_lvalue(block, expr));
                this.move_lvalue(expr.span, &operand);
                block.and(Operand::Consume(operand))
            }
            */
            _ => {
                let category = Category::of(&expr.node).unwrap();
                debug!("expr_as_operand: category={:?} for={:?}", category, expr.node);
                match category {
                    Category::Constant => {
                        let constant = this.as_constant(expr);
                        block.and(Operand::Constant(constant))
                    }
                    Category::Lvalue |
                    Category::Rvalue(..) => {
                        let operand = unpack!(block = this.as_temp(block, expr));
                        block.and(Operand::Consume(operand))
                    }
                }
            }
        }
    }
}
