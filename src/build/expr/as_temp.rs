// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! See docs in build/expr/mod.rs

use build::expr::category::Category;
use build::mac::{is_mac, parse_mac};
use build::{BlockAnd, BlockAndExtension, Builder};
use mir::*;
use syntax::ast::{self, ExprKind};
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn as_temp(&mut self,
                   mut block: BasicBlock,
                   expr: &P<ast::Expr>) -> BlockAnd<Lvalue> {
        debug!("expr_as_temp(block={:?}, expr={:?})", block, expr);
        let this = self;

        let temp = this.temp(block, expr.span, "temp_expr");
        let expr_span = expr.span;

        match expr.node {
            ExprKind::Mac(ref mac) if is_mac(mac, "moved") => {
                let expr = parse_mac(this.cx, mac);
                this.as_temp(block, &expr)
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "copied") => {
                let expr = parse_mac(this.cx, mac);
                this.as_temp(block, &expr)
            }

            _ => {
                // Careful here not to cause an infinite cycle. If we always
                // called `into`, then for lvalues like `x.f`, it would
                // eventually fallback to us, and we'd loop. There's a reason
                // for this: `as_temp` is the point where we bridge the "by
                // reference" semantics of `as_lvalue` with the "by value"
                // semantics of `into`, `as_operand`, `as_rvalue`, and (of
                // course) `as_temp`.
                match Category::of(&expr.node).unwrap() {
                    Category::Lvalue => {
                        let lvalue = unpack!(block = this.as_lvalue(block, expr));
                        let rvalue = Rvalue::Use(Operand::Consume(lvalue));
                        this.push_assign(block, expr_span, &temp, rvalue);
                    }
                    _ => {
                        unpack!(block = this.into(temp.clone(), block, expr));
                    }
                }

                /*
                // In constants, temp_lifetime is None. We should not need to drop
                // anything because no values with a destructor can be created in
                // a constant at this time, even if the type may need dropping.
                if let Some(temp_lifetime) = temp_lifetime {
                    this.schedule_drop(expr_span, temp_lifetime, &temp, expr_ty);
                }
                */

                block.and(temp)
            }
        }
    }
}
