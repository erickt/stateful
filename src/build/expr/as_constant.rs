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

use build::Builder;
use mir::*;
use syntax::ast::{self, ExprKind};
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Compile `expr`, yielding a compile-time constant. Assumes that
    /// `expr` is a valid compile-time constant!
    pub fn as_constant(&mut self, expr: &P<ast::Expr>) -> Constant {
        let span = expr.span;
        match expr.node {
            ExprKind::Lit(ref literal) =>
                Constant { span: span, literal: literal.clone() },
            _ =>
                self.cx.span_bug(
                    span,
                    &format!("expression is not a valid constant {:?}", expr))
        }
    }
}
