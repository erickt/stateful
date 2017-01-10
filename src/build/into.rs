// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! In general, there are a number of things for which it's convenient
//! to just call `builder.into` and have it emit its result into a
//! given location. This is basically for expressions or things that can be
//! wrapped up as expressions (e.g. blocks). To make this ergonomic, we use this
//! latter `EvalInto` trait.

use build::{BlockAnd, Builder};
use mir::*;
use syntax::ast;
use syntax::ptr::P;

pub trait EvalInto {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 block: BasicBlock) -> BlockAnd<()>;
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn into<E>(&mut self,
                   destination: Lvalue,
                   block: BasicBlock,
                   expr: E) -> BlockAnd<()>
        where E: EvalInto
    {
        expr.eval_into(self, destination, block)
    }
}

impl<'a> EvalInto for &'a P<ast::Expr> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 block: BasicBlock) -> BlockAnd<()> {
        builder.into_expr(destination, block, self)
    }
}

impl EvalInto for P<ast::Expr> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 block: BasicBlock) -> BlockAnd<()> {
        builder.into_expr(destination, block, &self)
    }
}

impl<'a> EvalInto for &'a P<ast::Block> {
    fn eval_into(self,
                 builder: &mut Builder,
                 destination: Lvalue,
                 block: BasicBlock) -> BlockAnd<()> {
        builder.ast_block(destination, block, self)
    }
}
