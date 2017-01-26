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
    pub fn as_lvalue(&mut self,
                     mut block: BasicBlock,
                     expr: &P<ast::Expr>) -> BlockAnd<Lvalue> {
        debug!("expr_as_lvalue(block={:?}, expr={:?})", block, expr);

        let this = self;

        match expr.node {
            /*
            ExprKind::Scope { extent, value } => {
                this.in_scope(extent, block, |this| this.as_lvalue(block, value))
            }
            */
            ExprKind::Field(ref _lhs, ref _name) => {
                panic!("not supported yet: {:?}", expr)
                /*
                let lvalue = unpack!(block = this.as_lvalue(block, lhs));
                let lvalue = lvalue.field(name, expr.ty);
                block.and(lvalue)
                */
            }
            ExprKind::TupField(ref _lhs, ref _index) => {
                panic!("not supported yet: {:?}", expr)

                /*
                let lvalue = unpack!(block = this.as_lvalue(block, lhs));
                let lvalue = lvalue.field(name, expr.ty);
                block.and(lvalue)
                */
            }
            ExprKind::Unary(ast::UnOp::Deref, ref arg) => {
                let lvalue = unpack!(block = this.as_lvalue(block, arg));
                let lvalue = lvalue.deref();
                block.and(lvalue)
            }
            ExprKind::Index(ref _lhs, ref _index) => {
                panic!("not supported yet: {:?}", expr)

                /*
                let (usize_ty, bool_ty) = (this.hir.usize_ty(), this.hir.bool_ty());

                let slice = unpack!(block = this.as_lvalue(block, lhs));

                let idx = unpack!(block = this.as_operand(block, index));

                // bounds check:
                let (len, lt) = (this.temp(usize_ty.clone()), this.temp(bool_ty));
                this.cfg.push_assign(block, source_info, // len = len(slice)
                                     &len, Rvalue::Len(slice.clone()));
                this.cfg.push_assign(block, source_info, // lt = idx < len
                                     &lt, Rvalue::BinaryOp(BinOp::Lt,
                                                           idx.clone(),
                                                           Operand::Consume(len.clone())));

                let msg = AssertMessage::BoundsCheck {
                    len: Operand::Consume(len),
                    index: idx.clone()
                };
                let success = this.assert(block, Operand::Consume(lt), true,
                                          msg, expr_span);
                success.and(slice.index(idx))
                */
            }
            ExprKind::Path(None, ref path) => {
                if let Some(local) = this.get_local_from_path(&path) {
                    block.and(Lvalue::Local(local))
                } else {
                    block.and(Lvalue::Static(expr.clone()))
                }
            }
            ExprKind::Path(..) => {
                block.and(Lvalue::Static(expr.clone()))
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "moved") => {
                let expr = parse_mac(this.cx, mac);
                this.as_lvalue(block, &expr)
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "copied") => {
                let expr = parse_mac(this.cx, mac);
                this.as_lvalue(block, &expr)
            }

            ExprKind::Try(..) |
            ExprKind::Mac(..) |
            ExprKind::Paren(..) |
            ExprKind::Array(..) |
            ExprKind::Tup(..) |
            ExprKind::Struct(..) |
            ExprKind::Range(..) |
            ExprKind::Closure(..) |
            ExprKind::Unary(..) |
            ExprKind::Binary(..) |
            ExprKind::Box(..) |
            ExprKind::Cast(..) |
            ExprKind::Repeat(..) |
            ExprKind::AddrOf(..) |
            ExprKind::If(..) |
            ExprKind::IfLet(..) |
            ExprKind::Match(..) |
            ExprKind::Loop(..) |
            ExprKind::ForLoop(..) |
            ExprKind::While(..) |
            ExprKind::WhileLet(..) |
            ExprKind::Block(..) |
            ExprKind::Assign(..) |
            ExprKind::AssignOp(..) |
            ExprKind::Break(..) |
            ExprKind::Continue(..) |
            ExprKind::Ret(..) |
            ExprKind::Lit(..) |
            ExprKind::InlineAsm(..) |
            ExprKind::Call(..) |
            ExprKind::MethodCall(..) => {
                // these are not lvalues, so we need to make a temporary.
                debug_assert!(match Category::of(&expr.node) {
                    Some(Category::Lvalue) => false,
                    _ => true,
                });
                this.as_temp(block, expr)
            }

            ExprKind::InPlace(..) |
            ExprKind::Type(..) =>
                panic!("not supported yet: {:?}", expr)
        }
    }
}
