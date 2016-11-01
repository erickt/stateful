// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::repr::*;
use syntax::ast::{self, StmtKind};
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn ast_block(&mut self,
                     destination: Lvalue,
                     mut block: BasicBlock,
                     ast_block: &ast::Block) -> BlockAnd<()> {
        self.in_scope(ast_block.span, block, |this| {
            let (stmts, expr) = split_stmts(&ast_block.stmts[..]);

            // This convoluted structure is to avoid using recursion as we walk down a list
            // of statements. Basically, the structure we get back is something like:
            //
            //    let x = <init> in {
            //       expr1;
            //       let y = <init> in {
            //           expr2;
            //           expr3;
            //           ...
            //       }
            //    }
            //
            // The let bindings are valid till the end of block so all we have to do is to pop all
            // the let-scopes at the end.
            //
            // First we build all the statements in the block.
            let outer_visibility_scope = this.visibility_scope;
            for stmt in stmts {
                match stmt.node {
                    StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => {
                        unpack!(block = this.in_scope(stmt.span, block, |this| {
                            this.stmt_expr(block, expr)
                        }));
                    }
                    StmtKind::Mac(ref mac) => {
                        unpack!(block = this.in_scope(stmt.span, block, |this| {
                            let (ref mac, _, _) = **mac;
                            let temp = this.temp(stmt.span, "temp_stmt_mac");

                            match this.expr_mac(temp.clone(), block, mac) {
                                Some(block) => block,
                                None => {
                                    this.cfg.push(block, Statement::Expr(stmt.clone()));

                                    this.push_assign_unit(stmt.span, block, temp);
                                    block.unit()
                                }
                            }
                        }));
                    }
                    StmtKind::Local(ref local) => {
                        let decls = this.declare_bindings(&local.pat, local.ty.clone());

                        if decls.is_empty() {
                            this.cx.span_bug(stmt.span, "No decls found?")
                        } else if decls.len() == 1 {
                            let decl = decls[0];
                            this.local_decls[decl].ty = local.ty.clone();

                            if let Some(ref init) = local.init {
                                block = unpack!(this.into(Lvalue::Local(decl), block, init));
                            }
                        } else {
                            this.cx.span_bug(stmt.span, "Cannot handle multiple decls at the moment?")
                        }
                    }
                    StmtKind::Item(..) => {
                        this.cx.span_bug(stmt.span, "Cannot handle item declarations yet");
                    }
                }
            }

            if let Some(expr) = expr {
                unpack!(block = this.into(destination, block, &expr));
            } else {
                this.push_assign_unit(ast_block.span, block, destination);
            }

            // Restore the original visibility scope.
            this.visibility_scope = outer_visibility_scope;
            block.unit()
        })
    }
}

fn split_stmts(stmts: &[ast::Stmt]) -> (&[ast::Stmt], Option<P<ast::Expr>>) {
    if let Some((last, remainder)) = stmts.split_last() {
        if let StmtKind::Expr(ref expr) = last.node {
            return (remainder, Some(expr.clone()));
        }
    }

    (stmts, None)
}
