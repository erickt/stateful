// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use build::{BlockAnd, BlockAndExtension, Builder};
use mir::*;
use syntax::ast::{self, StmtKind};
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn ast_block(&mut self,
                     destination: Lvalue,
                     mut block: BasicBlock,
                     ast_block: &ast::Block) -> BlockAnd<()> {
        let span = ast_block.span;
        let extent = self.start_new_extent();
        self.in_scope(extent, ast_block.span, block, |this| {
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
            let mut let_extent_stack = Vec::with_capacity(8);
            let outer_visibility_scope = this.visibility_scope;
            for stmt in stmts {
                match stmt.node {
                    StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => {
                        let extent = this.start_new_extent();
                        unpack!(block = this.in_scope(extent, stmt.span, block, |this| {
                            this.stmt_expr(block, expr)
                        }));
                    }
                    StmtKind::Mac(ref mac) => {
                        let extent = this.start_new_extent();
                        unpack!(block = this.in_scope(extent, stmt.span, block, |this| {
                            let (ref mac, _, _) = **mac;
                            let temp = this.temp(block, stmt.span, "temp_stmt_mac");

                            match this.expr_mac(temp.clone(), block, mac) {
                                Some(block) => block,
                                None => {
                                    let source_info = this.source_info(stmt.span);
                                    this.cfg.push(block, Statement {
                                        source_info: source_info,
                                        kind: StatementKind::Stmt(stmt.clone()),
                                    });

                                    this.cfg.push_assign_unit(block, source_info, &temp);
                                    block.unit()
                                }
                            }
                        }));
                    }
                    StmtKind::Local(ref local) => {
                        // Enter the remainder scope, i.e. the bindings' destruction scope.
                        let remainder_scope = this.start_new_extent();
                        this.push_scope(remainder_scope, stmt.span, block);
                        let_extent_stack.push(remainder_scope);

                        // Declare the bindings, which may cause a visibility scope.
                        let scope = this.declare_bindings(
                            block,
                            None,
                            stmt.span,
                            &local.pat,
                            &local.ty);

                        // Evaluate the initializer, if present.
                        if let Some(ref init) = local.init {
                            if let ast::ExprKind::Block(_) = init.node {
                                let temp = this.temp(block, stmt.span, "local_init");
                                let init_scope = this.start_new_extent();
                                unpack!(block = this.in_scope(init_scope, stmt.span, block, |this| {
                                    unpack!(block = this.into(temp.clone(), block, init));
                                    this.lvalue_into_pattern(block, &local.pat, &temp)
                                }));
                            } else {
                                let init_scope = this.start_new_extent();
                                unpack!(block = this.in_scope(init_scope, stmt.span, block, |this| {
                                    this.expr_into_pattern(block, &local.pat, init)
                                }));
                            }

                            /*
                            let init_scope = this.start_new_extent();
                            unpack!(block = this.in_scope(init_scope, stmt.span, block, |this| {
                                this.expr_into_pattern(block, &local.pat, init)
                            }));
                            */
                        } else {
                            this.storage_live_for_bindings(block, &local.pat);
                        }

                        // Enter the visibility scope, after evaluating the initializer.
                        if let Some(visibility_scope) = scope {
                            this.visibility_scope = visibility_scope;
                        }
                    }
                    StmtKind::Item(..) => {
                        span_bug!(this.cx, stmt.span, "Cannot handle item declarations yet");
                    }
                }
            }
            // Then, the block may have an optional trailing expression which is a "return" value
            // of the block.
            if let Some(expr) = expr {
                unpack!(block = this.into(destination, block, &expr));
            } else {
                let source_info = this.source_info(span);
                this.cfg.push_assign_unit(block, source_info, &destination);
            }
            // Finally, we pop all the let scopes before exiting out from teh scope of the block
            // itself.
            for extent in let_extent_stack.into_iter().rev() {
                unpack!(block = this.pop_scope(extent, ast_block.span, block));
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
