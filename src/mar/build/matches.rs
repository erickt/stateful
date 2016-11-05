// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! Code related to match expresions. These are sufficiently complex
//! to warrant their own module and submodules. :) This main module
//! includes the high-level algorithm, the submodules contain the
//! details.

use aster::ident::ToIdent;
use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::repr::*;
use std::ascii::AsciiExt;
use syntax::ast::{self, PatKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn match_expr(&mut self,
                      destination: Lvalue,
                      span: Span,
                      mut block: BasicBlock,
                      discriminant: P<ast::Expr>,
                      arms: &[ast::Arm])
                      -> BlockAnd<()> {
        let discriminant_lvalue = unpack!(block = self.as_lvalue(block, &discriminant));

        let targets = arms.iter()
            .map(|arm| {
                Arm {
                    pats: arm.pats.clone(),
                    guard: arm.guard.clone(),
                    block: self.start_new_block(span, Some("Arm")),
                }
            })
            .collect::<Vec<_>>();

        self.terminate(span, block, TerminatorKind::Match {
            discr: discriminant_lvalue,
            targets: targets.clone(),
        });

        let mut arm_bodies = vec![];

        let outer_source_info = self.source_info(span);
        self.in_conditional_scope(span, |this| {
            for (arm, target) in arms.iter().zip(targets) {
                this.next_conditional_scope();

                let scope = this.declare_bindings(
                    None,
                    arm.body.span,
                    &arm.pats[0],
                    &None);

                // Re-enter the visibility scope we created the bindings in.
                this.visibility_scope = scope.unwrap_or(this.visibility_scope);

                let body = this.into(destination.clone(), target.block, &arm.body);

                arm_bodies.push(unpack!(body));
            }
        });

        // all the arm blocks will rejoin here
        let end_block = self.start_new_block(span, Some("MatchEnd"));

        for body in arm_bodies {
            self.terminate(
                span,
                body,
                TerminatorKind::Goto { target: end_block, end_scope: true });
        }
        self.visibility_scope = outer_source_info.scope;

        end_block.unit()
    }

    pub fn expr_into_pattern(&mut self,
                             mut block: BasicBlock,
                             irrefutable_pat: &P<ast::Pat>,
                             initializer: &P<ast::Expr>)
                             -> BlockAnd<()> {
        // optimize the case of `let x = ...`
        match irrefutable_pat.node {
            PatKind::Ident(ast::BindingMode::ByValue(mutability), id, _) if self.is_local(id) => {
                let source_info = self.source_info(irrefutable_pat.span);
                let local = self.declare_binding(
                    source_info,
                    mutability,
                    id.node,
                    None);

                //self.storage_live_for_bindings(block, &irrefutable_pat);
                let lvalue = Lvalue::Local(local);
                return self.into(lvalue, block, initializer);
            }
            _ => {}
        }
        let lvalue = unpack!(block = self.as_lvalue(block, initializer));
        self.lvalue_into_pattern(block,
                                 irrefutable_pat,
                                 &lvalue)
    }

    fn add_decl_to_block(&mut self, block: BasicBlock, live_decl: LiveDecl) {
        let block_data = self.cfg.block_data_mut(block);
        let live_decls = block_data.live_decls.entry(self.visibility_scope)
            .or_insert_with(Vec::new);
        live_decls.push(live_decl);
    }

    pub fn lvalue_into_pattern(&mut self,
                               block: BasicBlock,
                               irrefutable_pat: &P<ast::Pat>,
                               initializer: &Lvalue)
                               -> BlockAnd<()> {
        match irrefutable_pat.node {
            PatKind::Ident(ast::BindingMode::ByValue(_mutability), id, _) if self.is_local(id) => {
                //let source_info = self.source_info(irrefutable_pat.span);
                //let local = self.declare_binding(source_info, mutability, id.node, None);
                //let binding_source = Lvalue::Local(local);
                //self.initialize(block, irrefutable_pat.span, binding_source);

                let local = match *initializer {
                    Lvalue::Local(local) => local,
                    _ => {
                        span_bug!(self.cx, irrefutable_pat.span,
                                  "initializer not local? {:?}", initializer)
                    }
                };

                //self.initialize(block, irrefutable_pat.span, initializer.clone());
                self.add_decl_to_block(block, LiveDecl::Active(local));

                /*
                let rvalue = Rvalue::Use(Operand::Consume(binding_source));

                self.push_assign(block, source_info.span, initializer, rvalue);
                */

                block.unit()
            }
            _ => {
                span_bug!(
                    self.cx,
                    irrefutable_pat.span,
                    "Canot handle pat {:?}",
                    irrefutable_pat)
            }
        }
    }

    pub fn declare_bindings(&mut self,
                            mut var_scope: Option<VisibilityScope>,
                            _scope_span: Span,
                            pat: &ast::Pat,
                            ty: &Option<P<ast::Ty>>) -> Option<VisibilityScope> {
        debug!("declare_bindings(scope={:?}, pat={:?}, ty={:?}", var_scope, pat, ty);

        match pat.node {
            PatKind::Ident(ast::BindingMode::ByValue(mutability), id, _) => {
                // Consider only lower case identities as a variable.
                if self.is_local(id) {
                    if var_scope.is_none() {
                        var_scope = Some(self.new_visibility_scope(pat.span));
                    }
                    let source_info = SourceInfo {
                        span: pat.span,
                        scope: var_scope.unwrap()
                    };
                    self.declare_binding(
                        source_info,
                        mutability,
                        id.node,
                        ty.clone());
                }
            }
            _ => {
                self.cx.span_bug(
                    pat.span,
                    &format!("Canot handle pat {:?}", pat))
            }
        }
        var_scope

        /*
        struct Visitor<'a, 'b: 'a, 'c: 'b> {
            builder: &'a mut Builder<'b, 'c>,
            new_vars: Vec<Local>,
            ty: Option<P<ast::Ty>>,
        }

        impl<'a, 'b: 'a, 'c: 'b> visit::Visitor for Visitor<'a, 'b, 'c> {
            fn visit_pat(&mut self, pat: &ast::Pat) {
                match pat.node {
                    PatKind::Ident(ast::BindingMode::ByValue(mutability), id, _) => {
                        // Consider only lower case identities as a variable.
                        let id_str = id.node.name.as_str();
                        let first_char = id_str.chars().next().unwrap();

                        if first_char == first_char.to_ascii_lowercase() {
                            let source_info = SourceInfo {
                                span: pat.span,
                                scope: self.builder.visibility_scope,
                            };

                            let var = self.builder.declare_binding(
                                source_info,
                                mutability,
                                id.node,
                                self.ty.clone(),
                                );
                            self.new_vars.push(var);
                        }
                    }
                    PatKind::Ident(..) => {
                        self.builder.cx.span_bug(
                            pat.span,
                            &format!("Canot handle pat {:?}", pat))
                    }
                    _ => { }
                }

                visit::walk_pat(self, pat);
            }

            fn visit_mac(&mut self, _mac: &ast::Mac) { }
        }

        let mut visitor = Visitor {
            builder: self,
            new_vars: vec![],
            ty: ty,
        };

        visit::Visitor::visit_pat(&mut visitor, pat);

        debug!("declare_bindings: {:?} => {:?}", pat, visitor.new_vars);

        visitor.new_vars
        */
    }

    pub fn declare_binding<T>(&mut self,
                              source_info: SourceInfo,
                              mutability: ast::Mutability,
                              name: T,
                              var_ty: Option<P<ast::Ty>>) -> Local
        where T: ToIdent,
    {
        let name = name.to_ident();
        debug!("declare_binding(source_info={:?}, name={:?}, var_ty={:?})",
               source_info, name, var_ty);

        let shadowed_decl = self.find_local(name);
        let var = self.local_decls.push(LocalDecl {
            mutability: mutability,
            ident: name,
            ty: var_ty,
            shadowed_decl: shadowed_decl,
            source_info: source_info,
        });
        let extent = self.extent_of_innermost_scope();
        self.schedule_drop(source_info.span, extent, &Lvalue::Local(var));

        debug!("declare_binding: var={:?}", var);

        var
    }

    fn is_local(&self, id: ast::SpannedIdent) -> bool {
        // Consider only lower case identities as a variable.
        let id_str = id.node.name.as_str();
        let first_char = id_str.chars().next().unwrap();

        first_char == first_char.to_ascii_lowercase()
    }
}
