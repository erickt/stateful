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
use build::{BlockAnd, BlockAndExtension, Builder};
use mir::*;
use std::ascii::AsciiExt;
use std::collections::HashMap;
use syntax::ast::{self, PatKind};
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::visit::{self, Visitor};

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn match_expr(&mut self,
                      destination: Lvalue,
                      span: Span,
                      mut block: BasicBlock,
                      discriminant: P<ast::Expr>,
                      arms: &[ast::Arm])
                      -> BlockAnd<()> {
        debug!("match_expr(destination={:?}, block={:?} discriminant={:?} arms={:?}",
               destination,
               block,
               discriminant,
               arms);

        let discriminant_lvalue = unpack!(block = self.as_operand(block, &discriminant));

        let targets = arms.iter()
            .map(|arm| {
                Arm {
                    pats: arm.pats.clone(),
                    guard: arm.guard.clone(),
                    block: self.cfg.start_new_block(span, Some("Arm")),
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
                this.next_conditional_scope(arm.body.span);

                let extent = this.start_new_extent();
                let body = unpack!(this.in_scope(extent, span, target.block, |this| {
                    let scope = this.declare_bindings(
                        None,
                        arm.body.span,
                        &arm.pats[0],
                        &None);

                    let pat_locals = this.locals_from_pat(&arm.pats[0]);

                    for local in &pat_locals {
                        this.initialize(block, arm.pats[0].span, &Lvalue::Local(*local));
                    }

                    // Re-enter the visibility scope we created the bindings in.
                    this.visibility_scope = scope.unwrap_or(this.visibility_scope);

                    this.into(destination.clone(), target.block, &arm.body)
                }));

                arm_bodies.push(body);
            }
        });

        // all the arm blocks will rejoin here
        let end_block = self.cfg.start_new_block(span, Some("MatchEnd"));

        for body in arm_bodies {
            self.terminate(span, body, TerminatorKind::Goto {
                target: end_block,
                phantom_target: None,
            });
        }
        self.visibility_scope = outer_source_info.scope;

        end_block.unit()
    }

    pub fn expr_into_pattern(&mut self,
                             mut block: BasicBlock,
                             irrefutable_pat: &P<ast::Pat>,
                             initializer: &P<ast::Expr>)
                             -> BlockAnd<()> {
        debug!("expr_into_pattern(pat={:?}, init={:?})", irrefutable_pat, initializer);

        // optimize the case of `let x = ...`
        match irrefutable_pat.node {
            PatKind::Ident(ast::BindingMode::ByValue(_mutability), id, _) if self.is_local(id) => {
                self.storage_live_for_bindings(block, &irrefutable_pat);
                let lvalue = Lvalue::Local(self.var_indices[&irrefutable_pat.id]);

                return self.into(lvalue, block, initializer);
            }
            _ => {}
        }
        let lvalue = unpack!(block = self.as_lvalue(block, initializer));
        self.lvalue_into_pattern(block,
                                 irrefutable_pat,
                                 &lvalue)
    }

    pub fn lvalue_into_pattern(&mut self,
                               block: BasicBlock,
                               irrefutable_pat: &P<ast::Pat>,
                               initializer: &Lvalue)
                               -> BlockAnd<()> {
        debug!("lvalue_into_pattern(pat={:?}, init={:?})", irrefutable_pat, initializer);

        let span = irrefutable_pat.span;
        let source_info = self.source_info(span);
        let locals = self.locals_from_pat(irrefutable_pat);

        if locals.is_empty() {
            span_bug!(self.cx, span, "no variables found in pattern: {:?}", irrefutable_pat);
        }

        // Initialize all the locals.
        for local in &locals {
            let lvalue = Lvalue::Local(*local);

            self.cfg.push(block,
                          Statement {
                              source_info: source_info,
                              kind: StatementKind::StorageLive(lvalue.clone()),
                          });

            self.initialize(block, span, &lvalue);
        }

        let source_info = self.source_info(span);
        self.cfg.push(block, Statement {
            source_info: source_info,
            kind: StatementKind::Let {
                pat: irrefutable_pat.clone(),
                ty: self.ty_indices.get(&irrefutable_pat.id).map(|ty| ty.clone()),
                lvalues: locals,
                rvalue: Rvalue::Use(Operand::Consume(initializer.clone())),
            }
        });

        block.unit()
    }

    pub fn declare_bindings(&mut self,
                            mut var_scope: Option<VisibilityScope>,
                            scope_span: Span,
                            pat: &P<ast::Pat>,
                            ty: &Option<P<ast::Ty>>) -> Option<VisibilityScope> {
        debug!("declare_bindings(scope={:?}, pat={:?}, ty={:?}", var_scope, pat, ty);

        // Cache the type of this pattern.
        if let Some(ref ty) = *ty {
            self.ty_indices.insert(pat.id, ty.clone());
        }

        match pat.node {
            // Consider only lower case identities as a variable.
            PatKind::Ident(ast::BindingMode::ByValue(mutability), id, _) if self.is_local(id) => {
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
                    pat.id,
                    ty.clone());
            }
            PatKind::Ident(ast::BindingMode::ByValue(_), _, _) => { }

            PatKind::Struct(_, ref subpatterns, _) => {
                for field in subpatterns {
                    var_scope = self.declare_bindings(
                        var_scope,
                        scope_span,
                        &field.node.pat,
                        &None);
                }
            }

            PatKind::TupleStruct(_, ref subpatterns, _) |
            PatKind::Tuple(ref subpatterns, _) => {
                for subpattern in subpatterns {
                    var_scope = self.declare_bindings(
                        var_scope,
                        scope_span,
                        subpattern,
                        &None);
                }
            }

            PatKind::Slice(ref prefix, ref slice, ref suffix) => {
                for subpattern in prefix.iter().chain(slice).chain(suffix) {
                    var_scope = self.declare_bindings(
                        var_scope,
                        scope_span,
                        subpattern,
                        &None);
                }
            }

            // These patterns don't contain any bindings
            PatKind::Lit(_) |
            PatKind::Path(_, _) |
            PatKind::Range(_, _) |
            PatKind::Wild => { }

            PatKind::Ident(ast::BindingMode::ByRef(_), _, _) |
            PatKind::Box(_) |
            PatKind::Ref(_, _) |
            PatKind::Mac(_) => {
                span_bug!(self.cx, pat.span, "Cannot handle pat {:?}", pat)
            }
        }

        var_scope
    }

    /// Emit `StorageLive` for every binding in the pattern.
    pub fn storage_live_for_bindings(&mut self,
                                     block: BasicBlock,
                                     pattern: &P<ast::Pat>) {
        match pattern.node {
            PatKind::Ident(_, id, _) if self.is_local(id) => {
                let lvalue = Lvalue::Local(self.var_indices[&pattern.id]);
                let source_info = self.source_info(pattern.span);
                self.cfg.push(block, Statement {
                    source_info: source_info,
                    kind: StatementKind::StorageLive(lvalue)
                });

                /*
                if let Some(subpattern) = subpattern.as_ref() {
                    self.storage_live_for_bindings(block, subpattern);
                }
                */
            }
            PatKind::Ident(ast::BindingMode::ByValue(_), _, _) => { }

            PatKind::Struct(_, ref subpatterns, _) => {
                for field in subpatterns {
                    self.storage_live_for_bindings(block, &field.node.pat);
                }
            }

            PatKind::TupleStruct(_, ref subpatterns, _) |
            PatKind::Tuple(ref subpatterns, _) => {
                for subpattern in subpatterns {
                    self.storage_live_for_bindings(block, subpattern);
                }
            }

            PatKind::Slice(ref prefix, ref slice, ref suffix) => {
                for subpattern in prefix.iter().chain(slice).chain(suffix) {
                    self.storage_live_for_bindings(block, subpattern);
                }
            }

            // These patterns don't contain any bindings
            PatKind::Lit(_) |
            PatKind::Path(_, _) |
            PatKind::Range(_, _) |
            PatKind::Wild => { }

            PatKind::Ident(ast::BindingMode::ByRef(_), _, _) |
            PatKind::Box(_) |
            PatKind::Ref(_, _) |
            PatKind::Mac(_) => {
                span_bug!(self.cx, pattern.span, "Cannot handle pat {:?}", pattern)
            }

            /*
            PatternKind::Array { ref prefix, ref slice, ref suffix } |
            PatternKind::Slice { ref prefix, ref slice, ref suffix } => {
                for subpattern in prefix.iter().chain(slice).chain(suffix) {
                    self.storage_live_for_bindings(block, subpattern);
                }
            }
            PatternKind::Constant { .. } | PatternKind::Range { .. } | PatternKind::Wild => {
            }
            PatternKind::Deref { ref subpattern } => {
                self.storage_live_for_bindings(block, subpattern);
            }
            PatternKind::Leaf { ref subpatterns } |
            PatternKind::Variant { ref subpatterns, .. } => {
                for subpattern in subpatterns {
                    self.storage_live_for_bindings(block, &subpattern.pattern);
                }
            }
            */
        }
    }

    fn declare_binding<T>(&mut self,
                          source_info: SourceInfo,
                          mutability: ast::Mutability,
                          name: T,
                          var_id: ast::NodeId,
                          var_ty: Option<P<ast::Ty>>) -> Local
        where T: ToIdent,
    {
        let name = name.to_ident();
        debug!("declare_binding(source_info={:?}, name={:?}, var_id={:?}, var_ty={:?})",
               source_info,
               name,
               var_id,
               var_ty);

        let shadowed_decl = self.find_local(name);
        let var = self.local_decls.push(LocalDecl {
            mutability: mutability,
            name: name,
            ty: var_ty,
            shadowed_decl: shadowed_decl,
            source_info: source_info,
        });
        let extent = self.extent_of_innermost_scope();
        self.schedule_drop(source_info.span, extent, &Lvalue::Local(var));
        self.var_indices.insert(var_id, var);

        debug!("declare_binding: var={:?}", var);

        var
    }

    // Extract locals from a pattern. This must be run after `declare_bindings` on this pattern.
    pub fn locals_from_pat(&self, pat: &P<ast::Pat>) -> Vec<Local> {
        struct PatVisitor<'a> {
            var_indices: &'a HashMap<ast::NodeId, Local>,
            locals: Vec<Local>,
        }

        impl<'a> Visitor<'a> for PatVisitor<'a> {
            fn visit_pat(&mut self, pat: &'a ast::Pat) {
                if let Some(&local) = self.var_indices.get(&pat.id) {
                    debug!("locals_from_pat: pat={:?} local={:?}", pat, local);
                    self.locals.push(local);
                }

                visit::walk_pat(self, pat);
            }
        }

        let mut visitor = PatVisitor {
            var_indices: &self.var_indices,
            locals: vec![],
        };

        visitor.visit_pat(pat);

        visitor.locals
    }

    fn is_local(&self, id: ast::SpannedIdent) -> bool {
        // Consider only lower case identities as a variable.
        let id_str = id.node.name.as_str();
        let first_char = id_str.chars().next().unwrap();

        first_char == first_char.to_ascii_lowercase()
    }
}
