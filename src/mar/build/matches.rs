use aster::ident::ToIdent;
use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::repr::*;
use std::ascii::AsciiExt;
use syntax::ast::{self, PatKind};
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::visit;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn expr_match(&mut self,
                      destination: Lvalue,
                      span: Span,
                      mut block: BasicBlock,
                      discriminant: P<ast::Expr>,
                      arms: &[ast::Arm])
                      -> BlockAnd<()> {
        let operand = unpack!(block = self.as_operand(block, &discriminant));

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
            discr: operand,
            targets: targets.clone(),
        });

        let mut arm_blocks = vec![];

        self.in_conditional_scope(span, |this| {
            for (arm, target) in arms.iter().zip(targets) {
                this.next_conditional_scope();

                let arm_block = this.in_scope(span, block, |this| {
                    this.add_decls_from_pats(target.block, arm.pats.iter());
                    this.into(destination.clone(), target.block, &arm.body)
                });

                arm_blocks.push(unpack!(arm_block));
            }
        });

        let join_block = self.start_new_block(span, Some("MatchJoin"));

        for arm_block in arm_blocks {
            self.terminate(
                span,
                arm_block,
                TerminatorKind::Goto { target: join_block, end_scope: true });
        }

        join_block.unit()
    }

    pub fn declare_bindings(&mut self,
                            pat: &ast::Pat,
                            ty: Option<P<ast::Ty>>) -> Vec<Local> {
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
        let local = self.local_decls.push(LocalDecl {
            mutability: mutability,
            ident: name,
            ty: var_ty,
            shadowed_decl: shadowed_decl,
            source_info: source_info,
        });

        self.schedule_drop(source_info.span, local);

        debug!("declare_binding: local={:?}", local);

        local
    }
}
