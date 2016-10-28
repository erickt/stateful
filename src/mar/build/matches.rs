use aster::ident::ToIdent;
use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

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

    pub fn declare_binding<T>(&mut self,
                              span: Span,
                              mutability: ast::Mutability,
                              name: T,
                              var_ty: Option<P<ast::Ty>>) -> Local
        where T: ToIdent,
    {
        let name = name.to_ident();
        debug!("declare_binding(name={:?}, var_ty={:?}, span={:?})",
               name, var_ty, span);

        let shadowed_decl = self.find_local(name);
        let local = self.local_decls.push(LocalDecl {
            mutability: mutability,
            ident: name,
            ty: var_ty,
            shadowed_decl: shadowed_decl,
            span: span,
        });

        self.schedule_drop(span, local);

        debug!("declare_binding: local={:?}", local);

        local
    }
}
