use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a> Builder<'a> {
    pub fn match_expr(&mut self,
                      extent: CodeExtent,
                      span: Span,
                      block: BasicBlock,
                      discriminant: P<ast::Expr>,
                      arms: &[ast::Arm])
                      -> BasicBlock {
        let targets = arms.iter()
            .map(|arm| {
                Arm {
                    pats: arm.pats.clone(),
                    guard: arm.guard.clone(),
                    block: self.start_new_block(Some("Arm")),
                }
            })
            .collect::<Vec<_>>();

        let join_block = self.start_new_block(Some("MatchJoin"));

        for (arm, target) in arms.iter().zip(targets.iter()) {
            let arm_block = self.in_scope(extent, block, |this| {
                this.add_decls_from_pats(span,
                                         extent,
                                         target.block,
                                         arm.pats.iter());
                this.expr(extent, target.block, &arm.body)
            });

            self.terminate(arm_block, TerminatorKind::Goto { target: join_block });
        }

        self.terminate(block, TerminatorKind::Match {
            discr: discriminant.clone(),
            targets: targets,
        });

        join_block
    }
}
