use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
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
                    block: self.start_new_block(span, Some("Arm")),
                }
            })
            .collect::<Vec<_>>();

        let join_block = self.start_new_block(span, Some("MatchJoin"));

        for (arm, target) in arms.iter().zip(targets.iter()) {
            let arm_block = self.in_scope(extent, span, block, |this| {
                this.add_decls_from_pats(extent,
                                         target.block,
                                         arm.pats.iter());
                this.expr(extent, target.block, &arm.body)
            });

            self.terminate(span, arm_block, TerminatorKind::Goto { target: join_block });
        }

        self.terminate(span, block, TerminatorKind::Match {
            discr: discriminant.clone(),
            targets: targets,
        });

        join_block
    }
}
