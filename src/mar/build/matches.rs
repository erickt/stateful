use aster::ident::ToIdent;
use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn match_expr(&mut self,
                      destination: Lvalue,
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
                this.add_decls_from_pats(target.block, arm.pats.iter());
                this.expr(destination.clone(), extent, target.block, &arm.body)
            });

            self.terminate(
                span,
                arm_block,
                TerminatorKind::Goto { target: join_block, end_scope: true });
        }

        self.terminate(span, block, TerminatorKind::Match {
            discr: discriminant.clone(),
            targets: targets,
        });

        join_block
    }

    pub fn declare_binding<T>(&mut self,
                              span: Span,
                              mutability: ast::Mutability,
                              name: T,
                              var_ty: Option<P<ast::Ty>>) -> Var
        where T: ToIdent,
    {
        let name = name.to_ident();
        debug!("declare_binding(name={:?}, var_ty={:?}, span={:?})",
               name, var_ty, span);

        let shadowed_decl = self.find_decl(name);
        let var = self.var_decls.push(VarDecl {
            mutability: mutability,
            ident: name,
            ty: var_ty,
            shadowed_decl: shadowed_decl,
        });
        let extent = self.extent_of_innermost_scope();
        self.schedule_drop(span, extent, var);

        debug!("declare_binding: var={:?}", var);

        var
    }

    /// Create a new temporary variable that has a unique name.
    pub fn declare_temp<T>(&mut self, span: Span, name: T) -> Var
        where T: ToIdent,
    {
        // Add a unique number to the name.
        let name = format!("{}{}", name.to_ident(), self.var_decls.len());
        self.declare_binding(span, ast::Mutability::Immutable, name, None)
    }
}
