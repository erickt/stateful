use aster::ident::ToIdent;
use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn expr_match(&mut self,
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

        let (block, temp_local, discriminant) = self.expr_temp(
            extent,
            block,
            &discriminant,
            "temp_match_cond");

        self.schedule_move(span, temp_local);

        self.terminate(span, block, TerminatorKind::Match {
            discr: discriminant,
            targets: targets.clone(),
        });

        let mut arm_blocks = vec![];

        self.in_conditional_scope(span, extent, |this| {
            for (arm, target) in arms.iter().zip(targets) {
                this.next_conditional_scope();

                let arm_block = this.in_scope(extent, span, block, |this| {
                    println!("QQQQQQQQQQQQQQQQQQQQ");
                    this.add_decls_from_pats(target.block, arm.pats.iter());
                    this.expr(destination.clone(), extent, target.block, &arm.body)
                });

                println!("HEY: {:?}", this.find_live_decls());

                arm_blocks.push(arm_block);
            }
        });

        println!("aaaaaaaaaaaaaaaaa");

        let join_block = self.start_new_block(span, Some("MatchJoin"));

        for arm_block in arm_blocks {
            self.terminate(
                span,
                arm_block,
                TerminatorKind::Goto { target: join_block, end_scope: true });
        }

        join_block
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

        let shadowed_decl = self.find_decl(name);
        let local = self.local_decls.push(LocalDecl {
            mutability: mutability,
            ident: name,
            ty: var_ty,
            shadowed_decl: shadowed_decl,
            span: span,
        });
        let extent = self.extent_of_innermost_scope();
        self.schedule_drop(span, extent, local);

        debug!("declare_binding: local={:?}", local);

        local
    }

    /// Create a new temporary variable that has a unique name.
    pub fn declare_temp<T>(&mut self, span: Span, name: T) -> Local
        where T: ToIdent,
    {
        // Add a unique number to the name.
        let name = format!("{}{}", name.to_ident(), self.local_decls.len());
        self.declare_binding(span, ast::Mutability::Mutable, name, None)
    }

    /// Create a new temporary lvalue that has a unique name.
    pub fn declare_temp_lvalue<T>(&mut self, span: Span, name: T) -> Lvalue
        where T: ToIdent,
    {
        let temp_decl = self.declare_temp(span, name);
        Lvalue::Local {
            span: span,
            decl: temp_decl,
        }
    }
}
