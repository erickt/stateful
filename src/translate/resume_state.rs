use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::HashSet;
use super::builder::Builder;
use super::local_stack::LocalStack;
use super::state::StateKind;
use syntax::ast;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn resume_state(&self) -> ResumeState {
        let blocks = &self.resume_blocks;

        let mut variants = Vec::with_capacity(blocks.len());
        let mut seen_ty_param_ids = HashSet::new();
        let mut ty_param_ids = vec![];
        let arms = blocks.iter()
            .map(|&block| {
                let (variant, tp) = self.state_variant(block, StateKind::Resume);
                variants.push(variant);

                // It's possible for a declaration to be created but not actually get used in the
                // state variables, so we only create a type parameter for a declaration if it's
                // actually used.
                for ty_param_id in tp {
                    if !seen_ty_param_ids.contains(&ty_param_id) {
                        seen_ty_param_ids.insert(ty_param_id);
                        ty_param_ids.push(ty_param_id);
                    }
                }

                self.resume_arm(block)
            })
            .collect::<Vec<_>>();

        let generics = self.ast_builder.from_generics(self.mir.fn_decl.generics.clone())
            .with_ty_param_ids(ty_param_ids.iter())
            .build();

        let enum_item = self.ast_builder.item().enum_("ResumeState")
            .generics().with(generics.clone()).build()
            .with_variants(variants)
            .id("Illegal")
            .build();

        let state_path = self.ast_builder
            .path()
                .segment("ResumeState")
                .with_generics(self.mir.fn_decl.generics.clone())
                .with_tys(
                    ty_param_ids.iter().map(|variable| self.ast_builder.ty().id(variable))
                )
                .build()
            .build();

        let default_item = quote_item!(self.cx,
            impl $generics ::std::default::Default for $state_path {
                fn default() -> Self {
                    ResumeState::Illegal
                }
            }
        ).expect("state default item");

        let stmts = vec![
            self.ast_builder.stmt().build_item(enum_item),
            self.ast_builder.stmt().build_item(default_item),
        ];

        let expr = quote_expr!(self.cx,
            match resume_state {
                $arms
                ResumeState::Illegal => { panic!("illegal state") }
            }
        );

        ResumeState {
            stmts: stmts,
            expr: expr,
        }
    }

    pub fn start_state_expr(&self) -> P<ast::Expr> {
        let span = self.block_span(START_BLOCK);
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(START_BLOCK, StateKind::Resume);

        let scope_locals = self.scope_locals[&START_BLOCK].iter()
            // The start expression doesn't yet have coroutine args initialized yet.
            .filter(|&(&scope, _)| scope != COROUTINE_ARGS_VISIBILITY_SCOPE)
            .collect::<Vec<_>>();
        
        if scope_locals.is_empty() {
            ast_builder.expr().build_path(state_path)
        } else {
            // Pack up all the locals back into scope tuples.
            let exprs = scope_locals.iter()
                .map(|&(_, locals)| {
                    ast_builder.expr().tuple()
                        .with_exprs(
                            locals.iter().map(|local| {
                                let name = &self.mir.local_decls[*local].name;
                                ast_builder.expr().id(name)
                            })
                        ).build()
                });

            ast_builder.expr().call()
                .build_path(state_path)
                .with_args(exprs)
                .build()
        }
    }

    pub fn resume_state_expr(&self,
                             block: BasicBlock,
                             local_stack: &LocalStack) -> P<ast::Expr> {
        self.state_expr(block, local_stack, StateKind::Resume)
    }

    /// Build up an `ast::Arm` for a resume state variant. This arm's role is to lift up the
    /// resume arguments into the state machine, which is simply generating a conversion like
    /// this:
    ///
    /// ```rust
    /// ResumeInternal::State1(scope1, scope2) => {
    ///     InternalState::State1(scope1, scope2, args)
    /// }
    /// ```
    fn resume_arm(&self, block: BasicBlock) -> ast::Arm {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let scope_ids = self.scope_locals[&block].iter()
            .map(|(&scope, _)| (scope, ast_builder.id(format!("scope{}", scope.index()))))
            .collect::<Vec<_>>();

        // Build the resume arm. Note that the resume start block does not get the
        // `coroutine_args`.
        let resume_path = self.state_path(block, StateKind::Resume);
        let resume_pat = if false { //scope_ids.is_empty() {
            ast_builder.pat().build_path(resume_path)
        } else {
            let pats = scope_ids.iter()
                .filter(|&&(scope, _)| {
                    block != START_BLOCK || scope != COROUTINE_ARGS_VISIBILITY_SCOPE
                })
                .map(|&(_, id)| ast_builder.pat().id(id))
                .collect::<Vec<_>>();

            if pats.is_empty() {
                ast_builder.pat().build_path(resume_path)
            } else {
                ast_builder.pat().enum_().build(resume_path)
                    .with_pats(pats)
                    .build()
            }
        };

        let internal_path = self.state_path(block, StateKind::Internal);

        let coroutine_stmt = ast_builder.stmt()
            .let_id(format!("scope{}", COROUTINE_ARGS_VISIBILITY_SCOPE.index()))
            .expr().tuple()
                .expr().id("coroutine_args")
                .build();

        let internal_expr = ast_builder.expr().call()
            .build_path(internal_path)
            .with_args(
                scope_ids.iter().map(|&(_, id)| ast_builder.expr().id(id))
            )
            .build();

        let block = ast_builder.expr().block()
            .with_stmt(coroutine_stmt)
            .build_expr(internal_expr);

        ast_builder.arm()
            .with_pat(resume_pat)
            .body().build(block)
    }
}

pub struct ResumeState {
    pub stmts: Vec<ast::Stmt>,
    pub expr: P<ast::Expr>,
}
