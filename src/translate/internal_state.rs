use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::HashSet;
use super::builder::Builder;
use super::local_stack::LocalStack;
use super::state::StateKind;
use syntax::ast;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn internal_state(&mut self) -> InternalState {
        let blocks = self.mir.basic_blocks();

        let mut ty_param_ids = Vec::new();
        let mut seen_ty_param_ids = HashSet::new();
        let mut variants = Vec::with_capacity(blocks.len());
        let mut arms = Vec::with_capacity(blocks.len());

        for block in blocks.indices() {
            let (variant, tp) = self.state_variant(block, StateKind::Internal);
            variants.push(variant);

            // It's possible for a declaration to be created but not actually get used in the state
            // variables, so we only create a type parameter for a declaration if it's actually
            // used.
            for ty_param_id in tp {
                if !seen_ty_param_ids.contains(&ty_param_id) {
                    seen_ty_param_ids.insert(ty_param_id);
                    ty_param_ids.push(ty_param_id);
                }
            }

            arms.push(self.internal_arm(block));
        }

        let generics = self.ast_builder.from_generics(self.mir.fn_decl.generics.clone())
            .with_ty_param_ids(ty_param_ids.iter())
            .build();

        let enum_name = self.ast_builder.id("InternalState");

        let enum_item = self.ast_builder.item().enum_(enum_name)
            .generics().with(generics.clone()).build()
            .with_variants(variants)
            .build();

        let stmts = vec![
            self.ast_builder.stmt().build_item(enum_item),
        ];

        let expr = quote_expr!(self.cx,
            loop {
                match state {
                    $arms
                }
            }
        );

        InternalState {
            stmts: stmts,
            expr: expr,
        }
    }

    /// Construct a `P<ast::Expr>` to represent the state expression for a given basic block.
    pub fn internal_state_expr(&self,
                               block: BasicBlock,
                               local_stack: &LocalStack) -> P<ast::Expr> {
        self.state_expr(block, local_stack, StateKind::Internal)
    }

    /*
    fn recurse<I>(&mut self,
                  block: BasicBlock,
                  local_stack: &mut LocalStack,
                  iter: I) -> (bool, Vec<ast::Stmt>)
        where I: Iterator<Item=(VisibilityScope, Vec<Local>)>,
    {
                return (true, self.block(block, local_stack));

                /*
        let (scope, locals) = match iter.next() {
            Some((scope, locals)) => (scope, locals),
            None => {
                return (true, vec![])
                //return (true, self.block(block, local_stack));
            }
        };

        let (terminated, stmt) = local_stack.in_scope(scope, |local_stack| {
            for local in locals {
                local_stack.push_lvalue(&Lvalue::Local(local), false);
            }

            self.recurse(block, local_stack, iter)
        });

        (terminated, vec![stmt])
        */
    }
*/

    /// Build up an `ast::Arm` for an internal state variant.
    fn internal_arm(&mut self, block: BasicBlock) -> ast::Arm {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let mut local_stack = LocalStack::new(self, block);

        /*
        let locals = self.scope_locals[&block].iter()
            .map(|(scope, locals)| (*scope, locals.clone()))
            .collect::<Vec<_>>();
            */

        //let (terminated, stmts) = self.recurse(block, &mut local_stack, locals.into_iter());
        let stmts = self.block(block, &mut local_stack);

        /*
        // First, push start a scope for all the arm parameters.
        let (terminated, stmt) = local_stack.in_scope(scope, |local_stack| {
            /*
            // Next, Load up all the locals into the stack.
            local_stack.push_lvalue(&Lvalue::Local(COROUTINE_ARGS), false);
            */

            let locals = self.scope_locals[&block].values()
                .map(|locals| locals.clone())
                .collect::<Vec<_>>();

            self.recurse(block, local_stack, locals.into_iter())

            /*
            // Insert all the locals that have been initialized prior to this block.
            for locals in self.scope_locals[&block].values() {
                local_stack.scope_stack.push(Scope::new());

                for local in locals {
                    local_stack.push_lvalue(&Lvalue::Local(*local), false);
                }
            }

            // Next, setup the arm body.
            self.block(block, &mut local_stack)
            */
        });

        if !terminated {
            span_bug!(
                self.cx,
                self.mir[block].span,
                "scope block did not terminate?");
        }
        */

        let body = ast_builder.block()
            .with_stmts(stmts)
            .build();

        let state_path = self.state_path(block, StateKind::Internal);
        let scope_locals = &self.scope_locals[&block];

        let pats = scope_locals.iter()
            .map(|(scope, _)| {
                ast_builder.pat().id(format!("scope{}", scope.index()))
            })
            .collect::<Vec<_>>();

        // Construct the pattern, which looks like:
        //
        // ```rust
        // InternalState::State2(scope1, scope2, ...)
        // ```
        let pat = if pats.is_empty() {
            ast_builder.pat().build_path(state_path)
        } else {
            ast_builder.pat().enum_().build(state_path)
                .with_pats(pats)
                .build()
        };

        ast_builder.arm()
            .with_pat(pat)
            .body().build_block(body)
    }
}

pub struct InternalState {
    pub stmts: Vec<ast::Stmt>,
    pub expr: P<ast::Expr>,
}
