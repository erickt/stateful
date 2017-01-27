use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::HashSet;
use super::builder::Builder;
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
    pub fn internal_state_expr(&self, block: BasicBlock) -> P<ast::Expr> {
        self.state_expr(block, StateKind::Internal)
    }

    /// Build up an `ast::Arm` for an internal state variant.
    fn internal_arm(&mut self, block: BasicBlock) -> ast::Arm {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        // First, setup the arm body.
        let mut body = ast_builder.block()
            .with_stmts(self.block(block))
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

        let local_names = &self.local_names[&block];

        // Finally, we'll unpack the variables in a unique block in order to get shadowing to work
        // correctly.
        for (scope, locals) in scope_locals.iter().rev() {
            let pat = ast_builder.pat()
                .tuple()
                .with_pats(
                    locals.iter().map(|local| {
                        let name = local_names[local];
                        let local_data = &self.mir.local_decls[*local];

                        match local_data.mutability {
                            ast::Mutability::Immutable => ast_builder.pat().id(name),
                            ast::Mutability::Mutable => ast_builder.pat().mut_id(name),
                        }
                    })
                )
                .build();

            let stmt = ast_builder.stmt()
                .let_().build(pat)
                .expr().id(format!("scope{}", scope.index()));

            body = ast_builder.block()
                .stmt().build(stmt)
                .expr().build_block(body);
        }

        /*
        let state_path = self.state_path(block, StateKind::Internal);

        let pats = self.scope_locals[&block].iter()
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

        //let mut stmts = self.block(block);

        // Next, setup the arm body.
        let mut body = ast_builder.block()
            .with_stmts(self.block(block))
            .build();

        // Finally, we'll unpack the variables in a unique block in order to get shadowing to work
        // correctly.
        for (scope, locals) in self.scope_locals[&block].iter().rev() {
            let mut stmts = vec![];
            let mut pats = vec![];

            for local in locals {
                let name = self.local_names[&block][local];
                let local_data = &self.mir.local_decls[*local];

                // If this local is shadowing another local, 
                if let Some(stmt) = self.rename_shadowed_local(*local) {
                    stmts.push(stmt);
                }

                let pat = match local_data.mutability {
                    ast::Mutability::Immutable => ast_builder.pat().id(name),
                    ast::Mutability::Mutable => ast_builder.pat().mut_id(name),
                };

                pats.push(pat);
            }

            let pat = ast_builder.pat()
                .tuple()
                .with_pats(pats)
                .build();

            let stmt = ast_builder.stmt()
                .let_().build(pat)
                .expr().id(format!("scope{}", scope.index()));

            body = ast_builder.block()
                .with_stmts(stmts)
                .stmt().build(stmt)
                .expr().build_block(body);
        }
        */

        /*
        let successors = self.mir[block].terminator().successors();
        let next_block = successors.first();

        // Finally, we'll unpack the variables in a unique block in order to get shadowing to work
        // correctly.
        for (scope, locals) in scope_locals.iter().rev() {
            let pat = ast_builder.pat()
                .tuple()
                .with_pats(
                    locals.iter().map(|local| {
                        let name = local_names[local];
                        let local_data = &self.mir.local_decls[*local];

                        match local_data.mutability {
                            ast::Mutability::Immutable => ast_builder.pat().id(name),
                            ast::Mutability::Mutable => ast_builder.pat().mut_id(name),
                        }
                    })
                )
                .build();

            let stmt = ast_builder.stmt()
                .let_().build(pat)
                .expr().id(format!("scope{}", scope.index()));

            let mut body_builder = ast_builder.block()
                .stmt().build(stmt)
                .with_stmts(stmts);

            if let Some(next_block) = next_block {
                let next_scope_locals = &self.scope_locals[next_block];
                let next_local_names = &self.local_names[next_block];

                if let Some(ref next_locals) = next_scope_locals.get(scope) {
                    let next_scope = ast_builder.expr().tuple()
                        .with_exprs(
                            next_locals.iter().map(|local| {
                                let name = next_local_names[local];
                                ast_builder.expr().id(name)
                            })
                        )
                        .build();

                    body_builder = body_builder.stmt().semi().assign()
                        .id(format!("out_scope{}", scope.index()))
                        .build(next_scope);
                }
            }

            let body = body_builder.build();

            stmts = vec![
                ast_builder.stmt().semi().build_block(body),
            ];
        }

        let mut body_builder = ast_builder.block();

        // Finally, declare all the out scopes
        if let Some(next_block) = next_block {
            for scope in self.scope_locals[next_block].keys() {
                body_builder = body_builder.stmt()
                    .let_id(format!("out_scope{}", scope.index()))
                    .build();
            }
        }

        let body = body_builder
            .with_stmts(stmts)
            .build();
        */

        ast_builder.arm()
            .with_pat(pat)
            .body().build_block(body)
    }
}

pub struct InternalState {
    pub stmts: Vec<ast::Stmt>,
    pub expr: P<ast::Expr>,
}
