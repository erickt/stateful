use data_structures::indexed_vec::Idx;
use mir::*;
use super::builder::Builder;
use syntax::ast;
use syntax::ptr::P;

#[derive(Copy, Clone, PartialEq)]
pub enum StateKind {
    Resume,
    Internal,
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Construct a `P<ast::Expr>` to represent the state expression for a given basic block.
    pub fn state_expr(&self, block: BasicBlock, kind: StateKind) -> P<ast::Expr> {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let state_path = self.state_path(block, kind);
        let scope_locals = &self.scope_locals[&block];

        let local_names = &self.local_names[&block];

        if scope_locals.is_empty() {
            ast_builder.expr().build_path(state_path)
        } else {
            // Pack up all the locals back into scope tuples.
            let exprs = scope_locals.iter()
                .map(|(_, locals)| {
                    ast_builder.expr().tuple()
                        .with_exprs(
                            locals.iter().map(|local| {
                                if let Some(name) = local_names.get(local) {
                                    ast_builder.expr().id(name)
                                } else {
                                    span_bug!(
                                        self.cx,
                                        span,
                                        "No name for local={:?} local_names={:#?}",
                                        local,
                                        local_names);
                                }
                            })
                        )
                        .build()
                });

            ast_builder.expr().call()
                .build_path(state_path)
                .with_args(exprs)
                .build()
        }
    }

    pub fn state_id(&self, block: BasicBlock) -> ast::Ident {
        match self.mir[block].name {
            Some(name) => {
                self.ast_builder.id(format!("State{}{}", block.index(), name))
            }
            None => {
                self.ast_builder.id(format!("State{}", block.index()))
            }
        }
    }

    pub fn state_path(&self, block: BasicBlock, kind: StateKind) -> ast::Path {
        let enum_name = match kind {
            StateKind::Resume => "ResumeState",
            StateKind::Internal => "InternalState",
        };

        self.ast_builder
            .path()
            .span(self.mir.span)
            .id(enum_name)
            .id(self.state_id(block))
            .build()
    }

    pub fn state_variant(&self,
                         block: BasicBlock,
                         kind: StateKind) -> (ast::Variant, Vec<ast::Ident>) {
        let span = self.block_span(block);
        let ast_builder = self.ast_builder.span(span);

        let state_id = self.state_id(block);

        // Create type parameters for each alive local in this block.
        let scope_locals = &self.scope_locals[&block];

        let mut tys = vec![];
        let mut ty_param_ids = vec![];

        for (scope, locals) in scope_locals.iter() {
            // The start block doesn't get the coroutine arguments.
            if kind == StateKind::Resume
                && block == START_BLOCK
                && *scope == COROUTINE_ARGS_VISIBILITY_SCOPE
            {
                continue;
            }

            let mut tuple_tys = vec![];

            for local in locals {
                if let Some(ref ty) = self.mir.local_decls[*local].ty {
                    tuple_tys.push(ty.clone());
                } else {
                    let id = ast_builder.id(format!("T{}", local.index()));
                    tuple_tys.push(ast_builder.ty().id(id));
                    ty_param_ids.push(id);
                }
            }

            let ty = ast_builder.ty().tuple()
                .with_tys(tuple_tys)
                .build();

            tys.push(ty);
        }

        let mut tys = tys.into_iter();

        let variant = if let Some(ty) = tys.next() {
            ast_builder.variant(state_id).tuple().ty().build(ty)
                .with_fields(
                    tys.map(|ty| ast_builder.tuple_field().ty().build(ty))
                )
                .build()
        } else {
            ast_builder.variant(state_id).unit()
        };

        (variant, ty_param_ids)
    }
}
