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
        let locals = &self.scope_locals[&block];

        if locals.is_empty() {
            ast_builder.expr().build_path(state_path)
        } else {
            // Pack up all the locals back into scope tuples.
            let exprs = locals.iter()
                .map(|&(_, ref locals)| {
                    ast_builder.expr().tuple()
                        .with_exprs(
                            locals.iter().map(|local| {
                                let name = &self.mir.local_decls[*local].name;
                                ast_builder.expr().id(name)
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

    pub fn get_shadowed_decls(&self, decls: &mut Vec<(Local, ast::Ident)>, local: Local) {
        if let Some(decl) = self.mir.local_decls[local].shadowed_decl {
            debug!("get_shadowed_decl: {:?} {:?}", decl, self.mir.local_decls[decl]);

            decls.push((decl, self.shadowed_ident(decl)));

            self.get_shadowed_decls(decls, decl);
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

        // Internal states get an extra `Args` typaram if the block is a resume block.
        let args_param = if kind == StateKind::Internal && self.resume_blocks.contains(&block) {
            Some(ast_builder.id("Args"))
        } else {
            None
        };

        let ty_param_ids = scope_locals.iter()
            .flat_map(|&(_, ref locals)| locals)
            .map(|local| ast_builder.id(format!("T{}", local.index())))
            .chain(args_param)
            .collect::<Vec<_>>();

        let variant = if ty_param_ids.is_empty() {
            ast_builder.variant(state_id).unit()
        } else {
            let mut tys = scope_locals.iter()
                .map(|&(_, ref locals)| {
                    ast_builder.ty().tuple()
                        .with_tys(
                            locals.iter()
                                .map(|local| {
                                    ast_builder.ty().id(format!("T{}", local.index()))
                                })
                        )
                        .build()
                })
                .chain(
                    args_param.iter().map(|id| ast_builder.ty().id(id))
                );

            let ty = tys.next().unwrap();

            ast_builder.variant(state_id).tuple().ty().build(ty)
                .with_fields(
                    tys.map(|ty| ast_builder.tuple_field().ty().build(ty))
                )
                .build()
        };

        (variant, ty_param_ids)
    }
}
