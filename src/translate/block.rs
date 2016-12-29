use mir::*;
use std::collections::HashMap;
use super::builder::Builder;
use super::state::StateKind;
use syntax::ast;
use syntax::codemap::Span;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn block(&self, block: BasicBlock) -> Vec<ast::Stmt> {
        let block_data = &self.mir[block];

        assert!(block_data.terminator.is_some(),
                "block does not have a terminator");

        let scope_block = self.build_scope_block(block);
        let mut stmts = self.scope_block(block, scope_block);

        stmts.extend(
            block_data.terminator.iter()
                .flat_map(|terminator| self.terminator(terminator))
        );

        /*
        if let Some(initialized) = self.assignments.initialized(block) {
            initialized.iter()
                .flat_map(|local| self.declare(block, *local))
                .chain(stmts).collect()
        } else {
            stmts.collect()
        }
        */

        stmts
    }

    /// Rebuild an AST block from a MIR block.
    ///
    /// Stateful doesn't need to care about drops because that's automatically handled by the
    /// compiler when we raise MIR back into the AST. Unfortunately, MIR flattens out all of our
    /// scopes, so we need to rebuild this from the visibility scope information we parsed out
    /// during MIR construction.
    fn build_scope_block(&self, block: BasicBlock) -> ScopeBlock<'a> {
        // First, we need to group all our declarations by their scope.
        let mut scope_decls = HashMap::new();

        if let Some(initialized) = self.assignments.initialized(block) {
            for &local in initialized {
                let mut scope = self.mir.local_decls[local].source_info.scope;

                // In AST, non-argument variables are actually defined in their parent scope.
                if scope != ARGUMENT_VISIBILITY_SCOPE {
                    scope = self.mir.visibility_scopes[scope].parent_scope.unwrap();
                }

                scope_decls.entry(scope).or_insert_with(Vec::new).push(local);
            }
        }

        // Next, we'll start rebuilding the scope stack. We'll start with the root block, and work
        // our ways up. Each time we build a block, we pull all the associated declarations and
        // move them into this block.
        let mut stack = vec![
            ScopeBlock::new(
                ARGUMENT_VISIBILITY_SCOPE,
                scope_decls.remove(&ARGUMENT_VISIBILITY_SCOPE),
            ),
        ];

        let block_data = &self.mir[block];

        // Next, loop through the statements and insert them into the right block. If they have a
        // different scope, we'll push and pop the scopes until.
        for stmt in block_data.statements() {
            let last_scope = stack.last().unwrap().scope;
            let current_scope = stmt.source_info.scope;

            // Things are a little tricky when this statement's scope is different from the prior
            // statement. Consider:
            //
            // ```rust
            // {
            //     {
            //         a;
            //     }
            // }
            // {
            //     {
            //         b;
            //     }
            // }
            // ```
            //
            // In MIR, we'll just have two statements, `a;` and `b;` with different scopes. We need
            // to pop and push on scopes in order to get to the right level.
            if current_scope != last_scope {
                let last_scope_path = &self.scope_paths[&last_scope];
                let current_scope_path = &self.scope_paths[&current_scope];

                // Find the last common anscestor between the paths.
                let common = last_scope_path.iter()
                    .zip(current_scope_path.iter())
                    .take_while(|&(lhs, rhs)| lhs == rhs)
                    .count();

                // Walk down the scope tree to the last common point. We'll commit all the scopes
                // along the way.
                for i in common .. last_scope_path.len() {
                    let scope_block = stack.pop().unwrap();
                    stack.last_mut().unwrap().push_block(scope_block);
                }

                // Walk up the scope tree to the current scope point, pushing new blocks along the
                // way.
                for i in common .. current_scope_path.len() {
                    stack.push(
                        ScopeBlock::new(
                            current_scope,
                            scope_decls.remove(&current_scope)));
                }
            }

            stack.last_mut().unwrap().push_stmt(stmt);
        }

        // Pop off the remaining scopes.
        while stack.len() != 1 {
            let scope_block = stack.pop().unwrap();
            stack.last_mut().unwrap().push_block(scope_block);
        }

        let scope_block = stack.pop().unwrap();

        assert!(stack.is_empty(), "stack is not empty: {:#?}", stack);
        assert_eq!(scope_block.scope,
                   ARGUMENT_VISIBILITY_SCOPE,
                   "block is not root scope: {:?}",
                   scope_block);

        scope_block
    }

    fn scope_block(&self, block: BasicBlock, scope_block: ScopeBlock) -> Vec<ast::Stmt> {
        scope_block.stmts.into_iter()
            .flat_map(|stmt| {
                match stmt {
                    ScopeStatement::Declare(local) => {
                        self.declare(block, local)
                    }
                    ScopeStatement::Statement(stmt) => {
                        self.stmt(block, stmt)
                    }
                    ScopeStatement::Block(scope_block) => {
                        let stmts = self.scope_block(block, scope_block);

                        vec![
                            self.ast_builder.stmt().semi().block()
                                .with_stmts(stmts)
                                .build()
                        ]
                    }
                }
            })
            .collect()
    }

    fn declare(&self, block: BasicBlock, local: Local) -> Vec<ast::Stmt> {
        let local_decl = self.mir.local_decl_data(local);

        let ast_builder = self.ast_builder.span(local_decl.source_info.span);

        let mut stmts = self.rename_shadowed_local(&ast_builder, local)
            .into_iter()
            .collect::<Vec<_>>();

        let stmt_builder = match local_decl.mutability {
            ast::Mutability::Mutable => ast_builder.stmt().let_().mut_id(local_decl.name),
            ast::Mutability::Immutable => ast_builder.stmt().let_().id(local_decl.name),
        };

        stmts.push(stmt_builder.build_option_ty(local_decl.ty.clone())
            .build());

        stmts
    }

    fn terminator(&self, terminator: &Terminator) -> Vec<ast::Stmt> {
        let span = terminator.source_info.span;
        let ast_builder = self.ast_builder.span(span);

        match terminator.kind {
            TerminatorKind::Goto { target, .. } => {
                self.goto(span, target)
            }
            TerminatorKind::If { ref cond, targets: (then_block, else_block) } => {
                let cond = cond.to_expr(&self.mir.local_decls);

                let then_block = ast_builder
                    .span(self.block_span(then_block))
                    .block()
                    .with_stmts(self.goto(span, then_block))
                    .build();

                let else_block = ast_builder
                    .span(self.block_span(else_block))
                    .block()
                    .with_stmts(self.goto(span, else_block))
                    .build();

                vec![
                    ast_builder.stmt().expr().if_()
                        .build(cond)
                        .build_then(then_block)
                        .build_else(else_block),
                ]
            }
            TerminatorKind::Match { ref discr, ref targets } => {
                let discr = discr.to_expr(&self.mir.local_decls);

                let arms = targets.iter()
                    .map(|target| {
                        let ast_builder = ast_builder.span(self.block_span(target.block));

                        let block = ast_builder.block()
                            .span(self.mir.span)
                            .with_stmts(self.goto(span, target.block))
                            .build();

                        ast_builder.arm()
                            .with_pats(target.pats.iter().cloned())
                            .with_guard(target.guard.clone())
                            .body().build_block(block)
                    });

                vec![
                    ast_builder.stmt().expr().match_()
                        .build(discr)
                        .with_arms(arms)
                        .build()
                ]
            }
            TerminatorKind::Return => {
                let next_state = ast_builder.expr().path()
                    .span(self.mir.span)
                    .ids(&["ResumeState", "Illegal"])
                    .build();

                match self.mir.state_machine_kind {
                    StateMachineKind::Generator => {
                        vec![
                            /*
                            // generate `let () = return_;` to make sure it's been assigned the
                            // right type.
                            ast_builder.stmt().let_()
                                    .tuple().build()
                                .expr().id("return_"),
                                */
                            ast_builder.stmt().semi().return_expr().tuple()
                                .expr().none()
                                .expr().build(next_state)
                                .build()
                        ]
                    }
                    StateMachineKind::Async => {
                        let return_expr = ast_builder.expr().id("return_");
                        let ready_expr = ast_builder.expr().call()
                            .path()
                                .global()
                                .ids(&["futures", "Async", "Ready"])
                                .build()
                            .with_arg(return_expr)
                            .build();

                        vec![
                            ast_builder.stmt().semi().return_expr().ok().tuple()
                                .expr().build(ready_expr)
                                .expr().build(next_state)
                                .build()
                        ]
                    }
                }
            }
            TerminatorKind::Suspend {
                destination: (_, target),
                ref rvalue,
            } => {
                let rvalue = rvalue.to_expr(&self.mir.local_decls);
                let next_state = self.resume_state_expr(target);

                let ast_builder = ast_builder.span(rvalue.span);
                let expr = ast_builder.expr().tuple()
                    .expr().build(rvalue)
                    .expr().build(next_state)
                    .build();

                let expr = match self.mir.state_machine_kind {
                    StateMachineKind::Generator => expr,
                    StateMachineKind::Async => ast_builder.expr().ok().build(expr),
                };

                vec![
                    ast_builder.stmt().semi().return_expr()
                        .build(expr)
                ]
            }
        }
    }

    fn goto(&self, span: Span, target: BasicBlock) -> Vec<ast::Stmt> {
        let next_state = self.internal_state_expr(target);

        let ast_builder = self.ast_builder.span(span);
        let next_expr = ast_builder.expr()
            .assign()
            .id("state")
            .build(next_state);

        vec![
            ast_builder.stmt().semi().build(next_expr),
            ast_builder.stmt().semi().continue_(),
        ]
    }
}

#[derive(Debug)]
struct ScopeBlock<'a> {
    scope: VisibilityScope,
    stmts: Vec<ScopeStatement<'a>>,
}

#[derive(Debug)]
enum ScopeStatement<'a> {
    Declare(Local),
    Statement(&'a Statement),
    Block(ScopeBlock<'a>),
}

impl<'a> ScopeBlock<'a> {
    fn new(scope: VisibilityScope, locals: Option<Vec<Local>>) -> Self {
        ScopeBlock {
            scope: scope,
            stmts: locals.unwrap_or_else(Vec::new).into_iter()
                .map(ScopeStatement::Declare)
                .collect(),
        }
    }

    fn push_stmt(&mut self, stmt: &'a Statement) {
        self.stmts.push(ScopeStatement::Statement(stmt));
    }

    fn push_block(&mut self, block: ScopeBlock<'a>) {
        self.stmts.push(ScopeStatement::Block(block));
    }
}
