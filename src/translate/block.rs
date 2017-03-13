use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::BTreeMap;
use std::iter;
use super::builder::Builder;
use super::local_stack::LocalStack;
use syntax::ast;
use syntax::codemap::Span;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn block(&mut self, block: BasicBlock) -> Vec<ast::Stmt> {
        let block_data = &self.mir[block];

        debug!("block: {:?}", block);
        debug!("block: decls: {:#?}",
                 self.mir.local_decls.iter_enumerated()
                 .collect::<Vec<_>>());
        debug!("block: stmts: {:#?}",
                 block_data.statements().iter()
                    .map(|stmt| {
                        format!(
                            "stmt: {:?}; // {:?} -> {:?}",
                            stmt,
                            stmt.source_info.scope,
                            get_dest_scopes(self.mir, stmt))
                    })
                    .collect::<Vec<_>>());
        debug!("block: terminator: {:#?}",
                 block_data.terminator.as_ref().map(|terminator| {
                     format!("terminator: {:?}; // {:?}",
                             terminator,
                             terminator.source_info.scope)
                 }));
        debug!("block: scope_locals: {:#?}",
                 self.scope_locals[&block].iter()
                    .map(|(scope, locals)| {
                        (
                            scope,
                            locals.iter()
                                .map(|local| (local, self.mir.local_decls[*local].name))
                                .collect::<Vec<_>>(),
                        )
                    })
                    .collect::<BTreeMap<_, _>>());

        // First, group all our statements by their scope.
        let scope_block = self.build_scope_block(block);
        debug!("block: scope_block: {:#?}", scope_block);

        let mut local_stack = LocalStack::new(self, block);
        let (terminated, stmts) = self.scope_block(
            block,
            &mut local_stack,
            ARGUMENT_VISIBILITY_SCOPE,
            scope_block);

        debug!("block: local_stack: {:#?}", local_stack.scope_stack);

        if !terminated {
            span_bug!(
                self.cx,
                block_data.span,
                "scope block did not terminate?");
        }

        /*
        // Now that we're done, pop all the remaining scopes off the stack that we pushed
        // previously.
        for _ in self.scope_locals[&block].iter().rev() {
            let mut stmts_ = vec![];
            mem::swap(&mut stmts, &mut stmts_);
            let (_, stmt) = local_stack.pop(true, stmts_).expect("stack empty?");
            stmts = vec![stmt];
        }
        */

        stmts
    }

    fn build_scope_block(&mut self, block: BasicBlock) -> ScopeBlock<'a> {
        // First, we'll create a stack for all of our blocks. Since we might have some locals
        // flowing into this block, we'll initialize the stack with their corresponding blocks.
        let mut stack = self.scope_locals[&block].keys()
            .map(|scope| ScopeBlock::new(*scope))
            .collect::<Vec<_>>();

        let block_data = &self.mir[block];
        let stmts = self.mir[block].statements();
        let terminator = block_data.terminator.as_ref().expect("terminator not set?");

        // Only process the statements if we have any.
        if !stmts.is_empty() {
            // First, we need to adjust the scope from the the last scope local to the scope of
            // this statement.
            {
                let prev_scope = stack.last().unwrap().scope;
                let first_scope = stmts.first().unwrap().source_info.scope;

                if prev_scope != first_scope {
                    self.adjust_scope(&mut stack, prev_scope, first_scope);
                };
            }

            // Now that our stack is ready, we need to process all the statements.
            let next_scopes = stmts.iter()
                .skip(1)
                .map(|stmt| stmt.source_info.scope)
                .chain(iter::once(terminator.source_info.scope));

            for (stmt, next_scope) in stmts.iter().zip(next_scopes) {
                let current_scope = stmt.source_info.scope;

                if current_scope != next_scope {
                    self.adjust_scope(&mut stack, current_scope, next_scope);
                };

                stack.last_mut().unwrap().push(ScopeStatement::Statement(stmt));
            }
        }

        // Finally, push the terminator.
        stack.last_mut().unwrap().push(ScopeStatement::Terminator(terminator));

        // Our block should now be terminated, but we still might have a few scopes on our stack,
        // so pop them off.
        while stack.len() != 1 {
            let scope_block = stack.pop().unwrap();
            let scope_block = ScopeStatement::Block(scope_block);
            stack.last_mut().unwrap().push(scope_block);
        }

        let scope_block = stack.pop().unwrap();

        assert!(stack.is_empty(), "stack is not empty: {:#?}", stack);
        assert_eq!(scope_block.scope,
                   ARGUMENT_VISIBILITY_SCOPE,
                   "block is not root scope: {:?}",
                   scope_block);

        scope_block
    }

    /// Things are a little tricky when this statement's scope is different from the prior
    /// statement. Consider:
    ///
    /// ```rust
    /// {
    ///     {
    ///         a;
    ///     }
    /// }
    /// {
    ///     {
    ///         b;
    ///     }
    /// }
    /// ```
    ///
    /// In MIR, we'll just have two statements, `a;` and `b;` with different scopes. We need
    /// to pop and push on scopes in order to get to the right level.
    fn adjust_scope(&self,
                    stack: &mut Vec<ScopeBlock>,
                    last_scope: VisibilityScope,
                    current_scope: VisibilityScope) {
        let last_scope_path = &self.scope_paths[&last_scope];
        let current_scope_path = &self.scope_paths[&current_scope];

        // Find the last common anscestor between the paths.
        let common = last_scope_path.iter()
            .zip(current_scope_path.iter())
            .take_while(|&(lhs, rhs)| lhs == rhs)
            .count();

        let last_scope_path = last_scope_path.split_at(common).1;
        let current_scope_path = current_scope_path.split_at(common).1;

        // Walk down the scope tree to the last common point. We'll commit all the scopes
        // along the way.
        for scope in last_scope_path.iter().rev() {
            let scope_block = stack.pop().unwrap();
            assert_eq!(*scope, scope_block.scope);

            stack.last_mut().unwrap().push(ScopeStatement::Block(scope_block));
        }

        // Walk up the scope tree to the current scope point, pushing new blocks along the
        // way.
        for scope in current_scope_path {
            stack.push(ScopeBlock::new(*scope));
        }
    }

    fn scope_block(&mut self,
                   block: BasicBlock,
                   local_stack: &mut LocalStack,
                   parent_scope: VisibilityScope,
                   scope_block: ScopeBlock) -> (bool, Vec<ast::Stmt>) {
        let scope = scope_block.scope;
        let scope_stmts = scope_block.stmts;

        let mut stmts = vec![];

        stmts.extend(self.extract_scope_locals(block, local_stack, scope));
        let mut terminated = false;

        for stmt in scope_stmts.into_iter() {
            match stmt {
                ScopeStatement::Statement(stmt) => {
                    stmts.extend(self.stmt(block, local_stack, stmt));
                }
                ScopeStatement::Block(scope_block) => {
                    let (terminated_, child_stmts) = self.scope_block(
                        block,
                        local_stack,
                        scope,
                        scope_block);

                    terminated = terminated_;

                    stmts.extend(child_stmts);
                }
                ScopeStatement::Terminator(terminator) => {
                    terminated = true;

                    stmts.extend(self.terminator(local_stack, scope, terminator));
                }
            }
        }

        // Pop off the scopes we've processed.
        let stmts = local_stack.pop_scopes(parent_scope, terminated, stmts);

        (terminated, stmts)
    }

    fn extract_scope_locals(&mut self,
                            block: BasicBlock,
                            local_stack: &mut LocalStack,
                            scope: VisibilityScope) -> Option<ast::Stmt> {
        let ast_builder = self.ast_builder.span(self.mir[block].span);

        let pat = if let Some(locals) = self.scope_locals[&block].get(&scope) {
            // Push any locals onto the stack.
            for local in locals.iter() {
                local_stack.shadow_local(*local);
            }

            // Next, extract the locals from the scope tuple.
            ast_builder.pat()
                .tuple()
                .with_pats(
                    locals.iter().map(|local| {
                        let name = if let Some(name) = local_stack.get_name(*local) {
                            name
                        } else {
                            //self.mir.local_decls[*local].name
                            span_bug!(
                                self.cx,
                                self.mir.local_decls[*local].source_info.span,
                                "local {:?} has no associated name?",
                                local);
                        };

                        match self.mir.local_decls[*local].mutability {
                            ast::Mutability::Immutable => ast_builder.pat().id(name),
                            ast::Mutability::Mutable => ast_builder.pat().mut_id(name),
                        }
                    })
                )
                .build()
        } else {
            return None;
        };

        let stmt = ast_builder.stmt()
            .let_().build(pat)
            .expr().id(format!("scope{}", scope.index()));

        Some(stmt)
    }

    fn terminator(&self,
                  local_stack: &mut LocalStack,
                  scope: VisibilityScope,
                  terminator: &Terminator) -> Vec<ast::Stmt> {
        let span = terminator.source_info.span;
        let ast_builder = self.ast_builder.span(span);

        match terminator.kind {
            TerminatorKind::Goto { target } => {
                self.goto(span, target, local_stack)
            }
            TerminatorKind::Break { target, after_target: _ } => {
                self.goto(span, target, local_stack)
            }
            TerminatorKind::If { ref cond, targets: (then_block, else_block) } => {
                let cond = cond.to_expr(&self.mir.local_decls);

                let then_block = ast_builder
                    .span(self.block_span(then_block))
                    .block()
                    .with_stmts(self.goto(span, then_block, local_stack))
                    .build();

                let else_block = ast_builder
                    .span(self.block_span(else_block))
                    .block()
                    .with_stmts(self.goto(span, else_block, local_stack))
                    .build();

                vec![
                    ast_builder.stmt().expr().if_()
                        .build(cond)
                        .build_then(then_block)
                        .build_else(else_block),
                ]
            }
            TerminatorKind::Match { ref discr, ref arms } => {
                let discr = discr.to_expr(&self.mir.local_decls);

                let arms = arms.iter()
                    .map(|arm| {
                        /*
                        // Push any locals defined in the match arm onto the stack to make sure
                        // values are properly shadowed.
                        let scope_locals = arm.lvalues.iter()
                            .map(|lvalue| {
                                let local = lvalue.to_local().expect("lvalue not a local?");
                                let scope = self.mir.local_decls[local].source_info.scope;
                                (scope, local)
                            })
                            .collect::<Vec<_>>();

                        let (terminated, stmts) = local_stack.with_locals(
                            scope_locals,
                            |local_stack| (true, self.goto(span, arm.block, local_stack)));
                        assert!(terminated);
                        */

                        // Push any locals defined in the match arm onto the stack to make sure
                        // values are properly shadowed.
                        for lvalue in arm.lvalues.iter() {
                            let local = lvalue.to_local().expect("arm lvalue is not local?");
                            local_stack.shadow_local(local);
                        }

                        let stmts = self.goto(span, arm.block, local_stack);
                        let stmts = local_stack.pop_scopes(scope, true, stmts);

                        let ast_builder = ast_builder.span(self.block_span(arm.block));

                        let block = ast_builder.block()
                            .with_stmts(stmts)
                            .build();

                        ast_builder.arm()
                            .with_pats(arm.pats.iter().cloned())
                            .with_guard(arm.guard.clone())
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
                ref arg,
            } => {
                let arg = arg.to_expr(&self.mir.local_decls);
                let next_state = self.resume_state_expr(target, local_stack);

                let ast_builder = ast_builder.span(arg.span);
                let expr = ast_builder.expr().tuple()
                    .expr().build(arg)
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

    fn goto(&self, span: Span,
            target: BasicBlock,
            local_stack: &LocalStack) -> Vec<ast::Stmt> {
        let next_state = self.internal_state_expr(target, local_stack);

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
    Statement(&'a Statement),
    Block(ScopeBlock<'a>),
    Terminator(&'a Terminator),
}

impl<'a> ScopeBlock<'a> {
    fn new(scope: VisibilityScope) -> Self {
        ScopeBlock {
            scope: scope,
            stmts: vec![],
        }
    }

    fn push(&mut self, stmt: ScopeStatement<'a>) {
        self.stmts.push(stmt);
    }
}

fn get_dest_scopes(mir: &Mir, stmt: &Statement) -> Vec<VisibilityScope> {
    match stmt.kind {
        StatementKind::Stmt(_) |
        StatementKind::StorageLive(_) |
        StatementKind::StorageDead(_) => {
            vec![]
        }

        StatementKind::Let { ref lvalues, .. } => {
            lvalues.iter()
                .map(|lvalue| {
                    match *lvalue {
                        Lvalue::Local(local) => {
                            mir.local_decls[local].source_info.scope
                        }
                        _ => { unreachable!() }
                    }
                })
                .collect()
        }

        StatementKind::Assign(ref lvalue, _) |
        StatementKind::Call { destination: ref lvalue, .. } |
        StatementKind::MethodCall { destination: ref lvalue, .. } => {
            match *lvalue {
                Lvalue::Local(local) => {
                    vec![mir.local_decls[local].source_info.scope]
                }
                _ => { unreachable!() }
            }
        }
    }
}
