use mir::*;
use syntax::ast;
use syntax::codemap::Span;
use translate::Builder;

use super::state::StateKind;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn block(&self, block: BasicBlock) -> Vec<ast::Stmt> {
        let block_data = &self.mir[block];

        assert!(block_data.terminator.is_some(),
                "block does not have a terminator");

        let decls = self.assignments
            .initialized(block)
            .into_iter()
            .flat_map(|local| self.declare(block, *local));

        let stmts = block_data.statements()
            .iter()
            .flat_map(|statement| self.stmt(block, statement));

        let terminator = block_data.terminator
            .iter()
            .flat_map(|terminator| self.terminator(terminator));

        decls.chain(stmts).chain(terminator).collect()
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
                    .ids(&["CoroutineState", "Illegal"])
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
            TerminatorKind::Drop { ref location, target, moved } => {
                let mut stmts = vec![];

                match *location {
                    Lvalue::Local(local) => {
                        let decl = self.mir.local_decl_data(local);

                        // We need an explicit drop here to make sure we drop variables as they go out of
                        // a block scope. Otherwise, they won't be dropped until the next yield point,
                        // which wouldn't match the Rust semantics.
                        let mut stmts = vec![];

                        // Only drop if we were not moved.
                        if !moved {
                            stmts.push(
                                ast_builder.stmt().semi().call()
                                    .path()
                                        .global()
                                        .ids(&["std", "mem", "drop"])
                                        .build()
                                    .arg().id(decl.name)
                                    .build()
                            );
                        }

                        if let Some(shadowed_decl) = decl.shadowed_decl {
                            let shadowed_ident = self.shadowed_ident(shadowed_decl);

                            stmts.push(
                                ast_builder.stmt().let_id(decl.name).expr().id(shadowed_ident)
                            );
                        }
                    }
                    _ => {}
                }

                stmts.extend(self.goto(span, target));
                stmts
            }
            TerminatorKind::Call {
                destination: (ref lvalue, target),
                ref func,
                ref args,
            } => {
                let lvalue = lvalue.to_expr(&self.mir.local_decls);

                let func = func.to_expr(&self.mir.local_decls);
                let args = args.iter()
                    .map(|arg| arg.to_expr(&self.mir.local_decls));

                let rvalue = ast_builder.expr()
                    .call().build(func)
                    .with_args(args)
                    .build();

                let mut stmts = vec![
                    ast_builder.stmt().semi()
                        .assign().build(lvalue)
                        .build(rvalue)
                ];

                stmts.extend(self.goto(span, target));

                stmts
            }
            TerminatorKind::MethodCall {
                destination: (ref lvalue, target),
                ident,
                ref tys,
                ref self_,
                ref args,
            } => {
                let lvalue = lvalue.to_expr(&self.mir.local_decls);
                let self_ = self_.to_expr(&self.mir.local_decls);

                let args = args.iter()
                    .map(|arg| arg.to_expr(&self.mir.local_decls));

                let rvalue = ast_builder.expr()
                    .span(ident.span).method_call(ident.node)
                    .span(span).build(self_)
                    .with_tys(tys.clone())
                    .with_args(args)
                    .build();

                let mut stmts = vec![
                    ast_builder.stmt().semi().assign()
                        .build(lvalue)
                        .build(rvalue)
                ];

                stmts.extend(self.goto(span, target));

                stmts
            }
            TerminatorKind::Suspend {
                destination: (_, target),
                ref rvalue,
            } => {
                let rvalue = rvalue.to_expr(&self.mir.local_decls);
                let next_state = self.coroutine_state_expr(target);

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

    pub fn block_span(&self, block: BasicBlock) -> Span {
        self.mir[block].span
    }
}
