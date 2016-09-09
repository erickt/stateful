use mar::repr::*;
use mar::translate::Builder;
use syntax::ast;
use syntax::codemap::Span;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn block(&self, block: BasicBlock) -> Vec<ast::Stmt> {
        let block_data = self.mar.basic_block_data(block);

        assert!(block_data.terminator.is_some(),
                "block does not have a terminator");

        block_data.declared_decls().iter()
            .map(|declare_decl| self.declare_decl(declare_decl))
            .chain(
                block_data.statements().iter()
                .flat_map(|statement| self.stmt(block, statement))
            )
            .chain(
                block_data.terminator.iter()
                    .flat_map(|terminator| self.terminator(terminator))
            )
            .collect()
    }

    fn terminator(&self, terminator: &Terminator) -> Vec<ast::Stmt> {
        let ast_builder = self.ast_builder.span(terminator.span);

        match terminator.kind {
            TerminatorKind::Goto { target } => {
                self.goto(terminator.span, target)
            }
            TerminatorKind::If { ref cond, targets: (then_block, else_block) } => {
                let then_block = ast_builder
                    .span(self.block_span(then_block))
                    .block()
                    .with_stmts(self.goto(terminator.span, then_block))
                    .build();

                let else_block = ast_builder
                    .span(self.block_span(else_block))
                    .block()
                    .with_stmts(self.goto(terminator.span, else_block))
                    .build();

                vec![
                    ast_builder.stmt().expr().if_()
                        .build(cond.clone())
                        .build_then(then_block)
                        .build_else(else_block),
                ]
            }
            TerminatorKind::Match { ref discr, ref targets } => {
                let arms = targets.iter()
                    .map(|target| {
                        let ast_builder = ast_builder.span(self.block_span(target.block));

                        let block = ast_builder.block()
                            .span(self.mar.span)
                            .with_stmts(self.goto(terminator.span, target.block))
                            .build();

                        ast_builder.arm()
                            .with_pats(target.pats.iter().cloned())
                            .with_guard(target.guard.clone())
                            .body().build_block(block)
                    });

                vec![
                    ast_builder.stmt().expr().match_()
                        .build(discr.clone())
                        .with_arms(arms)
                        .build()
                ]
            }
            TerminatorKind::Yield { ref expr, target } => {
                let ast_builder = ast_builder.span(expr.span);
                let yielded_expr = ast_builder.expr()
                    .some()
                    .build(expr.clone());
                let next_state = self.state_expr(terminator.span, target);

                let tuple = ast_builder.expr().tuple()
                    .expr().build(yielded_expr)
                    .expr().build(next_state)
                    .build();

                vec![
                    ast_builder.stmt().semi()
                        .return_expr()
                        .build(tuple)
                ]
            }
            TerminatorKind::Return => {
                let return_expr = ast_builder.expr().none();
                let next_state = self.state_expr(terminator.span, END_BLOCK);
                let tuple = ast_builder.expr().tuple()
                    .expr().build(return_expr)
                    .expr().build(next_state)
                    .build();

                vec![
                    ast_builder.stmt().semi()
                        .return_expr()
                        .build(tuple)
                ]
            }
        }
    }

    fn goto(&self, span: Span, target: BasicBlock) -> Vec<ast::Stmt> {
        let ast_builder = self.ast_builder.span(span);
        let next_state = self.state_expr(span, target);
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
        self.mar.basic_block_data(block).span
    }

    fn declare_decl(&self, declared_decl: &DeclaredDecl) -> ast::Stmt {
        let ast_builder = self.ast_builder.span(declared_decl.span);

        let lvalue = self.mar.var_decl_data(declared_decl.decl);

        let assign_builder = ast_builder.stmt().let_()
            .id(lvalue.ident);

        if let Some(ref ty) = declared_decl.ty {
            assign_builder.ty()
                .build(ty.clone())
                .build()
        } else {
            assign_builder.build()
        }
    }
}
