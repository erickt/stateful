use aster::AstBuilder;
use aster::ident::ToIdent;
use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::build::scope::LoopScope;
use mar::repr::*;
use syntax::ast::{self, ExprKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt_expr(&mut self,
                     mut block: BasicBlock,
                     expr: &P<ast::Expr>) -> BlockAnd<()> {
        let expr_span = expr.span;

        match expr.node {
            ExprKind::Assign(ref lhs, ref rhs) => {
                // Note: we evaluate assignments right-to-left. This
                // is better for borrowck interaction with overloaded
                // operators like x[j] = x[i].

                let rhs = unpack!(block = self.as_rvalue(block, rhs));
                let lhs = unpack!(block = self.as_lvalue(block, lhs));
                self.cfg.push_assign(block, lhs, rhs);

                block.unit()
            }
            ExprKind::Ret(ref returned_expr) => {
                self.expr_ret(block, expr.span, returned_expr)
            }
            _ => {
                let temp = self.declare_temp(expr_span, "temp_stmt_expr");
                unpack!(block = self.into(Lvalue::Local(temp), block, expr));
                self.schedule_drop(expr_span, temp);
                block.unit()
            }
        }
    }

    /// Compile `return $expr` into:
    ///
    /// ```
    /// 'block:
    ///     $return pointer = $expr;
    ///     goto 'exit;
    ///
    /// 'after_return:
    ///     ...
    /// ```
    fn expr_ret(&mut self,
                mut block: BasicBlock,
                span: Span,
                value: &Option<P<ast::Expr>>) -> BlockAnd<()> {
        block = match *value {
            Some(ref value) => {
                unpack!(self.into(Lvalue::Local(RETURN_POINTER), block, value))
            }
            None => {
                self.assign_lvalue_unit(span, block, Lvalue::Local(RETURN_POINTER));
                block
            }
        };
        let extent = self.extent_of_return_scope();
        let return_block = self.return_block();
        self.exit_scope(span, extent, block, return_block);

        // We need to start a new block after this one since there might be trailing expressions
        // that we need to type check.
        block = self.start_new_block(span, Some("AfterReturn"));

        block.unit()
    }

    fn break_or_continue<F>(&mut self,
                            destination: Lvalue,
                            span: Span,
                            label: Option<ast::Ident>,
                            block: BasicBlock,
                            exit_selector: F)
                            -> BasicBlock
        where F: FnOnce(&LoopScope) -> BasicBlock
    {
        if !self.is_in_loop() {
            self.cx.span_err(span, "cannot break outside of a loop");
        }

        // `break` or `continue` has a type of `()`.
        self.assign_lvalue_unit(span, block, destination);

        let loop_scope = self.find_loop_scope(span, label);
        let exit_block = exit_selector(&loop_scope);
        self.exit_scope(span, loop_scope.extent, block, exit_block);

        // Even though we've exited `block`, there could be code following the break/continue. To
        // keep rust happy, we'll create a new block that has an edge to `block`, even though
        // control will never actually flow into this block.
        self.start_new_block(span, Some("AfterBreakOrContinue"))
    }
}
