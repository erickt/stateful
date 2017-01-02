use build::mac::{is_mac, parse_mac};
use build::{BlockAnd, BlockAndExtension, Builder};
use mir::*;
use syntax::ast::{self, ExprKind};
use syntax::codemap::Span;
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn stmt_expr(&mut self,
                     mut block: BasicBlock,
                     expr: &P<ast::Expr>) -> BlockAnd<()> {
        debug!("stmt_expr(block={:?}, expr={:?})", block, expr);

        let this = self;
        let expr_span = expr.span;

        match expr.node {
            ExprKind::Continue(label) => {
                this.break_or_continue(expr_span, block, |this| {
                    let loop_scope = this.find_loop_scope(expr_span, label);
                    (loop_scope.continue_block, loop_scope.extent)
                })
            }
            ExprKind::Break(label, None) => {
                this.break_or_continue(expr_span, block, |this| {
                    let loop_scope = this.find_loop_scope(expr_span, label);
                    loop_scope.might_break = true;
                    (loop_scope.break_block, loop_scope.extent)
                })
            }
            ExprKind::Break(_label, Some(ref _value)) => {
                span_bug!(this.cx, expr_span, "break returning values is not supported yet");
            }
            ExprKind::Assign(ref lhs, ref rhs) => {
                // Note: we evaluate assignments right-to-left. This
                // is better for borrowck interaction with overloaded
                // operators like x[j] = x[i].

                let rhs = unpack!(block = this.as_rvalue(block, rhs));
                let lhs = unpack!(block = this.as_lvalue(block, lhs));

                this.push_assign(block, expr_span, &lhs, rhs);

                block.unit()
            }
            ExprKind::AssignOp(op, ref lhs, ref rhs) => {
                // Note: we evaluate assignments right-to-left. This
                // is better for borrowck interaction with overloaded
                // operators like x[j] = x[i].

                let rhs = unpack!(block = this.as_operand(block, rhs));
                let lhs = unpack!(block = this.as_lvalue(block, lhs));
                let result = unpack!(
                    block = this.build_binary_op(
                        block,
                        op,
                        Operand::Consume(lhs.clone()),
                        rhs));

                this.push_assign(block, expr_span, &lhs, result);

                block.unit()
            }
            ExprKind::Ret(ref returned_expr) => {
                this.expr_ret(block, expr.span, returned_expr)
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "moved") => {
                let expr = parse_mac(this.cx, mac);
                this.moved_exprs.insert(expr.id);
                this.stmt_expr(block, &expr)
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "copied") => {
                let expr = parse_mac(this.cx, mac);
                this.copied_exprs.insert(expr.id);
                this.stmt_expr(block, &expr)
            }

            _ => {
                let temp = this.temp(expr_span, "temp_stmt_expr");
                unpack!(block = this.into(temp, block, expr));
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
                self.push_assign_unit(span, block, &Lvalue::Local(RETURN_POINTER));
                block
            }
        };
        let extent = self.extent_of_return_scope();
        let return_block = self.return_block();
        self.exit_scope(span, extent, block, return_block);

        // We need to start a new block after this one since there might be trailing expressions
        // that we need to type check.
        block = self.cfg.start_new_block(span, Some("AfterReturn"));

        block.unit()
    }

    fn break_or_continue<F>(&mut self,
                            span: Span,
                            mut block: BasicBlock,
                            exit_selector: F)
                            -> BlockAnd<()>
        where F: FnOnce(&mut Builder<'a, 'b>) -> (BasicBlock, CodeExtent)
    {
        debug!("break_or_continue(block={:?})", block);

        if !self.is_in_loop() {
            self.cx.span_err(span, "cannot break outside of a loop");
        }

        let (exit_block, extent) = exit_selector(self);
        debug!("break_or_continue(extent={:?}, exit_block={:?})", extent, exit_block);

        self.exit_scope(span, extent, block, exit_block);
        block = self.cfg.start_new_block(span, Some("AfterBreakOrContinue"));

        block.unit()
    }
}
