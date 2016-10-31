use mar::build::expr::category::Category;
use mar::build::{BlockAndExtension, Builder, BlockAnd};
use mar::repr::*;
use syntax::ast::{self, ExprKind};
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn as_operand(&mut self,
                      mut block: BasicBlock,
                      expr: &P<ast::Expr>) -> BlockAnd<Operand> {
        debug!("expr_as_operand(block={:?}, expr={:?})", block, expr);
        let this = self;

        match expr.node {
            ExprKind::Path(..) => {
                // Path operands don't need a temporary.
                let operand = unpack!(block = this.as_lvalue(block, expr));
                this.move_lvalue(expr.span, operand.clone());
                block.and(Operand::Consume(operand))
            }
            _ => {
                let category = Category::of(&expr.node).unwrap();
                debug!("expr_as_operand: category={:?} for={:?}", category, expr.node);
                match category {
                    Category::Constant => {
                        let constant = this.as_constant(expr);
                        block.and(Operand::Constant(constant))
                    }
                    Category::Lvalue |
                    Category::Rvalue(..) => {
                        let operand = unpack!(block = this.as_temp(block, expr));
                        this.move_lvalue(expr.span, operand.clone());
                        block.and(Operand::Consume(operand))
                    }
                }
            }
        }
    }
}
