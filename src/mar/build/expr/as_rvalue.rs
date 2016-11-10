use mar::build::expr::category::{Category, RvalueFunc};
use mar::build::mac::{is_mac, parse_mac};
use mar::build::{BlockAnd, BlockAndExtension, Builder};
use mar::repr::*;
use syntax::ast::{self, ExprKind};
use syntax::ptr::P;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn as_rvalue(&mut self,
                     mut block: BasicBlock,
                     expr: &P<ast::Expr>) -> BlockAnd<Rvalue> {
        debug!("expr_as_rvalue(block={:?}, expr={:?})", block, expr);

        let this = self;

        match expr.node {
            /*
            ExprKind::Scope { extent, value } => {
                this.in_scope(extent, block, |this| this.as_rvalue(block, value))
            }
            ExprKind::InlineAsm { asm, outputs, inputs } => {
                let outputs = outputs.into_iter().map(|output| {
                    unpack!(block = this.as_lvalue(block, output))
                }).collect();

                let inputs = inputs.into_iter().map(|input| {
                    unpack!(block = this.as_operand(block, input))
                }).collect();

                block.and(Rvalue::InlineAsm {
                    asm: asm.clone(),
                    outputs: outputs,
                    inputs: inputs
                })
            }
            ExprKind::Repeat { value, count } => {
                let value_operand = unpack!(block = this.as_operand(block, value));
                block.and(Rvalue::Repeat(value_operand, count))
            }
            */
            ExprKind::AddrOf(mutability, ref arg) => {
                let arg_lvalue = unpack!(block = this.as_lvalue(block, arg));
                block.and(Rvalue::Ref(mutability, arg_lvalue))
            }
            ExprKind::Binary(op, ref lhs, ref rhs) => {
                let lhs = unpack!(block = this.as_operand(block, lhs));
                let rhs = unpack!(block = this.as_operand(block, rhs));
                this.build_binary_op(block, op, lhs, rhs)
            }
            ExprKind::Unary(op, ref arg) => {
                let arg = unpack!(block = this.as_operand(block, arg));
                block.and(Rvalue::Unary(op, arg))
            }
            /*
            ExprKind::Box { value, value_extents } => {
                let value = this.hir.mirror(value);
                let result = this.temp(expr.ty);
                // to start, malloc some memory of suitable type (thus far, uninitialized):
                this.cfg.push_assign(block, source_info, &result, Rvalue::Box(value.ty));
                this.in_scope(value_extents, block, |this| {
                    // schedule a shallow free of that memory, lest we unwind:
                    this.schedule_box_free(expr_span, value_extents, &result, value.ty);
                    // initialize the box contents:
                    unpack!(block = this.into(&result.clone().deref(), block, value));
                    block.and(Rvalue::Use(Operand::Consume(result)))
                })
            }
            ExprKind::Cast { source } => {
                let source = this.hir.mirror(source);

                let source = unpack!(block = this.as_operand(block, source));
                block.and(Rvalue::Cast(CastKind::Misc, source, expr.ty))
            }
            ExprKind::Use { source } => {
                let source = unpack!(block = this.as_operand(block, source));
                block.and(Rvalue::Use(source))
            }
            ExprKind::ReifyFnPointer { source } => {
                let source = unpack!(block = this.as_operand(block, source));
                block.and(Rvalue::Cast(CastKind::ReifyFnPointer, source, expr.ty))
            }
            ExprKind::UnsafeFnPointer { source } => {
                let source = unpack!(block = this.as_operand(block, source));
                block.and(Rvalue::Cast(CastKind::UnsafeFnPointer, source, expr.ty))
            }
            ExprKind::Unsize { source } => {
                let source = unpack!(block = this.as_operand(block, source));
                block.and(Rvalue::Cast(CastKind::Unsize, source, expr.ty))
            }
            ExprKind::Vec { fields } => {
                // (*) We would (maybe) be closer to trans if we
                // handled this and other aggregate cases via
                // `into()`, not `as_rvalue` -- in that case, instead
                // of generating
                //
                //     let tmp1 = ...1;
                //     let tmp2 = ...2;
                //     dest = Rvalue::Aggregate(Foo, [tmp1, tmp2])
                //
                // we could just generate
                //
                //     dest.f = ...1;
                //     dest.g = ...2;
                //
                // The problem is that then we would need to:
                //
                // (a) have a more complex mechanism for handling
                //     partial cleanup;
                // (b) distinguish the case where the type `Foo` has a
                //     destructor, in which case creating an instance
                //     as a whole "arms" the destructor, and you can't
                //     write individual fields; and,
                // (c) handle the case where the type Foo has no
                //     fields. We don't want `let x: ();` to compile
                //     to the same MIR as `let x = ();`.

                // first process the set of fields
                let fields: Vec<_> =
                    fields.into_iter()
                          .map(|f| unpack!(block = this.as_operand(block, f)))
                          .collect();

                block.and(Rvalue::Aggregate(AggregateKind::Array, fields))
            }
            */
            ExprKind::Tup(ref fields) => { // see (*) above
                // first process the set of fields
                let fields: Vec<_> =
                    fields.into_iter()
                          .map(|f| unpack!(block = this.as_operand(block, f)))
                          .collect();

                block.and(Rvalue::Tuple(fields))
            }
            /*
            ExprKind::Closure { closure_id, substs, upvars } => { // see (*) above
                let upvars =
                    upvars.into_iter()
                          .map(|upvar| unpack!(block = this.as_operand(block, upvar)))
                          .collect();
                block.and(Rvalue::Aggregate(AggregateKind::Closure(closure_id, substs), upvars))
            }
            */
            ExprKind::Struct(ref path, ref fields, ref wth) => { // see (*) above
                let operands: Vec<_> = fields.iter()
                    .map(|f| unpack!(block = this.as_operand(block, &f.expr)))
                    .collect();

                let wth = wth.as_ref().map(|wth| unpack!(block = this.as_operand(block, wth)));

                block.and(Rvalue::Struct(path.clone(), fields.clone(), operands, wth))
            }
            ExprKind::Range(ref from, ref to, ref limits) => {
                let from = from.as_ref().map(|from| {
                    unpack!(block = this.as_operand(block, from))
                });

                let to = to.as_ref().map(|to| {
                    unpack!(block = this.as_operand(block, to))
                });

                block.and(Rvalue::Range(from, to, *limits))
            }

            ExprKind::Assign(..) |
            ExprKind::AssignOp(..) => {
                block = unpack!(this.stmt_expr(block, expr));
                block.and(this.unit_rvalue())
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "moved") => {
                let expr = parse_mac(this.cx, mac);
                this.moved_exprs.insert(expr.id);
                this.as_rvalue(block, &expr)
            }

            ExprKind::Mac(ref mac) if is_mac(mac, "copied") => {
                let expr = parse_mac(this.cx, mac);
                this.copied_exprs.insert(expr.id);
                this.as_rvalue(block, &expr)
            }

            ExprKind::Lit(..) |
            ExprKind::Block(..) |
            ExprKind::Match(..) |
            ExprKind::If(..) |
            ExprKind::Loop(..) |
            ExprKind::Call(..) |
            ExprKind::Field(..) |
            ExprKind::Index(..) |
            ExprKind::Break(..) |
            ExprKind::Continue(..) |
            ExprKind::Ret(..) |
            ExprKind::Path(..) => {
                // these do not have corresponding `Rvalue` variants,
                // so make an operand and then return that
                debug_assert!(match Category::of(&expr.node) {
                    Some(Category::Rvalue(RvalueFunc::AsRvalue)) => false,
                    _ => true,
                });
                let operand = unpack!(block = this.as_operand(block, expr));
                block.and(Rvalue::Use(operand))
            }
            _ => {
                // these do not have corresponding `Rvalue` variants,
                // so make an operand and then return that
                debug_assert!(match Category::of(&expr.node) {
                    Some(Category::Rvalue(RvalueFunc::AsRvalue)) => false,
                    _ => true,
                });
                let operand = unpack!(block = this.as_operand(block, expr));
                block.and(Rvalue::Use(operand))
            }
        }
    }

    pub fn build_binary_op(&mut self,
                           block: BasicBlock,
                           op: ast::BinOp,
                           lhs: Operand,
                           rhs: Operand) -> BlockAnd<Rvalue> {
        block.and(Rvalue::Binary(op, lhs, rhs))
    }
}
