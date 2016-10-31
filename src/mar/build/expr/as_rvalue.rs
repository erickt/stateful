use mar::build::expr::category::{Category, RvalueFunc};
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
            ExprKind::Borrow { region, borrow_kind, arg } => {
                let arg_lvalue = unpack!(block = this.as_lvalue(block, arg));
                block.and(Rvalue::Ref(region, borrow_kind, arg_lvalue))
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let lhs = unpack!(block = this.as_operand(block, lhs));
                let rhs = unpack!(block = this.as_operand(block, rhs));
                this.build_binary_op(block, op, expr_span, expr.ty,
                                     lhs, rhs)
            }
            ExprKind::Unary { op, arg } => {
                let arg = unpack!(block = this.as_operand(block, arg));
                // Check for -MIN on signed integers
                if this.hir.check_overflow() && op == UnOp::Neg && expr.ty.is_signed() {
                    let bool_ty = this.hir.bool_ty();

                    let minval = this.minval_literal(expr_span, expr.ty);
                    let is_min = this.temp(bool_ty);

                    this.cfg.push_assign(block, source_info, &is_min,
                                         Rvalue::BinaryOp(BinOp::Eq, arg.clone(), minval));

                    let err = ConstMathErr::Overflow(Op::Neg);
                    block = this.assert(block, Operand::Consume(is_min), false,
                                        AssertMessage::Math(err), expr_span);
                }
                block.and(Rvalue::UnaryOp(op, arg))
            }
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
            ExprKind::Tuple { fields } => { // see (*) above
                // first process the set of fields
                let fields: Vec<_> =
                    fields.into_iter()
                          .map(|f| unpack!(block = this.as_operand(block, f)))
                          .collect();

                block.and(Rvalue::Aggregate(AggregateKind::Tuple, fields))
            }
            ExprKind::Closure { closure_id, substs, upvars } => { // see (*) above
                let upvars =
                    upvars.into_iter()
                          .map(|upvar| unpack!(block = this.as_operand(block, upvar)))
                          .collect();
                block.and(Rvalue::Aggregate(AggregateKind::Closure(closure_id, substs), upvars))
            }
            ExprKind::Adt {
                adt_def, variant_index, substs, fields, base
            } => { // see (*) above
                let is_union = adt_def.is_union();
                let active_field_index = if is_union { Some(fields[0].name.index()) } else { None };

                // first process the set of fields that were provided
                // (evaluating them in order given by user)
                let fields_map: FnvHashMap<_, _> =
                    fields.into_iter()
                          .map(|f| (f.name, unpack!(block = this.as_operand(block, f.expr))))
                          .collect();

                let field_names = this.hir.all_fields(adt_def, variant_index);

                let fields = if let Some(FruInfo { base, field_types }) = base {
                    let base = unpack!(block = this.as_lvalue(block, base));

                    // MIR does not natively support FRU, so for each
                    // base-supplied field, generate an operand that
                    // reads it from the base.
                    field_names.into_iter()
                        .zip(field_types.into_iter())
                        .map(|(n, ty)| match fields_map.get(&n) {
                            Some(v) => v.clone(),
                            None => Operand::Consume(base.clone().field(n, ty))
                        })
                        .collect()
                } else {
                    field_names.iter().filter_map(|n| fields_map.get(n).cloned()).collect()
                };

                let adt = AggregateKind::Adt(adt_def, variant_index, substs, active_field_index);
                block.and(Rvalue::Aggregate(adt, fields))
            }
            */
            ExprKind::Struct(ref path, ref fields, ref wth) => {
                // see (*) above

                // first process the set of fields that were provided
                // (evaluating them in order given by user)
                let operands: Vec<_> =
                    fields.iter()
                          .map(|f| unpack!(block = this.as_operand(block, &f.expr)))
                          .collect();

                let wth = wth.as_ref().map(|wth| unpack!(block = this.as_operand(block, wth)));

                block.and(Rvalue::Struct(path.clone(), fields.clone(), operands, wth))
            }

            ExprKind::Assign(..) |
            ExprKind::AssignOp(..) => {
                block = unpack!(this.stmt_expr(block, expr));
                block.and(this.unit_rvalue())
            }
            ExprKind::Lit(..) |
            ExprKind::Block(..) |
            ExprKind::Match(..) |
            ExprKind::If(..) |
            ExprKind::Loop(..) |
            ExprKind::Call(..) |
            ExprKind::Field(..) |
            ExprKind::AddrOf(..) |
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
}
