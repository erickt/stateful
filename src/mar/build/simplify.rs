use aster::AstBuilder;

use syntax::ast::{self, ExprKind};
use syntax::fold::{self, Folder};
use syntax::ptr::P;
use syntax::codemap::{respan, Span};

use mar::build::transition::ContainsTransition;

pub fn simplify_item(item: P<ast::Item>) -> ast::Item {
    let mut expander = Expander::new();
    let mut assigner = Assigner { next_node_id: ast::NodeId::new(1) };

    let expanded = expander.fold_item_simple(item.unwrap());
    assigner.fold_item_simple(expanded)
}

pub struct Assigner {
    pub next_node_id: ast::NodeId,
}

impl Folder for Assigner {
    fn new_id(&mut self, old_id: ast::NodeId) -> ast::NodeId {
        assert_eq!(old_id, ast::DUMMY_NODE_ID);

        let node_id = self.next_node_id;

        let next_node_id = match self.next_node_id.as_usize().checked_add(1) {
            Some(next_node_id) => ast::NodeId::new(next_node_id),
            None => { panic!("ran out of node ids!") }
        };
        self.next_node_id = next_node_id;

        node_id
    }

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
        fold::noop_fold_mac(mac, self)
    }
}

pub struct Expander {
    loop_depth: usize,
}

impl Expander {
    pub fn new() -> Expander {
        Expander { loop_depth: 0 }
    }

    fn is_in_loop(&self) -> bool {
        self.loop_depth != 0
    }

    fn fold_sub_expr(&mut self, e: ast::Expr) -> P<ast::Expr> {
        P(fold::noop_fold_expr(e, self))
    }

    fn construct_expr(&mut self,
                      node: ast::ExprKind,
                      id: ast::NodeId,
                      span: Span,
                      attrs: ast::ThinVec<ast::Attribute>) -> P<ast::Expr> {
        P(ast::Expr {
            node: node,
            id: self.new_id(id),
            span: self.new_span(span),
            attrs: fold::fold_attrs(attrs.into(), self).into(),
        })
    }
}

impl fold::Folder for Expander {
    fn fold_expr(&mut self, e: P<ast::Expr>) -> P<ast::Expr> {
        if !e.contains_transition(self.is_in_loop()) {
            return e;
        }

        let expr = e.unwrap();

        match expr.node {
            ExprKind::ForLoop(pat, expr, loop_block, label) => {
                self.fold_sub_expr(desugar_for_loop(pat, expr, loop_block, label))
            }
            ExprKind::IfLet(pat, expr, then_block, else_block) => {
                self.fold_sub_expr(desugar_if_let(pat, expr, then_block, else_block))
            }
            ExprKind::WhileLet(pat, expr, then_block, label) => {
                self.fold_sub_expr(desugar_while_let(pat, expr, then_block, label))
            }
            ExprKind::While(cond, body, opt_ident) => {
                self.loop_depth += 1;
                let node = ExprKind::While(self.fold_expr(cond),
                                    self.fold_block(body),
                                    opt_ident.map(|label| respan(self.new_span(label.span),
                                                                self.fold_ident(label.node))));
                self.loop_depth -= 1;
                self.construct_expr(node, expr.id, expr.span, expr.attrs)
            }
            ExprKind::Loop(body, opt_ident) => {
                self.loop_depth += 1;
                let node = ExprKind::Loop(self.fold_block(body),
                                    opt_ident.map(|label| respan(self.new_span(label.span),
                                                                self.fold_ident(label.node))));
                self.loop_depth -= 1;
                self.construct_expr(node, expr.id, expr.span, expr.attrs)
            }
            _ => P(expr),
        }
    }

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
        fold::noop_fold_mac(mac, self)
    }
}


fn desugar_for_loop(pat: P<ast::Pat>,
                    expr: P<ast::Expr>,
                    loop_block: P<ast::Block>,
                    label: Option<ast::SpannedIdent>) -> ast::Expr {
    // Desugar a for loop into:
    //
    // {
    //     let mut iter = ::std::iter::IntoIterator::into_iter($expr);
    //     'label: loop {
    //         match iter.next() {
    //             ::std::option::Option::Some($pat) => $loop_block,
    //             ::std::option::Option::None => break,
    //         }
    //     }
    // }

    let builder = AstBuilder::new().span(expr.span);

    // ::std::iter::IntoIterator::into_iter($expr)
    let into_iter = builder.expr().call()
        .path()
            .global()
            .ids(&["std", "iter", "IntoIterator", "into_iter"])
            .build()
        .with_arg(expr.clone())
        .build();

    // iter.next()
    let iter_next = builder.expr().method_call("next")
        .id("__stateful_iter")
        .build();

    // ::std::option::Option::Some($pat)
    let some_pat = builder.pat().enum_()
        .global().ids(&["std", "option", "Option", "Some"]).build()
        .pat().build(pat.clone())
        .build();

    // $some_pat => $loop_block
    let some_arm = builder.arm()
        .with_pat(some_pat)
        .body().build_block(loop_block.clone());

    // ::std::option::Option::None
    let none_pat = builder.pat().path()
        .global().ids(&["std", "option", "Option", "None"]).build();

    // $none_pat => break,
    let none_arm = builder.arm()
        .with_pat(none_pat)
        .body().break_();

    // match $iter_next() {
    //     Some($pat) => $block,
    //     None => break,
    // }
    let match_expr = builder.expr()
        .match_().build(iter_next)
        .with_arm(some_arm)
        .with_arm(none_arm)
        .build();

    // `loop { $match_expr; };`
    let mut loop_builder = builder.expr().loop_();

    if let Some(label) = label {
        loop_builder = loop_builder.label(label.node);
    }

    let loop_ = loop_builder.block()
        .stmt().build_expr(match_expr)
        .build();

    // `let mut iter = $into_iter;`
    let iter = builder.stmt()
        .let_().mut_id("__stateful_iter")
        .build_expr(into_iter);

    // {
    //     $into_iter;
    //     $loop;
    // }
    builder.expr().block()
        .with_stmt(iter)
        .stmt().build_expr(loop_)
        .build().unwrap()
}

fn desugar_if_let(pat: P<ast::Pat>,
                  expr: P<ast::Expr>,
                  then_block: P<ast::Block>,
                  else_block: Option<P<ast::Expr>>) -> ast::Expr {
    // Desugar an if-let:
    //
    // match $expr {
    //     $pat => $then_block,
    //     _ => $else_block,
    // }

    let builder = AstBuilder::new().span(expr.span);

    // $then_pat => $then_block
    let then_arm = builder.arm()
        .with_pat(pat.clone())
        .body().build_block(then_block.clone());

    // _ => $else_block
    let else_arm = match else_block {
        Some(ref else_block) => builder.arm().pat().wild().body().build(else_block.clone()),
        None => builder.arm().pat().wild().body().unit(),
    };

    // match $iter_next() {
    //     $pat => $then_block,
    //     _ => #else_block,
    // }
    builder.expr()
        .match_().build(expr.clone())
        .with_arm(then_arm)
        .with_arm(else_arm)
        .build().unwrap()
}

fn desugar_while_let(pat: P<ast::Pat>,
                     expr: P<ast::Expr>,
                     then_block: P<ast::Block>,
                     label: Option<ast::SpannedIdent>) -> ast::Expr {
    // Desugar an while-let:
    //
    // 'label: loop {
    //     match $expr {
    //         $pat => $body_block,
    //         _ => break,
    //     }
    // }

    let builder = AstBuilder::new().span(expr.span);

    // $pat => $then_block
    let then_arm = builder.arm()
        .with_pat(pat.clone())
        .body().build_block(then_block.clone());

    // _ => break
    let else_arm = builder.arm()
        .pat().wild()
        .body().break_();

    // match $expr {
    //     $then_arm,
    //     $else_arm,
    // }
    let match_expr = builder.expr()
        .match_().build(expr.clone())
        .with_arm(then_arm)
        .with_arm(else_arm)
        .build();

    // `'$label: loop { $match_expr; };`
    let mut loop_builder = builder.expr().loop_();

    if let Some(label) = label {
        loop_builder = loop_builder.label(label.node);
    }

    loop_builder.block()
        .stmt().build_expr(match_expr)
        .build().unwrap()
}
