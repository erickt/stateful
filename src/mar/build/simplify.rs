use aster::AstBuilder;
use mar::build::transition::{self, ContainsTransition, Transition};
use syntax::ast::{self, ExprKind, StmtKind};
use syntax::codemap::{respan, Span};
use syntax::ext::base::ExtCtxt;
use syntax::ext::tt::transcribe::new_tt_reader;
use syntax::fold::{self, Folder};
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

pub fn simplify_item(cx: &ExtCtxt, item: P<ast::Item>) -> ast::Item {
    let mut expander = Expander::new(cx);
    let mut assigner = Assigner { next_node_id: ast::NodeId::new(1) };

    let expanded = expander.fold_item_simple(item.unwrap());
    assigner.fold_item_simple(expanded)
}

struct Assigner {
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

struct Expander<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    loop_depth: usize,
}

impl<'a, 'b> Expander<'a, 'b> {
    fn new(cx: &'a ExtCtxt<'b>) -> Self {
        Expander {
            cx: cx,
            loop_depth: 0,
        }
    }

    fn is_in_loop(&self) -> bool {
        self.loop_depth != 0
    }

    fn fold_sub_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        expr.map(|expr| fold::noop_fold_expr(expr, self))
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

impl<'a, 'b: 'a> fold::Folder for Expander<'a, 'b> {
    fn fold_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        if !expr.contains_transition(self.is_in_loop()) {
            return expr;
        }

        let expr = expr.unwrap();

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
            ExprKind::Mac(mac) => {
                if is_try_path(&mac.node.path) {
                    let expr = desugar_try(self.cx, &mac);
                    self.fold_expr(expr)
                } else {
                    self.construct_expr(ExprKind::Mac(mac), expr.id, expr.span, expr.attrs)
                }
            }
            _ => {
                P(expr)
            }
        }
    }

    fn fold_stmt(&mut self, stmt: ast::Stmt) -> SmallVector<ast::Stmt> {
        match stmt.node {
            StmtKind::Mac(ref mac) if is_try_path(&mac.0.node.path) => {
                let expr = self.fold_expr(desugar_try(self.cx, &mac.0));

                let stmt = match mac.1 {
                    ast::MacStmtStyle::Semicolon => {
                        AstBuilder::new().stmt().semi().build(expr)
                    }
                    ast::MacStmtStyle::Braces | ast::MacStmtStyle::NoBraces => {
                        AstBuilder::new().stmt().expr().build(expr)
                    }
                };

                SmallVector::one(stmt)
            }
            _ => fold::noop_fold_stmt(stmt, self),
        }
    }

    fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
        // this macro may contain transitions. So parse it, expand the inner expression,
        // then convert it back into a macro.
        match transition::parse_mac_transition(self.cx, &mac) {
            Some(Transition::Yield(expr)) | Some(Transition::Await(expr)) => {
                let expr = self.fold_expr(expr);
                AstBuilder::new().span(mac.span).mac()
                    .build_path(mac.node.path.clone())
                    .with_arg(expr)
                    .build()
            }
            Some(Transition::Suspend(_)) => {
                self.cx.span_err(mac.span, "suspend!(...) cannot be directly called");
                fold::noop_fold_mac(mac, self)
            }
            None => {
                fold::noop_fold_mac(mac, self)
            }
        }
    }
}

pub fn is_try_path(path: &ast::Path) -> bool {
    let builder = AstBuilder::new();
    let yield_ = builder.path().id("try").build();

    !path.global && path.segments == yield_.segments
}

/// Desugar a for loop into:
///
/// ```
/// {
///     let mut iter = ::std::iter::IntoIterator::into_iter($iter);
///     'label: loop {
///         match iter.next() {
///             ::std::option::Option::Some($pat) => $body,
///             ::std::option::Option::None => break,
///         }
///     }
/// }
/// ```
fn desugar_for_loop(pat: P<ast::Pat>,
                    iter: P<ast::Expr>,
                    body: P<ast::Block>,
                    label: Option<ast::SpannedIdent>) -> P<ast::Expr> {
    let builder = AstBuilder::new().span(iter.span);

    // ::std::iter::IntoIterator::into_iter($expr)
    let into_iter = builder.expr().call()
        .path()
            .global()
            .ids(&["std", "iter", "IntoIterator", "into_iter"])
            .build()
        .with_arg(iter.clone())
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

    // $some_pat => $body
    let some_arm = builder.arm()
        .with_pat(some_pat)
        .body().build_block(body.clone());

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
        .build()
}

/// Desugar an if-let:
///
/// ```rust
/// match $expr {
///     $pat => $then_block,
///     _ => $else_block,
/// }
/// ```
fn desugar_if_let(pat: P<ast::Pat>,
                  expr: P<ast::Expr>,
                  then_block: P<ast::Block>,
                  else_block: Option<P<ast::Expr>>) -> P<ast::Expr> {

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
        .build()
}

/// Desugar an while-let:
///
/// ```rust
/// 'label: loop {
///     match $expr {
///         $pat => $body_block,
///         _ => break,
///     }
/// }
/// ```
fn desugar_while_let(pat: P<ast::Pat>,
                     expr: P<ast::Expr>,
                     then_block: P<ast::Block>,
                     label: Option<ast::SpannedIdent>) -> P<ast::Expr> {
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
        .build()
}

fn parse_mac_try(cx: &ExtCtxt, mac: &ast::Mac) -> P<ast::Expr> {
    let rdr = new_tt_reader(
        &cx.parse_sess().span_diagnostic,
        None,
        None,
        mac.node.tts.clone());

    let mut parser = Parser::new(cx.parse_sess(), cx.cfg(), Box::new(rdr.clone()));
    let expr = panictry!(parser.parse_expr());
    panictry!(parser.expect(&Token::Eof));

    expr
}

/// Desugar a `try!(...)`:
///
/// ```
/// match $expr {
///     ::std::result::Result::Ok(expr) => expr,
///     ::std::result::Result::Err(err) =>
///         return ::std::result::Result::Err(::std::convert::From::from(err)),
/// }
/// ```
fn desugar_try(cx: &ExtCtxt, mac: &ast::Mac) -> P<ast::Expr> {
    let expr = parse_mac_try(cx, mac);
    AstBuilder::new().span(expr.span).expr().try().build(expr)
}
