use aster::AstBuilder;
use build::transition::{self, Transition};
use mir::*;
use syntax::ast::{self, ExprKind, StmtKind};
use syntax::codemap::respan;
use syntax::ext::base::ExtCtxt;
use syntax::ext::tt::transcribe::new_tt_reader;
use syntax::fold::{self, Folder};
use syntax::parse::parser::Parser;
use syntax::parse::token::Token;
use syntax::ptr::P;
use syntax::util::small_vector::SmallVector;

pub fn desugar_block(cx: &ExtCtxt,
                     state_machine_kind: StateMachineKind,
                     fn_decl: FunctionDecl,
                     block: P<ast::Block>) -> (FunctionDecl, P<ast::Block>) {
    let mut desugar = Desugar {
        cx: cx,
        state_machine_kind: state_machine_kind,
    };

    let mut assigner = AssignIds {
        next_node_id: ast::NodeId::new(1),
    };

    let fn_decl = FunctionDecl::new(
        assigner.fold_ident(desugar.fold_ident(fn_decl.ident)),
        assigner.fold_fn_decl(desugar.fold_fn_decl(fn_decl.fn_decl)),
        fn_decl.unsafety,
        fn_decl.abi,
        assigner.fold_generics(desugar.fold_generics(fn_decl.generics)),
    );

    let block = assigner.fold_block(desugar.fold_block(block));

    (fn_decl, block)
}

struct Desugar<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    state_machine_kind: StateMachineKind,
}

impl<'a, 'b> Desugar<'a, 'b> {
    fn fold_sub_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        expr.map(|expr| fold::noop_fold_expr(expr, self))
    }

    pub fn expr_mac(&mut self, mac: &ast::Mac) -> Option<P<ast::Expr>> {
        // NOTE: we're folding then desugaring because `suspend!()` cannot currently be directly
        // called.
        match (self.state_machine_kind, transition::parse_mac_transition(self.cx, mac)) {
            (StateMachineKind::Generator, Some(transition::Transition::Yield(expr))) => {
                let expr = self.fold_sub_expr(expr);
                Some(desugar_yield(self.cx, expr))
            }
            (StateMachineKind::Async, Some(transition::Transition::Await(expr))) => {
                let expr = self.fold_sub_expr(expr);
                Some(desugar_await(self.cx, expr))
            }
            _ => {
                if is_try_path(&mac.node.path) {
                    let expr = parse_mac_try(self.cx, mac);
                    let expr = self.fold_sub_expr(expr);
                    Some(desugar_try(expr))
                } else {
                    None
                }
            }
        }
    }
}

impl<'a, 'b: 'a> fold::Folder for Desugar<'a, 'b> {
    fn fold_expr(&mut self, expr: P<ast::Expr>) -> P<ast::Expr> {
        expr.map(|expr| {
            match expr.node {
                ExprKind::ForLoop(pat, expr, loop_block, label) => {
                    desugar_for_loop(
                        self.fold_pat(pat),
                        self.fold_expr(expr),
                        self.fold_block(loop_block),
                        label.map(|label| {
                            respan(
                                self.new_span(label.span),
                                self.fold_ident(label.node)
                            )
                        })
                    ).unwrap()
                }
                ExprKind::IfLet(pat, expr, then_block, else_block) => {
                    let expr = desugar_if_let(pat, expr, then_block, else_block).unwrap();
                    fold::noop_fold_expr(expr, self)
                }
                ExprKind::WhileLet(pat, expr, then_block, label) => {
                    let expr = desugar_while_let(pat, expr, then_block, label).unwrap();
                    fold::noop_fold_expr(expr, self)
                }
                ExprKind::While(cond, body, opt_ident) => {
                    let body = self.fold_block(body);
                    let opt_ident = opt_ident.map(|label| {
                        respan(self.new_span(label.span), self.fold_ident(label.node))
                    });

                    let node = ExprKind::While(self.fold_expr(cond),
                                        self.fold_block(body),
                                        opt_ident.map(|label| respan(self.new_span(label.span),
                                                                    self.fold_ident(label.node))));
                    ast::Expr { node: node, .. expr }
                }
                ExprKind::Loop(body, opt_ident) => {
                    let body = self.fold_block(body);
                    let opt_ident = opt_ident.map(|label| {
                        respan(self.new_span(label.span), self.fold_ident(label.node))
                    });
                    let node = ExprKind::Loop(body, opt_ident);
                    ast::Expr { node: node, .. expr }
                }
                ExprKind::Mac(mac) => {
                    if let Some(expr) = self.expr_mac(&mac) {
                        expr.unwrap()
                    } else {
                        ast::Expr { node: ExprKind::Mac(mac), .. expr }
                    }
                }
                _ => {
                    fold::noop_fold_expr(expr, self)
                }
            }
        })
    }

    fn fold_stmt(&mut self, stmt: ast::Stmt) -> SmallVector<ast::Stmt> {
        match stmt.node {
            StmtKind::Mac(ref mac) => {
                if let Some(expr) = self.expr_mac(&mac.0) {
                    let stmt = match mac.1 {
                        ast::MacStmtStyle::Semicolon => {
                            AstBuilder::new().stmt().semi().build(expr)
                        }
                        ast::MacStmtStyle::Braces | ast::MacStmtStyle::NoBraces => {
                            AstBuilder::new().stmt().expr().build(expr)
                        }
                    };

                    return SmallVector::one(stmt);
                }
            }
            _ => {}
        }

        fold::noop_fold_stmt(stmt, self)
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

struct AssignIds {
    next_node_id: ast::NodeId,
}

impl fold::Folder for AssignIds {
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

fn is_try_path(path: &ast::Path) -> bool {
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

    // ::std::iter::Iterator::next(&mut $into_iter)
    let iter_next = builder.expr().call()
        .path()
            .global()
            .ids(&["std", "iter", "Iterator", "next"])
            .build()
        .arg().mut_ref().id("__stateful_iter")
        .build();

    /*
    // moved!(iter.next())
    let iter_next = builder.expr().mac().path().id("moved").build()
        .expr().build(iter_next)
        .build();
    */

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
        mac.node.tts.clone());

    let mut parser = Parser::new(
        cx.parse_sess(),
        Box::new(rdr.clone()),
        None,
        false);

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
fn desugar_try(expr: P<ast::Expr>) -> P<ast::Expr> {
    AstBuilder::new().span(expr.span).expr().try().build(expr)
}

/// Compile `yield_!($expr)` into:
///
/// ```
/// 'before: {
///     ...
///     let () = suspend!(Some($expr));
///     goto!('after);
/// }
///
/// 'after: {
///     ...
/// }
/// ```
fn desugar_yield(cx: &ExtCtxt, expr: P<ast::Expr>) -> P<ast::Expr> {
    quote_expr!(cx, suspend!(Some($expr)))
}

/// Compile `$result = await!($expr)` into:
///
/// ```
/// 'before: {
///     ...
///     let future = $expr;
///     goto!('await_loop);
/// }
///
/// let result;
///
/// loop {
///     match future.poll() {
///         Ok(Async::NotReady) => {
///             let () = suspend!(Ok(Async::NotReady));
///             goto!('await_loop);
///         }
///         Ok(Async::Ready(result)) => {
///             $result = Ok(result);
///             goto!('await_exit);
///         }
///         Err(err) => {
///             $result = Err(err);
///             goto!('await_exit);
///         }
///     }
/// }
///
/// 'await_exit: {
///     ...
/// }
/// ```
fn desugar_await(cx: &ExtCtxt, future_expr: P<ast::Expr>) -> P<ast::Expr> {
    quote_expr!(cx,
        {
            let mut await_result = ::std::option::Option::None;

            loop {
                match ::futures::Future::poll(&mut $future_expr) {
                    ::std::result::Result::Ok(::futures::Async::NotReady) => {
                        suspend!(::futures::Async::NotReady);
                    }
                    ::std::result::Result::Ok(::futures::Async::Ready(ok)) => {
                        await_result = ::std::option::Option::Some(
                            ::std::result::Result::Ok(moved!(ok)));

                        break;
                    }
                    ::std::result::Result::Err(err) => {
                        await_result = ::std::option::Option::Some(
                            ::std::result::Result::Err(moved!(err)));

                        break;
                    }
                }
            }

            moved!(await_result).unwrap()
        }
    )
}
