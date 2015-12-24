#![feature(plugin_registrar, rustc_private, quote)]

extern crate aster;
extern crate petgraph;
extern crate rustc_plugin;
extern crate syntax;

use std::collections::BTreeMap;

use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::{
    Annotatable,
    ExtCtxt,
    MultiModifier,
};
use syntax::ext::build::AstBuilder;
use syntax::ptr::P;

use petgraph::EdgeDirection;
use petgraph::graph::NodeIndex;
use petgraph::visit::DfsIter;

//////////////////////////////////////////////////////////////////////////////

mod cfg;

fn make_state_variant(cfg: &cfg::CFG,
                      nx: NodeIndex,
                      state_variables: usize) -> (P<ast::Variant>, Vec<ast::Ident>) {
    let ast_builder = aster::AstBuilder::new();

    let decl_idents = cfg.get_node_decl_idents(nx);

    let state_id = make_state_id(cfg, nx);

    let variant_ty_idents = (0 .. decl_idents.len())
        .map(|id| {
            ast_builder.id(format!("T{}", state_variables + id))
        })
        .collect::<Vec<_>>();

    let variant = {
        let mut variant_tys = variant_ty_idents.iter()
            .map(|id| ast_builder.ty().id(id));

        match variant_tys.next() {
            Some(ty) => {
                ast_builder.variant(state_id).tuple()
                    .build_ty(ty)
                    .with_fields(
                        variant_tys
                            .map(|ty| {
                                ast_builder.tuple_field().ty().build(ty)
                            })
                    )
                    .build()
            }
            None => {
                ast_builder.variant(state_id).unit()
            }
        }
    };

    (variant, variant_ty_idents)
}

fn make_state_pat(cfg: &cfg::CFG, nx: NodeIndex) -> P<ast::Pat> {
    let ast_builder = aster::AstBuilder::new();

    let state_id_path = make_state_id_path(cfg, nx);
    let decl_idents = cfg.get_node_decl_idents(nx);

    ast_builder.pat().enum_().build(state_id_path)
        .with_pats(
            decl_idents.iter()
                .map(|&(mutability, ident)| {
                    let mode = ast::BindingMode::ByValue(mutability);
                    ast_builder.pat().build_id(mode, ident, None)
                })
        )
        .build()
}

fn make_state_map(cx: &ExtCtxt, cfg: &cfg::CFG) -> BTreeMap<NodeIndex, P<ast::Block>> {
    let mut state_map = BTreeMap::new();

    for root_nx in cfg.graph.externals(EdgeDirection::Incoming) {
        for nx in DfsIter::new(&cfg.graph, root_nx) {
            let node = cfg.get_node(nx);

            for _ in cfg.get_child_edges(nx) {
                let block = match *node {
                    cfg::Node::BasicBlock(ref bb) => {
                        let stmts = bb.stmts.iter()
                            .flat_map(|stmt| make_stmt(cx, cfg, stmt))
                            .collect::<Vec<_>>();

                        quote_block!(cx, {
                            $stmts
                        })
                    }
                    cfg::Node::Exit => {
                        let next_state = make_state_expr(cfg, nx);

                        quote_block!(cx, {
                            return (
                                ::std::option::Option::None,
                                $next_state,
                            );
                        })
                    }
                };

                state_map.insert(nx, block);
            }
        }
    }

    state_map
}


fn make_exit_block(cx: &ExtCtxt, cfg: &cfg::CFG) -> P<ast::Block> {
    let next_state = make_state_expr(cfg, cfg.exit);

    quote_block!(cx, {
        return (
            ::std::option::Option::None,
            $next_state,
        );
    })
}


fn make_state_enum_and_arms(cx: &ExtCtxt,
                            cfg: &cfg::CFG) -> (P<ast::Item>, P<ast::Item>, Vec<ast::Arm>) {
    let ast_builder = aster::AstBuilder::new();

    let state_map = make_state_map(cx, &cfg);
    let exit_block = make_exit_block(cx, cfg);

    let mut state_variables = Vec::new();;
    let mut state_variants = Vec::new();
    let mut state_arms = Vec::new();

    for (nx, block) in state_map.iter()
            .map(|(nx, block)| (*nx, block))
            .filter(|&(nx, _)| nx.index() != cfg.exit.index())
            .chain(Some((cfg.exit, &exit_block))) {
        let (variant, variables) = make_state_variant(cfg, nx, state_variables.len());
        state_variables.extend(variables);

        state_variants.push(variant);

        let pat = make_state_pat(cfg, nx);

        state_arms.push(quote_arm!(cx, $pat => $block));
    }

    let state_generics = ast_builder.generics()
        .with_ty_param_ids(state_variables.iter())
        .build();

    let state_enum = ast_builder.item().enum_("State")
        .generics().with(state_generics.clone()).build()
        .with_variants(state_variants)
        .build();

    let state_path = ast_builder.path()
        .segment("State")
            .with_tys(
                state_variables.iter()
                    .map(|variable| ast_builder.ty().id(variable))
            )
            .build()
        .build();

    let exit_expr = make_state_expr(&cfg, cfg.exit);

    let state_default = quote_item!(cx,
        impl $state_generics Default for $state_path {
            fn default() -> Self {
                $exit_expr
            }
        }
    ).expect("state default item");

    (state_enum, state_default, state_arms)
}

//////////////////////////////////////////////////////////////////////////////

fn make_state_id(cfg: &cfg::CFG, nx: NodeIndex) -> ast::Ident {
    let ast_builder = aster::AstBuilder::new();

    let name = cfg.get_node(nx).name();
    ast_builder.id(format!("State{}{}", nx.index(), name))
}

fn make_state_id_path(cfg: &cfg::CFG, nx: NodeIndex) -> ast::Path {
    let ast_builder = aster::AstBuilder::new();

    ast_builder.path()
        .id("State")
        .id(make_state_id(cfg, nx))
        .build()
}

/*
fn make_return(cx: &ExtCtxt, expr: P<ast::Expr>) -> P<ast::Stmt> {
    quote_stmt!(cx,
        return (
            ::std::option::Option::Some($expr),
            State::Exit,
        );
    ).unwrap()
}
*/

fn make_state_expr(cfg: &cfg::CFG, nx: NodeIndex) -> P<ast::Expr> {
    let ast_builder = aster::AstBuilder::new();

    let state_id_path = make_state_id_path(cfg, nx);
    let decl_idents = cfg.get_node_decl_idents(nx);

    if decl_idents.is_empty() {
        ast_builder.expr().path().build(state_id_path)
    } else {
        ast_builder.expr().call().path().build(state_id_path)
            .with_args(
                decl_idents.iter()
                    .map(|&(_, ident)| ast_builder.expr().id(ident))
            )
            .build()
    }
}

fn make_goto(cfg: &cfg::CFG,
             next_state: NodeIndex) -> Vec<P<ast::Stmt>> {
    let builder = aster::AstBuilder::new();

    let next_state = make_state_expr(cfg, next_state);

    vec![
        builder.stmt().expr().assign()
            .id("state")
            .build(next_state),
        builder.stmt().expr().continue_(),
    ]
}

fn make_yield(cx: &ExtCtxt,
              cfg: &cfg::CFG,
              data: &P<ast::Expr>,
              next_state: NodeIndex) -> Vec<P<ast::Stmt>> {
    let next_state = make_state_expr(cfg, next_state);

    vec![
        quote_stmt!(cx,
            return (
                ::std::option::Option::Some($data),
                $next_state,
            );
        ).expect("yield stmt"),
    ]
}

fn make_if(cfg: &cfg::CFG,
           expr: &P<ast::Expr>,
           then: NodeIndex,
           else_: NodeIndex) -> Vec<P<ast::Stmt>> {
    let builder = aster::AstBuilder::new();

    let then = make_goto(cfg, then);
    let then = builder.block()
        .with_stmts(then)
        .build();

    let else_ = make_goto(cfg, else_);
    let else_ = builder.block()
        .with_stmts(else_)
        .build();

    vec![
        builder.stmt().expr().if_()
            .build(expr.clone())
            .build_then(then)
            .build_else(else_),
    ]
}

fn make_stmt(cx: &ExtCtxt,
             cfg: &cfg::CFG,
             stmt: &cfg::Stmt) -> Vec<P<ast::Stmt>> {
    match *stmt {
        cfg::Stmt::Stmt(ref stmt) => vec![stmt.clone()],
        cfg::Stmt::Goto(nx) => make_goto(cfg, nx),
        cfg::Stmt::Yield(nx, ref expr) => make_yield(cx, cfg, expr, nx),
        cfg::Stmt::If(ref expr, then, else_) => make_if(cfg, expr, then, else_),
    }
}

//////////////////////////////////////////////////////////////////////////////

fn expand_state_machine(cx: &mut ExtCtxt,
                        _sp: Span,
                        meta_item: &ast::MetaItem,
                        annotatable: Annotatable) -> Annotatable {
    let builder = aster::AstBuilder::new();

    let item = match annotatable {
        Annotatable::Item(item) => item,
        _ => {
            cx.span_err(
                meta_item.span,
                "`state_machine` may only be applied to functions");

            return annotatable;
        }
    };

    let ident = item.ident;

    let (fn_decl, block) = match item.node {
        ast::ItemFn(ref fn_decl, _, _, _, _, ref block) => (fn_decl, block),
        _ => {
            cx.span_err(
                meta_item.span,
                "`state_machine` may only be applied to functions");

            return Annotatable::Item(item.clone());
        }
    };

    let ret_ty = match fn_decl.output {
        ast::FunctionRetTy::NoReturn(..) => {
            cx.span_err(
                meta_item.span,
                "`state_machine` cannot return `!` types");

            return Annotatable::Item(item.clone());
        }
        ast::FunctionRetTy::DefaultReturn(span) => builder.ty().span(span).unit(),
        ast::FunctionRetTy::Return(ref ty) => ty.clone(),
    };

    let cfg_builder = cfg::CFGBuilder::new(cx);
    let cfg = cfg_builder.build(fn_decl, block);

    let (state_enum, state_default, state_arms) = make_state_enum_and_arms(cx, &cfg);

    let entry_expr = make_state_expr(&cfg, cfg.entry);

    let block = quote_block!(cx, {
        struct Wrapper<S, F> {
            state: S,
            next: F,
        }

        impl<S, T, F> Wrapper<S, F>
            where F: Fn(S) -> (Option<T>, S),
        {
            fn new(initial_state: S, next: F) -> Self {
                Wrapper {
                    state: initial_state,
                    next: next,
                }
            }
        }

        impl<S, T, F> Iterator for Wrapper<S, F>
            where S: Default,
                  F: Fn(S) -> (Option<T>, S)
        {
            type Item = T;

            fn next(&mut self) -> Option<Self::Item> {
                let old_state = ::std::mem::replace(&mut self.state, S::default());
                let (value, next_state) = (self.next)(old_state);
                self.state = next_state;
                value
            }
        }

        $state_enum
        $state_default

        Box::new(Wrapper::new(
            $entry_expr,
            |mut state| {
                loop {
                    match state {
                        $state_arms
                    }
                }
            }
        ))
    });

    let item = builder.item().fn_(ident)
        .with_args(fn_decl.inputs.iter().cloned())
        .build_return(
            quote_ty!(cx, ::std::boxed::Box<::std::iter::Iterator<Item=$ret_ty>>)
        )
        .build(block);

    Annotatable::Item(item)
}

#[plugin_registrar]
#[doc(hidden)]
pub fn plugin_registrar(registry: &mut rustc_plugin::Registry) {
    let builder = aster::AstBuilder::new();

    registry.register_syntax_extension(builder.name("state_machine"),
                                       MultiModifier(Box::new(expand_state_machine)));
}
