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
use syntax::visit;
use syntax::ptr::P;

use rustc_plugin::Registry;

use petgraph::EdgeDirection;
use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::Dfs;

//////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum Node {
    BasicBlock(BasicBlock),
    Entry,
    Exit,
}

#[derive(Debug)]
enum Edge {
    Goto(Vec<ast::Ident>),
    Yield(P<ast::Expr>, Vec<ast::Ident>),
}

#[derive(Debug)]
struct BasicBlock {
    decl_idents: Vec<ast::Ident>,
    stmts: Vec<P<ast::Stmt>>,
    expr: Option<P<ast::Expr>>,
}

impl BasicBlock {
    fn new() -> Self {
        BasicBlock {
            decl_idents: Vec::new(),
            stmts: Vec::new(),
            expr: None,
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

struct CFGBuilder {
    graph: Graph<Node, Edge>,
}

impl CFGBuilder {
    fn new() -> Self {
        CFGBuilder {
            graph: Graph::new(),
        }
    }

    fn build(mut self, block: &ast::Block) -> CFG {
        let entry = self.graph.add_node(Node::Entry);
        let exit = self.graph.add_node(Node::Exit);

        let pred = self.block(block, entry);
        self.graph.add_edge(pred, exit, Edge::Goto(vec![]));

        CFG {
            graph: self.graph,
            entry: entry,
            exit: exit,
        }
    }

    fn block(&mut self, block: &ast::Block, mut pred: NodeIndex) -> NodeIndex {
        let nx = self.add_node();
        self.graph.add_edge(pred, nx, Edge::Goto(vec![]));
        pred = nx;

        for stmt in block.stmts.iter() {
            pred = self.stmt(stmt, pred);
        }

        pred
    }

    fn stmt(&mut self, stmt: &P<ast::Stmt>, pred: NodeIndex) -> NodeIndex {
        if let Some(edge) = self.get_edge(stmt, pred) {
            let nx = self.add_node();
            self.graph.add_edge(pred, nx, edge);
            return nx;
        }

        let mut decl_idents = None;

        match stmt.node {
            ast::Stmt_::StmtDecl(ref decl, _) => {
                match decl.node {
                    ast::Decl_::DeclLocal(ref local) => {
                        decl_idents = Some(self.find_decl_idents(&local.pat));
                    }
                    _ => {}
                }
            }
            _ => {}
        };

        let mut bb = self.get_node_mut(pred);
        bb.stmts.push(stmt.clone());

        match decl_idents {
            Some(decl_idents) => bb.decl_idents.extend(decl_idents),
            None => {}
        }

        pred
    }

    fn add_node(&mut self) -> NodeIndex {
        self.graph.add_node(Node::BasicBlock(BasicBlock::new()))
    }

    fn get_node(&self, index: NodeIndex) -> &BasicBlock {
        match self.graph.node_weight(index) {
            Some(node) => {
                match *node {
                    Node::BasicBlock(ref bb) => bb,
                    _ => {
                        panic!("node is not a basic block")
                    }
                }
            }
            None => {
                panic!("missing node!")
            }
        }
    }

    fn get_node_mut(&mut self, index: NodeIndex) -> &mut BasicBlock {
        match self.graph.node_weight_mut(index) {
            Some(node) => {
                match *node {
                    Node::BasicBlock(ref mut bb) => bb,
                    _ => {
                        panic!("node is not a basic block")
                    }
                }
            }
            None => {
                panic!("missing node!")
            }
        }
    }

    fn get_edge(&self, stmt: &ast::Stmt, pred: NodeIndex) -> Option<Edge> {
        struct Visitor<'a> {
            builder: &'a CFGBuilder,
            pred: NodeIndex,
            edge: Option<Edge>,
        }

        impl<'a> visit::Visitor<'a> for Visitor<'a> {
            fn visit_expr(&mut self, expr: &'a ast::Expr) {
                match expr.node {
                    ast::Expr_::ExprRet(Some(ref e)) => {
                        assert!(self.edge.is_none());

                        let bb = self.builder.get_node(self.pred);

                        self.edge = Some(Edge::Yield(e.clone(), bb.decl_idents.clone()));
                    }
                    ast::Expr_::ExprRet(None) => {
                        panic!();
                    }
                    _ => { }
                }
                visit::walk_expr(self, expr)
            }
        }

        let mut visitor = Visitor {
            builder: self,
            pred: pred,
            edge: None,
        };

        visit::Visitor::visit_stmt(&mut visitor, stmt);
        visitor.edge
    }

    fn find_decl_idents(&self, pat: &ast::Pat) -> Vec<ast::Ident> {
        struct Visitor(Vec<ast::Ident>);

        impl<'a> visit::Visitor<'a> for Visitor {
            fn visit_ident(&mut self, _span: Span, ident: ast::Ident) {
                self.0.push(ident);
            }
        }

        let mut visitor = Visitor(Vec::new());
        visit::Visitor::visit_pat(&mut visitor, pat);
        visitor.0
    }
}

//////////////////////////////////////////////////////////////////////////////

struct CFG {
    graph: Graph<Node, Edge>,
    entry: NodeIndex,
    exit: NodeIndex,
}

impl CFG {
    fn walk<F>(&self, mut f: F)
        where F: FnMut(NodeIndex, &Node, Option<(NodeIndex, &Edge)>)
    {
        let mut dfs = Dfs::new(&self.graph, self.entry);

        let mut node_nx = self.entry;

        loop {
            let node = self.graph.node_weight(node_nx).unwrap();

            let mut edges = self.graph
                .edges_directed(node_nx, EdgeDirection::Outgoing);

            let exit = match edges.next() {
                Some(exit) => {
                    assert!(edges.next().is_none());
                    Some(exit)
                }
                None => None
            };

            f(node_nx, node, exit);

            if let Some(nx) = dfs.next(&self.graph) {
                node_nx = nx;
            } else {
                break;
            }
        }
    }
}

//////////////////////////////////////////////////////////////////////////////

fn make_state_map(cx: &ExtCtxt, cfg: &CFG) -> BTreeMap<NodeIndex, P<ast::Block>> {
    let mut state_map = BTreeMap::new();

    cfg.walk(|nx, node, exit| {
        if state_map.contains_key(&nx) {
            return;
        }

        let mut stmts = vec![];
        let mut expr = None;

        match *node {
            Node::BasicBlock(ref bb) => {
                stmts.extend(bb.stmts.iter().cloned());
                expr = bb.expr.clone();
            }
            Node::Entry => {}
            Node::Exit => {
                stmts.push(quote_stmt!(cx,
                    return (
                        ::std::option::Option::None,
                        State::Exit,
                    );
                ).unwrap());
            }
        }

        let transition = exit.map(|(exit_nx, edge)| {
            make_transition_stmt(cx, exit_nx, edge)
        });

        assert!(expr.is_none());

        let block = quote_block!(cx, {
            $stmts
            $transition
        });

        state_map.insert(nx, block);
    });

    state_map
}

//////////////////////////////////////////////////////////////////////////////

fn make_state_id(id: NodeIndex) -> ast::Ident {
    let builder = aster::AstBuilder::new();
    builder.id(format!("State{}", id.index()))
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

fn make_state_expr(cx: &ExtCtxt,
                   state: NodeIndex,
                   args: Vec<ast::Ident>) -> P<ast::Expr> {
    let id = make_state_id(state);

    if args.is_empty() {
        quote_expr!(cx, State::$id)
    } else {
        quote_expr!(cx, State::$id($args))
    }
}

fn make_continue_to(cx: &ExtCtxt,
                    next_state: NodeIndex,
                    args: Vec<ast::Ident>) -> P<ast::Stmt> {
    let next_state = make_state_expr(cx, next_state, args);

    quote_stmt!(cx, {
        state = $next_state;
        continue;
    }).unwrap()
}

fn make_return_and_goto(cx: &ExtCtxt,
                        data: P<ast::Expr>,
                        next_state: NodeIndex,
                        args: Vec<ast::Ident>) -> P<ast::Stmt> {
    let next_state = make_state_expr(cx, next_state, args);

    quote_stmt!(cx,
        return (
            ::std::option::Option::Some($data),
            $next_state,
        );
    ).unwrap()
}

fn make_transition_stmt(cx: &ExtCtxt,
                        dst: NodeIndex,
                        edge: &Edge) -> P<ast::Stmt> {
    match *edge {
        Edge::Goto(ref idents) => {
            make_continue_to(cx, dst, idents.clone())
        }
        Edge::Yield(ref expr, ref idents) => {
            make_return_and_goto(cx, expr.clone(), dst, idents.clone())
        }
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

    let cfg_builder = CFGBuilder::new();
    let cfg = cfg_builder.build(block);

    let state_map = make_state_map(cx, &cfg);
    let exit_block = state_map.get(&cfg.exit).unwrap();

    let mut state_variants = Vec::with_capacity(state_map.len());
    let mut state_arms = Vec::with_capacity(state_map.len());

    for (nx, block) in state_map.iter()
            .map(|(nx, block)| (*nx, block))
            .filter(|&(nx, _)| nx.index() != cfg.exit.index())
            .chain(Some((cfg.exit, exit_block))) {
        let id = make_state_id(nx);
        state_variants.push(id);
        state_arms.push(quote_arm!(cx, State::$id => $block));
    }

    let state_enum = builder.item().enum_("State")
        .ids(state_variants.iter())
        .build();

    let item = quote_item!(cx,
        fn $ident() -> ::std::boxed::Box<::std::iter::Iterator<Item=$ret_ty>> {
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

            impl Default for State {
                fn default() -> Self {
                    State::Exit
                }
            }

            Box::new(Wrapper::new(
                State::Exit,
                |mut state| {
                    loop {
                        match state {
                            $state_arms
                        }
                    }
                }
            ))
        }
    ).unwrap();

    Annotatable::Item(item)
}

#[plugin_registrar]
#[doc(hidden)]
pub fn plugin_registrar(registry: &mut rustc_plugin::Registry) {
    let builder = aster::AstBuilder::new();

    registry.register_syntax_extension(builder.name("state_machine"),
                                       MultiModifier(Box::new(expand_state_machine)));
}
