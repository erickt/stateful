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
use petgraph::visit::{Reversed, DfsIter};

//////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
enum Node {
    BasicBlock(BasicBlock),
    Exit,
}

impl Node {
    fn decls(&self) -> &[ast::Ident] {
        match *self {
            Node::BasicBlock(ref bb) => &bb.decl_idents[..],
            Node::Exit => &[]
        }
    }
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
        let entry = self.add_node();
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
    fn get_node(&self, nx: NodeIndex) -> &Node {
        &self.graph[nx]
    }

    fn get_node_decls(&self, nx: NodeIndex) -> Vec<ast::Ident> {
        let mut decls = Vec::new();

        // Exit node doesn't need any state.
        if let Node::Exit = self.graph[nx] {
            return decls;
        }

        for nx in DfsIter::new(&Reversed(&self.graph), nx) {
            decls.extend(self.graph[nx].decls());
        }

        decls.reverse();

        decls
    }

    fn get_parent_decls(&self, nx: NodeIndex) -> Vec<ast::Ident> {
        // Exit node doesn't need any state.
        if let Node::Exit = self.graph[nx] {
            return Vec::new();
        }

        match self.get_parent(nx) {
            Some(parent_nx) => self.get_node_decls(parent_nx),
            None => Vec::new(),
        }
    }

    fn get_edge(&self, nx: NodeIndex, direction: EdgeDirection) -> Option<(NodeIndex, &Edge)> {
        let mut edges = self.graph.edges_directed(nx, direction);

        match edges.next() {
            Some(edge) => {
                assert!(edges.next().is_none());
                Some(edge)
            }
            None => None,
        }
    }

    fn get_child_edge(&self, nx: NodeIndex) -> Option<(NodeIndex, &Edge)> {
        self.get_edge(nx, EdgeDirection::Outgoing)
    }

    fn get_parent_edge(&self, nx: NodeIndex) -> Option<(NodeIndex, &Edge)> {
        self.get_edge(nx, EdgeDirection::Incoming)
    }

    fn get_parent(&self, nx: NodeIndex) -> Option<NodeIndex> {
        self.get_parent_edge(nx).map(|(nx, _)| nx)
    }

    /*
    fn get_parent_node(&self, nx: NodeIndex) -> Option<&Node> {
        self.get_parent_edge(nx).map(|(nx, _)| self.get_node(nx))
    }
    */
}

//////////////////////////////////////////////////////////////////////////////

fn make_state_variant(cfg: &CFG,
                      nx: NodeIndex,
                      state_variables: usize) -> (P<ast::Variant>, Vec<ast::Ident>) {
    let ast_builder = aster::AstBuilder::new();

    let decl_idents = cfg.get_parent_decls(nx);

    let state_id = make_state_id(nx);

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

fn make_state_pat(cfg: &CFG, nx: NodeIndex) -> P<ast::Pat> {
    let ast_builder = aster::AstBuilder::new();

    let state_id_path = make_state_id_path(nx);
    let decl_idents = cfg.get_parent_decls(nx);

    ast_builder.pat().enum_().build(state_id_path)
        .with_ids(decl_idents)
        .build()
}

fn make_state_map(cx: &ExtCtxt, cfg: &CFG) -> BTreeMap<NodeIndex, P<ast::Block>> {
    let mut state_map = BTreeMap::new();

    for nx in DfsIter::new(&cfg.graph, cfg.entry) {
        let node = cfg.get_node(nx);
        let edge = cfg.get_child_edge(nx);
        
        let block = match *node {
            Node::BasicBlock(ref bb) => {
                let stmts = &bb.stmts;
                //let expr = &bb.expr;

                let transition = edge.map(|(edge_nx, edge)| {
                    make_transition_stmt(cx, cfg, edge_nx, edge)
                });

                quote_block!(cx, {
                    $stmts
                    $transition
                })
            }
            Node::Exit => {
                assert!(edge.is_none());

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

    state_map
}

fn make_state_enum_and_arms(cx: &ExtCtxt,
                            cfg: &CFG) -> (P<ast::Item>, P<ast::Item>, Vec<ast::Arm>) {
    let ast_builder = aster::AstBuilder::new();

    let state_map = make_state_map(cx, &cfg);
    let exit_block = state_map.get(&cfg.exit).unwrap();

    let mut state_variables = Vec::new();;
    let mut state_variants = Vec::new();
    let mut state_arms = Vec::new();

    for (nx, block) in state_map.iter()
            .map(|(nx, block)| (*nx, block))
            .filter(|&(nx, _)| nx.index() != cfg.exit.index())
            .chain(Some((cfg.exit, exit_block))) {
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
    ).unwrap();

    (state_enum, state_default, state_arms)
}

//////////////////////////////////////////////////////////////////////////////

fn make_state_id(nx: NodeIndex) -> ast::Ident {
    let ast_builder = aster::AstBuilder::new();
    ast_builder.id(format!("State{}", nx.index()))
}

fn make_state_id_path(nx: NodeIndex) -> ast::Path {
    let ast_builder = aster::AstBuilder::new();

    ast_builder.path()
        .id("State")
        .id(make_state_id(nx))
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

fn make_state_expr(cfg: &CFG, nx: NodeIndex) -> P<ast::Expr> {
    let ast_builder = aster::AstBuilder::new();


    let state_id_path = make_state_id_path(nx);
    let decl_idents = cfg.get_parent_decls(nx);

    if decl_idents.is_empty() {
        ast_builder.expr().path().build(state_id_path)
    } else {
        ast_builder.expr().call().path().build(state_id_path)
            .with_args(
                decl_idents.iter()
                    .map(|ident| ast_builder.expr().id(ident))
            )
            .build()
    }
}

fn make_continue_to(cx: &ExtCtxt, cfg: &CFG, next_state: NodeIndex) -> P<ast::Stmt> {
    let next_state = make_state_expr(cfg, next_state);

    quote_stmt!(cx, {
        state = $next_state;
        continue;
    }).unwrap()
}

fn make_return_and_goto(cx: &ExtCtxt,
                        cfg: &CFG,
                        data: P<ast::Expr>,
                        next_state: NodeIndex) -> P<ast::Stmt> {
    let next_state = make_state_expr(cfg, next_state);

    quote_stmt!(cx,
        return (
            ::std::option::Option::Some($data),
            $next_state,
        );
    ).unwrap()
}

fn make_transition_stmt(cx: &ExtCtxt,
                        cfg: &CFG,
                        dst: NodeIndex,
                        edge: &Edge) -> P<ast::Stmt> {
    match *edge {
        Edge::Goto(_) => {
            make_continue_to(cx, cfg, dst)
        }
        Edge::Yield(ref expr, _) => {
            make_return_and_goto(cx, cfg, expr.clone(), dst)
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

    let (state_enum, state_default, state_arms) = make_state_enum_and_arms(cx, &cfg);

    let entry_expr = make_state_expr(&cfg, cfg.entry);

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
