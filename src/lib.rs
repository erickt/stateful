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
use syntax::visit::{self, Visitor};
use syntax::ptr::P;

use rustc_plugin::Registry;

use petgraph::EdgeDirection;
use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::Dfs;

fn contains_yield(stmt: &ast::Stmt) -> bool {
    struct ContainsYieldVisitor(bool);

    impl<'a> Visitor<'a> for ContainsYieldVisitor {
        fn visit_expr(&mut self, expr: &'a ast::Expr) {
            match expr.node {
                ast::Expr_::ExprRet(_) => { self.0 = true; }
                _ => { }
            }
            visit::walk_expr(self, expr)
        }
    }

    let mut visitor = ContainsYieldVisitor(false);
    visitor.visit_stmt(stmt);
    visitor.0
}

#[derive(Debug)]
enum Node {
    BasicBlock(BasicBlock),
    Entry,
    Exit,
}

#[derive(Debug)]
enum Edge {
    Yield,
    Goto,
}

#[derive(Debug)]
struct BasicBlock {
    stmts: Vec<P<ast::Stmt>>,
    expr: Option<P<ast::Expr>>,
}

impl BasicBlock {
    fn new() -> Self {
        BasicBlock {
            stmts: Vec::new(),
            expr: None,
        }
    }
}

struct CFGBuilder<'a> {
    cx: &'a ExtCtxt<'a>,
    graph: Graph<Node, Edge>,
}

impl<'a> CFGBuilder<'a> {
    fn new(cx: &'a ExtCtxt) -> Self {
        let mut graph = Graph::new();
        CFGBuilder {
            cx: cx,
            graph: graph,
        }
    }

    fn build(mut self, block: &ast::Block) -> CFG {
        let entry = self.graph.add_node(Node::Entry);

        let pred = self.block(block, entry);

        let exit = self.graph.add_node(Node::Exit);
        self.graph.add_edge(pred, exit, Edge::Goto);

        CFG {
            graph: self.graph,
            entry: entry,
            exit: exit,
        }
    }

    fn block(&mut self, block: &ast::Block, pred: NodeIndex) -> NodeIndex {
        let mut nx = self.add_node();
        self.graph.add_edge(pred, nx, Edge::Goto);

        let mut pred = nx;

        for stmt in block.stmts.iter() {
            {
                let mut bb = self.get_node(pred);
                bb.stmts.push(stmt.clone());
            }

            if contains_yield(stmt) {
                let nx = self.add_node();
                self.graph.add_edge(pred, nx, Edge::Yield);
                pred = nx;
            }
        }

        pred

        /*
        let mut stmts_exit = bb_index;
        for stmt in &block.stmts {
            stmts_exit = self.stmt(stmt, stmts_exit);
        }

        match block.expr {
            Some(ref expr) => self.expr(expr, stmts_exit),
            None => stmts_exit,
        }
        */
    }

    /*
    fn stmt(&mut self, stmt: &P<ast::Stmt>, pred: NodeIndex) -> NodeIndex {
        match stmt.node {
            ast::Stmt_::StmtSemi(ref expr, _) => {
                match self.expr(expr, pred) {
                    Some(exit) => exit,
                    None => {
                        self.add_stmt(pred, stmt);
                        pred
                    }
                }
            }
            _ => {
                self.add_stmt(pred, stmt);
                pred
            }
        }
    }

    fn expr(&mut self, expr: &P<ast::Expr>, pred: NodeIndex) -> Option<NodeIndex> {
        let exit = match expr.node {
            /*
            ast::Expr_::ExprBlock(ref block) => {
                self.block(block, pred)
            }
            */
            ast::Expr_::ExprRet(ref expr) => {
                let nx = add_node();
                let node = get_node(index);
                node.expr = match *expr {
                    Some(ref expr) => Some(expr.clone()),
                    None => Some(builder.expr().unit()),
                };

                Some(nx)
            }
            _ => {
                None
            }
        }
    }
    */

    fn add_node(&mut self) -> NodeIndex {
        self.graph.add_node(Node::BasicBlock(BasicBlock::new()))
    }

    fn get_node(&mut self, index: NodeIndex) -> &mut BasicBlock {
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

    /*
    fn add_stmt(&mut self, index: NodeIndex, stmt: &P<ast::Stmt>) {
        let node = self.get_node(index);
        node.stmts.push(stmt.clone());
    }
    */
}

struct CFG {
    graph: Graph<Node, Edge>,
    entry: NodeIndex,
    exit: NodeIndex,
}

impl CFG {
    fn walk<F>(&self, mut f: F)
        where F: FnMut(NodeIndex, &Node, Option<NodeIndex>)
    {
        let mut dfs = Dfs::new(&self.graph, self.entry);

        let mut node_nx = self.entry;

        loop {
            let node = self.graph.node_weight(node_nx).unwrap();

            let mut outgoing = self.graph
                .neighbors_directed(node_nx, EdgeDirection::Outgoing);

            let exit_nx = match outgoing.next() {
                Some(exit_nx) => {
                    assert!(outgoing.next().is_none());
                    Some(exit_nx)
                }
                None => None
            };

            f(node_nx, node, exit_nx);

            if let Some(nx) = dfs.next(&self.graph) {
                node_nx = nx;
            } else {
                break;
            }
        }
    }
}

fn return_(cx: &ExtCtxt) -> P<ast::Stmt> {
    quote_stmt!(cx,
        return (
            ::std::option::Option::None,
            State::Exit,
        );
    ).unwrap()
}

fn state_expr(cx: &ExtCtxt,
              state: NodeIndex,
              args: Vec<P<ast::Expr>>) -> P<ast::Expr> {
    let id = state_id(state);

    if args.is_empty() {
        quote_expr!(cx, State::$id)
    } else {
        quote_expr!(cx, State::$id($args))
    }
}

fn continue_to(cx: &ExtCtxt,
               next_state: NodeIndex,
               args: Vec<P<ast::Expr>>) -> P<ast::Stmt> {
    let next_state = state_expr(cx, next_state, args);

    quote_stmt!(cx, {
        state = $next_state;
        continue;
    }).unwrap()
}

fn _return_and_goto(cx: &ExtCtxt,
                   data: P<ast::Expr>,
                   next_state: NodeIndex,
                   args: Vec<P<ast::Expr>>) -> P<ast::Stmt> {
    let next_state = state_expr(cx, next_state, args);

    quote_stmt!(cx,
        return (
            ::std::option::Option::Some($data),
            $next_state,
        );
    ).unwrap()
}

fn state_id(id: NodeIndex) -> ast::Ident {
    let builder = aster::AstBuilder::new();
    builder.id(format!("State{}", id.index()))
}


/*
fn visit_stmt(graph: &mut Graph<BasicBlock, ()>,
       parent_index: NodeIndex,
       stmt: P<ast::Stmt>) -> NodeIndex {
    let builder = aster::AstBuilder::new();

    match stmt.node {
        ast::Stmt_::StmtExpr(ref expr, _) | ast::Stmt_::StmtSemi(ref expr, _) => {
            match expr.node {
                ast::Expr_::ExprRet(ref expr) => {
                    let node = BasicBlock::new();
                    let nx = graph.add_node(node);
                    graph.add_edge(parent_index, nx, ());


                    let parent = graph.node_weight_mut(parent_index).unwrap();
                    parent.expr = match *expr {
                        Some(ref expr) => Some(expr.clone()),
                        None => Some(builder.expr().unit()),
                    };

                    return nx;
                }
                _ => { }
            }
        }
        _ => { }
    }

    let parent = graph.node_weight_mut(parent_index).unwrap();
    parent.stmts.push(stmt.clone());
    parent_index
}

fn visit_stmts(cx: &ExtCtxt,
               block: &ast::Block) -> Graph<BasicBlock, ()>, NodeIndex) {
    let mut graph = Graph::new();

    let entry = BasicBlock::new();
    let entry_index = graph.add_node(entry);

    let mut exit = BasicBlock::new();


    let mut index = entry_index;

    for stmt in block.stmts.iter() {
        index = visit_stmt(graph, entry_index, stmt.clone());
    }

    match block.expr {
        Some(ref expr) => {
            let bb = BasicBlock::new();
            exit.expr = Some(expr.clone());
        }
        None => { }
    }

    (graph, entry_index)
}
*/


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

    let cfg_builder = CFGBuilder::new(cx);
    let cfg = cfg_builder.build(block);

    let state_blocks = vec![
        quote_block!(cx, {
            return (::std::option::Option::None::<$ret_ty>, State::Exit)
        }),
    ];

    let mut state_map = BTreeMap::new();

    cfg.walk(|nx, node, exit_nx| {
        println!("visit: {:?} {:?}", nx, node);

        if !state_map.contains_key(&nx) {
            let block = match *node {
                Node::BasicBlock(ref bb) => {
                    let stmts = &bb.stmts;
                    let expr = &bb.expr;

                    let transition_stmt = exit_nx.map(|exit_nx| {
                        continue_to(cx, exit_nx, vec![])
                    });

                    quote_block!(cx, {
                        $stmts
                        $transition_stmt
                        $expr
                    })
                }
                Node::Entry => {
                    let exit_nx = exit_nx.unwrap();
                    let transition_stmt = continue_to(cx, exit_nx, vec![]);

                    quote_block!(cx, {
                        "entry";
                        $transition_stmt
                    })
                }
                Node::Exit => { 
                    assert!(exit_nx.is_none());

                    let transition_stmt = return_(cx);
                    quote_block!(cx, {
                        "exit";
                        $transition_stmt
                    })
                }
            };

            state_map.insert(nx, block);
        }
    });





    /*
    let mut current_stmts = vec![];

    for stmt in block.stmts.iter().rev() {
        match stmt.node {
            ast::Stmt_::StmtExpr(ref e, _) | ast::Stmt_::StmtSemi(ref e, _) => {
                match e.node {
                    ast::Expr_::ExprRet(ref e) => {
                        let src_node = state_blocks.len();
                        let dst_node = state_blocks.len() + 1;

                        let target_state = state_blocks.len();
                        let target_state_id = builder.id(format!("State{}", target_state));

                        let mut stmts = mem::replace(&mut current_stmts, vec![]);
                        stmts.reverse();

                        state_blocks.push(quote_block!(cx, {
                            $stmts
                            return (::std::option::Option::Some($e), State::$target_state_id);
                        }));
                    }
                    _ => {
                        current_stmts.push(stmt.clone());
                    }
                }
            }
            _ => {
                current_stmts.push(stmt.clone());
            }
        }
    }

    if !current_stmts.is_empty() {
        current_stmts.reverse();

        state_blocks.push(quote_block!(cx, {
            $current_stmts
        }));
    }
    */

    /*
    let state_ids = (0 .. state_blocks.len())
        .map(|i| {
            builder.id(format!("State{}", i))
        })
        .collect::<Vec<_>>();
        */

    let state_ids = state_map.iter()
        .map(|(nx, _)| state_id(*nx))
        .collect::<Vec<_>>();

    let state_enum = builder.item().enum_("State")
        .ids(state_ids.iter())
        .id("Exit")
        .build();

    let state_arms = state_map.iter()
        .map(|(nx, block)| {
            let id = state_id(*nx);
            quote_arm!(cx, State::$id => $block)
        })
        .collect::<Vec<_>>();

    /*
    let mut state_arms = state_ids.iter()
        .zip(state_blocks.iter().rev())
        .map(|(state, block)| quote_arm!(cx, State::$state => $block))
        .collect::<Vec<_>>();

    state_arms.push(quote_arm!(cx,
        State::Exit => {
            return (::std::option::Option::None::<$ret_ty>, State::Exit)
        }
    ));
    */

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

    //let expr = cx.expr_str(sp, InternedString::new("hello world"));
    //push(Annotatable(
}

#[plugin_registrar]
#[doc(hidden)]
pub fn plugin_registrar(registry: &mut rustc_plugin::Registry) {
    let builder = aster::AstBuilder::new();

    registry.register_syntax_extension(builder.name("state_machine"),
                                       MultiModifier(Box::new(expand_state_machine)));
}
