use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::build::AstBuilder;
use syntax::visit;
use syntax::ptr::P;

use petgraph::EdgeDirection;
use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::{Reversed, DfsIter};

//////////////////////////////////////////////////////////////////////////////

pub struct CFGBuilder {
    graph: Graph<Node, Edge>,
}

impl CFGBuilder {
    pub fn new() -> Self {
        CFGBuilder {
            graph: Graph::new(),
        }
    }

    pub fn build(mut self, block: &ast::Block) -> CFG {
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

        let decl_idents = match stmt.node {
            ast::Stmt_::StmtDecl(ref decl, _) => {
                match decl.node {
                    ast::Decl_::DeclLocal(ref local) => self.find_decl_idents(&local.pat),
                    _ => vec![],
                }
            }
            _ => vec![],
        };
        let live_idents = self.find_live_idents(stmt);

        let mut bb = self.get_node_mut(pred);
        bb.decl_idents.extend(decl_idents);
        bb.live_idents.extend(live_idents);
        bb.stmts.push(stmt.clone());

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

    fn find_live_idents(&self, stmt: &ast::Stmt) -> Vec<ast::Ident> {
        struct Visitor(Vec<ast::Ident>);

        impl<'a> visit::Visitor<'a> for Visitor {
            fn visit_expr(&mut self, expr: &ast::Expr) {
                match expr.node {
                    ast::ExprPath(_, ref path) => {
                        if path.segments.len() == 1 {
                            self.0.push(path.segments[0].identifier);
                        }
                    }
                    _ => {}
                }
                visit::walk_expr(self, expr);
            }
        }

        let mut visitor = Visitor(Vec::new());
        visit::Visitor::visit_stmt(&mut visitor, stmt);
        visitor.0
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct CFG {
    pub graph: Graph<Node, Edge>,
    pub entry: NodeIndex,
    pub exit: NodeIndex,
}

impl CFG {
    pub fn get_node(&self, nx: NodeIndex) -> &Node {
        &self.graph[nx]
    }

    pub fn get_node_decls(&self, nx: NodeIndex) -> Vec<ast::Ident> {
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

    pub fn get_parent_decls(&self, nx: NodeIndex) -> Vec<ast::Ident> {
        // Exit node doesn't need any state.
        if let Node::Exit = self.graph[nx] {
            return Vec::new();
        }

        match self.get_parent(nx) {
            Some(parent_nx) => self.get_node_decls(parent_nx),
            None => Vec::new(),
        }
    }

    pub fn get_edge(&self, nx: NodeIndex, direction: EdgeDirection) -> Option<(NodeIndex, &Edge)> {
        let mut edges = self.graph.edges_directed(nx, direction);

        match edges.next() {
            Some(edge) => {
                assert!(edges.next().is_none());
                Some(edge)
            }
            None => None,
        }
    }

    pub fn get_child_edge(&self, nx: NodeIndex) -> Option<(NodeIndex, &Edge)> {
        self.get_edge(nx, EdgeDirection::Outgoing)
    }

    pub fn get_parent_edge(&self, nx: NodeIndex) -> Option<(NodeIndex, &Edge)> {
        self.get_edge(nx, EdgeDirection::Incoming)
    }

    pub fn get_parent(&self, nx: NodeIndex) -> Option<NodeIndex> {
        self.get_parent_edge(nx).map(|(nx, _)| nx)
    }

    /*
    fn get_parent_node(&self, nx: NodeIndex) -> Option<&Node> {
        self.get_parent_edge(nx).map(|(nx, _)| self.get_node(nx))
    }
    */
}

//////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub enum Node {
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
pub enum Edge {
    Goto(Vec<ast::Ident>),
    Yield(P<ast::Expr>, Vec<ast::Ident>),
}

#[derive(Debug)]
pub struct BasicBlock {
    decl_idents: Vec<ast::Ident>,
    live_idents: Vec<ast::Ident>,
    pub stmts: Vec<P<ast::Stmt>>,
    expr: Option<P<ast::Expr>>,
}

impl BasicBlock {
    fn new() -> Self {
        BasicBlock {
            decl_idents: Vec::new(),
            live_idents: Vec::new(),
            stmts: Vec::new(),
            expr: None,
        }
    }
}
