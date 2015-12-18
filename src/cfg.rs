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
    scope: Vec<usize>,
}

impl CFGBuilder {
    pub fn new() -> Self {
        CFGBuilder {
            graph: Graph::new(),
            scope: Vec::new(),
        }
    }

    pub fn build(mut self, block: &ast::Block) -> CFG {
        let entry = self.add_bb("Entry");
        let exit = self.graph.add_node(Node::Exit);

        let pred = self.block(block, entry);
        self.goto(pred, exit);

        CFG {
            graph: self.graph,
            entry: entry,
            exit: exit,
        }
    }

    fn block(&mut self, block: &ast::Block, mut pred: NodeIndex) -> NodeIndex {
        self.push_scope();

        // We need to determine what declarations are in scope for this block so we can propogate
        // the state. These then will be threaded through the child blocks.
        //let pred_bb = self.get_node(pred);
        //let pred_decls = pred_bb.decls();

        let entry = self.add_bb("BlockEntry");
        self.goto(pred, entry);
        pred = entry;


        let mut decls = vec![];

        for stmt in block.stmts.iter() {
            decls.extend(self.get_decls(stmt));

            pred = self.stmt(stmt, pred);
        }

        pred = match block.expr {
            Some(ref _expr) => {
                panic!("cannot handle block expressions yet");

                /*
                {
                    let mut bb = self.get_node_mut(pred);
                    bb.expr = block.expr.clone();
                }

                match self.expr(expr, pred) {
                    Some(edge) => {
                        let nx = self.add_bb();
                        self.graph.add_edge(pred, nx, edge);
                        nx
                    }
                    None => pred,
                }
                */
            }
            None => pred,
        };

        {
            let bb = self.get_node_mut(pred);
            bb.close_scope = true;
            bb.dead_decls = decls;
        }

        self.pop_scope();

        let exit = self.add_bb("BlockExit");
        self.goto(pred, exit);
        exit
    }

    fn get_decls(&self, stmt: &P<ast::Stmt>) -> Vec<ast::Ident> {
        let mut decls = vec![];

        if let ast::Stmt_::StmtDecl(ref decl, _) = stmt.node {
            match decl.node {
                ast::Decl_::DeclLocal(ref local) => {
                    decls.extend(self.find_decl_idents(&local.pat));
                }
                _ => {
                    panic!("cannot handle item declarations yet");
                }
            }
        }

        decls
    }

    fn add_edge(&mut self, src: NodeIndex, dst: NodeIndex, edge: Edge) -> NodeIndex {
        self.graph.add_edge(src, dst, edge);
        dst
    }

    /*
    fn return_(&mut self, src: NodeIndex, name: String, expr: P<ast::Expr>) -> NodeIndex {
        self.add_edge(src, Edge::Return {
            name: name,
            expr: expr,
        })
    }
    */

    fn goto(&mut self, src: NodeIndex, dst: NodeIndex) -> NodeIndex {
        self.add_edge(src, dst, Edge::Goto)
    }

    fn yield_(&mut self, src: NodeIndex, expr: &P<ast::Expr>) -> NodeIndex {
        let dst = self.add_bb("Yield");
        self.add_edge(src, dst, Edge::Yield {
            expr: expr.clone(),
        })
    }

    fn stmt(&mut self, stmt: &P<ast::Stmt>, pred: NodeIndex) -> NodeIndex {
        match stmt.node {
            ast::Stmt_::StmtSemi(ref expr, _) => {
                let nx = self.expr(expr, pred);
                if pred != nx {
                    return nx;
                }
            }
            _ => {}
        }

        let decls = self.get_decls(stmt);

        let live_idents = self.find_live_idents(stmt);

        let mut bb = self.get_node_mut(pred);
        bb.decl_idents.extend(decls);
        bb.live_idents.extend(live_idents);
        bb.stmts.push(stmt.clone());

        pred
    }

    fn expr(&mut self, expr: &P<ast::Expr>, pred: NodeIndex) -> NodeIndex {
        match expr.node {
            ast::Expr_::ExprRet(Some(ref expr)) => {
                self.yield_(pred, expr)
            }
            ast::Expr_::ExprRet(None) => {
                panic!("cannot handle empty returns yet");
            }
            ast::Expr_::ExprBlock(ref block) => {
                self.block(block, pred)
            }
            _ => pred,
        }
    }

    fn push_scope(&mut self) {
        self.scope.push(0);
    }

    fn pop_scope(&mut self) {
        self.scope.pop();
    }

    fn add_bb<T>(&mut self, name: T) -> NodeIndex
        where T: Into<String>
    {
        let name = name.into();
        self.graph.add_node(Node::BasicBlock(BasicBlock::new(name, self.scope.clone())))
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

    /*
    fn get_edge(&self, stmt: &ast::Stmt, pred: NodeIndex) -> Option<Edge> {
        /*
        struct Visitor<'a> {
            builder: &'a CFGBuilder,
            pred: NodeIndex,
            edge: Option<Edge>,
        }

        impl<'a> visit::Visitor<'a> for Visitor<'a> {
            fn visit_expr(&mut self, expr: &'a ast::Expr) {
                match expr.node {
                    ast::Expr_::ExprRet(Some(ref e)) => {
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
        */

        match stmt.node {
            ast::Stmt_::StmtSemi(ref expr, _) => self.expr(expr, pred),
            _ => None,
        }
    }
    */

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
    pub fn name(&self) -> &str {
        match *self {
            Node::BasicBlock(ref bb) => &bb.name[..],
            Node::Exit => "Exit",
        }
    }

    pub fn scope(&self) -> &[usize] {
        match *self {
            Node::BasicBlock(ref bb) => &bb.scope[..],
            Node::Exit => &[],
        }
    }

    pub fn decls(&self) -> &[ast::Ident] {
        match *self {
            Node::BasicBlock(ref bb) => {
                if bb.close_scope {
                    &[]
                } else {
                    &bb.decl_idents[..]
                }
            }
            Node::Exit => &[],
        }
    }
}

#[derive(Debug)]
pub enum Edge {
    Goto,
    Yield {
        expr: P<ast::Expr>,
    },
}

#[derive(Debug)]
pub struct BasicBlock {
    name: String,
    scope: Vec<usize>,
    close_scope: bool,
    pub decl_idents: Vec<ast::Ident>,
    pub live_idents: Vec<ast::Ident>,
    pub dead_decls: Vec<ast::Ident>,
    pub stmts: Vec<P<ast::Stmt>>,
    pub expr: Option<P<ast::Expr>>,
}

impl BasicBlock {
    fn new(name: String, scope: Vec<usize>) -> Self {
        BasicBlock {
            name: name,
            scope: scope,
            close_scope: false,
            decl_idents: Vec::new(),
            live_idents: Vec::new(),
            dead_decls: Vec::new(),
            stmts: Vec::new(),
            expr: None,
        }
    }
}
