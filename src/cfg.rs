use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::build::AstBuilder;
use syntax::visit;
use syntax::ptr::P;

use petgraph::EdgeDirection;
use petgraph::graph::{Graph, NodeIndex};

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
        let scope = Vec::new();

        let entry = self.add_bb("Entry", &scope);
        let exit = self.graph.add_node(Node::Exit);

        let pred = self.block(block, entry, &scope);
        self.goto(pred, exit);

        CFG {
            graph: self.graph,
            entry: entry,
            exit: exit,
        }
    }

    fn block(&mut self, block: &ast::Block,
             mut pred: NodeIndex,
             parent_scope: &Vec<ast::Ident>) -> NodeIndex {
        // Create a new scope so that all our declarations will be dropped when it goes out of
        // bounds.
        let mut scope = parent_scope.clone();

        let entry = self.add_bb("BlockEntry", &scope);
        self.goto(pred, entry);
        pred = entry;

        for stmt in block.stmts.iter() {
            pred = self.stmt(stmt, pred, &mut scope);
        }

        if block.expr.is_some() {
            panic!("cannot handle block expressions yet");
        }

        let exit = self.add_bb("BlockExit", &parent_scope);
        self.goto(pred, exit);
        exit
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

    fn goto(&mut self,
            src: NodeIndex,
            dst: NodeIndex) -> NodeIndex {
        self.add_edge(src, dst, Edge::Goto)
    }

    fn yield_(&mut self,
              src: NodeIndex,
              expr: &P<ast::Expr>,
              scope: &Vec<ast::Ident>) -> NodeIndex {
        let dst = self.add_bb("Yield", scope);
        self.add_edge(src, dst, Edge::Yield {
            expr: expr.clone(),
        })
    }

    fn stmt(&mut self, stmt: &P<ast::Stmt>,
            pred: NodeIndex,
            scope: &mut Vec<ast::Ident>) -> NodeIndex {
        match stmt.node {
            ast::Stmt_::StmtDecl(ref decl, _) => {
                match decl.node {
                    ast::Decl_::DeclLocal(ref local) => {
                        scope.extend(self.find_decl_idents(&local.pat));
                    }
                    _ => {
                        panic!("cannot handle item declarations yet");
                    }
                }
            }

            ast::Stmt_::StmtSemi(ref expr, _) => {
                let nx = self.expr(expr, pred, &*scope);
                if pred != nx {
                    return nx;
                }
            }
            _ => {}
        }

        let mut bb = self.get_node_mut(pred);
        bb.stmts.push(stmt.clone());

        pred
    }

    fn expr(&mut self,
            expr: &P<ast::Expr>,
            pred: NodeIndex,
            scope: &Vec<ast::Ident>) -> NodeIndex {
        match expr.node {
            ast::Expr_::ExprRet(Some(ref expr)) => {
                self.yield_(pred, expr, scope)
            }
            ast::Expr_::ExprRet(None) => {
                panic!("cannot handle empty returns yet");
            }
            ast::Expr_::ExprBlock(ref block) => {
                self.block(block, pred, scope)
            }
            _ => pred,
        }
    }

    fn add_bb<T>(&mut self, name: T, scope: &Vec<ast::Ident>) -> NodeIndex
        where T: Into<String>
    {
        let name = name.into();
        let decls = scope.clone();
        let bb = BasicBlock::new(name, decls);

        self.graph.add_node(Node::BasicBlock(bb))
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

pub struct CFG {
    pub graph: Graph<Node, Edge>,
    pub entry: NodeIndex,
    pub exit: NodeIndex,
}

impl CFG {
    pub fn get_node(&self, nx: NodeIndex) -> &Node {
        &self.graph[nx]
    }

    pub fn get_node_decls(&self, nx: NodeIndex) -> &[ast::Ident] {
        self.get_node(nx).decls()
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

    pub fn decls(&self) -> &[ast::Ident] {
        match *self {
            Node::BasicBlock(ref bb) => &bb.decls[..],
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
    decls: Vec<ast::Ident>,
    pub stmts: Vec<P<ast::Stmt>>,
    pub expr: Option<P<ast::Expr>>,
}

impl BasicBlock {
    fn new(name: String, decls: Vec<ast::Ident>) -> Self {
        BasicBlock {
            name: name,
            decls: decls,
            stmts: Vec::new(),
            expr: None,
        }
    }
}
