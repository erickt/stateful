use std::collections::HashMap;
use std::collections::hash_map::Entry;

use aster::AstBuilder;

use petgraph::EdgeDirection;
use petgraph::graph::{self, Graph, NodeIndex};

use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::codemap::{DUMMY_SP, Span};
use syntax::visit;
use syntax::ptr::P;

//////////////////////////////////////////////////////////////////////////////

pub struct CFGBuilder<'a> {
    cx: &'a ExtCtxt<'a>,
    graph: Graph<Node, ()>,
    labeled_loop_map: HashMap<ast::Ident, Vec<(NodeIndex, NodeIndex)>>,
    unlabeled_loop_stack: Vec<(NodeIndex, NodeIndex)>,
    scopes: Vec<Vec<Decl>>,
}

impl<'a> CFGBuilder<'a> {
    pub fn new(cx: &'a ExtCtxt<'a>) -> Self {
        CFGBuilder {
            cx: cx,
            graph: Graph::new(),
            labeled_loop_map: HashMap::new(),
            unlabeled_loop_stack: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn build(mut self, fn_decl: &ast::FnDecl, block: &ast::Block) -> CFG {
        self.scopes.push(Vec::new());

        // The initial scope is the function scope arguments.
        for arg in fn_decl.inputs.iter() {
            let decl = self.get_decl(&arg.pat);
            self.scope().push(decl);
        }

        let entry = self.add_bb("Entry");
        let exit = self.graph.add_node(Node::Exit);

        let pred = self.block(entry, block);
        self.goto(pred, exit);

        self.scopes.pop();

        CFG {
            graph: self.graph,
            entry: entry,
            exit: exit,
        }
    }

    fn scope(&mut self) -> &mut Vec<Decl> {
        self.scopes.last_mut().unwrap()
    }

    fn block(&mut self,
             pred: NodeIndex,
             block: &ast::Block) -> NodeIndex {
        let pred = self.block_inner(block, pred);
        let exit = self.add_bb("BlockExit");
        self.goto(pred, exit);
        exit
    }

    fn block_inner(&mut self, block: &ast::Block, mut pred: NodeIndex) -> NodeIndex {
        // Create a new scope so that all our declarations will be dropped when it goes out of
        // bounds.
        self.scopes.push(Vec::new());

        for stmt in block.stmts.iter() {
            pred = self.stmt(pred, stmt);
        }

        if block.expr.is_some() {
            panic!("cannot handle block expressions yet");
        }

        self.scopes.pop();

        pred
    }

    fn add_edge(&mut self, src: NodeIndex, dst: NodeIndex) {
        self.graph.add_edge(src, dst, ());
    }

    /*
    fn return_(&mut self, src: NodeIndex, name: String, expr: P<ast::Expr>) -> NodeIndex {
        self.add_edge(src, Edge::Return {
            name: name,
            expr: expr,
        })
    }
    */

    fn goto(&mut self, src: NodeIndex, dst: NodeIndex) {
        self.add_edge(src, dst);
        self.add_stmt(src, Stmt::Goto(dst));
    }

    fn yield_(&mut self, src: NodeIndex, expr: &P<ast::Expr>) -> NodeIndex {
        let dst = self.add_bb("Yield");
        self.add_edge(src, dst);
        self.add_stmt(src, Stmt::Yield(dst, expr.clone()));

        dst
    }

    fn add_stmt(&mut self, nx: NodeIndex, stmt: Stmt) {
        let bb = self.get_node_mut(nx);
        bb.stmts.push(stmt);
    }

    fn stmt(&mut self, pred: NodeIndex, stmt: &P<ast::Stmt>) -> NodeIndex {
        match stmt.node {
            ast::Stmt_::StmtDecl(ref decl, _) => {
                match decl.node {
                    ast::Decl_::DeclLocal(ref local) => {
                        let decl = self.get_decl(&local.pat);
                        self.scope().push(decl);
                    }
                    _ => {
                        panic!("cannot handle item declarations yet");
                    }
                }

                self.add_stmt(pred, Stmt::Stmt(stmt.clone()));
                pred
            }
            ast::Stmt_::StmtSemi(ref expr, _) if self.contains_transition_expr(expr) => {
                self.stmt_semi(pred, expr)
            }
            _ => {
                self.add_stmt(pred, Stmt::Stmt(stmt.clone()));
                pred
            }
        }
    }

    fn stmt_semi(&mut self, pred: NodeIndex, expr: &P<ast::Expr>) -> NodeIndex {
        match expr.node {
            ast::Expr_::ExprRet(Some(ref expr)) => {
                self.yield_(pred, expr)
            }
            ast::Expr_::ExprRet(None) => {
                panic!("cannot handle empty returns yet");
            }
            ast::Expr_::ExprAgain(Some(_)) => {
                panic!("cannot handle labeled continues yet");
            }
            ast::Expr_::ExprAgain(None) => {
                let entry = self.unlabeled_loop_stack.last().unwrap().0;
                self.goto(pred, entry);
                pred
            }
            ast::Expr_::ExprBreak(Some(_)) => {
                panic!("cannot handle labeled breaks yet");
            }
            ast::Expr_::ExprBreak(None) => {
                let exit = self.unlabeled_loop_stack.last().unwrap().1;
                self.goto(pred, exit);
                pred
            }
            ast::Expr_::ExprBlock(ref block) => {
                self.expr_block(pred, block)
            }
            ast::Expr_::ExprLoop(ref block, label) => {
                self.expr_loop(pred, block, label)
            }
            ast::Expr_::ExprIf(ref expr, ref then, ref else_) => {
                self.expr_if(pred, expr, then, else_)
            }
            ref expr => {
                panic!("cannot handle {:?} yet", expr);
            }
        }
    }

    fn expr_block(&mut self, pred: NodeIndex, block: &ast::Block) -> NodeIndex {
        self.block(pred, block)
    }

    fn expr_loop(&mut self,
                 pred: NodeIndex,
                 block: &ast::Block,
                 label: Option<ast::Ident>) -> NodeIndex {
        let loop_entry = self.add_bb("LoopEntry");
        let loop_exit = self.add_bb("LoopExit");
        self.goto(pred, loop_entry);

        // Add this loop into the loop stacks.
        self.unlabeled_loop_stack.push((loop_entry, loop_exit));

        if let Some(label) = label {
            let label_stack = match self.labeled_loop_map.entry(label) {
                Entry::Occupied(entry) => {
                    let msg = format!(
                        "label name `{}` shadows a label name that is already in scope",
                        label);
                    self.cx.span_warn(DUMMY_SP, &msg);

                    entry.into_mut()
                }
                Entry::Vacant(entry) => {
                    entry.insert(Vec::new())
                }
            };

            label_stack.push((loop_entry, loop_exit));
        }

        let pred = self.block_inner(block, loop_entry);

        // Loop back to the beginning.
        self.goto(pred, loop_entry);

        // Remove ourselves from the loop stacks.
        self.unlabeled_loop_stack.pop();

        if let Some(label) = label {
            self.labeled_loop_map.get_mut(&label).unwrap().pop();
        }

        loop_exit
    }

    fn expr_if(&mut self,
               pred: NodeIndex,
               expr: &P<ast::Expr>,
               then: &P<ast::Block>,
               else_: &Option<P<ast::Expr>>) -> NodeIndex {
        assert!(!self.contains_transition_expr(expr));
        assert!(then.expr.is_none());

        let builder = AstBuilder::new();

        let then_nx = self.add_bb("Then");
        let else_nx = self.add_bb("Else");
        let endif_nx = self.add_bb("EndIf");

        self.add_stmt(pred, Stmt::If(expr.clone(), then_nx, else_nx));
        self.add_edge(pred, then_nx);
        self.add_edge(pred, else_nx);

        let pred = self.block_inner(then, then_nx);
        self.goto(pred, endif_nx);

        let else_ = match *else_ {
            Some(ref else_) => {
                builder.block()
                    .stmt().semi().build(else_.clone())
                    .build()
            }
            None => {
                builder.block().build()
            }
        };

        let pred = self.block_inner(&else_, else_nx);
        self.goto(pred, endif_nx);

        endif_nx
    }

    fn add_bb<T>(&mut self, name: T) -> NodeIndex
        where T: Into<String>
    {
        let name = name.into();
        let scope = self.scopes.iter()
            .flat_map(|scope| scope.iter())
            .map(|scope| scope.clone())
            .collect::<Vec<_>>();

        let bb = BasicBlock::new(name, scope);

        self.graph.add_node(Node::BasicBlock(bb))
    }

    fn get_node_mut(&mut self, index: NodeIndex) -> &mut BasicBlock {
        match self.graph.node_weight_mut(index) {
            Some(node) => {
                match *node {
                    Node::BasicBlock(ref mut bb) => bb,
                    ref node => {
                        panic!("node is not a basic block: {:?}", node)
                    }
                }
            }
            None => {
                panic!("missing node!")
            }
        }
    }

    fn get_decl(&self, pat: &P<ast::Pat>) -> Decl {
        struct Visitor(Vec<(ast::Mutability, ast::Ident)>);

        impl<'a> visit::Visitor<'a> for Visitor {
            fn visit_pat(&mut self, pat: &ast::Pat) {
                match pat.node {
                    ast::PatIdent(ast::BindingMode::ByValue(mutability), id, _) => {
                        self.0.push((mutability, id.node));
                    }
                    _ => { }
                }

                visit::walk_pat(self, pat);
            }
        }

        let mut visitor = Visitor(Vec::new());
        visit::Visitor::visit_pat(&mut visitor, pat);

        Decl {
            pat: pat.clone(),
            idents: visitor.0,
        }
    }

    fn contains_transition_expr(&self, expr: &ast::Expr) -> bool {
        struct Visitor {
            contains_transition: bool,
            inside_loop: bool,
        }

        impl<'a> visit::Visitor<'a> for Visitor {
            fn visit_expr(&mut self, expr: &ast::Expr) {
                match expr.node {
                    ast::Expr_::ExprRet(Some(_)) => {
                        self.contains_transition = true;
                    }
                    ast::Expr_::ExprBreak(_) if self.inside_loop => {
                        self.contains_transition = true;
                    }
                    ast::Expr_::ExprAgain(_) if self.inside_loop => {
                        self.contains_transition = true;
                    }
                    _ => {
                        visit::walk_expr(self, expr)
                    }
                }
            }
        }

        let mut visitor = Visitor {
            contains_transition: false,
            inside_loop: !self.unlabeled_loop_stack.is_empty(),
        };

        visit::Visitor::visit_expr(&mut visitor, expr);
        visitor.contains_transition
    }
}

//////////////////////////////////////////////////////////////////////////////

pub struct CFG {
    pub graph: Graph<Node, ()>,
    pub entry: NodeIndex,
    pub exit: NodeIndex,
}

impl CFG {
    pub fn get_node(&self, nx: NodeIndex) -> &Node {
        &self.graph[nx]
    }

    pub fn get_node_decl_pats(&self, nx: NodeIndex) -> Vec<P<ast::Pat>> {
        self.get_node(nx).decl_pats()
    }

    pub fn get_node_decl_idents(&self, nx: NodeIndex) -> Vec<(ast::Mutability, ast::Ident)> {
        self.get_node(nx).decl_idents()
    }

    pub fn get_edges(&self, nx: NodeIndex, direction: EdgeDirection) -> graph::Edges<()> {
        self.graph.edges_directed(nx, direction)
    }

    pub fn get_child_edges(&self, nx: NodeIndex) -> graph::Edges<()> {
        self.get_edges(nx, EdgeDirection::Outgoing)
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

    pub fn decl_pats(&self) -> Vec<P<ast::Pat>> {
        match *self {
            Node::BasicBlock(ref bb) => bb.decl_pats(),
            Node::Exit => Vec::new(),
        }
    }

    pub fn decl_idents(&self) -> Vec<(ast::Mutability, ast::Ident)> {
        match *self {
            Node::BasicBlock(ref bb) => bb.decl_idents(),
            Node::Exit => Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    name: String,
    decls: Vec<Decl>,
    pub stmts: Vec<Stmt>,
}

impl BasicBlock {
    fn new(name: String, decls: Vec<Decl>) -> Self {
        BasicBlock {
            name: name,
            decls: decls,
            stmts: Vec::new(),
        }
    }

    fn decl_pats(&self) -> Vec<P<ast::Pat>> {
        self.decls.iter()
            .map(|decl| decl.pat.clone())
            .collect::<Vec<_>>()
    }

    fn decl_idents(&self) -> Vec<(ast::Mutability, ast::Ident)> {
        self.decls.iter()
            .flat_map(|decl| decl.idents.iter())
            .map(|ident| *ident)
            .collect::<Vec<_>>()
    }
}

#[derive(Clone, Debug)]
pub struct Decl {
    pat: P<ast::Pat>,
    idents: Vec<(ast::Mutability, ast::Ident)>,
}

#[derive(Debug)]
pub enum Stmt {
    Stmt(P<ast::Stmt>),
    Goto(NodeIndex),
    Yield(NodeIndex, P<ast::Expr>),
    If(P<ast::Expr>, NodeIndex, NodeIndex),
}
