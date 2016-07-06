use std::collections::{HashSet, HashMap};
use std::collections::hash_map::Entry;
use std::iter;

use aster::AstBuilder;

use petgraph::EdgeDirection;
use petgraph::graph::{self, Graph, NodeIndex};

use syntax::ast;
use syntax::codemap::DUMMY_SP;
use syntax::ext::base::ExtCtxt;
use syntax::ext::tt::transcribe::new_tt_reader;
use syntax::parse::lexer::{Reader, TokenAndSpan};
use syntax::parse::token::Token;
use syntax::parse::parser::Parser;
use syntax::parse::common::seq_sep_trailing_allowed;
use syntax::ptr::P;
use syntax::visit;

//////////////////////////////////////////////////////////////////////////////

pub struct CFGBuilder<'a> {
    cx: &'a ExtCtxt<'a>,
    graph: Graph<Node, ()>,
    labeled_loop_map: HashMap<ast::Ident, Vec<(NodeIndex, NodeIndex)>>,
    block_stack: Vec<Block>,
    unlabeled_loop_stack: Vec<(NodeIndex, NodeIndex)>,
    scopes: Vec<Scope>,
}

impl<'a> CFGBuilder<'a> {
    pub fn new(cx: &'a ExtCtxt<'a>) -> Self {
        CFGBuilder {
            cx: cx,
            graph: Graph::new(),
            labeled_loop_map: HashMap::new(),
            block_stack: Vec::new(),
            unlabeled_loop_stack: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn build(mut self, fn_decl: &ast::FnDecl, block: &ast::Block) -> CFG {
        // The initial scope is the function scope arguments.
        self.scopes.push(fn_decl.inputs.iter()
            .flat_map(|arg| get_decl_from_pat(&arg.pat))
            .collect());

        self.push_block();

        let entry = self.add_bb("Entry");
        let pred = self.block_inner(entry, block);

        let exit = self.graph.add_node(Node::Exit);
        self.return_(pred, exit);

        self.pop_block(pred);
        self.scopes.pop();

        CFG {
            graph: self.graph,
            entry: entry,
            exit: exit,
        }
    }

    fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn push_block(&mut self) {
        self.block_stack.push(Block::new());
    }

    fn current_block(&mut self) -> &mut Block {
        self.block_stack.last_mut().expect("No blocks found")
    }

    fn pop_block(&mut self, nx: NodeIndex) {
        let block = self.block_stack.pop().expect("block");

        let bb = self.get_node_mut(nx);
        bb.block.stmts.extend(block.stmts);
    }

    fn block_inner(&mut self, mut pred: NodeIndex, block: &ast::Block) -> NodeIndex {
        // Create a new scope so that all our declarations will be dropped when it goes out of
        // bounds.
        self.scopes.push(Scope::new());
        self.push_block();

        for stmt in block.stmts.iter() {
            pred = self.stmt(pred, stmt);
        }

        if block.expr.is_some() {
            self.cx.span_fatal(block.span,
                               "cannot handle block expressions yet");
        }

        self.scopes.pop();
        self.pop_block(pred);

        pred
    }

    fn add_edge(&mut self, src: NodeIndex, dst: NodeIndex) {
        self.graph.add_edge(src, dst, ());
    }

    fn return_(&mut self, src: NodeIndex, dst: NodeIndex) {
        self.add_edge(src, dst);
        self.current_block().stmts.push(Stmt::Return);
        self.pop_block(src);
        self.push_block();
    }

    /*
    fn make_goto(&mut self, src: NodeIndex, dst: NodeIndex) -> Stmt {
        self.add_edge(src, dst);
        Stmt::Goto(dst)
    }
    */

    fn goto(&mut self, src: NodeIndex, dst: NodeIndex) {
        self.add_edge(src, dst);
        self.current_block().stmts.push(Stmt::Goto(dst));
        self.pop_block(src);
        self.push_block();
    }

    fn yield_(&mut self,
              src: NodeIndex,
              expr: &P<ast::Expr>,
              idents: Vec<ast::Ident>) -> NodeIndex {
        self.scope().extend(
            idents.into_iter()
                .map(|ident| (ast::Mutability::MutImmutable, ident))
        );

        let dst = self.add_bb("Yield");
        self.add_edge(src, dst);

        self.current_block().stmts.push(Stmt::Yield(dst, expr.clone()));
        self.pop_block(src);
        self.push_block();

        dst
    }

    fn add_stmt(&mut self, nx: NodeIndex, stmt: Stmt) {
        let bb = self.get_node_mut(nx);
        bb.block.stmts.push(stmt);
    }

    fn stmt(&mut self, pred: NodeIndex, stmt: &ast::Stmt) -> NodeIndex {
        match stmt.node {
            ast::Stmt_::StmtDecl(ref decl, _) => {
                match decl.node {
                    ast::Decl_::DeclLocal(ref local) => {
                        self.scope().extend(get_decl_from_pat(&local.pat));
                    }
                    _ => {
                        panic!("cannot handle item declarations yet");
                    }
                }

                self.add_stmt(pred, Stmt::Stmt(stmt.clone()));
                pred
            }
            ast::Stmt_::StmtSemi(ref expr, _)
            if expr.contains_transition(self.is_inside_loop()) => {
                self.stmt_semi(pred, expr)
            }
            ast::Stmt_::StmtMac(ref mac, _, _) if is_yield_path(&mac.node.path) => {
                self.mac_yield(pred, mac)
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
                self.yield_(pred, expr, Vec::new())
            }
            ast::Expr_::ExprRet(None) => {
                panic!("cannot handle empty returns yet");
            }
            ast::Expr_::ExprAgain(Some(_)) => {
                panic!("cannot handle labeled continues yet");
            }
            ast::Expr_::ExprAgain(None) => {
                self.expr_continue(pred)
            }
            ast::Expr_::ExprBreak(Some(_)) => {
                panic!("cannot handle labeled breaks yet");
            }
            ast::Expr_::ExprBreak(None) => {
                self.expr_break(pred)
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
            ast::Expr_::ExprMatch(ref expr, ref arms) => {
                self.expr_match(pred, expr, arms)
            }
            ast::Expr_::ExprMac(ref mac) if is_yield_path(&mac.node.path) => {
                self.mac_yield(pred, mac)
            }
            ast::Expr_::ExprCall(ref func, ref args) => {
                match func.node {
                    ast::Expr_::ExprPath(None, ref path) if is_yield_path(path) => {
                        self.expr_yield(pred, args)
                    }
                    _ => {
                        panic!("cannot handle {:?} yet", expr);
                    }
                }
            }
            ref expr => {
                panic!("cannot handle {:?} yet", expr);
            }
        }
    }

    fn expr_continue(&mut self, pred: NodeIndex) -> NodeIndex {
        let entry = self.unlabeled_loop_stack.last().unwrap().0;
        self.goto(pred, entry);
        pred
    }

    fn expr_break(&mut self, pred: NodeIndex) -> NodeIndex {
        let exit = self.unlabeled_loop_stack.last().unwrap().1;
        self.goto(pred, exit);
        pred
    }

    fn expr_block(&mut self, pred: NodeIndex, block: &ast::Block) -> NodeIndex {
        let pred = self.block_inner(pred, block);

        let exit = self.add_bb("BlockExit");
        self.goto(pred, exit);

        exit
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

        let pred = self.block_inner(loop_entry, block);

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
        assert!(!expr.contains_transition(self.is_inside_loop()));
        assert!(then.expr.is_none());

        let builder = AstBuilder::new();

        let then_nx = self.add_bb("Then");
        let else_nx = self.add_bb("Else");
        let endif_nx = self.add_bb("EndIf");

        self.add_stmt(pred, Stmt::If(expr.clone(), then_nx, else_nx));
        self.add_edge(pred, then_nx);
        self.add_edge(pred, else_nx);

        let pred = self.block_inner(then_nx, then);
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

        let pred = self.block_inner(else_nx, &else_);
        self.goto(pred, endif_nx);

        endif_nx
    }

    fn expr_match(&mut self,
                  pred: NodeIndex,
                  expr: &P<ast::Expr>,
                  arms: &[ast::Arm]) -> NodeIndex {
        let inside_loop = self.is_inside_loop();
        assert!(!expr.contains_transition(inside_loop));

        let endmatch = self.add_bb("EndMatch");

        let arms = arms.iter()
            .map(|arm| self.make_arm(pred, endmatch, arm))
            .collect();

        let bb = self.get_node_mut(pred);
        bb.block.stmts.push(Stmt::Match(expr.clone(), arms));

        endmatch
    }

    fn make_arm(&mut self,
                entry: NodeIndex,
                exit: NodeIndex,
                arm: &ast::Arm) -> Arm {
        let block = match arm.body.node {
            ast::ExprBlock(ref block) => block,
            _ => {
                self.cx.span_fatal(arm.body.span,
                                   "only support match arm blocks at the moment");
            }
        };

        // We're making a temporary node here.
        let arm_entry = self.add_bb("ArmEntry");
        //self.add_edge(pred, entry);

        let pred = self.block_inner(arm_entry, block);

        // Make sure we have an endge from 
        let neighbors = self.graph.neighbors(arm_entry)
            .collect::<Vec<_>>();

        for child in neighbors {
            self.add_edge(entry, child);
        }

        self.goto(pred, exit);

        let body = self.get_node(arm_entry).block.clone();
        self.graph.remove_node(arm_entry);

        /*
        let pred = self.block_inner(pred, block);
        self.goto(pred, endmatch);
        */

        /*
        //self.scopes.push(Scope::new());
        self.push_block();

        for stmt in block.stmts.iter() {
            pred = self.stmt(pred, stmt);
        }

        if block.expr.is_some() {
            self.cx.span_fatal(block.span,
                               "cannot handle block expressions yet");
        }

        //self.scopes.pop();
        let body = self.block_stack.pop().expect("block");

        /*
        // Make sure we go to the exit.
        //

        if ast_body.expr.is_some() {
            self.cx.span_fatal(ast_body.span,
                               "blocks cannot return expressions yet");
        }

        let inside_loop = self.is_inside_loop();
        let mut found_transition = false;
        let mut body = Block::new();

        /*
        for stmt in ast_body.stmts.iter() {
            if stmt.contains_transition(inside_loop) {
                found_transition = true;
            } else {
                body.stmts.push(Stmt::Stmt(stmt.clone()));
            }
        }
        */

        // Go to the exit if this arm didn't contain any transitions.
        if !found_transition {
            body.stmts.push(self.make_goto(pred, endmatch));
        }
        */

        body.stmts.push(self.make_goto(pred, endmatch));
        */

        Arm {
            pats: arm.pats.clone(),
            guard: arm.guard.clone(),
            body: body,
        }
    }

    fn mac_yield(&mut self, pred: NodeIndex, mac: &ast::Mac) -> NodeIndex {
        let (expr, idents) = parse_mac_yield(self.cx, mac);

        self.yield_(pred, &expr, idents)

        /*
        let mut iter = args.iter();

        let expr = match iter.next() {
            Some(arg) => arg,
            None => { panic!("yield needs an argument"); }
        };

        let mut state_ids = Vec::new();

        for arg in iter {
            match arg.node {
                ast::Expr_::ExprPath(None, ast::Path {
                    global: false,
                    ref segments,
                    ..
                }) => {
                    if segments.len() != 1 {
                        panic!("must pass in identifiers: `{:?}`", arg);
                    }

                    let segment = &segments[0];

                    if segment.parameters.is_empty() {
                        state_ids.push(segment.identifier);
                    } else {
                        panic!("state id cannot have parameters: `{:?}`", arg);
                    }
                }
                _ => {
                    panic!("don't know how to handle `{:?}`", arg);
                }
            }
        }

        let dst = self.add_bb("Yield");
        self.add_edge(pred, dst);
        self.add_stmt(pred, Stmt::Yield(dst, expr.clone())); //, state_ids));

        dst
        */
    }

    fn expr_yield(&mut self, pred: NodeIndex, args: &[P<ast::Expr>]) -> NodeIndex {
        let mut iter = args.iter();

        let expr = match iter.next() {
            Some(arg) => arg,
            None => { panic!("yield needs an argument"); }
        };

        let mut state_ids = Vec::new();

        for arg in iter {
            match arg.node {
                ast::Expr_::ExprPath(None, ast::Path {
                    global: false,
                    ref segments,
                    ..
                }) => {
                    if segments.len() != 1 {
                        panic!("must pass in identifiers: `{:?}`", arg);
                    }

                    let segment = &segments[0];

                    if segment.parameters.is_empty() {
                        state_ids.push(segment.identifier);
                    } else {
                        panic!("state id cannot have parameters: `{:?}`", arg);
                    }
                }
                _ => {
                    panic!("don't know how to handle `{:?}`", arg);
                }
            }
        }

        let dst = self.add_bb("Yield");
        self.add_edge(pred, dst);
        self.add_stmt(pred, Stmt::Yield(dst, expr.clone())); //, state_ids));

        dst
    }

    fn add_bb<T>(&mut self, name: T) -> NodeIndex
        where T: Into<String>
    {
        let name = name.into();
        let scope = self.scopes.iter()
            .flat_map(|scope| scope.decls.iter())
            .map(|decl| decl.clone())
            .collect::<Scope>();

        let bb = BasicBlock::new(name, scope);

        self.graph.add_node(Node::BasicBlock(bb))
    }

    fn get_node(&self, index: NodeIndex) -> &BasicBlock {
        match self.graph.node_weight(index) {
            Some(node) => {
                match *node {
                    Node::BasicBlock(ref bb) => bb,
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

    fn is_inside_loop(&self) -> bool {
        !self.unlabeled_loop_stack.is_empty()
    }
}

trait ContainsTransition {
    fn contains_transition(&self, inside_loop: bool) -> bool;
}

impl ContainsTransition for ast::Block {
    fn contains_transition(&self, inside_loop: bool) -> bool {
        let mut visitor = ContainsTransitionVisitor::new(inside_loop);

        visit::Visitor::visit_block(&mut visitor, self);
        visitor.contains_transition
    }
}

impl ContainsTransition for ast::Stmt {
    fn contains_transition(&self, inside_loop: bool) -> bool {
        let mut visitor = ContainsTransitionVisitor::new(inside_loop);

        visit::Visitor::visit_stmt(&mut visitor, self);
        visitor.contains_transition
    }
}

impl ContainsTransition for ast::Expr {
    fn contains_transition(&self, inside_loop: bool) -> bool {
        let mut visitor = ContainsTransitionVisitor::new(inside_loop);

        visit::Visitor::visit_expr(&mut visitor, self);
        visitor.contains_transition
    }
}

struct ContainsTransitionVisitor {
    inside_loop: bool,
    contains_transition: bool,
}

impl ContainsTransitionVisitor {
    fn new(inside_loop: bool) -> Self {
        ContainsTransitionVisitor {
            inside_loop: inside_loop,
            contains_transition: false,
        }
    }
}

impl<'a> visit::Visitor<'a> for ContainsTransitionVisitor {
    fn visit_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt.node {
            ast::Stmt_::StmtMac(ref mac, _, _) if is_yield_path(&mac.node.path) => {
                self.contains_transition = true;
            }
            _ => {
                visit::walk_stmt(self, stmt)
            }
        }
    }

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
            ast::Expr_::ExprMac(ref mac) if is_transition_path(&mac.node.path) => {
                self.contains_transition = true;
            }
            ast::Expr_::ExprPath(None, ref path) if is_transition_path(path) => {
                self.contains_transition = true;
            }
            _ => {
                visit::walk_expr(self, expr)
            }
        }
    }

    fn visit_mac(&mut self, _mac: &ast::Mac) { }
}

fn get_decl_from_pat(pat: &P<ast::Pat>) -> Vec<(ast::Mutability, ast::Ident)> {
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

        fn visit_mac(&mut self, _mac: &ast::Mac) { }
    }

    let mut visitor = Visitor(Vec::new());
    visit::Visitor::visit_pat(&mut visitor, pat);

    visitor.0
}

fn is_transition_path(path: &ast::Path) -> bool {
    if is_yield_path(path) {
        true
    } else {
        false
    }
}

fn is_yield_path(path: &ast::Path) -> bool {
    let builder = AstBuilder::new();
    let yield_ = builder.path()
        .id("yield_")
        .build();

    !path.global && path.segments == yield_.segments
}

fn parse_mac_yield(cx: &ExtCtxt, mac: &ast::Mac) -> (P<ast::Expr>, Vec<ast::Ident>) {
    let mut rdr = new_tt_reader(&cx.parse_sess().span_diagnostic,
                                None,
                                None,
                                mac.node.tts.clone());

    let mut parser = Parser::new(cx.parse_sess(), cx.cfg(), Box::new(rdr.clone()));

    let expr = panictry!(parser.parse_expr());

    for _ in 0..parser.tokens_consumed {
        let _ = rdr.next_token();
    }

    let TokenAndSpan { tok, sp } = rdr.peek();

    let idents = match tok {
        Token::Eof => Vec::new(),
        Token::Comma => {
            parser.bump();

            let seq_sep = seq_sep_trailing_allowed(Token::Comma);
            let idents = panictry!(parser.parse_seq_to_end(&Token::Eof,
                                                           seq_sep,
                                                           |p| p.parse_ident()));

            if idents.is_empty() {
                cx.span_fatal(sp, &format!("unexpected end of macro"));
            }

            idents
        }
        _ => {
            let token_str = parser.this_token_to_string();
            cx.span_fatal(sp, &format!("expected ident, found `{}`", token_str));
        }
    };

    (expr, idents)
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

    /*
    pub fn get_node_decl_pats(&self, nx: NodeIndex) -> Vec<P<ast::Pat>> {
        self.get_node(nx).decl_pats()
    }
    */

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

    /*
    pub fn decl_pats(&self) -> Vec<P<ast::Pat>> {
        match *self {
            Node::BasicBlock(ref bb) => bb.decl_pats(),
            Node::Exit => Vec::new(),
        }
    }
    */

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
    scope: Scope,
    pub block: Block,
}

impl BasicBlock {
    fn new(name: String, scope: Scope) -> Self {
        BasicBlock {
            name: name,
            scope: scope,
            block: Block::new(),
        }
    }

    /*
    fn decl_pats(&self) -> Vec<P<ast::Pat>> {
        self.decls.iter()
            .map(|decl| decl.pat.clone())
            .collect::<Vec<_>>()
    }
    */

    fn decl_idents(&self) -> Vec<(ast::Mutability, ast::Ident)> {
        self.scope.idents()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Decl {
    //pat: P<ast::Pat>,
    idents: Vec<(ast::Mutability, ast::Ident)>,
}

#[derive(Clone, Debug)]
pub struct Arm {
    pub pats: Vec<P<ast::Pat>>,
    pub guard: Option<P<ast::Expr>>,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl Block {
    fn new() -> Self {
        Block {
            stmts: Vec::new(),
        }
    }
}

impl iter::Extend<Stmt> for Block {
    fn extend<T: IntoIterator<Item=Stmt>>(&mut self, iter: T) {
        self.stmts.extend(iter)
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Stmt(ast::Stmt),
    Return,
    Goto(NodeIndex),
    Yield(NodeIndex, P<ast::Expr>),
    If(P<ast::Expr>, NodeIndex, NodeIndex),
    Match(P<ast::Expr>, Vec<Arm>),
}

#[derive(Clone, Debug)]
pub struct Scope {
    decls: Vec<(ast::Mutability, ast::Ident)>,
    set: HashSet<ast::Ident>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            decls: Vec::new(),
            set: HashSet::new(),
        }
    }

    fn push(&mut self, mutability: ast::Mutability, ident: ast::Ident) {
        if !self.set.contains(&ident) {
            self.set.insert(ident);
            self.decls.push((mutability, ident));
        }
    }

    fn idents<'a>(&'a self) -> Vec<(ast::Mutability, ast::Ident)> {
        self.decls.clone()
    }
}

/*
impl iter::FromIterator<Decl> for Scope {
    fn from_iter<T: IntoIterator<Item=Decl>>(iter: T) -> Self {
        let mut scope = Scope::new();

        for decl in iter.into_iter() {
            scope.push(decl);
        }

        scope
    }
}
*/

impl iter::FromIterator<(ast::Mutability, ast::Ident)> for Scope {
    fn from_iter<T: IntoIterator<Item=(ast::Mutability, ast::Ident)>>(iter: T) -> Self {
        let mut scope = Scope::new();

        for (mutability, ident) in iter.into_iter() {
            scope.push(mutability, ident);
        }

        scope
    }
}

impl iter::Extend<(ast::Mutability, ast::Ident)> for Scope {
    fn extend<T: IntoIterator<Item=(ast::Mutability, ast::Ident)>>(&mut self, iter: T) {
        for (mutability, ident) in iter {
            self.push(mutability, ident);
        }
    }
}
