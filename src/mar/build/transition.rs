use aster::AstBuilder;
use mar::build::Builder;
use syntax::ast::{self, ExprKind, StmtKind};
use syntax::ptr::P;
use syntax::visit;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn contains_transition<E: ContainsTransition>(&self, expr: E) -> bool {
        expr.contains_transition(self.is_inside_loop())
    }
}

pub trait ContainsTransition {
    fn contains_transition(&self, inside_loop: bool) -> bool;
}

impl<'a> ContainsTransition for &'a P<ast::Block> {
    fn contains_transition(&self, inside_loop: bool) -> bool {
        (**self).contains_transition(inside_loop)
    }
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

impl<'a> ContainsTransition for &'a P<ast::Expr> {
    fn contains_transition(&self, inside_loop: bool) -> bool {
        (**self).contains_transition(inside_loop)
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

impl visit::Visitor for ContainsTransitionVisitor {
    fn visit_stmt(&mut self, stmt: &ast::Stmt) {
        match stmt.node {
            StmtKind::Mac(ref mac) if is_yield_path(&(*mac).0.node.path) => {
                self.contains_transition = true;
            }
            _ => {
                visit::walk_stmt(self, stmt)
            }
        }
    }

    fn visit_expr(&mut self, expr: &ast::Expr) {
        match expr.node {
            ExprKind::Ret(Some(_)) => {
                self.contains_transition = true;
            }
            ExprKind::Break(_) if self.inside_loop => {
                self.contains_transition = true;
            }
            ExprKind::Continue(_) if self.inside_loop => {
                self.contains_transition = true;
            }
            ExprKind::Mac(ref mac) if is_transition_path(&mac.node.path) => {
                self.contains_transition = true;
            }
            ExprKind::Path(None, ref path) if is_transition_path(path) => {
                self.contains_transition = true;
            }
            _ => {
                visit::walk_expr(self, expr)
            }
        }
    }

    fn visit_mac(&mut self, _mac: &ast::Mac) { }
}

pub fn is_transition_path(path: &ast::Path) -> bool {
    if is_yield_path(path) {
        true
    } else {
        false
    }
}

pub fn is_yield_path(path: &ast::Path) -> bool {
    let builder = AstBuilder::new();
    let yield_ = builder.path()
        .id("yield_")
        .build();

    !path.global && path.segments == yield_.segments
}
