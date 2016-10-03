/**
### Early exit

There are numerous "normal" ways to early exit a scope: `break`, `continue`, `return`. Whenever an
early exit occurs, the method `exit_scope` is called. It is given the current point in execution
where the early exit occurs, as well as the scope you want to branch to (note that all early exits
from to some other enclosing scope). `exit_scope` will record the exit point and also add all
drops.

### Loop scopes

In addition to the normal scope stack, we track a loop scope stack that contains only loops. It
tracks where a `break` and `continue` should go to.
*/

use aster::AstBuilder;
use mar::build::{Builder, CFG};
use mar::repr::*;
use std::ascii::AsciiExt;
use std::collections::{HashMap, HashSet};
use syntax::ast::{self, PatKind};
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::visit;

#[derive(Debug)]
pub struct Scope {
    extent: CodeExtent,
    forward_decls: HashMap<ast::Ident, ForwardDecl>,
    drops: Vec<DropData>,
    moved_decls: HashSet<Var>,
}

#[derive(Debug)]
struct ForwardDecl {
    span: Span,
    decl: Var,
}

#[derive(Debug)]
struct DropData {
    span: Span,
    decl: Var,
}

impl From<ForwardDecl> for DropData {
    fn from(forward_decl: ForwardDecl) -> Self {
        DropData {
            span: forward_decl.span,
            decl: forward_decl.decl,
        }
    }
}

#[derive(Clone)]
pub struct LoopScope {
    pub extent: CodeExtent,
    pub label: Option<ast::SpannedIdent>,
    pub continue_block: BasicBlock,
    pub break_block: BasicBlock,
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Start a loop scope, which tracks where `continue` and `break`
    /// should branch to. See module comment for more details.
    pub fn in_loop_scope<F>(&mut self,
                            extent: CodeExtent,
                            label: Option<ast::SpannedIdent>,
                            loop_block: BasicBlock,
                            break_block: BasicBlock,
                            f: F) -> BasicBlock
        where F: FnOnce(&mut Builder) -> BasicBlock
    {
        let loop_scope = LoopScope {
            extent: extent,
            label: label,
            continue_block: loop_block,
            break_block: break_block,
        };
        self.loop_scopes.push(loop_scope);
        let block = f(self);
        self.loop_scopes.pop();
        block
    }

    /// Convenience wrapper that pushes a scope and then executes `f`
    /// to build its contents, popping the scope afterwards.
    pub fn in_scope<F>(&mut self,
                       extent: CodeExtent,
                       span: Span,
                       mut block: BasicBlock,
                       f: F) -> BasicBlock
        where F: FnOnce(&mut Builder) -> BasicBlock
    {
        self.push_scope(extent, block);
        block = f(self);
        self.pop_scope(extent, block);

        // At this point, we can't tell that variables are not being accessed. So we'll create a
        // new block to make sure variables are properly not referenced.
        let end_scope_block = self.start_new_block(span, Some("EndScope"));
        self.terminate(
            span,
            block,
            TerminatorKind::Goto {
                target: end_scope_block,
                end_scope: true,
            });
        end_scope_block
    }

    /// Push a scope onto the stack. You can then build code in this
    /// scope and call `pop_scope` afterwards. Note that these two
    /// calls must be paired; using `in_scope` as a convenience
    /// wrapper maybe preferable.
    pub fn push_scope(&mut self, extent: CodeExtent, _block: BasicBlock) {
        self.scopes.push(Scope {
            extent: extent,
            drops: vec![],
            forward_decls: HashMap::new(),
            moved_decls: HashSet::new(),
        });
    }

    /// Pops a scope, which should have extent `extent`, adding any
    /// drops onto the end of `block` that are needed.  This must
    /// match 1-to-1 with `push_scope`.
    pub fn pop_scope(&mut self, extent: CodeExtent, block: BasicBlock) {
        let scope = self.scopes.pop().unwrap();

        assert_eq!(scope.extent, extent);

        // add in any drops needed on the fallthrough path (any other
        // exiting paths, such as those that arise from `break`, will
        // have drops already)
        for dropped_decl in scope.drops.iter().rev() {
            drop_decl(&mut self.cfg, block, &scope, dropped_decl);
        }
    }

    /// Finds the loop scope for a given label. This is used for
    /// resolving `break` and `continue`.
    pub fn find_loop_scope(&mut self,
                           span: Span,
                           label: Option<ast::Ident>) -> LoopScope {
        let loop_scope =
            match label {
                None => {
                    // no label? return the innermost loop scope
                    self.loop_scopes.iter()
                                    .rev()
                                    .next()
                }
                Some(label) => {
                    // otherwise, find the loop-scope with the correct id
                    self.loop_scopes.iter()
                                    .rev()
                                    .find(|loop_scope| {
                                        match loop_scope.label {
                                            Some(l) => l.node == label,
                                            None => false,
                                        }
                                    })
                }
            };

        match loop_scope {
            Some(loop_scope) => loop_scope.clone(),
            None => self.cx.span_bug(span, "no enclosing loop scope found?"),
        }
    }

    /// Branch out of `block` to `target`, exiting all scopes up to
    /// and including `extent`.  This will insert whatever drops are
    /// needed, as well as tracking this exit for the SEME region. See
    /// module comment for details.
    pub fn exit_scope(&mut self,
                      span: Span,
                      extent: CodeExtent,
                      block: BasicBlock,
                      target: BasicBlock) {
        let popped_scopes =
            match self.scopes.iter().rev().position(|scope| scope.extent == extent) {
                Some(p) => p + 1,
                None => self.cx.span_bug(span, &format!("extent {:?} does not enclose",
                                                        extent)),
            };

        for scope in self.scopes.iter_mut().rev().take(popped_scopes) {
            for dropped_decl in &scope.drops {
                drop_decl(&mut self.cfg, block, scope, dropped_decl);
            }
        }

        self.terminate(
            span,
            block,
            TerminatorKind::Goto {
                target: target,
                end_scope: true,
            });
    }

    pub fn declare_decl(&mut self,
                        mutability: ast::Mutability,
                        ident: ast::Ident,
                        ty: Option<P<ast::Ty>>) -> Var {
        let shadowed_decl = self.find_decl(ident);

        let decl = self.var_decls.push(VarDecl {
            mutability: mutability,
            ident: ident,
            ty: ty,
            shadowed_decl: shadowed_decl,
        });

        debug!("declaring {:?} as {:?} shadowing {:?}", ident, decl, shadowed_decl);

        decl
    }

    pub fn declare_binding(&mut self, span: Span, decl: Var) {
        if let Some(scope) = self.scopes.last_mut() {
            let ident = self.var_decls[decl].ident;

            scope.forward_decls.insert(ident, ForwardDecl {
                span: span,
                decl: decl,
            });
        } else {
            self.cx.span_bug(span, "no scopes?");
        }
    }

    pub fn assign_decl(&mut self,
                       block: BasicBlock,
                       lvalue: ast::Ident) {
        for scope in self.scopes.iter_mut().rev() {
            // Check if we are shadowing another variable.
            for dropped_decl in scope.drops.iter().rev() {
                if lvalue == self.var_decls[dropped_decl.decl].ident {
                    return;
                }
            }

            // Declare the lvalue now that we're assigning to it.
            if let Some(forward_decl) = scope.forward_decls.remove(&lvalue) {
                self.cfg.push_declare_decl(
                    block,
                    forward_decl.span,
                    forward_decl.decl,
                    self.var_decls[forward_decl.decl].ty.clone(),
                );

                scope.drops.push(DropData::from(forward_decl));
                break;
            }
        }
    }

    pub fn assign_lvalue(&mut self,
                         block: BasicBlock,
                         lvalue: Lvalue,
                         rvalue: P<ast::Expr>) {
        if let Some(lvalue_decl) = lvalue.decl() {
            let lvalue_ident = self.var_decls[lvalue_decl].ident;

            for scope in self.scopes.iter_mut().rev() {
                // Declare the lvalue now that we're assigning to it.
                if let Some(forward_decl) = scope.forward_decls.remove(&lvalue_ident) {
                    self.cfg.push_declare_decl(
                        block,
                        forward_decl.span,
                        forward_decl.decl,
                        self.var_decls[forward_decl.decl].ty.clone(),
                    );

                    scope.drops.push(DropData::from(forward_decl));
                    break;
                }
            }
        }

        self.cfg.push_assign(block, lvalue, rvalue);
    }

    pub fn assign_lvalue_unit(&mut self,
                              span: Span,
                              block: BasicBlock,
                              lvalue: Lvalue) {
        let rvalue = AstBuilder::new().span(span).expr().unit();
        self.assign_lvalue(block, lvalue, rvalue)
    }

    pub fn find_decl(&self, lvalue: ast::Ident) -> Option<Var> {
        for scope in self.scopes.iter().rev() {
            // Check if we are shadowing another variable.
            for dropped_decl in scope.drops.iter().rev() {
                if lvalue == self.var_decls[dropped_decl.decl].ident {
                    return Some(dropped_decl.decl);
                }
            }

            if let Some(forward_decl) = scope.forward_decls.get(&lvalue) {
                return Some(forward_decl.decl);
            }
        }

        None
    }

    pub fn find_forward_decl(&self, lvalue: ast::Ident) -> Option<Var> {
        for scope in self.scopes.iter().rev() {
            // Check if we are shadowing another variable.
            if let Some(forward_decl) = scope.forward_decls.get(&lvalue) {
                return Some(forward_decl.decl);
            }
        }

        None
    }

    pub fn terminate(&mut self,
                     span: Span,
                     block: BasicBlock,
                     kind: TerminatorKind) {
        self.cfg.terminate(span, block, kind)
    }

    /// This function constructs a vector of all of the variables in scope, and returns if the
    /// variables are currently shadowed.
    pub fn find_live_decls(&self) -> Vec<LiveDecl> {
        let mut decls = vec![];
        let mut visited_decls = HashSet::new();

        // We build up the list of declarations by walking up the scopes, and walking through each
        // scope backwards. If this is the first time we've seen a variable with this name, we just
        // add it to our list. However, if we've seen it before, then we need to rename it so that
        // it'll be accessible once the shading variable goes out of scope.
        for scope in self.scopes.iter().rev() {
            for dropped_decl in scope.drops.iter().rev() {
                let decl = dropped_decl.decl;
                let ident = self.var_decls[decl].ident;

                if visited_decls.insert(ident) {
                    if scope.moved_decls.contains(&decl) {
                        debug!("find_live_decls: decl moved {:?}", decl);
                        decls.push(LiveDecl::Moved(decl));
                    } else {
                        decls.push(LiveDecl::Active(decl));
                    }
                }
            }
        }

        decls.reverse();
        debug!("find_live_decls: live decls: {:?}", decls);
        decls
    }

    /// Indicates that `lvalue` should be dropped on exit from
    /// `extent`.
    pub fn schedule_drop(&mut self, span: Span, extent: CodeExtent, decl: Var) {
        // FIXME: Make sure we aren't double dropping a variable.
        for scope in self.scopes.iter_mut().rev() {
            for drop in &scope.drops {
                if drop.decl == decl {
                    self.cx.span_bug(
                        span,
                        &format!("variable already scheduled for drop: {:?}", decl));
                }
            }
        }
        for scope in self.scopes.iter_mut().rev() {
            if scope.extent == extent {
                scope.drops.push(DropData {
                    span: span,
                    decl: decl,
                });
                return;
            }
        }
        self.cx.span_bug(span,
                         &format!("extent {:?} not in scope to drop {:?}", extent, decl));
    }

    pub fn schedule_move(&mut self, decl: Var) {
        for scope in self.scopes.iter_mut().rev() {
            scope.moved_decls.insert(decl);
        }
    }

    pub fn add_decls_from_pats<'c, I>(&mut self,
                                      extent: CodeExtent,
                                      block: BasicBlock,
                                      pats: I)
        where I: Iterator<Item=&'c P<ast::Pat>>,
    {
        for pat in pats {
            for decl in self.add_decls_from_pat(pat.span, extent, pat) {
                self.cfg.block_data_mut(block).decls.push(LiveDecl::Active(decl));
            }
        }
    }

    pub fn add_decls_from_pat(&mut self,
                              span: Span,
                              extent: CodeExtent,
                              pat: &P<ast::Pat>) -> Vec<Var> {
        let decls = self.get_decls_from_pat(pat);

        for decl in &decls {
            self.schedule_drop(span, extent, *decl);
        }

        decls
    }

    pub fn get_decls_from_pat(&mut self, pat: &ast::Pat) -> Vec<Var> {
        struct Visitor<'a, 'b: 'a, 'c: 'b> {
            builder: &'a mut Builder<'b, 'c>,
            new_vars: Vec<Var>,
        }

        impl<'a, 'b: 'a, 'c: 'b> visit::Visitor for Visitor<'a, 'b, 'c> {
            fn visit_pat(&mut self, pat: &ast::Pat) {
                match pat.node {
                    PatKind::Ident(ast::BindingMode::ByValue(mutability), id, _) => {
                        // Consider only lower case identities as a variable.
                        let id_str = id.node.name.as_str();
                        let first_char = id_str.chars().next().unwrap();

                        if first_char == first_char.to_ascii_lowercase() {
                            let var = self.builder.declare_decl(
                                mutability,
                                id.node,
                                None,
                            );
                            self.new_vars.push(var);
                        }
                    }
                    PatKind::Ident(..) => {
                        self.builder.cx.span_bug(
                            pat.span,
                            &format!("Canot handle pat {:?}", pat))
                    }
                    _ => { }
                }

                visit::walk_pat(self, pat);
            }

            fn visit_mac(&mut self, _mac: &ast::Mac) { }
        }

        let mut visitor = Visitor {
            builder: self,
            new_vars: vec![],
        };

        visit::Visitor::visit_pat(&mut visitor, pat);

        visitor.new_vars
    }

    pub fn get_decls_from_expr(&self, expr: &P<ast::Expr>) -> Vec<Var> {
        struct Visitor<'a, 'b: 'a> {
            builder: &'a Builder<'a, 'b>,
            var_decls: Vec<Var>,
        }

        impl<'a, 'b: 'a> visit::Visitor for Visitor<'a, 'b> {
            fn visit_expr(&mut self, expr: &ast::Expr) {
                debug!("get_decls_from_expr: {:?}", expr.node);

                if let ast::ExprKind::Path(None, ref path) = expr.node {
                    if let Some(decl) = self.builder.get_decl_from_path(path) {
                        debug!("get_decls_from_expr: decl `{:?}", decl);
                        self.var_decls.push(decl);
                    }
                }

                visit::walk_expr(self, expr);
            }

            fn visit_mac(&mut self, _mac: &ast::Mac) { }
        }

        let mut visitor = Visitor {
            builder: self,
            var_decls: Vec::new(),
        };

        visit::Visitor::visit_expr(&mut visitor, expr);

        visitor.var_decls
    }

    pub fn get_decl_from_path(&self, path: &ast::Path) -> Option<Var> {
        if !path.global && path.segments.len() == 1 {
            let segment = &path.segments[0];

            if segment.parameters.is_empty() {
                debug!("get_decls_from_path: {:?}", segment.identifier);
                return self.find_decl(segment.identifier);
            }
        }

        None
    }
}

fn drop_decl(cfg: &mut CFG,
             block: BasicBlock,
             scope: &Scope,
             dropped_decl: &DropData) {
    let moved = scope.moved_decls.contains(&dropped_decl.decl);
    cfg.push_drop(
        block,
        dropped_decl.span,
        dropped_decl.decl,
        moved,
    );
}
