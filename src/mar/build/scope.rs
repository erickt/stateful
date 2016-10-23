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
    forward_decls: HashMap<ast::Ident, Var>,
    drops: Vec<Var>,
    moved_decls: HashSet<Var>,
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
            drop_decl(&mut self.cfg, block, &scope, *dropped_decl);
        }
    }

    /// Returns if we are currently in a loop.
    pub fn is_in_loop(&self) -> bool {
        !self.loop_scopes.is_empty()
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
            for var in &scope.drops {
                drop_decl(&mut self.cfg, block, scope, *var);
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

    pub fn extent_of_innermost_scope(&self) -> CodeExtent {
        self.scopes.last().map(|scope| scope.extent).unwrap()
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
                        forward_decl,
                    );

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
            for var in scope.drops.iter().rev() {
                if lvalue == self.var_decls[*var].ident {
                    return Some(*var);
                }
            }

            if let Some(var) = scope.forward_decls.get(&lvalue) {
                return Some(*var);
            }
        }

        None
    }

    pub fn find_forward_decl(&self, lvalue: ast::Ident) -> Option<Var> {
        for scope in self.scopes.iter().rev() {
            // Check if we are shadowing another variable.
            if let Some(var) = scope.forward_decls.get(&lvalue) {
                return Some(*var);
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
            debug!("find_live_decls: scope={:?}", scope);

            for var in scope.drops.iter().rev() {
                let ident = self.var_decls[*var].ident;

                if visited_decls.insert(ident) {
                    if scope.forward_decls.contains_key(&ident) {
                        decls.push(LiveDecl::Forward(*var));
                    } else if scope.moved_decls.contains(var) {
                        decls.push(LiveDecl::Moved(*var));
                    } else {
                        decls.push(LiveDecl::Active(*var));
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
    pub fn schedule_drop(&mut self, span: Span, extent: CodeExtent, var: Var) {
        // FIXME: Make sure we aren't double dropping a variable.
        for scope in self.scopes.iter_mut().rev() {
            for drop in &scope.drops {
                if var == *drop {
                    self.cx.span_bug(
                        span,
                        &format!("variable already scheduled for drop: {:?}", var));
                }
            }
        }

        if let Some(scope) = self.scopes.last_mut() {
            let ident = self.var_decls[var].ident;

            scope.forward_decls.insert(ident, var);
        } else {
            self.cx.span_bug(span, "no scopes?");
        }

        for scope in self.scopes.iter_mut().rev() {
            if scope.extent == extent {
                scope.drops.push(var);
                return;
            }
        }
        self.cx.span_bug(span,
                         &format!("extent {:?} not in scope to drop {:?}", extent, var));
    }

    pub fn schedule_move(&mut self, decl: Var) {
        for scope in self.scopes.iter_mut().rev() {
            scope.moved_decls.insert(decl);
        }
    }

    pub fn add_decls_from_pats<'c, I>(&mut self, block: BasicBlock, pats: I)
        where I: Iterator<Item=&'c P<ast::Pat>>,
    {
        for pat in pats {
            for var in self.add_decls_from_pat(pat) {
                let ident = self.var_decls[var].ident;

                self.cfg.block_data_mut(block).decls.push(LiveDecl::Active(var));
                self.scopes.last_mut().unwrap().forward_decls.remove(&ident);
            }
        }
    }

    pub fn add_decls_from_pat(&mut self, pat: &P<ast::Pat>) -> Vec<Var> {
        self.get_decls_from_pat(pat, None)
    }

    pub fn get_decls_from_pat(&mut self,
                              pat: &ast::Pat,
                              ty: Option<P<ast::Ty>>) -> Vec<Var> {
        struct Visitor<'a, 'b: 'a, 'c: 'b> {
            builder: &'a mut Builder<'b, 'c>,
            new_vars: Vec<Var>,
            ty: Option<P<ast::Ty>>,
        }

        impl<'a, 'b: 'a, 'c: 'b> visit::Visitor for Visitor<'a, 'b, 'c> {
            fn visit_pat(&mut self, pat: &ast::Pat) {
                match pat.node {
                    PatKind::Ident(ast::BindingMode::ByValue(mutability), id, _) => {
                        // Consider only lower case identities as a variable.
                        let id_str = id.node.name.as_str();
                        let first_char = id_str.chars().next().unwrap();

                        if first_char == first_char.to_ascii_lowercase() {
                            let var = self.builder.declare_binding(
                                pat.span,
                                mutability,
                                id.node,
                                self.ty.clone(),
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
            ty: ty,
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

fn drop_decl(cfg: &mut CFG, block: BasicBlock, scope: &Scope, var: Var) {
    let moved = scope.moved_decls.contains(&var);
    cfg.push_drop(block, var, moved);
}
