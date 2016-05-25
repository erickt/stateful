/**
### Early exit

There are numerous "normal" ways to early exit a scope: `break`, `continue`, `return`. Whenever an
early exit occurs, the method `exit_scope` is called. It is given the current point in execution
where the early exit occurs, as well as the scope you want to branch to (note that all early exits
from to some other enclosing scope). `exit_scope` will record thid exit point and also add all
drops.

### Loop scopes

In addition to the normal scope stack, we track a loop scope stack that contains only loops. It
tracks where a `break` and `continue` should go to.
*/

use aster::AstBuilder;
use mar::build::Builder;
use mar::repr::*;
use std::ascii::AsciiExt;
use std::collections::HashSet;
use syntax::ast::{self, PatKind};
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::visit;

#[derive(Debug)]
pub struct Scope {
    extent: CodeExtent,
    drops: Vec<(Span, VarDecl, Option<Alias>)>,
}

#[derive(Clone)]
pub struct LoopScope {
    pub extent: CodeExtent,
    pub label: Option<ast::Ident>,
    pub continue_block: BasicBlock,
    pub break_block: BasicBlock,
}

impl<'a> Builder<'a> {
    /// Start a loop scope, which tracks where `continue` and `break`
    /// should branch to. See module comment for more details.
    pub fn in_loop_scope<F>(&mut self,
                            extent: CodeExtent,
                            label: Option<ast::Ident>,
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
                       mut block: BasicBlock,
                       f: F) -> BasicBlock
        where F: FnOnce(&mut Builder) -> BasicBlock
    {
        self.push_scope(extent, block);
        block = f(self);
        self.pop_scope(extent, block);
        block
    }

    /// Push a scope onto the stack. You can then build code in this
    /// scope and call `pop_scope` afterwards. Note that these two
    /// calls must be paired; using `in_scope` as a convenience
    /// wrapper maybe preferable.
    pub fn push_scope(&mut self, extent: CodeExtent, _block: BasicBlock) {
        self.scopes.push(Scope {
            extent: extent,
            drops: vec![],
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
        for (span, decl, alias) in scope.drops.into_iter().rev() {
            self.cfg.push_drop(block, span, decl, alias);
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
                                    .filter(|loop_scope| {
                                        match loop_scope.label {
                                            Some(l) => l == label,
                                            None => false,
                                        }
                                    })
                                    .next()
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
            for &(drop_span, lvalue, alias) in &scope.drops {
                self.cfg.push_drop(block, drop_span, lvalue, alias);
            }
        }

        self.terminate(block, Terminator::Goto { target: target });
    }

    pub fn find_decl(&self, lvalue: ast::Ident) -> Option<VarDecl> {
        for scope in self.scopes.iter().rev() {
            // Check if we are shadowing another variable.
            for &(_, decl, _) in scope.drops.iter().rev() {
                let decl_data = self.cfg.var_decl_data(decl);
                if lvalue == decl_data.ident {
                    return Some(decl);
                }
            }
        }

        None
    }

    pub fn terminate(&mut self, block: BasicBlock, terminator: Terminator) {
        self.cfg.terminate(block, terminator)
    }

    /// This function constructs a vector of all of the variables in scope, and returns if the
    /// variables are currently shadowed.
    pub fn find_live_decls(&self) -> Vec<(VarDecl, ast::Ident)> {
        let mut decls = vec![];
        let mut visited_decls = HashSet::new();

        // We build up the list of declarations by walking up the scopes, and walking through each
        // scope backwards. If this is the first time we've seen a variable with this name, we just
        // add it to our list. However, if we've seen it before, then we need to rename it so that
        // it'll be accessible once the shading variable goes out of scope.
        for scope in self.scopes.iter().rev() {
            for &(_, decl, _) in scope.drops.iter().rev() {
                let ident = self.cfg.var_decl_data(decl).ident;

                if visited_decls.insert(ident) {
                    // We haven't seen this decl before, so keep it's name.
                    decls.push((decl, ident));
                } else {
                    // Otherwise, we need to rename it.
                    let shadowed_ident = AstBuilder::new().id(
                        format!("{}_shadowed_{}", ident, decl.index()));

                    decls.push((decl, shadowed_ident));
                }
            }
        }

        decls.reverse();
        decls
    }

    /// Indicates that `lvalue` should be dropped on exit from
    /// `extent`.
    pub fn schedule_drop(&mut self,
                         span: Span,
                         extent: CodeExtent,
                         decl: VarDecl,
                         alias: Option<Alias>) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.extent == extent {
                scope.drops.push((span, decl, alias));
                return;
            }
        }
        self.cx.span_bug(span,
                         &format!("extent {:?} not in scope to drop {:?}", extent, decl));
    }

    pub fn add_decls_from_pats<'b, I>(&mut self,
                                      span: Span,
                                      extent: CodeExtent,
                                      block: BasicBlock,
                                      pats: I)
        where I: Iterator<Item=&'b P<ast::Pat>>,
    {
        for pat in pats {
            let decls = self.add_decls_from_pat(span, extent, pat);
            self.cfg.block_data_mut(block).decls.extend(decls);
        }
    }

    pub fn add_decls_from_pat(&mut self,
                              span: Span,
                              extent: CodeExtent,
                              pat: &P<ast::Pat>) -> Vec<(VarDecl, ast::Ident)> {
        let decls = self.get_decls_from_pat(pat);

        for &(decl, _) in decls.iter() {
            self.schedule_drop(span, extent, decl, None);
        }

        decls
    }

    pub fn get_decls_from_pat(&mut self, pat: &ast::Pat) -> Vec<(VarDecl, ast::Ident)> {
        struct Visitor<'a, 'b: 'a> {
            builder: &'a mut Builder<'b>,
            var_decls: Vec<(VarDecl, ast::Ident)>,
        }

        impl<'a, 'b, 'c> visit::Visitor<'a> for Visitor<'b, 'c> {
            fn visit_pat(&mut self, pat: &ast::Pat) {
                match pat.node {
                    PatKind::Ident(ast::BindingMode::ByValue(mutability), id, _) => {
                        // Consider only lower case identities as a variable.
                        let id_str = id.node.name.as_str();
                        let first_char = id_str.chars().next().unwrap();

                        if first_char == first_char.to_ascii_lowercase() {
                            let decl = self.builder.cfg.push_decl(mutability, id.node);
                            self.var_decls.push((decl, id.node));
                        }
                    }
                    PatKind::Ident(..) => {
                        self.builder.cx.span_bug(pat.span,
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
            var_decls: Vec::new(),
        };

        visit::Visitor::visit_pat(&mut visitor, pat);

        visitor.var_decls
    }

}
