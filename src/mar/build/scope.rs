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

use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;

pub struct Scope {
    extent: CodeExtent,
    drops: Vec<(Span, ast::Ident)>,
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
        for (span, lvalue) in scope.drops {
            self.cfg.push_drop(block, span, lvalue);
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
            for &(drop_span, lvalue) in &scope.drops {
                self.cfg.push_drop(block, drop_span, lvalue);
            }
        }

        self.cfg.terminate(block, Terminator::Goto { target: target })
    }

    /// Indicates that `lvalue` should be dropped on exit from
    /// `extent`.
    pub fn schedule_drop(&mut self,
                         span: Span,
                         extent: CodeExtent,
                         lvalue: ast::Ident) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.extent == extent {
                scope.drops.push((span, lvalue.clone()));
                return;
            }
        }
        self.cx.span_bug(span,
                          &format!("extent {:?} not in scope to drop {:?}", extent, lvalue));
    }
}
