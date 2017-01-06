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

use build::{BlockAnd, BlockAndExtension, Builder, CFG, ScopeAuxiliary, ScopeId};
use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::{BTreeSet, HashSet};
use std::mem;
use syntax::ast;
use syntax::codemap::Span;

#[derive(Debug)]
pub struct Scope {
    /// the scope-id within the scope_auxiliary
    id: ScopeId,

    /// The visibility scope this scope was created in.
    visibility_scope: VisibilityScope,

    extent: CodeExtent,

    /// Declarations created in this scope.
    decls: HashSet<Local>,

    /// Declarations that are initialized, and may be in this scope or a parent scope.
    initialized_decls: HashSet<Local>,

    /// set of lvalues to drop when exiting this scope. This starts
    /// out empty but grows as variables are declared during the
    /// building process. This is a stack, so we always drop from the
    /// end of the vector (top of the stack) first.
    drops: BTreeSet<Local>,

    moved_decls: HashSet<Local>,

    /// Any conditionally initialized variables from this or other scopes that were modified in
    /// this scope.
    conditionals: Vec<ConditionalScope>,
}

impl Scope {
    /// Given a span and this scope's visibility scope, make a SourceInfo.
    fn source_info(&self, span: Span) -> SourceInfo {
        SourceInfo {
            span: span,
            scope: self.visibility_scope
        }
    }
}

#[derive(Debug)]
pub struct ConditionalScope {
    initialized_decls: HashSet<Local>,
    moved_decls: HashSet<Local>,
}

impl ConditionalScope {
    fn new() -> Self {
        ConditionalScope {
            initialized_decls: HashSet::new(),
            moved_decls: HashSet::new(),
        }
    }
}

#[derive(Clone)]
pub struct LoopScope {
    /// Extent of the loop
    pub extent: CodeExtent,
    pub label: Option<ast::SpannedIdent>,
    /// Where the body of the loop begins
    pub continue_block: BasicBlock,
    /// Block to branch into when the loop terminates (either by being `break`-en out from, or by
    /// having its condition to become false)
    pub break_block: BasicBlock,
    /// Indicates the reachability of the break_block for this loop
    pub might_break: bool
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Start a loop scope, which tracks where `continue` and `break`
    /// should branch to. See module comment for more details.
    pub fn in_loop_scope<F>(&mut self,
                            label: Option<ast::SpannedIdent>,
                            loop_block: BasicBlock,
                            break_block: BasicBlock,
                            f: F) -> bool
        where F: FnOnce(&mut Builder)
    {
        debug!("in_loop_scope(label={:?}, loop_block={:?}, break_block={:?})", label, loop_block, break_block);

        let extent = self.extent_of_innermost_scope();
        let loop_scope = LoopScope {
            extent: extent.clone(),
            label: label,
            continue_block: loop_block,
            break_block: break_block,
            might_break: false
        };
        self.loop_scopes.push(loop_scope);
        f(self);
        let loop_scope = self.loop_scopes.pop().unwrap();
        assert!(loop_scope.extent == extent);
        loop_scope.might_break
    }

    /// Start a loop scope, which tracks where `continue` and `break`
    /// should branch to. See module comment for more details.
    pub fn in_conditional_scope<F, T>(&mut self, span: Span, f: F) -> T
        where F: FnOnce(&mut Builder) -> T
    {
        debug!("in_conditional_scope");

        let scope_id = self.scopes.last().unwrap().id;

        let result = f(self);

        let conditionals = {
            // First, make sure we've got the same scope we started with.
            let scope = self.scopes.last_mut().unwrap();

            if scope_id != scope.id {
                span_bug!(self.cx, span, "expected scope {:?}, not scope {:?}", scope_id, scope.id);
            }

            // Next, grab all the conditionals that were just created.
            mem::replace(&mut scope.conditionals, vec![])
        };

        let mut iter = conditionals.iter();

        // We need to make sure all the conditional scopes initialized the same variables. We'll
        // do this by grabbing the first one and then checking the rest initialized the same
        // things.
        let conditional = match iter.next() {
            Some(conditional) => conditional,
            None => {
                span_bug!(self.cx, span, "there should have been conditionals");
            }
        };

        for c in iter {
            if conditional.initialized_decls != c.initialized_decls {
                span_err!(self.cx, span,
                          "some variables not conditionally initialized? {:#?} != {:#?}",
                          conditional.initialized_decls,
                          c.initialized_decls);
            }

            if conditional.moved_decls != c.moved_decls {
                span_err!(self.cx, span,
                          "some variables not conditionally moved? {:#?} != {:#?}",
                          conditional.moved_decls,
                          c.moved_decls);
            }
        }

        // Finally, push down the conditionally initialized and moved variables.
        for local in &conditional.initialized_decls {
            self.initialize_decl(*local);
        }

        result
    }

    pub fn next_conditional_scope(&mut self, span: Span) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.conditionals.push(ConditionalScope::new());
        } else {
            span_bug!(self.cx, span, "scope is not currently conditional?");
        }
    }

    /// Convenience wrapper that pushes a scope and then executes `f`
    /// to build its contents, popping the scope afterwards.
    pub fn in_scope<F, R>(&mut self,
                          extent: CodeExtent,
                          span: Span,
                          mut block: BasicBlock,
                          f: F) -> BlockAnd<R>
        where F: FnOnce(&mut Builder) -> BlockAnd<R>
    {
        debug!("in_scope(extent={:?}, block={:?})", extent, block);
        self.push_scope(extent, span, block);
        let rv = unpack!(block = f(self));
        unpack!(block = self.pop_scope(extent, span, block));
        debug!("in_scope: exiting extent={:?} block={:?}", extent, block);

        block.and(rv)
    }

    /// Push a scope onto the stack. You can then build code in this
    /// scope and call `pop_scope` afterwards. Note that these two
    /// calls must be paired; using `in_scope` as a convenience
    /// wrapper maybe preferable.
    pub fn push_scope(&mut self,
                      extent: CodeExtent,
                      _span: Span,
                      _block: BasicBlock) {
        debug!("push_scope({:?})", extent);

        let id = ScopeId::new(self.scope_auxiliary.len());
        let vis_scope = self.visibility_scope;
        self.scopes.push(Scope {
            id: id,
            visibility_scope: vis_scope,
            extent: extent,
            decls: HashSet::new(),
            initialized_decls: HashSet::new(),
            moved_decls: HashSet::new(),
            drops: BTreeSet::new(),
            conditionals: vec![],
        });

        self.scope_auxiliary.push(ScopeAuxiliary {
            extent: extent,
        });
    }

    /// Pops a scope, which should have extent `extent`, adding any
    /// drops onto the end of `block` that are needed.  This must
    /// match 1-to-1 with `push_scope`.
    pub fn pop_scope(&mut self,
                     extent: CodeExtent,
                     span: Span,
                     mut block: BasicBlock)
                     -> BlockAnd<()> {
        let scope = self.scopes.pop().unwrap();
        debug!("pop_scope: extent={:?} block={:?} scope={:#?}", extent, block, scope);

        assert_eq!(scope.extent, extent);
        assert!(scope.conditionals.is_empty());

        // add in any drops needed on the fallthrough path (any other
        // exiting paths, such as those that arise from `break`, will
        // have drops already)
        for local in scope.drops.iter().rev() {
            // FIXME: Make sure we aren't double dropping a variable.
            for scope in self.scopes.iter_mut().rev() {
                if scope.drops.contains(&local) {
                    span_err!(self.cx, span,
                              "variable already scheduled for drop: {:?}",
                              local);
                }
            }

            let source_info = scope.source_info(span);
            unpack!(block = build_scope_drops(&mut self.cfg, block, &scope, source_info, *local));
        }

        debug!("pop_scope: remaining scopes={:#?}", self.scopes); 

        block.unit()
    }

    /// Creates a new visibility scope, nested in the current one.
    pub fn new_visibility_scope(&mut self, span: Span) -> VisibilityScope {
        let parent = self.visibility_scope;
        let scope = VisibilityScope::new(self.visibility_scopes.len());
        self.visibility_scopes.push(VisibilityScopeData {
            span: span,
            parent_scope: Some(parent),
        });
        scope
    }

    /// Returns if we are currently in a loop.
    pub fn is_in_loop(&self) -> bool {
        !self.loop_scopes.is_empty()
    }

    /// Finds the loop scope for a given label. This is used for
    /// resolving `break` and `continue`.
    pub fn find_loop_scope(&mut self,
                           span: Span,
                           label: Option<ast::SpannedIdent>) -> &mut LoopScope {
        let loop_scopes = &mut self.loop_scopes;
        let loop_scope = match label {
            None => {
                // no label? return the innermost loop scope
                loop_scopes.iter_mut().rev().next()
            }
            Some(label) => {
                // otherwise, find the loop-scope with the correct id
                loop_scopes.iter_mut()
                           .rev()
                           .filter(|loop_scope| loop_scope.label == Some(label))
                           .next()
            }
        };

        match loop_scope {
            Some(loop_scope) => loop_scope,
            None => {
                span_bug!(self.cx, span, "no enclosing loop scope found?")
            }
        }
    }

    /// Given a span and the current visibility scope, make a SourceInfo.
    pub fn source_info(&self, span: Span) -> SourceInfo {
        SourceInfo {
            span: span,
            scope: self.visibility_scope
        }
    }

    /// Branch out of `block` to `target`, exiting all scopes up to
    /// and including `extent`.  This will insert whatever drops are
    /// needed, as well as tracking this exit for the SEME region. See
    /// module comment for details.
    pub fn exit_scope(&mut self,
                      span: Span,
                      extent: CodeExtent,
                      mut block: BasicBlock,
                      target: BasicBlock,
                      mut phantom_target: Option<BasicBlock>) {
        debug!("exit_scope(extent={:?}, block={:?}, target={:?}, phantom_target={:?})",
               extent,
               block,
               target,
               phantom_target);
        let scope_count = 1 + self.scopes.iter().rev().position(|scope| scope.extent == extent)
                                                      .unwrap_or_else(||{
            span_bug!(self.cx, span, "extent {:?} does not enclose", extent)
        });
        let len = self.scopes.len();
        assert!(scope_count < len, "should not use `exit_scope` to pop ALL scopes");

        {
            for scope_index in (len - scope_count + 1 .. len).rev() {
                block = {
                    let b = self.cfg.start_new_block(span, Some("Drop"));
                    self.terminate(span, block, TerminatorKind::Goto {
                        target: b,
                        phantom_target: phantom_target.take(),
                    });
                    b
                };

                let scope = &self.scopes[scope_index];
                debug!("exit_scope: dropping: block={:?} scope={:#?}", block, scope);
                let source_info = scope.source_info(span);

                for local in &scope.drops {
                    // FIXME: Make sure we aren't double dropping a variable.
                    for scope in self.scopes[..scope_index].iter().rev() {
                        if scope.drops.contains(&local) {
                            span_err!(self.cx, span,
                                      "variable already scheduled for drop: {:?}",
                                      local);
                        }
                    }

                    // If the variable has already been initialized, drop it. Otherwise we want the rust
                    // warning if a variable hasn't been initialized, so just insert the declaration into
                    // our block.
                    if scope.initialized_decls.contains(local) {
                        unpack!(block = build_scope_drops(
                            &mut self.cfg,
                            block,
                            scope,
                            source_info,
                            *local));
                    }
                }
            }
        }

        self.terminate(span, block, TerminatorKind::Goto {
            target: target,
            phantom_target: phantom_target.take(),
        });
    }

    pub fn extent_of_innermost_scope(&self) -> CodeExtent {
        self.scopes.last().map(|scope| scope.extent).unwrap()
    }

    /// Returns the extent of the scope which should be exited by a
    /// return.
    pub fn extent_of_return_scope(&self) -> CodeExtent {
        // The outermost scope (`scopes[0]`) will be the `CallSiteScope`.
        // We want `scopes[1]`, which is the `ParameterScope`.
        assert!(self.scopes.len() >= 2);
        assert!(match self.extents[self.scopes[1].extent] {
            CodeExtentData::ParameterScope { .. } => true,
            _ => false,
        });
        self.scopes[1].extent
    }

    /// Mark the lvalue initialized. This assumes that the lvalue has been previously declared.
    pub fn initialize(&mut self,
                      block: BasicBlock,
                      span: Span,
                      lvalue: &Lvalue) {
        debug!("initialize: block={:?} lvalue={:?}", block, lvalue);

        match *lvalue {
            Lvalue::Local(local) => {
                if !self.is_initialized(local) {
                    self.initialize_decl(local);
                }
            }
            _ => {
                span_bug!(self.cx, span, "cannot initialize yet: {:?}", lvalue);
            }
        }
    }

    pub fn push_assign(&mut self,
                       block: BasicBlock,
                       span: Span,
                       lvalue: &Lvalue,
                       rvalue: Rvalue) {
        debug!("push_assign: block={:?} lvalue={:?} rvalue={:?}", block, lvalue, rvalue);

        match *lvalue {
            Lvalue::Local(_) => {
                let source_info = self.source_info(span);

                self.cfg.push(block, Statement {
                    source_info: source_info,
                    kind: StatementKind::Assign(lvalue.clone(), rvalue),
                });
            }
            _ => {
                span_bug!(self.cx, span, "cannot assign yet: {:?}", lvalue)
            }
        }
    }

    pub fn push_assign_unit(&mut self,
                            span: Span,
                            block: BasicBlock,
                            lvalue: &Lvalue) {
        let rvalue = self.unit_rvalue();
        self.push_assign(block, span, lvalue, rvalue)
    }

    /// Walk up the scopes to discover if this variable has been initialized.
    fn is_initialized(&mut self, var: Local) -> bool {
        for scope in self.scopes.iter_mut().rev() {
            debug!("is_initialized: scope={:?}", scope.id);

            // Don't consider a variable initialized if the `scope` is currently conditional.
            if scope.initialized_decls.contains(&var) {
                if self.conditional_scopes.contains_key(&scope.id) {
                    debug!("is_initialized: scope is conditional: {:?}", scope.id);
                    return false;
                } else {
                    debug!("is_initialized: var {:?} already initialized", var);
                    return true;
                }
            }

            if scope.decls.contains(&var) {
                //debug!("is_initialized: found scope at {:?}", scope.id);
                return false;
            }
        }

        span_warn!(self.cx, self.local_decls[var].source_info.span,
                   "var {:?} not in any scope?", var);

        true
    }

    /// Mark a variable initialized.
    fn initialize_decl(&mut self, var: Local) {
        let ident = self.local_decls[var].name;
        debug!("initialize_decl: scope={:?} var={:?} ident={:?}", self.scopes.last().unwrap().id, var, ident);
        
        for scope in self.scopes.iter_mut().rev() {
            // If the scope is conditional, buffer it there instead of pushing it up the scope.
            if let Some(conditional) = scope.conditionals.last_mut() {
                debug!("initialize_decl: found conditional scope at {:?}", scope.id);

                conditional.initialized_decls.insert(var);

                return;

            // Otherwise initialize it in the stack that created the variable.
            } else if scope.decls.contains(&var) {
                debug!("initialize_decl: found scope at {:?}", scope.id);

                scope.initialized_decls.insert(var);

                return;
            }
        }

        self.cx.span_bug(self.local_decls[var].source_info.span,
                         &format!("var {:?} not in scope to initialize", var));
    }

    pub fn find_local(&self, ident: ast::Ident) -> Option<Local> {
        debug!("find_local: {:?} scopes={:#?}", ident, self.scopes); 

        for scope in self.scopes.iter().rev() {
            // Check first if the lvalue has been conditionally initialized.
            if let Some(conditional) = scope.conditionals.last() {
                for var in &conditional.initialized_decls {
                    if ident == self.local_decls[*var].name {
                        return Some(*var);
                    }
                }
            }

            // If not, check if we are shadowing another variable.
            for var in scope.drops.iter().rev() {
                if ident == self.local_decls[*var].name {
                    return Some(*var);
                }
            }
        }

        None
    }

    pub fn terminate(&mut self,
                     span: Span,
                     block: BasicBlock,
                     kind: TerminatorKind) {
        let source_info = self.source_info(span);
        self.cfg.terminate(block, source_info, kind);
    }

    /// Indicates that `lvalue` should be dropped on exit from
    /// `extent`.
    pub fn schedule_drop(&mut self,
                         span: Span,
                         extent: CodeExtent,
                         lvalue: &Lvalue) {
        debug!("schedule_drop(extent={:?}, lvalue={:?})", extent, lvalue);

        // Only temps and vars need their storage dead.
        let local = match *lvalue {
            Lvalue::Local(local) => local,
            _ => {
                debug!("schedule_drop: lvalue is not local, ignoring");
                return;
            }
        };

        // FIXME: Make sure we aren't double dropping a variable.
        for scope in self.scopes.iter_mut().rev() {
            if scope.drops.contains(&local) {
                span_err!(self.cx,
                          span,
                          "variable already scheduled for drop: {:?}",
                          local);
            }
        }

        if let Some(scope) = self.scopes.last_mut() {
            scope.decls.insert(local);
        } else {
            self.cx.span_bug(span, "no scopes?");
        }

        for scope in self.scopes.iter_mut().rev() {
            if scope.extent == extent {
                if !scope.drops.insert(local) {
                    span_err!(self.cx,
                              span,
                              "variable already scheduled for drop: {:?}",
                              local);

                }

                return;
            }
        }

        span_bug!(self.cx, span, "extent {:?} not in scope to drop {:?}", extent, lvalue);
    }

    pub fn get_local_from_path(&self, path: &ast::Path) -> Option<Local> {
        if !path.is_global() && path.segments.len() == 1 {
            let segment = &path.segments[0];

            if segment.parameters.is_none() {
                debug!("get_local_from_path: {:?}", segment.identifier);
                return self.find_local(segment.identifier);
            }
        }

        None
    }
}

/// Builds drops for pop_scope and exit_scope.
fn build_scope_drops(cfg: &mut CFG,
                     block: BasicBlock,
                     scope: &Scope,
                     source_info: SourceInfo,
                     var: Local) -> BlockAnd<()> {
    debug!("build_scope_drops(block={:?}, scope={:?}, var={:?})", block, scope.id, var);
    let location = Lvalue::Local(var);

    cfg.push(block, Statement {
        source_info: source_info,
        kind: StatementKind::StorageDead(location),
    });

    block.unit()
}
