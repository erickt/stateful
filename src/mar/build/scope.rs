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

use mar::build::{BlockAnd, BlockAndExtension, Builder, CFG, ScopeAuxiliary, ScopeId};
use mar::indexed_vec::Idx;
use mar::repr::*;
use std::ascii::AsciiExt;
use std::collections::{HashMap, HashSet};
use std::fmt;
use syntax::ast::{self, PatKind};
use syntax::codemap::Span;
use syntax::ptr::P;
use syntax::visit;

//#[derive(Debug)]
pub struct Scope {
    /// the scope-id within the scope_auxiliary
    id: ScopeId,

    kind: ScopeKind,

    extent: CodeExtent,

    /// Declarations created in this scope.
    decls: HashSet<Local>,

    /// Uninitialized declarations in this scope
    forward_decls: HashMap<ast::Ident, Local>,

    /// Declarations that are initialized, and may be in this scope or a parent scope.
    initialized_decls: HashSet<Local>,

    drops: Vec<Local>,
    moved_decls: HashSet<Local>,
}

impl fmt::Debug for Scope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scope({:?}, decls={:?} forward={:?} init={:?}, drops={:?}, kind={:?})",
               self.id,
               self.decls,
               self.forward_decls,
               self.initialized_decls,
               self.drops,
               self.kind)
    }
}

#[derive(Debug)]
enum ScopeKind {
    Normal,
}

#[derive(Clone)]
pub struct LoopScope {
    pub extent: CodeExtent,
    pub label: Option<ast::SpannedIdent>,
    pub continue_block: BasicBlock,
    pub break_block: BasicBlock,
}

pub struct ConditionalScope {
    initialized_decls: Vec<HashSet<Local>>,
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Start a loop scope, which tracks where `continue` and `break`
    /// should branch to. See module comment for more details.
    pub fn in_loop_scope<F>(&mut self,
                            label: Option<ast::SpannedIdent>,
                            loop_block: BasicBlock,
                            break_block: BasicBlock,
                            f: F)
        where F: FnOnce(&mut Builder)
    {
        let extent = self.extent_of_innermost_scope();
        let loop_scope = LoopScope {
            extent: extent.clone(),
            label: label,
            continue_block: loop_block,
            break_block: break_block,
        };
        self.loop_scopes.push(loop_scope);
        f(self);
        let loop_scope = self.loop_scopes.pop().unwrap();
        assert!(loop_scope.extent == extent);
    }

    /// Start a loop scope, which tracks where `continue` and `break`
    /// should branch to. See module comment for more details.
    pub fn in_conditional_scope<F, T>(&mut self, span: Span, f: F) -> T
        where F: FnOnce(&mut Builder) -> T
    {
        let scope_id = self.scopes.last().unwrap().id;

        debug!("\n+++++++");
        debug!("in_conditional_scope: ++++++++++ {:?}", scope_id);

        self.conditional_scopes.insert(scope_id, ConditionalScope {
            initialized_decls: vec![],
        });

        let result = f(self);

        let conditional_scopes = self.conditional_scopes.remove(&scope_id)
            .unwrap();

        debug!("in_conditional_scope: ---------- {:?}", scope_id);

        // check that the same variables have been initialized.
        let mut iter = conditional_scopes.initialized_decls.iter();
        let initialized_decls = iter.next().unwrap();

        if iter.any(|decls| decls != initialized_decls) {
            self.cx.span_err(
                span,
                &format!("some variables not conditionally initialized? {:#?}",
                         conditional_scopes.initialized_decls));
        }

        // The conditionally initialized variables should be initialized.
        for var in initialized_decls {
            self.initialize_decl(*var);
        }

        debug!("-------\n");

        result


        /*
        self.push_scope_kind(extent, ScopeKind::Conditional {
            scopes: vec![],
        });
        let block = f(self);

        {
            let current_scope = self.scopes.last_mut().unwrap();

            match current_scope.kind {
                ScopeKind::Conditional { ref scopes } => {
                    debug!("in_conditional_scope: {:?}", scopes);

                    for scope in scopes {
                        for var in &scope.initialized_decls {
                            let ident = self.local_decls[*var].ident;
                            current_scope.forward_decls.remove(&ident);

                            if !current_scope.decls.contains(var) {
                                current_scope.initialized_decls.insert(*var);
                            }
                        }
                    }
                }
                ScopeKind::Normal => unreachable!(),
            }
        }

        self.pop_scope(extent, block);

        for scope in self.scopes.iter().rev() {
            debug!("in_conditional_scope: scope={:?}", scope); 
        }

        debug!("in_conditional_scope: -------");

        block
        */
    }

    pub fn next_conditional_scope(&mut self) {
        println!("==========\nnext conditional scope\n=========");

        let scope_id = self.scopes.last().unwrap().id;
        let conditional_scope = self.conditional_scopes.get_mut(&scope_id).unwrap();
        conditional_scope.initialized_decls.push(HashSet::new());
    }

    /// Convenience wrapper that pushes a scope and then executes `f`
    /// to build its contents, popping the scope afterwards.
    pub fn in_scope<F, R>(&mut self,
                          span: Span,
                          mut block: BasicBlock,
                          f: F) -> BlockAnd<R>
        where F: FnOnce(&mut Builder) -> BlockAnd<R>
    {
        let extent = self.start_new_extent();
        debug!("in_scope(extent={:?}, block={:?}", extent, block);

        self.push_scope(extent, block);
        let rv = unpack!(block = f(self));
        self.pop_scope(extent, block);
        debug!("in_scope: exiting extent={:?} block={:?}", extent, block);

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

        end_scope_block.and(rv)
    }

    /// Push a scope onto the stack. You can then build code in this
    /// scope and call `pop_scope` afterwards. Note that these two
    /// calls must be paired; using `in_scope` as a convenience
    /// wrapper maybe preferable.
    pub fn push_scope(&mut self, extent: CodeExtent, _block: BasicBlock) {
        self.push_scope_kind(extent, ScopeKind::Normal);
    }

    fn push_scope_kind(&mut self, extent: CodeExtent, kind: ScopeKind) {
        let id = ScopeId::new(self.scope_auxiliary.len());

        debug!("\n+++++++++");
        debug!("push_scope_kind: ++++++ id={:?} kind={:?}", id, kind);

        //if id.index() == 4 { panic!() }

        self.scopes.push(Scope {
            id: id,
            kind: kind,
            extent: extent,
            decls: HashSet::new(),
            forward_decls: HashMap::new(),
            initialized_decls: HashSet::new(),
            moved_decls: HashSet::new(),
            drops: vec![],
        });

        self.scope_auxiliary.push(ScopeAuxiliary {
            extent: extent,
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

        debug!("pop_scope: ------- id={:?}\n", scope.id);

        for scope in self.scopes.iter().rev() {
            debug!("in_scope: scope={:?}", scope); 
        }

        debug!("\n--------");

        /*
        // Bubble up the initializations.
        if let Some(current_scope) = self.scopes.last_mut() {
            debug!("pop_scope: {:?} scope={:?}", block, current_scope);

            match self.conditional_scopes.last() {
                Some(conditional_scope) if => {
                    if 
                }
            }


            for var in &scope.initialized_decls {
                let ident = self.local_decls[*var].ident;
                current_scope.forward_decls.remove(&ident);

                if !current_scope.decls.contains(var) {
                    current_scope.initialized_decls.insert(*var);
                }
            }
        }
        */

        /*
        if let Some(current_scope) = self.scopes.last_mut() {
            match current_scope.kind {
                ScopeKind::Normal => {
                    debug!("pop_scope: {:?} scope={:?}", block, current_scope);
                    for var in &scope.initialized_decls {
                        let ident = self.local_decls[*var].ident;
                        current_scope.forward_decls.remove(&ident);

                        if !current_scope.decls.contains(var) {
                            current_scope.initialized_decls.insert(*var);
                        }
                    }
                }
                ScopeKind::Conditional { ref mut scopes } => {
                    debug!("pop_scope: conditional: {:?}", scope);
                    scopes.push(scope);
                }
            }
        }
        */

    }

    /// Returns if we are currently in a loop.
    pub fn is_in_loop(&self) -> bool {
        !self.loop_scopes.is_empty()
    }

    /// Finds the loop scope for a given label. This is used for
    /// resolving `break` and `continue`.
    pub fn find_loop_scope(&mut self,
                           span: Span,
                           label: Option<ast::SpannedIdent>) -> LoopScope {
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
                                            Some(l) => l == label,
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

    pub fn assign_lvalue(&mut self,
                         block: BasicBlock,
                         span: Span,
                         lvalue: Lvalue,
                         rvalue: Rvalue) {
        debug!("assign_lvalue: block={:?} lvalue={:?} rvalue={:?}", block, lvalue, rvalue);

        let Lvalue::Local(local) = lvalue;

        if !self.is_initialized(local) {
            self.initialize_decl(local);
            self.cfg.push_declare_decl(block, local);
        }

        self.cfg.push_assign(block, span, lvalue, rvalue);
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

        self.cx.span_bug(self.local_decls[var].span,
                         &format!("var {:?} not in any scope?", var));
    }

    /// Mark a variable initialized.
    fn initialize_decl(&mut self, var: Local) {
        let ident = self.local_decls[var].ident;
        debug!("initialize_decl: scope={:?} var={:?} ident={:?}", self.scopes.last().unwrap().id, var, ident);
        
        for scope in self.scopes.iter_mut().rev() {
            // If the scope is conditional, buffer it there instead of pushing it up the scope.
            if let Some(conditional_scope) = self.conditional_scopes.get_mut(&scope.id) {
                debug!("initialize_decl: found cond scope at {:?}", scope.id);
                conditional_scope.initialized_decls.last_mut().unwrap().insert(var);

                return;
            } else {
                // Otherwise initialize it in this scope and keep moving up the stack.
                scope.initialized_decls.insert(var);
                scope.forward_decls.remove(&ident);

                if scope.decls.contains(&var) {
                    debug!("initialize_decl: found scope at {:?}", scope.id);
                    return;
                }
            }
        }

        /*
        for scope in self.scopes.iter_mut().rev() {
            scope.initialized_decls.insert(var);
            scope.forward_decls.remove(&ident);

            if scope.decls.contains(&var) {
                //debug!("initialize_decl: found scope at {:?}", scope.id);
                return;
            }
        }
        */

        /*
        // Only initialize if it hasn't already been initialized.
        for scope in self.scopes.iter_mut().rev() {
            debug!("initialize_decl: scope={:?}", scope);

            if let ScopeKind::Conditional { .. } = scope.kind {
                debug!("initialize_decl: conditional, exiting early");
                return true;
            }

            scope.initialized_decls.insert(var);
            scope.forward_decls.remove(&ident);

            if scope.decls.contains(&var) {
                debug!("initialize_decl: found scope at {:?}", scope.id);
                return true;
            }
        }
        */

        self.cx.span_bug(self.local_decls[var].span,
                         &format!("var {:?} not in scope to initialize", var));
    }

    pub fn assign_lvalue_unit(&mut self,
                              span: Span,
                              block: BasicBlock,
                              lvalue: Lvalue) {
        let rvalue = self.unit_rvalue();
        self.assign_lvalue(block, span, lvalue, rvalue)
    }

    pub fn find_local(&self, ident: ast::Ident) -> Option<Local> {
        for scope in self.scopes.iter().rev() {
            debug!("find_local: {:?} scope={:?}", ident, scope); 
        }

        for scope in self.scopes.iter().rev() {
            // Check if the lvalue is a conditionally initialized value.
            if let Some(conditional_scope) = self.conditional_scopes.get(&scope.id) {
                for var in conditional_scope.initialized_decls.last().unwrap() {
                    if ident == self.local_decls[*var].ident {
                        return Some(*var);
                    }
                }
            }

            // Check if we are shadowing another variable.
            for var in scope.drops.iter().rev() {
                if ident == self.local_decls[*var].ident {
                    return Some(*var);
                }
            }

            if let Some(var) = scope.forward_decls.get(&ident) {
                return Some(*var);
            }
        }

        None
    }

    pub fn find_forward_decl(&self, lvalue: ast::Ident) -> Option<Local> {
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
        self.cfg.terminate(span, block, kind);

        /*
        let live_decls = self.find_live_decls();

        let successors = self.cfg.block_data(block).terminator().successors();

        debug!("terminate: block={:?} succ={:?} vars={:?}",
               block, successors, live_decls);

        for successor in successors {
            let block_data = &mut self.cfg.block_data_mut(successor);

            if block_data.decls.is_empty() {
                block_data.decls.extend(live_decls.iter().cloned());
            } else {
                assert_eq!(block_data.decls, live_decls,
                           "block {:?} has inconsistent live decls: {:?} != {:?}",
                           successor,
                           block_data.decls,
                           live_decls);
            }
        }
        */
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
            debug!("find_live_decls: live decls1: {:?}", decls);

            if let Some(conditional_scope) = self.conditional_scopes.get(&scope.id) {
                debug!(
                    "find_live_decls: conditional_scope={:?}",
                    conditional_scope.initialized_decls.last().unwrap());

                for var in conditional_scope.initialized_decls.last().unwrap() {
                    let ident = self.local_decls[*var].ident;

                    if visited_decls.insert(ident) {
                        if scope.moved_decls.contains(var) {
                            decls.push(LiveDecl::Moved(*var));
                        } else {
                            decls.push(LiveDecl::Active(*var));
                        }
                    }
                }
            }

            debug!("find_live_decls: live decls2: {:?}", decls);

            for var in scope.drops.iter().rev() {
                let ident = self.local_decls[*var].ident;

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

            debug!("find_live_decls: live decls3: {:?}", decls);
        }

        decls.reverse();

        debug!("find_live_decls: live decls: {:?}", decls);

        decls
    }

    /// Indicates that `lvalue` should be dropped on exit from
    /// `extent`.
    pub fn schedule_drop(&mut self, span: Span, var: Local) {
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
            let ident = self.local_decls[var].ident;

            scope.decls.insert(var);
            scope.forward_decls.insert(ident, var);
        } else {
            self.cx.span_bug(span, "no scopes?");
        }

        let scope = self.scopes.last_mut().unwrap();
        scope.drops.push(var);
    }

    pub fn schedule_move(&mut self, span: Span, var: Local) {
        let ident = self.local_decls[var].ident;

        for scope in self.scopes.iter_mut().rev() {
            // Make sure the decl isn't a forward declaration.
            if scope.forward_decls.contains_key(&ident) {
                self.cx.span_warn(span,
                                  &format!("trying to move an uninitialized var {:?}?", var));
            }

            scope.moved_decls.insert(var);
        }
    }

    pub fn add_decls_from_pats<'c, I>(&mut self, block: BasicBlock, pats: I)
        where I: Iterator<Item=&'c P<ast::Pat>>,
    {
        for pat in pats {
            for var in self.get_decls_from_pat(pat, None) {
                let ident = self.local_decls[var].ident;

                self.cfg.block_data_mut(block).decls.push(LiveDecl::Active(var));
                self.scopes.last_mut().unwrap().forward_decls.remove(&ident);
            }
        }
    }

    pub fn get_decls_from_pat(&mut self,
                              pat: &ast::Pat,
                              ty: Option<P<ast::Ty>>) -> Vec<Local> {
        struct Visitor<'a, 'b: 'a, 'c: 'b> {
            builder: &'a mut Builder<'b, 'c>,
            new_vars: Vec<Local>,
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

        debug!("get_decls_from_pat: {:?} => {:?}", pat, visitor.new_vars);

        visitor.new_vars
    }

    pub fn get_decls_from_expr(&self, expr: &P<ast::Expr>) -> Vec<Local> {
        struct Visitor<'a, 'b: 'a> {
            builder: &'a Builder<'a, 'b>,
            locals: Vec<Local>,
        }

        impl<'a, 'b: 'a> visit::Visitor for Visitor<'a, 'b> {
            fn visit_expr(&mut self, expr: &ast::Expr) {
                debug!("get_decls_from_expr: {:?}", expr.node);

                if let ast::ExprKind::Path(None, ref path) = expr.node {
                    if let Some(local) = self.builder.get_local_from_path(path) {
                        debug!("get_decls_from_expr: decl `{:?}", local);
                        self.locals.push(local);
                    }
                }

                visit::walk_expr(self, expr);
            }

            fn visit_mac(&mut self, _mac: &ast::Mac) { }
        }

        let mut visitor = Visitor {
            builder: self,
            locals: Vec::new(),
        };

        visit::Visitor::visit_expr(&mut visitor, expr);

        visitor.locals
    }

    pub fn get_local_from_path(&self, path: &ast::Path) -> Option<Local> {
        if !path.global && path.segments.len() == 1 {
            let segment = &path.segments[0];

            if segment.parameters.is_empty() {
                debug!("get_local_from_path: {:?}", segment.identifier);
                return self.find_local(segment.identifier);
            }
        }

        None
    }
}

fn drop_decl(cfg: &mut CFG, block: BasicBlock, scope: &Scope, var: Local) {
    debug!("drop_decl: scope={:?} var={:?}", scope.id, var);
    let moved = scope.moved_decls.contains(&var);
    cfg.push_drop(block, var, moved);
}

/*
pub struct ConditionalScope<'a, 'b: 'a, 'c: 'b> {
    builder: &'a mut Builder<'b, 'c>,
}

impl<'a, 'b: 'a, 'c: 'b> ConditionalScope<'a, 'b, 'c> {
    fn scope<F>(&mut self, f: F) where F: FnOnce(&mut Builder) -> BasicBlock {

    }
}
*/
