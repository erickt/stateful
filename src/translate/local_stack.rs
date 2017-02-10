use aster::AstBuilder;
use data_structures::indexed_vec::Idx;
use mir::{self, BasicBlock, Local, LocalDecl, Lvalue, Mir, VisibilityScope};
use std::collections::{BTreeMap, HashMap, HashSet};
use super::builder::Builder;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;

/// `LocalStack` tracks the definitions and aliases of locals during the translation of MIR into
/// Rust source code.
pub struct LocalStack<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    mir: &'a Mir,
    span: Span,
    pub scope_stack: Vec<Scope>,
    uninitialized_locals: HashSet<Local>,
}

#[derive(Debug)]
pub struct Scope {
    visibility_scope: VisibilityScope,
    name_to_local: HashMap<ast::Ident, Vec<Local>>,
    local_to_name: BTreeMap<Local, ast::Ident>,
}

impl Scope {
    fn new(visibility_scope: VisibilityScope) -> Self {
        Scope {
            visibility_scope: visibility_scope,
            name_to_local: HashMap::new(),
            local_to_name: BTreeMap::new(),
        }
    }
}

impl<'a, 'b: 'a> LocalStack<'a, 'b> {
    pub fn new(builder: &Builder<'a, 'b>, block: BasicBlock) -> Self {
        let span = builder.mir[block].span;

        // Next, collect all the uninitialized locals for this block.
        let uninitialized_locals = builder.assignments.initialized(block).into_iter()
            .flat_map(|initialized| initialized.iter())
            .map(|local| *local)
            .filter(|local| *local == mir::COROUTINE_ARGS) // coroutine args is always initialized.
            .collect::<HashSet<_>>();

        LocalStack {
            cx: builder.cx,
            mir: builder.mir,
            span: span,
            scope_stack: vec![],
            uninitialized_locals: uninitialized_locals,
        }
    }

    /*
    pub fn last_visibility_scope(&self) -> VisibilityScope {
        self.scope_stack.last().expect("scope stack is empty?").visibility_scope
    }
    */

    /// Enter into a new scope. When the closure returns, this method returns all the statements
    /// necessary to rename the aliased locals back into the original names.
    pub fn in_scope<F>(&mut self, visibility_scope: VisibilityScope, f: F) -> (bool, ast::Stmt)
        where F: FnOnce(&mut Self) -> (bool, Vec<ast::Stmt>)
    {
        debug!("push scope: {:?}", visibility_scope);

        self.scope_stack.push(Scope::new(visibility_scope));

        let (terminated, mut stmts) = f(self);

        // When we exit a scope, we need to also rename all the shadowed names to their real names.
        // We can figure out what's been renamed by just going through all the names we aliased,
        // and for any that still have locals, rename them back to the original name.
        let scope = self.scope_stack.pop().expect("scope stack is empty?");

        debug!("popped scope: {:#?}", scope);

        let builder = AstBuilder::new().span(self.span);

        // If the inner scope terminated, it would have already consumed all the scope variables.
        // Otherwise, we need to bubble up all the shadowed locals up the scope.
        let stmt = if terminated {
            builder.stmt().semi().block()
                .with_stmts(stmts)
                .build()
        } else {
            let mut pats = vec![];
            let mut exprs = vec![];

            // Step through all the locals defined in the scope we just popped. If any are still
            // defined, then we'll create `let (a,b) = { ... (shadowed_a, shadowed_b) }` to extract
            // out locals from the inner scope.
            for (local, child_name) in scope.local_to_name.iter() {
                debug!("popping: local={:?} child_name={:?}", local, child_name);

                let local_scope = self.mir.local_decls[*local].source_info.scope;

                // Only include the locals that are still in scope.
                if scope.visibility_scope != local_scope {
                    if let Some(parent_name) = self.get_name(*local) {
                        debug!("parent_name: local={:?} parent_name={:?}", local, parent_name);
                        pats.push(builder.pat().id(parent_name));
                        exprs.push(builder.expr().id(child_name));
                    } else {
                        pats.push(builder.pat().id(child_name));
                        exprs.push(builder.expr().id(child_name));
                    }
                }
            }

            // Don't do anything if we didn't rename any locals.
            if pats.is_empty() {
                builder.stmt().semi().block()
                    .with_stmts(stmts)
                    .build()
            } else {
                // Insert an expression into the block that returns all the locals.
                stmts.push(
                    builder.stmt().expr().tuple()
                        .with_exprs(exprs)
                        .build()
                );

                // Bundle up the stmts into a block.
                let block_expr = builder.expr().block()
                    .with_stmts(stmts)
                    .build();

                let tuple_pat = builder.pat().tuple()
                    .with_pats(pats)
                    .build();

                builder.stmt().let_()
                    .build(tuple_pat)
                    .build_expr(block_expr)
            }
        };

        (terminated, stmt)
    }

    pub fn extend<'c, T>(&mut self, iter: T, declare: bool) -> Vec<ast::Stmt>
        where T: Iterator<Item=&'c Lvalue>
    {
        iter.flat_map(|lvalue| self.push_lvalue(lvalue, declare))
            .collect()
    }

    /// Push a new local on the stack. If this local is uninitialized, or shadowing another local,
    /// this method will return a series of statements that renames the local to a unique alias.
    /// This allows it to stay alive but unreachable until the end of the scope.
    pub fn push_lvalue(&mut self, lvalue: &Lvalue, declare: bool) -> Vec<ast::Stmt> {
        debug!("local_stack.push_lvalue({:?}, {:?}, {:?})", lvalue, declare, self.uninitialized_locals);

        // Exit early if the lvalue is not local.
        let local = match *lvalue {
            Lvalue::Local(local) => local,
            _ => {
                debug!("local_stack.push_lvalue: not local");
                return vec![];
            }
        };

        let mut stmts = self.shadow(local);

        if declare {
            stmts.extend(self.declare(local));
        }

        stmts
    }

    fn shadow(&mut self, local: Local) -> Vec<ast::Stmt> {
        let LocalDecl { name, source_info, .. } = self.mir.local_decls[local];
        let local_scope = source_info.scope;

        debug!("shadow: local={:?}, local_name={:?}, local_scope={:?}",
               local, name, local_scope);

        // First, make sure the scope of this local is actually in our stack.
        if !self.scope_stack.iter().any(|scope| scope.visibility_scope == local_scope) {
            span_bug!(
                self.cx,
                source_info.span,
                "local's scope not in stack? local={:?} scope={:?} stack={:#?}",
                local,
                local_scope,
                self.scope_stack);
        }

        // First, get the local that's currently using this name.
        let shadowed_local = self.get_local(name);

        // Next, insert the local into our mapping. Since it's just been defined, it gets to
        // use the real name.
        for scope in self.scope_stack.iter_mut().rev() {
            debug!("shadow: adding local={:?} with scope={:?} to scope={:?}",
                   local,
                   source_info.scope,
                   scope.visibility_scope);

            scope.name_to_local.entry(name).or_insert_with(Vec::new).push(local);
            scope.local_to_name.insert(local, name);

            if scope.visibility_scope == source_info.scope {
                break;
            }
        }

        /*
        let scope = self.scope_stack.last_mut().expect("empty scope stack");

        // Next, insert the local into our mapping. Since it's just been defined, it gets to
        // use the real name.
        //let name = AstBuilder::new().id(format!("{:?}_shadowed_{}", name, local.index()));
        scope.name_to_local.entry(name).or_insert_with(Vec::new).push(local);
        scope.local_to_name.insert(local, name);
        */

        let mut stmts = vec![];

        // Next, we handle the shadowed local. We have to do two things. First, we generate a
        // it a unique name. Next, we update this scope's local-to-name mapping to reflect the
        // local's name at this point in the scope. Finally, we create a statement that renames
        // the local from the real name to the shadowed name.
        if let Some(shadowed_local) = shadowed_local {
            let builder = AstBuilder::new().span(source_info.span);

            let shadowed_name = format!("{}_shadowed_{}", name, shadowed_local.index());
            let shadowed_name = builder.id(shadowed_name);

            self.scope_stack.last_mut().expect("empty scope stack")
                .local_to_name.insert(shadowed_local, shadowed_name);

            stmts.push(
                builder.stmt().let_id(shadowed_name).expr().id(name)
            );
        }

        stmts
    }

    fn declare(&mut self, local: Local) -> Vec<ast::Stmt> {
        /*
        // Exit early if we've already initialized this local.
        if !self.uninitialized_locals.remove(&local) {
            debug!("local already initialized: {:?}", local);
            return vec![];
        }
        */

        let local_decl = self.mir.local_decl_data(local);

        let ast_builder = AstBuilder::new().span(local_decl.source_info.span);

        let stmt_builder = match local_decl.mutability {
            ast::Mutability::Mutable => ast_builder.stmt().let_().mut_id(local_decl.name),
            ast::Mutability::Immutable => ast_builder.stmt().let_().id(local_decl.name),
        };

        let stmt = stmt_builder
            .build_option_ty(local_decl.ty.clone())
            .build();

        vec![stmt]
    }

    pub fn get_local(&self, name: ast::Ident) -> Option<Local> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(locals) = scope.name_to_local.get(&name) {
                if let Some(local) = locals.last() {
                    return Some(*local);
                }
            }
        }

        None
    }

    pub fn get_name(&self, local: Local) -> Option<ast::Ident> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(name) = scope.local_to_name.get(&local) {
                return Some(*name);
            }
        }

        None
    }
}

impl<'a, 'b: 'a> Drop for LocalStack<'a, 'b> {
    fn drop(&mut self) {
        if !self.scope_stack.is_empty() {
            span_err!(
                self.cx,
                self.span,
                "still some unpopped scopes: {:#?}",
                self.scope_stack);
        }

        if !self.uninitialized_locals.is_empty() {
            span_err!(
                self.cx,
                self.span,
                "still some uninitialized locals: {:?}",
                self.uninitialized_locals);
        }
    }
}
