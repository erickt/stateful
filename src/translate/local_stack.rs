use aster::AstBuilder;
use data_structures::indexed_vec::Idx;
use mir::{Local, LocalDecl, Mir};
use std::collections::HashMap;
use syntax::ast;

/// `LocalStack` tracks the definitions and aliases of locals during the translation of MIR into
/// Rust source code.
pub struct LocalStack<'a> {
    mir: &'a Mir,
    scope_stack: Vec<Scope>,
}

struct Scope {
    name_to_local: HashMap<ast::Ident, Vec<Local>>,
    local_to_name: HashMap<Local, ast::Ident>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            name_to_local: HashMap::new(),
            local_to_name: HashMap::new(),
        }
    }
}

impl<'a> LocalStack<'a> {
    pub fn new(mir: &'a Mir) -> Self {
        LocalStack {
            mir: mir,
            scope_stack: vec![Scope::new()],
        }
    }

    /// Enter into a new scope. When the closure returns, this method returns all the statements
    /// necessary to rename the aliased locals back into the original names.
    pub fn in_scope<F>(&mut self, f: F) -> (bool, Vec<ast::Stmt>)
        where F: FnOnce(&mut Self) -> (bool, Vec<ast::Stmt>)
    {
        self.scope_stack.push(Scope::new());

        let (terminated, mut stmts) = f(self);

        // When we exit a scope, we need to also rename all the shadowed names to their real names.
        // We can figure out what's been renamed by just going through all the names we aliased,
        // and for any that still have locals, rename them back to the original name.
        let scope = self.scope_stack.pop().expect("scope stack is empty?");

        // Only insert the renames when we haven't terminated this block.
        if !terminated {
            stmts.extend(
                scope.local_to_name.iter().filter_map(|(&local, &shadowed_name)| {
                    self.get_name(local).map(|name| {
                        assert!(
                            name != shadowed_name,
                            "local {:?} should have name {:?}, not {:?}",
                            local,
                            name,
                            shadowed_name);

                        AstBuilder::new().stmt()
                            .span(self.mir.local_decls[local].source_info.span)
                            .let_id(name)
                            .expr().id(shadowed_name)
                    })
                })
            );
        }

        (terminated, stmts)
    }

    /// Push a new local on the stack. If this local is shadowing another local, this method will
    /// return a statement that renames the local to a unique alias. This allows it to stay alive
    /// but unreachable until the end of the scope.
    pub fn push(&mut self, local: Local) -> Option<ast::Stmt> {
        let LocalDecl { name, source_info, .. } = self.mir.local_decls[local];

        // First, get the local that's currently using this name.
        let shadowed_local = self.get_local(name);

        let scope = self.scope_stack.last_mut().expect("empty scope stack");

        // Next, insert the local into our mapping. Since it's just been defined, it gets to use
        // the real name.
        scope.name_to_local.entry(name).or_insert_with(Vec::new).push(local);
        scope.local_to_name.insert(local, name);

        // Finally, we handle the shadowed local. We have to do two things. First, we generate a
        // it a unique name. Next, we update this scope's local-to-name mapping to reflect the
        // local's name at this point in the scope. Finally, we create a statement that renames the
        // local from the real name to the shadowed name.
        shadowed_local.map(|shadowed_local| {
            let builder = AstBuilder::new().span(source_info.span);

            let shadowed_name = format!("{}_shadowed_{}", name, shadowed_local.index());
            let shadowed_name = builder.id(shadowed_name);

            scope.local_to_name.insert(shadowed_local, shadowed_name);

            builder.stmt()
                .let_id(shadowed_name)
                .expr().id(name)
        })
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
