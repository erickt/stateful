use aster::AstBuilder;
use data_structures::indexed_vec::Idx;
use mir::{self, BasicBlock, Local, LocalDecl, Lvalue, Mir};
use std::collections::HashMap;
use super::builder::Builder;
use syntax::ast;
use syntax::codemap::Span;

/// `LocalStack` tracks the definitions and aliases of locals during the translation of MIR into
/// Rust source code.
pub struct LocalStack<'a> {
    mir: &'a Mir,
    span: Span,
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
    pub fn new<'b>(builder: &Builder<'a, 'b>, block: BasicBlock) -> Self {
        let span = builder.mir[block].span;

        let mut local_stack = LocalStack {
            mir: builder.mir,
            span: span,
            scope_stack: vec![Scope::new()],
        };

        local_stack.push(&Lvalue::Local(mir::COROUTINE_ARGS), false);

        // Insert all the locals that have been initialized prior to this block.
        for locals in builder.scope_locals[&block].values() {
            for local in locals {
                local_stack.push(&Lvalue::Local(*local), false);
            }
        }

        local_stack
    }

    /// Enter into a new scope. When the closure returns, this method returns all the statements
    /// necessary to rename the aliased locals back into the original names.
    pub fn in_scope<F>(&mut self, f: F) -> (bool, Vec<ast::Stmt>)
        where F: FnOnce(&mut Self) -> (bool, Vec<ast::Stmt>)
    {
        self.scope_stack.push(Scope::new());

        let (terminated, stmts) = f(self);

        let builder = AstBuilder::new().span(self.span);
        let block_builder = AstBuilder::new().span(self.span).block()
            .with_stmts(stmts);

        // When we exit a scope, we need to also rename all the shadowed names to their real names.
        // We can figure out what's been renamed by just going through all the names we aliased,
        // and for any that still have locals, rename them back to the original name.
        let scope = self.scope_stack.pop().expect("scope stack is empty?");

        // Only insert the renames when we haven't terminated this block.
        let stmt = if terminated || scope.local_to_name.is_empty() {
            let block = block_builder.build();

            builder.stmt().semi()
                .build_block(block)
        } else {
            let pat = builder.pat().tuple()
                .with_pats(
                    scope.local_to_name.iter()
                        .filter_map(|(&local, _)| {
                            self.get_name(local).map(|name| builder.pat().id(name))
                        })
                )
                .build();

            let expr = builder.expr().tuple()
                .with_exprs(
                    scope.local_to_name.iter()
                        .filter_map(|(&local, &shadowed_name)| {
                            self.get_name(local).map(|_| {
                                builder.expr().id(shadowed_name)
                            })
                        })
                )
                .build();

            let block = block_builder.build_expr(expr);

            builder.stmt().let_().build(pat).expr()
                .build_block(block)
        };

        (terminated, vec![stmt])
    }

    pub fn extend<'c, T>(&mut self, iter: T, declare: bool) -> Vec<ast::Stmt>
        where T: Iterator<Item=&'c Lvalue>
    {
        iter.flat_map(|lvalue| self.push(lvalue, declare))
            .collect()
    }

    /// Push a new local on the stack. If this local is uninitialized, or shadowing another local,
    /// this method will return a series of statements that renames the local to a unique alias.
    /// This allows it to stay alive but unreachable until the end of the scope.
    pub fn push(&mut self, lvalue: &Lvalue, declare: bool) -> Vec<ast::Stmt> {
        // Exit early if the lvalue is not local.
        let local = match *lvalue {
            Lvalue::Local(local) => local,
            _ => {
                return vec![]; }
        };

        let mut stmts = self.shadow(local);

        if declare {
            stmts.extend(self.declare(local));
        }

        stmts
    }

    fn shadow(&mut self, local: Local) -> Vec<ast::Stmt> {
        let mut stmts = vec![];

        let LocalDecl { name, source_info, .. } = self.mir.local_decls[local];

        // First, get the local that's currently using this name.
        let shadowed_local = self.get_local(name);

        let scope = self.scope_stack.last_mut().expect("empty scope stack");

        // Next, insert the local into our mapping. Since it's just been defined, it gets to
        // use the real name.
        //let name = AstBuilder::new().id(format!("{:?}_shadowed_{}", name, local.index()));
        scope.name_to_local.entry(name).or_insert_with(Vec::new).push(local);
        scope.local_to_name.insert(local, name);

        // Next, we handle the shadowed local. We have to do two things. First, we generate a
        // it a unique name. Next, we update this scope's local-to-name mapping to reflect the
        // local's name at this point in the scope. Finally, we create a statement that renames
        // the local from the real name to the shadowed name.
        if let Some(shadowed_local) = shadowed_local {
            let builder = AstBuilder::new().span(source_info.span);

            let shadowed_name = format!("{}_shadowed_{}", name, shadowed_local.index());
            let shadowed_name = builder.id(shadowed_name);

            scope.local_to_name.insert(shadowed_local, shadowed_name);

            stmts.push(
                builder.stmt().let_id(shadowed_name).expr().id(name)
            );
        }

        stmts
    }

    fn declare(&mut self, local: Local) -> Vec<ast::Stmt> {
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
