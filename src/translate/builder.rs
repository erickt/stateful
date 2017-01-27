use analysis::elaborate_assignments::DefiniteAssignment;
use aster::AstBuilder;
use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use super::internal_state::InternalState;
use super::resume_state::ResumeState;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;
use ty::TyCtxt;

type ScopeLocals = HashMap<BasicBlock, BTreeMap<VisibilityScope, Vec<Local>>>;
type ScopePaths = BTreeMap<VisibilityScope, Vec<VisibilityScope>>;
type LocalNames = BTreeMap<BasicBlock, BTreeMap<Local, ast::Ident>>;

pub struct Builder<'a, 'b: 'a> {
    pub cx: &'a ExtCtxt<'b>,
    pub ast_builder: AstBuilder,
    pub mir: &'a Mir,
    pub assignments: &'a DefiniteAssignment,

    /// All the blocks that are the target of a resume.
    pub resume_blocks: BTreeSet<BasicBlock>,

    /// A map of basic blocks to their locals, grouped by scope.
    pub scope_locals: ScopeLocals,

    /// A map of a scope to their path to the root scope.
    pub scope_paths: ScopePaths,

    /// A map of basoc blocks to a mapping of the local names, taking in consideration for variable
    /// shadowing.
    pub local_names: LocalNames,
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn new(tcx: TyCtxt<'a, 'b>,
               mir: &'a Mir,
               assignments: &'a DefiniteAssignment) -> Self {
        let scope_locals = group_locals_by_scope(mir, assignments);
        let local_names = compute_local_names(mir, &scope_locals);

        println!("{:#?}", local_names);

        Builder {
            cx: &tcx,
            ast_builder: AstBuilder::new().span(mir.span),
            mir: mir,
            assignments: assignments,
            resume_blocks: find_resume_blocks(mir),
            scope_locals: scope_locals,
            scope_paths: compute_scope_paths(mir),
            local_names: local_names,
        }
    }

    pub fn state_machine(&mut self) -> P<ast::Block> {
        let start_state_expr = self.start_state_expr();

        let state_machine_impl = self.state_machine_impl();
        let state_machine_impl_driver = self.state_machine_impl_driver();

        let ResumeState {
            stmts: resume_state_stmts,
            expr: resume_state_expr,
        } = self.resume_state();

        let InternalState {
            stmts: internal_state_stmts,
            expr: internal_state_expr,
        } = self.internal_state();

        let expr = quote_expr!(self.cx,
            StateMachine::new($start_state_expr, resume)
        );

        // If we're not using impl trait, we need to wrap the closure in a box.
        let expr = match self.mir.fn_decl.return_ty().node {
            ast::TyKind::ImplTrait(_) => expr,
            _ => self.ast_builder.expr().box_().build(expr),
        };

        quote_block!(self.cx, {
            struct StateMachine<S, F> {
                state: S,
                resume: F,
            }

            $state_machine_impl
            $state_machine_impl_driver

            $resume_state_stmts
            $internal_state_stmts

            let resume = |mut resume_state, coroutine_args| {
                let mut state = $resume_state_expr;
                $internal_state_expr
            };

            $expr
        })
    }

    fn state_machine_impl(&self) -> P<ast::Item> {
        match self.mir.state_machine_kind {
            StateMachineKind::Generator => {
                quote_item!(self.cx,
                    impl<S, F, Item> StateMachine<S, F>
                        where S: ::std::default::Default,
                              F: Fn(S, ()) -> (::std::option::Option<Item>, S),
                    {
                        fn new(initial_state: S, resume: F) -> Self {
                            StateMachine {
                                state: initial_state,
                                resume: resume,
                            }
                        }
                    }
                ).unwrap()
            }
            StateMachineKind::Async => {
                quote_item!(self.cx,
                    impl<S, F, Item, Error> StateMachine<S, F>
                        where S: ::std::default::Default,
                              F: Fn(S, ())
                              -> ::std::result::Result<(::futures::Async<Item>, S), Error>,
                    {
                        fn new(initial_state: S, next: F) -> Self {
                            StateMachine {
                                state: initial_state,
                                resume: resume,
                            }
                        }
                    }
                ).unwrap()
            }
        }
    }

    fn state_machine_impl_driver(&self) -> P<ast::Item> {
        match self.mir.state_machine_kind {
            StateMachineKind::Generator => {
                quote_item!(self.cx,
                    impl<S, F, Item> ::std::iter::Iterator for StateMachine<S, F>
                        where S: ::std::default::Default,
                              F: Fn(S, ()) -> (::std::option::Option<Item>, S)
                    {
                        type Item = Item;

                        fn next(&mut self) -> ::std::option::Option<Item> {
                            let state = ::std::mem::replace(&mut self.state, S::default());
                            let (value, state) = (self.resume)(state, ());
                            self.state = state;
                            value
                        }
                    }
                ).unwrap()
            }
            StateMachineKind::Async => {
                quote_item!(self.cx,
                    impl<S, F, Item, Error> ::futures::Future for StateMachine<S, F>
                        where S: ::std::default::Default,
                              F: Fn(S, ())
                              -> ::std::result::Result<(::futures::Async<Item>, S), Error>,
                    {
                        type Item = Item;
                        type Error = Error;

                        fn poll(&mut self) -> ::futures::Poll<Item, Error> {
                            let state = ::std::mem::replace(&mut self.state, S::default());
                            let (value, state) = try!((self.resume)(state, ()));
                            self.state = state;
                            Ok(value)
                        }
                    }
                ).unwrap()
            }
        }
    }

    /// Get the span for a block.
    pub fn block_span(&self, block: BasicBlock) -> Span {
        self.mir[block].span
    }
}

/// Find all the blocks that could be resumed into.
///
/// In order properly lift any resume arguments into the state machine, we need to first
/// identify all the blocks that can be resumed into.
fn find_resume_blocks(mir: &Mir) -> BTreeSet<BasicBlock> {
    mir.basic_blocks().iter()
        .filter_map(|block_data| {
            if let Some(ref terminator) = block_data.terminator {
                match terminator.kind {
                    TerminatorKind::Suspend { destination: (_, block), .. } => {
                        Some(block)
                    }
                    _ => None
                }
            } else {
                None
            }
        })
        .chain(Some(START_BLOCK))
        .collect()
}

fn group_locals_by_scope(mir: &Mir, assignments: &DefiniteAssignment) -> ScopeLocals {
    let mut map = HashMap::new();

    for block in mir.basic_blocks().indices() {
        let mut block_map = BTreeMap::new();

        if let Some(ref locals) = assignments.on_entry(block) {
            for &local in locals.iter() {
                let local_data = &mir.local_decls[local];
                let decls = block_map.entry(local_data.source_info.scope)
                    .or_insert_with(Vec::new);

                decls.push(local);
            }
        }

        map.insert(block, block_map);
    }

    map
}

/// Compute the path from a scope to the root scope.
fn compute_scope_paths(mir: &Mir) -> ScopePaths {
    let mut scope_paths = BTreeMap::new();

    for scope in mir.visibility_scopes.indices() {
        let mut path = vec![scope];
        let mut s = scope;
        while let Some(parent) = mir.visibility_scopes[s].parent_scope {
            path.push(parent);
            s = parent;
        }
        path.reverse();
        scope_paths.insert(scope, path);
    }

    scope_paths
}

fn shadowed_ident(mir: &Mir, local: Local) -> ast::Ident {
    let name = mir.local_decls[local].name;
    AstBuilder::new().id(format!("{}_shadowed_{}", name, local.index()))
}

/// Determine the local idents taking consideration for shadowing. We'll walk backwards
/// through the scopes and the locals to find if there are any aliases. If so, we'll use
/// the alias name instead of the real name.
fn compute_local_names(mir: &Mir, scope_locals: &ScopeLocals) -> LocalNames {
    let mut local_names = BTreeMap::new();

    for block in mir.basic_blocks().indices() {
        let mut block_local_names = BTreeMap::new();
        let mut names = HashSet::new();

        for (_, locals) in scope_locals[&block].iter().rev() {
            for &local in locals.iter().rev() {
                let name = mir.local_decls[local].name;

                if names.insert(name) {
                    // We haven't seen this name yet, so insert it with the original name.
                    block_local_names.insert(local, name);
                } else {
                    // Otherwise, this local is aliased.
                    block_local_names.insert(local, shadowed_ident(mir, local));
                }
            }
        }

        local_names.insert(block, block_local_names);
    }

    local_names
}
