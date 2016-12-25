use analysis::DefiniteAssignment;
use aster::AstBuilder;
use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use super::internal_state::InternalState;
use super::resume_state::CoroutineState;
use super::state::StateKind;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;

pub struct Builder<'a, 'b: 'a> {
    pub cx: &'a ExtCtxt<'b>,
    pub ast_builder: AstBuilder,
    pub mir: &'a Mir,
    pub assignments: &'a DefiniteAssignment,

    /// All the blocks that are the target of a resume.
    pub resume_blocks: HashSet<BasicBlock>,

    /// A map of basic blocks to their locals, grouped by scope.
    pub scope_locals: ScopeLocals,
}

type ScopeLocals = HashMap<BasicBlock, Vec<(VisibilityScope, Vec<Local>)>>;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn new(cx: &'a ExtCtxt<'b>,
               mir: &'a Mir,
               assignments: &'a DefiniteAssignment) -> Self {
        Builder {
            cx: cx,
            ast_builder: AstBuilder::new().span(mir.span),
            mir: mir,
            assignments: assignments,
            resume_blocks: find_resume_blocks(mir),
            scope_locals: group_locals_by_scope(mir, assignments),
        }
    }

    pub fn shadowed_ident(&self, local: Local) -> ast::Ident {
        let decl_ident = self.mir.local_decls[local].name;
        self.ast_builder.id(format!("{}_shadowed_{}", decl_ident, local.index()))
    }

    pub fn state_machine(&self) -> P<ast::Block> {
        let start_state_expr = self.coroutine_state_expr(START_BLOCK);

        let state_machine_impl = self.state_machine_impl();
        let state_machine_impl_driver = self.state_machine_impl_driver();

        let CoroutineState {
            stmts: coroutine_state_stmts,
            expr: coroutine_state_expr,
        } = self.coroutine_state();

        let InternalState {
            stmts: internal_state_stmts,
            expr: internal_state_expr,
        } = self.internal_state();

        let expr = quote_expr!(self.cx,
            StateMachine::new($start_state_expr, coroutine)
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

            $coroutine_state_stmts
            $internal_state_stmts

            let coroutine = |mut coroutine_state, args| {
                let mut state = $coroutine_state_expr;
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
/// In order properly lift any coroutine arguments into the state machine, we need to first
/// identify all the blocks that can be resumed into.
fn find_resume_blocks(mir: &Mir) -> HashSet<BasicBlock> {
    let mut blocks = HashSet::new();
    blocks.insert(START_BLOCK);

    for block_data in mir.basic_blocks().iter() {
        if let Some(ref terminator) = block_data.terminator {
            match terminator.kind {
                TerminatorKind::Suspend { destination: (_, block), .. } => {
                    blocks.insert(block);
                }
                _ => {}
            }
        }
    }

    blocks
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

        let locals = block_map.into_iter().collect::<Vec<_>>();
        map.insert(block, locals);
    }

    map
}
