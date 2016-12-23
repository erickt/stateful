use analysis::DefiniteAssignment;
use aster::AstBuilder;
use data_structures::indexed_vec::Idx;
use mir::*;
use self::state::{CoroutineState, InternalState, StateKind};
use std::collections::BTreeMap;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;

pub fn translate(cx: &ExtCtxt,
                 mir: &Mir,
                 assignments: &DefiniteAssignment)
                 -> P<ast::Item> {
    let builder = Builder::new(cx, mir, assignments);

    let state_machine_impl = builder.state_machine_impl();
    let state_machine_impl_driver = builder.state_machine_impl_driver();
    let (state_machine_state, state_machine_closure) = builder.state_machine_closure();

    let block = quote_block!(cx, {
        struct StateMachine<S, F> {
            state: S,
            resume: F,
        }

        $state_machine_impl
        $state_machine_impl_driver

        $state_machine_state
        $state_machine_closure
    });

    let ast_builder = AstBuilder::new().span(mir.span);
        
    ast_builder.item()
        .fn_(mir.fn_decl.ident())
        .with_args(mir.fn_decl.inputs().iter().cloned())
        .build_return(mir.fn_decl.return_ty())
        .generics().with(mir.fn_decl.generics().clone()).build()
        .build(block)
}

///////////////////////////////////////////////////////////////////////////

pub struct Builder<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    ast_builder: AstBuilder,
    mir: &'a Mir,
    assignments: &'a DefiniteAssignment,

    /// All the blocks that are the target of a resume.
    resume_blocks: BTreeSet<Block>,
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    fn new(cx: &'a ExtCtxt<'b>,
               mir: &'a Mir,
               assignments: &'a DefiniteAssignment) -> Self {

        Builder {
            cx: cx,
            ast_builder: AstBuilder::new().span(mir.span),
            mir: mir,
            assignments: assignments,
            resume_blocks: find_resume_blocks(mir),
        }
    }

    pub fn shadowed_ident(&self, local: Local) -> ast::Ident {
        let decl_ident = self.mir.local_decls[local].name;
        self.ast_builder.id(format!("{}_shadowed_{}", decl_ident, local.index()))
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

    fn state_machine_closure(&self) -> (Vec<P<ast::Item>>, P<ast::Expr>) {
        let start_state_expr = self.coroutine_state_expr(START_BLOCK);

        let CoroutineState {
            enum_item: coroutine_state_enum_item,
            default_item: coroutine_state_default_item,
            expr: coroutine_state_expr,
        } = self.coroutine_state();

        let InternalState {
            enum_item: internal_state_enum_item,
            expr: internal_state_expr,
        } = self.internal_state();

        let items = vec![
            coroutine_state_enum_item,
            coroutine_state_default_item,
            internal_state_enum_item,
        ];

        let expr = quote_expr!(self.cx,
            StateMachine::new(
                $start_state_expr,
                |mut coroutine_state, args| {
                    let mut state = $coroutine_state_expr;
                    $internal_state_expr
                }
            )
        );

        // If we're not using impl trait, we need to wrap the closure in a box.
        let expr = match self.mir.fn_decl.return_ty().node {
            ast::TyKind::ImplTrait(_) => expr,
            _ => self.ast_builder.expr().box_().build(expr),
        };

        (items, expr)
    }
}

/// Find all the blocks that could be resumed into.
///
/// In order properly lift any coroutine arguments into the state machine, we need to first
/// identify all the blocks that can be resumed into.
fn find_resume_blocks(mir: &Mir) -> BTreeSet {
    let mut blocks = BTreeSet::new();
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

///////////////////////////////////////////////////////////////////////////

mod block;
mod state;
mod stmt;
