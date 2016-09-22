use aster::AstBuilder;
use mar::indexed_vec::Idx;
use mar::repr::*;
use syntax::ast::{self, FunctionRetTy};
use syntax::ext::base::ExtCtxt;
use syntax::fold;
use syntax::ptr::P;

pub fn translate(cx: &ExtCtxt, mar: &Mar) -> Option<P<ast::Item>> {
    let ast_builder = AstBuilder::new().span(mar.span);

    let return_ty = match mar.fn_decl.output {
        FunctionRetTy::Default(span) => ast_builder.span(span).ty().unit(),
        FunctionRetTy::Ty(ref ty) => ty.clone(),
    };

    let item_builder = ast_builder.item().fn_(mar.ident)
        .with_args(mar.fn_decl.inputs.iter().cloned())
        .build_return(return_ty.clone())
        .generics().with(mar.generics.clone())
        .build();

    let builder = Builder {
        cx: cx,
        ast_builder: ast_builder,
        mar: mar,
    };

    let start_state_expr = builder.state_expr(mar.span, START_BLOCK);
    let (state_enum, state_default, state_arms) =
        builder.state_enum_default_and_arms();

    let state_machine_impl;
    let state_machine_impl_future;

    match mar.state_machine_kind {
        StateMachineKind::Generator => {
            state_machine_impl = quote_item!(cx,
                impl<S, F, Item> StateMachine<S, F>
                    where S: ::std::default::Default,
                          F: Fn(S) -> (::std::option::Option<Item>, S),
                {
                    fn new(initial_state: S, next: F) -> Self {
                        StateMachine {
                            state: initial_state,
                            next: next,
                        }
                    }
                }
            ).unwrap();

            state_machine_impl_future = quote_item!(cx,
                impl<S, F, Item> ::std::iter::Iterator for StateMachine<S, F>
                    where S: ::std::default::Default,
                          F: Fn(S) -> (::std::option::Option<Item>, S)
                {
                    type Item = Item;

                    fn next(&mut self) -> ::std::option::Option<Item> {
                        let state = ::std::mem::replace(&mut self.state, S::default());
                        let (value, state) = (self.next)(state);
                        self.state = state;
                        value
                    }
                }
            ).unwrap();
        }
        StateMachineKind::Async => {
            state_machine_impl = quote_item!(cx,
                impl<S, F, Item, Error> StateMachine<S, F>
                    where S: ::std::default::Default,
                          F: Fn(S) -> ::std::result::Result<(::futures::Async<Item>, S), Error>,
                {
                    fn new(initial_state: S, next: F) -> Self {
                        StateMachine {
                            state: initial_state,
                            next: next,
                        }
                    }
                }
            ).unwrap();

            state_machine_impl_future = quote_item!(cx,
                impl<S, F, Item, Error> ::futures::Future for StateMachine<S, F>
                    where S: ::std::default::Default,
                          F: Fn(S) -> ::std::result::Result<(::futures::Async<Item>, S), Error>,
                {
                    type Item = Item;
                    type Error = Error;

                    fn poll(&mut self) -> ::futures::Poll<Item, Error> {
                        let state = ::std::mem::replace(&mut self.state, S::default());
                        let (value, state) = try!((self.next)(state));
                        self.state = state;
                        Ok(value)
                    }
                }
            ).unwrap();
        }
    }

    let mut state_machine_closure = quote_expr!(cx,
        StateMachine::new(
            $start_state_expr,
            |mut state| {
                loop {
                    match state {
                        $state_arms
                        State::Illegal => {
                            unreachable!("illegal state")
                        }
                    }
                }
            }
        )
    );

    // If we're not using impl trait, we need to wrap the closure in a box.
    match return_ty.node {
        ast::TyKind::ImplTrait(_) => { }
        _ => {
            state_machine_closure = ast_builder.expr()
                .box_()
                .build(state_machine_closure);
        }
    }

    let block = quote_block!(cx, {
        struct StateMachine<S, F> {
            state: S,
            next: F,
        }

        $state_machine_impl
        $state_machine_impl_future
        $state_enum
        $state_default
        $state_machine_closure
    });

    let item = item_builder.build(block);

    // Syntax extensions are not allowed to have any node ids, so we need to remove them before we
    // return the item to the caller.
    let item = strip_node_ids(item);

    Some(item)
}

fn strip_node_ids(item: P<ast::Item>) -> P<ast::Item> {
    struct Stripper;

    impl fold::Folder for Stripper {
        fn new_id(&mut self, _old_id: ast::NodeId) -> ast::NodeId {
            ast::DUMMY_NODE_ID
        }

        fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
            fold::noop_fold_mac(mac, self)
        }
    }

    let mut items = fold::Folder::fold_item(&mut Stripper, item);
    assert_eq!(items.len(), 1);
    items.pop().unwrap()
}

///////////////////////////////////////////////////////////////////////////

pub struct Builder<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    ast_builder: AstBuilder,
    mar: &'a Mar,
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn shadowed_ident(&self, decl: Var) -> ast::Ident {
        let decl_ident = self.mar.var_decls[decl].ident;
        self.ast_builder.id(format!("{}_shadowed_{}", decl_ident, decl.index()))
    }
}

///////////////////////////////////////////////////////////////////////////

mod block;
mod state;
mod stmt;
