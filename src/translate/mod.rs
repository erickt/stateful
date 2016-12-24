use analysis::DefiniteAssignment;
use aster::AstBuilder;
use data_structures::indexed_vec::Idx;
use mir::*;
use std::collections::BTreeMap;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;

pub fn translate(cx: &ExtCtxt,
                 mir: &Mir,
                 assignments: &DefiniteAssignment)
                 -> P<ast::Item> {
    let builder = builder::Builder::new(cx, mir, assignments);

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

mod block;
mod builder;
mod internal_state;
mod resume_state;
mod state;
mod stmt;
