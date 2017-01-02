use analysis::DefiniteAssignment;
use aster::AstBuilder;
use mir::*;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;

pub fn translate(cx: &ExtCtxt,
                 mir: &Mir,
                 assignments: &DefiniteAssignment)
                 -> P<ast::Item> {
    let builder = builder::Builder::new(cx, mir, assignments);
    let block = builder.state_machine();

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
