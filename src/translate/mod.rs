use analysis::elaborate_assignments;
use aster::AstBuilder;
use mir::*;
use syntax::ast;
use syntax::ptr::P;
use ty::TyCtxt;

pub fn translate(tcx: TyCtxt, mir: &Mir) -> P<ast::Item> {
    let assignments = elaborate_assignments::analyze_assignments(tcx, &mir);

    let builder = builder::Builder::new(tcx, mir, &assignments);
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
