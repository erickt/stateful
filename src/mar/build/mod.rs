use aster::AstBuilder;
use mar::build::simplify::simplify_item;
use mar::indexed_vec::{Idx, IndexVec};
use mar::repr::*;
use syntax::ast::{self, ItemKind};
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;

#[derive(Debug)]
pub struct CFG {
    basic_blocks: IndexVec<BasicBlock, BasicBlockData>,
    var_decls: IndexVec<VarDecl, VarDeclData>,
}

pub struct Builder<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    state_machine_kind: StateMachineKind,
    cfg: CFG,
    scopes: Vec<scope::Scope>,
    loop_scopes: Vec<scope::LoopScope>,
    extents: IndexVec<CodeExtent, CodeExtentData>,
}

#[derive(Debug)]
pub struct Error;


///////////////////////////////////////////////////////////////////////////
// construct() -- the main entry point for building SMIR for a function

pub fn construct(cx: &ExtCtxt,
                 item: P<ast::Item>,
                 state_machine_kind: StateMachineKind) -> Result<Mar, Error> {
    let item = simplify_item(item);

    let (fn_decl, unsafety, abi, generics, ast_block) = match item.node {
        ItemKind::Fn(fn_decl, unsafety, _, abi, generics, block) => {
            (fn_decl, unsafety, abi, generics, block)
        }

        _ => {
            cx.span_err(item.span, "`generator` may only be applied to functions");
            return Err(Error);
        }
    };

    let mut builder = Builder {
        cx: cx,
        state_machine_kind: state_machine_kind,
        cfg: CFG {
            basic_blocks: IndexVec::new(),
            var_decls: IndexVec::new(),
        },
        scopes: vec![],
        loop_scopes: vec![],
        extents: IndexVec::new(),
    };

    let extent = builder.start_new_extent();

    assert_eq!(builder.start_new_block(item.span, Some("Start")), START_BLOCK);
    assert_eq!(builder.start_new_block(item.span, Some("End")), END_BLOCK);

    let mut block = START_BLOCK;

    builder.push_scope(extent, block);

    // Register the arguments as declarations.
    builder.add_decls_from_pats(
        extent,
        block,
        fn_decl.inputs.iter().map(|arg| &arg.pat));

    // Register return pointer.
    let return_ident = AstBuilder::new().id("return_");
    let return_decl = builder.cfg.push_decl(
        ast::Mutability::Immutable,
        return_ident,
        None,
    );

    builder.schedule_forward_decl(item.span, return_decl, None, None);

    let destination = Lvalue::Var {
        span: ast_block.span,
        decl: return_decl,
    };
    builder.cfg.block_data_mut(END_BLOCK).decls.push((return_decl, return_ident));

    block = builder.ast_block(destination, extent, block, &ast_block);

    let live_decls = builder.find_live_decls();

    builder.pop_scope(extent, block);

    builder.terminate(item.span, block, TerminatorKind::Goto { target: END_BLOCK });
    builder.terminate(item.span, END_BLOCK, TerminatorKind::Return);

    Ok(Mar {
        state_machine_kind: builder.state_machine_kind,
        span: item.span,
        ident: item.ident,
        fn_decl: fn_decl.clone(),
        unsafety: unsafety,
        abi: abi,
        generics: generics.clone(),
        input_decls: live_decls,
        basic_blocks: builder.cfg.basic_blocks,
        var_decls: builder.cfg.var_decls,
        extents: builder.extents,
    })
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    pub fn start_new_block(&mut self, span: Span, name: Option<&'static str>) -> BasicBlock {
        let decls = self.find_live_decls();
        self.cfg.start_new_block(span, name, decls)
    }

    pub fn start_new_extent(&mut self) -> CodeExtent {
        let extent = CodeExtent::new(self.extents.len());
        self.extents.push(CodeExtentData::Misc(0));

        extent
    }

    pub fn is_inside_loop(&self) -> bool {
        !self.loop_scopes.is_empty()
    }
}

///////////////////////////////////////////////////////////////////////////
// Builder methods are broken up into modules, depending on what kind
// of thing is being translated.

mod block;
mod cfg;
mod expr;
mod into;
mod mac;
mod matches;
mod moved;
mod scope;
mod transition;
mod simplify;
