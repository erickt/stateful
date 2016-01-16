use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;

pub struct CFG {
    basic_blocks: Vec<BasicBlockData>,
}

pub struct Builder<'a> {
    cx: &'a ExtCtxt<'a>,
    cfg: CFG,
    scopes: Vec<scope::Scope>,
    loop_scopes: Vec<scope::LoopScope>,
    extents: Vec<CodeExtentData>,
}

///////////////////////////////////////////////////////////////////////////
// construct() -- the main entry point for building SMIR for a function

pub fn construct(cx: &ExtCtxt,
                 span: Span,
                 ident: ast::Ident,
                 fn_decl: &ast::FnDecl,
                 ast_block: &ast::Block)
                 -> Mar {
    let mut builder = Builder {
        cx: cx,
        cfg: CFG {
            basic_blocks: vec![],
        },
        scopes: vec![],
        loop_scopes: vec![],
        extents: vec![],
    };

    let extent = builder.start_new_extent();

    assert_eq!(builder.cfg.start_new_block(Some("Start")), START_BLOCK);
    assert_eq!(builder.cfg.start_new_block(Some("End")), END_BLOCK);

    let mut block = START_BLOCK;
    block = builder.args_and_body(extent, block, fn_decl, ast_block);

    builder.cfg.terminate(block, Terminator::Goto { target: END_BLOCK });
    builder.cfg.terminate(END_BLOCK, Terminator::Return);

    Mar {
        ident: ident,
        span: span,
        fn_decl: fn_decl.clone(),
        basic_blocks: builder.cfg.basic_blocks,
        extents: builder.extents,
    }
}

impl<'a> Builder<'a> {
    fn args_and_body(&mut self,
                     extent: CodeExtent,
                     block: BasicBlock,
                     _fn_decl: &ast::FnDecl,
                     ast_block: &ast::Block) -> BasicBlock {
        self.ast_block(extent, block, ast_block)
    }

    pub fn start_new_extent(&mut self) -> CodeExtent {
        let extent = CodeExtent::new(self.extents.len());
        self.extents.push(CodeExtentData);

        extent
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
mod scope;
mod stmt;
mod transition;
