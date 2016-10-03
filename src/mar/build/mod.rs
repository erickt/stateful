use aster::AstBuilder;
use mar::build::simplify::simplify_item;
use mar::indexed_vec::{Idx, IndexVec};
use mar::repr::*;
use syntax::abi;
use syntax::ast::{self, ItemKind};
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;

#[derive(Debug)]
pub struct CFG {
    basic_blocks: IndexVec<BasicBlock, BasicBlockData>,
}

pub struct Builder<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    cfg: CFG,
    state_machine_kind: StateMachineKind,

    fn_span: Span,

    /// the current set of scopes, updated as we traverse;
    /// see the `scope` module for more details
    scopes: Vec<scope::Scope>,

    /// the current set of loops; see the `scope` module for more
    /// details
    loop_scopes: Vec<scope::LoopScope>,

    extents: IndexVec<CodeExtent, CodeExtentData>,

    var_decls: IndexVec<Var, VarDecl>,
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

    let mut builder = Builder::new(cx, item.span, state_machine_kind);
    let extent = builder.start_new_extent();

    let mut block = START_BLOCK;

    builder.push_scope(extent, block);

    // Register the arguments as declarations.
    builder.add_decls_from_pats(
        extent,
        block,
        fn_decl.inputs.iter().map(|arg| &arg.pat));

    // Register return pointer.
    let return_ident = AstBuilder::new().id("return_");
    let return_decl = builder.declare_decl(
        ast::Mutability::Immutable,
        return_ident,
        None,
    );

    builder.declare_binding(item.span, return_decl);

    let destination = Lvalue::Var {
        span: ast_block.span,
        decl: return_decl,
    };
    builder.cfg.block_data_mut(END_BLOCK).decls.push(LiveDecl::Active(return_decl));

    block = builder.ast_block(destination, extent, block, &ast_block);

    let live_decls = builder.find_live_decls();

    builder.terminate(item.span, block, TerminatorKind::Goto {
        target: END_BLOCK,
        end_scope: false,
    });
    builder.terminate(item.span, END_BLOCK, TerminatorKind::Return);

    // The return value shouldn't be dropped.
    builder.schedule_move(return_decl);
    builder.pop_scope(extent, END_BLOCK);

    Ok(builder.finish(item.ident,
                      fn_decl,
                      unsafety,
                      abi,
                      generics))
}

impl<'a, 'b: 'a> Builder<'a, 'b> {
    fn new(cx: &'a ExtCtxt<'b>,
           span: Span,
           state_machine_kind: StateMachineKind) -> Self {
        let mut builder = Builder {
            cx: cx,
            cfg: CFG { basic_blocks: IndexVec::new() },
            fn_span: span,
            state_machine_kind: state_machine_kind,
            scopes: vec![],
            loop_scopes: vec![],
            var_decls: IndexVec::new(),
            extents: IndexVec::new(),
        };

        assert_eq!(builder.start_new_block(span, Some("Start")), START_BLOCK);
        assert_eq!(builder.start_new_block(span, Some("End")), END_BLOCK);

        builder
    }

    fn finish(self,
              ident: ast::Ident,
              fn_decl: P<ast::FnDecl>,
              unsafety: ast::Unsafety,
              abi: abi::Abi,
              generics: ast::Generics) -> Mar {
        for (index, block) in self.cfg.basic_blocks.iter().enumerate() {
            if block.terminator.is_none() {
                self.cx.span_bug(
                    self.fn_span,
                    &format!("no terminator on block {:?}", index));
            }
        }

        Mar {
            state_machine_kind: self.state_machine_kind,
            span: self.fn_span,
            ident: ident,
            fn_decl: fn_decl.clone(),
            unsafety: unsafety,
            abi: abi,
            generics: generics.clone(),
            basic_blocks: self.cfg.basic_blocks,
            var_decls: self.var_decls,
            extents: self.extents,
        }
    }

    pub fn start_new_block(&mut self, span: Span, name: Option<&'static str>) -> BasicBlock {
        let decls = self.find_live_decls();
        self.cfg.start_new_block(span, name, decls)
    }

    pub fn start_new_extent(&mut self) -> CodeExtent {
        let extent = CodeExtent::new(self.extents.len());
        self.extents.push(CodeExtentData::Misc(ast::DUMMY_NODE_ID));

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
