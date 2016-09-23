use aster::ident::ToIdent;
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

    /// cached block with the RETURN terminator
    cached_return_block: Option<BasicBlock>,
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
    {
        // Create an initial scope for the function.
        builder.push_scope(extent, block);

        block = builder.args_and_body(extent, block, &fn_decl.inputs, ast_block);

        let return_block = builder.return_block();

        builder.terminate(item.span, block, TerminatorKind::Goto {
            target: return_block,
            end_scope: true,
        });

        // The return value shouldn't be dropped when we pop the scope.
        builder.schedule_move(RETURN_POINTER);

        builder.pop_scope(extent, return_block);

        // Make sure the return pointer was actually initialized.
        // FIXME: The return pointer really shouldn't be considered active here, but I'm not sure
        // how to fix that yet.
        {
            let end_decls = &builder.cfg.basic_blocks[return_block].decls;
            if end_decls.first() != Some(&LiveDecl::Active(RETURN_POINTER)) {
                cx.span_bug(
                    item.span,
                    &format!("return pointer not initialized? {:?}", end_decls));
            }
        }

        builder.terminate(item.span, return_block, TerminatorKind::Return);
    }

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
            cached_return_block: None,
        };

        assert_eq!(builder.start_new_block(span, Some("Start")), START_BLOCK);

        builder.var_decls.push(VarDecl {
            mutability: ast::Mutability::Immutable,
            ident: "return_".to_ident(),
            ty: None,
            shadowed_decl: None,
        });

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

    fn args_and_body(&mut self,
                     extent: CodeExtent,
                     block: BasicBlock,
                     arguments: &[ast::Arg],
                     ast_block: P<ast::Block>) -> BasicBlock {
        self.schedule_drop(ast_block.span, extent, RETURN_POINTER);

        // Register the arguments as declarations.
        self.add_decls_from_pats(
            block,
            arguments.iter().map(|arg| &arg.pat));

        let destination = Lvalue::Var {
            span: ast_block.span,
            decl: RETURN_POINTER,
        };

        self.ast_block(destination, extent, block, &ast_block)
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

    fn return_block(&mut self) -> BasicBlock {
        match self.cached_return_block {
            Some(rb) => rb,
            None => {
                let span = self.fn_span;
                let rb = self.start_new_block(span, Some("End"));
                self.cached_return_block = Some(rb);
                rb
            }
        }
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
mod suspend;
