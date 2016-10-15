use aster::ident::ToIdent;
use mar::build::simplify::simplify_item;
use mar::indexed_vec::{Idx, IndexVec};
use mar::repr::*;
use std::collections::HashSet;
use std::u32;
use syntax::abi;
use syntax::ast::{self, ItemKind};
use syntax::codemap::Span;
use syntax::ext::base::ExtCtxt;
use syntax::ptr::P;

pub struct Builder<'a, 'b: 'a> {
    cx: &'a ExtCtxt<'b>,
    cfg: CFG,
    state_machine_kind: StateMachineKind,

    fn_span: Span,

    /// the current set of scopes, updated as we traverse;
    /// see the `scope` module for more details
    scopes: Vec<scope::Scope>,

    ///  for each scope, a span of blocks that defines it;
    ///  we track these for use in region and borrow checking,
    ///  but these are liable to get out of date once optimization
    ///  begins. They are also hopefully temporary, and will be
    ///  no longer needed when we adopt graph-based regions.
    scope_auxiliary: IndexVec<ScopeId, ScopeAuxiliary>,

    /// the current set of loops; see the `scope` module for more
    /// details
    loop_scopes: Vec<scope::LoopScope>,
    conditional_scopes: HashSet<ScopeId>,

    extents: IndexVec<CodeExtent, CodeExtentData>,

    var_decls: IndexVec<Var, VarDecl>,

    /// cached block with the RETURN terminator
    cached_return_block: Option<BasicBlock>,
}

#[derive(Debug)]
pub struct CFG {
    basic_blocks: IndexVec<BasicBlock, BasicBlockData>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

impl Idx for ScopeId {
    fn new(index: usize) -> ScopeId {
        assert!(index < (u32::MAX as usize));
        ScopeId(index as u32)
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

/// For each scope, we track the extent (from the HIR) and a
/// single-entry-multiple-exit subgraph that contains all the
/// statements/terminators within it.
///
/// This information is separated out from the main `ScopeData`
/// because it is short-lived. First, the extent contains node-ids,
/// so it cannot be saved and re-loaded. Second, any optimization will mess up
/// the dominator/postdominator information.
///
/// The intention is basically to use this information to do
/// regionck/borrowck and then throw it away once we are done.
pub struct ScopeAuxiliary {
    /// extent of this scope from the MIR.
    pub extent: CodeExtent,

    /*
    /// "entry point": dominator of all nodes in the scope
    pub dom: Location,

    /// "exit points": mutual postdominators of all nodes in the scope
    pub postdoms: Vec<Location>,
    */
}

#[derive(Debug)]
pub struct Error;


///////////////////////////////////////////////////////////////////////////
// construct() -- the main entry point for building SMIR for a function

pub fn construct(cx: &ExtCtxt,
                 item: P<ast::Item>,
                 state_machine_kind: StateMachineKind) -> Result<Mar, Error> {
    let item = simplify_item(cx, item);

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
            scope_auxiliary: IndexVec::new(),
            loop_scopes: vec![],
            conditional_scopes: HashSet::new(),
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
            span: span,
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
