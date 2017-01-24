use data_structures::indexed_vec::{Idx, IndexVec};
use mir::*;
use std::collections::HashMap;
use std::u32;
use syntax::ast;
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

    /// the vector of all scopes that we have created thus far;
    /// we track this for debuginfo later
    visibility_scopes: IndexVec<VisibilityScope, VisibilityScopeData>,
    visibility_scope: VisibilityScope,

    extents: IndexVec<CodeExtent, CodeExtentData>,

    var_indices: HashMap<ast::NodeId, Local>,
    ty_indices: HashMap<ast::NodeId, P<ast::Ty>>,
    local_decls: IndexVec<Local, LocalDecl>,

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

///////////////////////////////////////////////////////////////////////////
/// The `BlockAnd` "monad" packages up the new basic block along with a
/// produced value (sometimes just unit, of course). The `unpack!`
/// macro (and methods below) makes working with `BlockAnd` much more
/// convenient.

#[must_use] // if you don't use one of these results, you're leaving a dangling edge
pub struct BlockAnd<T>(BasicBlock, T);

trait BlockAndExtension {
    fn and<T>(self, v: T) -> BlockAnd<T>;
    fn unit(self) -> BlockAnd<()>;
}

impl BlockAndExtension for BasicBlock {
    fn and<T>(self, v: T) -> BlockAnd<T> {
        BlockAnd(self, v)
    }

    fn unit(self) -> BlockAnd<()> {
        BlockAnd(self, ())
    }
}

/// Update a block pointer and return the value.
/// Use it like `let x = unpack!(block = self.foo(block, foo))`.
macro_rules! unpack {
    ($x:ident = $c:expr) => {
        {
            let BlockAnd(b, v) = $c;
            $x = b;
            v
        }
    };

    ($c:expr) => {
        {
            let BlockAnd(b, ()) = $c;
            b
        }
    };
}

///////////////////////////////////////////////////////////////////////////
/// the main entry point for building MIR for a function

pub fn construct_fn(cx: &ExtCtxt,
                    state_machine_kind: StateMachineKind,
                    span: Span,
                    fn_decl: FunctionDecl,
                    ast_block: P<ast::Block>) -> Mir {
    let (fn_decl, ast_block) = desugar::desugar_block(
        cx,
        state_machine_kind,
        fn_decl,
        ast_block);

    let mut builder = Builder::new(
        cx,
        span,
        state_machine_kind);

    let call_site_extent = builder.extents.push(CodeExtentData::CallSiteScope);
    let arg_extent = builder.extents.push(CodeExtentData::ParameterScope);

    let mut block = START_BLOCK;
    unpack!(block = builder.in_scope(call_site_extent, span, block, |builder| {
        unpack!(block = builder.in_scope(arg_extent, span, block, |builder| {
            builder.args_and_body(block, fn_decl.inputs(), arg_extent, ast_block)
        }));
        // Attribute epilogue to function's closing brace
        let fn_end = Span { lo: span.hi, ..span };
        let source_info = builder.source_info(fn_end);
        let return_block = builder.return_block();
        builder.cfg.terminate(block, source_info, TerminatorKind::Goto { target: return_block });
        builder.cfg.terminate(return_block, source_info, TerminatorKind::Return);
        return_block.unit()
    }));
    assert_eq!(block, builder.return_block());

    builder.finish(fn_decl)
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
            visibility_scopes: IndexVec::new(),
            visibility_scope: ARGUMENT_VISIBILITY_SCOPE,
            loop_scopes: vec![],
            var_indices: HashMap::new(),
            ty_indices: HashMap::new(),
            local_decls: IndexVec::new(),
            extents: IndexVec::new(),
            cached_return_block: None,
        };

        assert_eq!(builder.cfg.start_new_block(span, Some("Start")), START_BLOCK);
        assert_eq!(builder.new_visibility_scope(span), ARGUMENT_VISIBILITY_SCOPE);
        builder.visibility_scopes[ARGUMENT_VISIBILITY_SCOPE].parent_scope = None;

        // FIXME: Stateful doesn't currently have real temporaries, they're just have unique names.
        // The way things work right now, a temporary gets the visibility of the top of the stack,
        // which would be ARGUMENT_VISIBILITY_SCOPE. That confuses the pretty printer, so we'll
        // just add on a new visibility scope to get that to work.
        builder.visibility_scope = builder.new_visibility_scope(span);

        let source_info = builder.source_info(span);
        assert_eq!(
            builder.local_decls.push(LocalDecl::new_return_pointer(source_info, None)),
            RETURN_POINTER);
        assert_eq!(
            builder.local_decls[RETURN_POINTER].source_info.scope,
            RETURN_PTR_VISIBILITY_SCOPE);

        // Give the coroutine arguments their own visibility scope.
        builder.visibility_scope = builder.new_visibility_scope(span);

        let source_info = builder.source_info(span);
        assert_eq!(builder.visibility_scope, COROUTINE_ARGS_VISIBILITY_SCOPE);
        assert_eq!(
            builder.local_decls.push(LocalDecl::new_coroutine_args(source_info, None)),
            COROUTINE_ARGS);
        assert_eq!(
            builder.local_decls[COROUTINE_ARGS].source_info.scope,
            COROUTINE_ARGS_VISIBILITY_SCOPE);

        builder
    }

    fn finish(self, fn_decl: FunctionDecl) -> Mir {
        for (index, block) in self.cfg.basic_blocks.iter().enumerate() {
            if block.terminator.is_none() {
                span_bug!(self.cx, self.fn_span, "no terminator on block {:?}", index);
            }
        }

        Mir::new(
            self.state_machine_kind,
            self.cfg.basic_blocks,
            self.visibility_scopes,
            self.local_decls,
            self.fn_span,
            fn_decl,
        )
    }

    fn args_and_body(&mut self,
                     mut block: BasicBlock,
                     arguments: &[ast::Arg],
                     _argument_extent: CodeExtent,
                     ast_block: P<ast::Block>) -> BlockAnd<()> {
        let mut scope = None;
        // Bind the argument patterns
        for arg in arguments.iter() {
            scope = self.declare_bindings(
                block,
                scope,
                ast_block.span,
                &arg.pat,
                &Some(arg.ty.clone()));

            {
                self.locals_from_pat(&arg.pat);
            }
        }

        // Enter the argument pattern bindings visibility scope, if it exists.
        if let Some(visibility_scope) = scope {
            self.visibility_scope = visibility_scope;
        }

        unpack!(block = self.ast_block(Lvalue::Local(RETURN_POINTER), block, &ast_block));

        block.unit()
    }

    pub fn start_new_extent(&mut self) -> CodeExtent {
        let extent = CodeExtent::new(self.extents.len());
        self.extents.push(CodeExtentData::Misc(ast::DUMMY_NODE_ID));

        extent
    }

    fn return_block(&mut self) -> BasicBlock {
        match self.cached_return_block {
            Some(rb) => rb,
            None => {
                let span = self.fn_span;
                let rb = self.cfg.start_new_block(span, Some("End"));
                debug!("return_block: created {:?}", rb);
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
mod desugar;
mod expr;
mod into;
mod mac;
mod matches;
mod misc;
mod moved;
mod scope;
mod suspend;
mod transition;
