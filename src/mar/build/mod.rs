use mar::build::scope::ConditionalScope;
use mar::indexed_vec::{Idx, IndexVec};
use mar::repr::*;
use std::collections::{HashMap, HashSet};
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
    conditional_scopes: HashMap<ScopeId, ConditionalScope>,

    /// the vector of all scopes that we have created thus far;
    /// we track this for debuginfo later
    visibility_scopes: IndexVec<VisibilityScope, VisibilityScopeData>,
    visibility_scope: VisibilityScope,

    extents: IndexVec<CodeExtent, CodeExtentData>,

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

#[derive(Debug)]
pub struct Error;

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
// construct() -- the main entry point for building SMIR for a function

pub fn construct(cx: &ExtCtxt,
                 state_machine_kind: StateMachineKind,
                 span: Span,
                 fn_decl: FunctionDecl,
                 ast_block: P<ast::Block>) -> Mar {
    let ast_block = desugar::desugar_block(cx, state_machine_kind, ast_block);

    let mut builder = Builder::new(cx, span, state_machine_kind);
    let extent = builder.start_new_extent();

    let mut block = START_BLOCK;
    {
        // Create an initial scope for the function.
        builder.push_scope(extent, block);

        let source_info = SourceInfo {
            span: span,
            scope: builder.visibility_scope,
        };

        // Declare the return pointer.
        builder.declare_binding(
            source_info,
            ast::Mutability::Immutable,
            "return_",
            None,
        );

        unpack!(block = builder.in_scope(span, block, |this| {
            this.args_and_body(block, fn_decl.inputs(), ast_block)
        }));

        let return_block = builder.return_block();

        // Make sure the return pointer was actually initialized.
        // FIXME: The return pointer really shouldn't be considered active here, but I'm not sure
        // how to fix that yet.
        {
            let end_decls = &builder.cfg.basic_blocks[return_block].decls;
            if end_decls.first() != Some(&LiveDecl::Active(RETURN_POINTER)) {
                cx.span_warn(
                    span,
                    &format!("return pointer not initialized? {:?}", end_decls));
            }
        }

        builder.terminate(span, block, TerminatorKind::Goto {
            target: return_block,
            end_scope: true,
        });

        // The return value shouldn't be dropped when we pop the scope.
        builder.schedule_move(span, RETURN_POINTER);

        builder.pop_scope(extent, return_block);

        builder.terminate(span, return_block, TerminatorKind::Return);
    }

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
            conditional_scopes: HashMap::new(),
            local_decls: IndexVec::new(),
            extents: IndexVec::new(),
            cached_return_block: None,
        };

        assert_eq!(builder.start_new_block(span, Some("Start")), START_BLOCK);
        assert_eq!(builder.new_visibility_scope(span), ARGUMENT_VISIBILITY_SCOPE);
        builder.visibility_scopes[ARGUMENT_VISIBILITY_SCOPE].parent_scope = None;

        builder
    }

    fn finish(self, fn_decl: FunctionDecl) -> Mar {
        for (index, block) in self.cfg.basic_blocks.iter().enumerate() {
            if block.terminator.is_none() {
                self.cx.span_bug(
                    self.fn_span,
                    &format!("no terminator on block {:?}", index));
            }
        }

        // We need to make sure all variables have actually been initialized.
        // First, gather up them all.
        let mut initialized_vars = HashSet::new();
        for block in self.cfg.basic_blocks.iter() {
            for live_decl in &block.decls {
                match *live_decl {
                    LiveDecl::Active(var) | LiveDecl::Moved(var) => {
                        initialized_vars.insert(var);
                    }
                }
            }
        }

        // Now take the difference between the initialized vars and the total set of variables.
        let mut uninitialized_vars = vec![];
        for var in self.local_decls.indices() {
            if !initialized_vars.contains(&var) {
                uninitialized_vars.push(var);
            }
        }

        if !uninitialized_vars.is_empty() {
            self.cx.span_warn(
                self.fn_span,
                &format!("uninitialized variables: {:?}", uninitialized_vars));
        }

        Mar::new(
            self.state_machine_kind,
            self.cfg.basic_blocks,
            self.visibility_scopes,
            self.local_decls,
            self.fn_span,
            fn_decl,
        )
    }

    fn args_and_body(&mut self,
                     block: BasicBlock,
                     arguments: &[ast::Arg],
                     ast_block: P<ast::Block>) -> BlockAnd<()> {
        //self.schedule_drop(ast_block.span, extent, RETURN_POINTER);

        // Register the arguments as declarations.
        self.add_decls_from_pats(
            block,
            arguments.iter().map(|arg| &arg.pat));

        let destination = Lvalue::Local(RETURN_POINTER);

        self.ast_block(destination, block, &ast_block)
    }

    pub fn start_new_block(&mut self, span: Span, name: Option<&'static str>) -> BasicBlock {
        let decls = self.find_live_decls();

        let block = self.cfg.start_new_block(span, name, decls.clone());

        debug!("start_new_block: id={:?} decls={:?}", block, decls); 

        block
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
