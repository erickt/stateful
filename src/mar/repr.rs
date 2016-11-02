use aster::AstBuilder;
use mar::indexed_vec::{Idx, IndexVec};
use std::fmt;
use std::ops::{Index, IndexMut};
use std::u32;
use syntax::abi;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

macro_rules! newtype_index {
    ($name:ident, $debug_name:expr) => (
        #[derive(Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(u32);

        impl Idx for $name {
            fn new(value: usize) -> Self {
                assert!(value < (u32::MAX) as usize);
                $name(value as u32)
            }
            fn index(self) -> usize {
                self.0 as usize
            }
        }

        impl fmt::Debug for $name {
            fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
                write!(fmt, "{}{}", $debug_name, self.0)
            }
        }
    )
}

#[derive(Debug)]
pub struct FunctionDecl {
    ident: ast::Ident,
    fn_decl: P<ast::FnDecl>,
    unsafety: ast::Unsafety,
    abi: abi::Abi,
    generics: ast::Generics,
}

impl FunctionDecl {
    pub fn new(ident: ast::Ident,
               fn_decl: P<ast::FnDecl>,
               unsafety: ast::Unsafety,
               abi: abi::Abi,
               generics: ast::Generics) -> Self {
        FunctionDecl {
            ident: ident,
            fn_decl: fn_decl,
            unsafety: unsafety,
            abi: abi,
            generics: generics,
        }
    }

    pub fn ident(&self) -> ast::Ident {
        self.ident
    }

    pub fn inputs(&self) -> &[ast::Arg] {
        &self.fn_decl.inputs
    }

    pub fn unsafety(&self) -> ast::Unsafety {
        self.unsafety
    }

    pub fn abi(&self) -> abi::Abi {
        self.abi
    }

    pub fn generics(&self) -> &ast::Generics {
        &self.generics
    }

    pub fn return_ty(&self) -> P<ast::Ty> {
        match self.fn_decl.output {
            ast::FunctionRetTy::Ty(ref ty) => ty.clone(),
            ast::FunctionRetTy::Default(span) => {
                let builder = AstBuilder::new();
                builder.span(span).ty().unit()
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum StateMachineKind {
    Generator,
    Async,
}

impl fmt::Display for StateMachineKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StateMachineKind::Generator => write!(f, "generator"),
            StateMachineKind::Async => write!(f, "async"),
        }
    }
}

/// Lowered representation of a single function.
#[derive(Debug)]
pub struct Mar {
    pub state_machine_kind: StateMachineKind,

    /// List of basic blocks. References to basic block use a newtyped index type `BasicBlock`
    /// that indexes into this vector.
    basic_blocks: IndexVec<BasicBlock, BasicBlockData>,

    /// List of visibility (lexical) scopes; these are referenced by statements
    /// and used (eventually) for debuginfo. Indexed by a `VisibilityScope`.
    pub visibility_scopes: IndexVec<VisibilityScope, VisibilityScopeData>,

    /// Declarations of locals.
    ///
    /// The first local is the return value pointer, followed by `arg_count`
    /// locals for the function arguments, followed by any user-declared
    /// variables and temporaries.
    pub local_decls: IndexVec<Local, LocalDecl>,

    /// A span representing this MIR, for error reporting
    pub span: Span,

    pub fn_decl: FunctionDecl,
}

/// Where execution begins
pub const START_BLOCK: BasicBlock = BasicBlock(0);

impl Mar {
    pub fn new(
        state_machine_kind: StateMachineKind,
        basic_blocks: IndexVec<BasicBlock, BasicBlockData>,
        visibility_scopes: IndexVec<VisibilityScope, VisibilityScopeData>,
        local_decls: IndexVec<Local, LocalDecl>,
        span: Span,
        fn_decl: FunctionDecl) -> Self
    {
        // We need `arg_count` locals, and one for the return pointer
        let arg_count = fn_decl.inputs().len();
        assert!(local_decls.len() >= arg_count + 1,
            "expected at least {} locals, got {}", arg_count + 1, local_decls.len());
        // FIXME: The return pointer's type isn't calculated correctly yet.
        // assert_eq!(local_decls[RETURN_POINTER].ty, Some(return_ty));

        Mar {
            state_machine_kind: state_machine_kind,
            basic_blocks: basic_blocks,
            visibility_scopes: visibility_scopes,
            local_decls: local_decls,
            span: span,
            fn_decl: fn_decl,
        }
    }

    #[inline]
    pub fn basic_blocks(&self) -> &IndexVec<BasicBlock, BasicBlockData> {
        &self.basic_blocks
    }

    #[inline]
    pub fn basic_blocks_mut(&mut self) -> &mut IndexVec<BasicBlock, BasicBlockData> {
        &mut self.basic_blocks
    }

    pub fn local_decl_data(&self, local: Local) -> &LocalDecl {
        &self.local_decls[local]
    }
}

impl Index<BasicBlock> for Mar {
    type Output = BasicBlockData;

    fn index(&self, index: BasicBlock) -> &BasicBlockData {
        &self.basic_blocks()[index]
    }
}

impl IndexMut<BasicBlock> for Mar {
    fn index_mut(&mut self, index: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks_mut()[index]
    }
}

/// Grouped information about the source code origin of a MIR entity.
/// Intended to be inspected by diagnostics and debuginfo.
/// Most passes can work with it as a whole, within a single function.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SourceInfo {
    /// Source span for the AST pertaining to this MIR entity.
    pub span: Span,

    /// The lexical visibility scope, i.e. which bindings can be seen.
    pub scope: VisibilityScope
}

///////////////////////////////////////////////////////////////////////////
// Variables and temps

newtype_index!(Local, "decl");

pub const RETURN_POINTER: Local = Local(0);

#[derive(Debug, PartialEq)]
pub struct LocalDecl {
    pub mutability: ast::Mutability,
    pub ident: ast::Ident,
    pub ty: Option<P<ast::Ty>>,
    pub shadowed_decl: Option<Local>,

    /// For user-declared variables, stores their source information.
    ///
    /// For temporaries, this is `None`.
    ///
    /// This is the primary way to differentiate between user-declared
    /// variables and compiler-generated temporaries.
    pub source_info: SourceInfo,
}

#[derive(Debug)]
pub struct DeclaredDecl {
    pub decl: Local,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiveDecl {
    Active(Local),
    Moved(Local),
}

///////////////////////////////////////////////////////////////////////////
// BasicBlock and Terminator

newtype_index!(BasicBlock, "bb");

#[derive(Debug)]
pub struct BasicBlockData {
    pub span: Span,
    pub name: Option<&'static str>,
    pub decls: Vec<LiveDecl>,
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
}

impl BasicBlockData {
    pub fn new(span: Span,
               name: Option<&'static str>,
               decls: Vec<LiveDecl>,
               terminator: Option<Terminator>) -> Self {
        BasicBlockData {
            span: span,
            name: name,
            decls: decls,
            statements: vec![],
            terminator: terminator,
        }
    }

    pub fn name(&self) -> Option<&'static str> {
        self.name
    }

    pub fn decls(&self) -> &[LiveDecl] {
        &self.decls
    }

    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }

    pub fn terminator(&self) -> &Terminator {
        self.terminator.as_ref().expect("invalid terminator state")
    }

    pub fn terminator_mut(&mut self) -> &mut Terminator {
        self.terminator.as_mut().expect("invalid terminator state")
    }
}

#[derive(Debug)]
pub struct Terminator {
    pub span: Span,
    pub kind: TerminatorKind,
}

#[derive(Debug)]
pub enum TerminatorKind {
    /// block should have one successor in the graph; we jump there
    Goto {
        target: BasicBlock,
        end_scope: bool,
    },

    /// jump to branch 0 if this lvalue evaluates to true
    If {
        cond: Operand,
        targets: (BasicBlock, BasicBlock),
    },

    /// lvalue evaluates to some enum; jump depending on the branch
    Match {
        discr: Operand,
        targets: Vec<Arm>,
    },

    /// Indicates a normal return. The ReturnPointer lvalue should
    /// have been filled in by now. This should only occur in the
    /// `END_BLOCK`.
    Return,

    Await {
        target: BasicBlock,
    },

    /// jump to target on next iteration.
    Suspend {
        // FIXME: We don't yet support resuming the coroutine with a value yet.
        // lvalue: Lvalue,
        rvalue: P<ast::Expr>,
        target: BasicBlock,
    },
}

impl Terminator {
    pub fn successors(&self) -> Vec<BasicBlock> {
        match self.kind {
            TerminatorKind::Goto { target, .. } => vec![target],
            TerminatorKind::Match { ref targets, .. } => {
                targets.iter().map(|arm| arm.block).collect()
            }
            TerminatorKind::If { targets: (then, else_), .. } => vec![then, else_],
            TerminatorKind::Return => vec![],
            TerminatorKind::Await { target } => vec![target],
            TerminatorKind::Suspend { target, .. } => vec![target],
        }
    }

    pub fn successors_mut(&mut self) -> Vec<&mut BasicBlock> {
        match self.kind {
            TerminatorKind::Goto { ref mut target, .. } => vec![target],
            TerminatorKind::Match { ref mut targets, .. } => {
                targets.iter_mut().map(|arm| &mut arm.block).collect()
            }
            TerminatorKind::If { targets: (ref mut then, ref mut else_), .. } => {
                vec![then, else_]
            }
            TerminatorKind::Return => vec![],
            TerminatorKind::Await { ref mut target } => vec![target],
            TerminatorKind::Suspend { ref mut target, .. } => vec![target],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub pats: Vec<P<ast::Pat>>,
    pub guard: Option<P<ast::Expr>>,
    pub block: BasicBlock,
}

///////////////////////////////////////////////////////////////////////////
// Lvalues

/// A path to a value; something that can be evaluated without
/// changing or disturbing program state.
#[derive(Clone, Debug, PartialEq)]
pub enum Lvalue {
    /// local variable
    Local(Local),

    /// static or static mut variable
    Static(P<ast::Expr>),

    /// projection out of an lvalue (access a field, deref a pointer, etc)
    Projection(Box<LvalueProjection>),
}

/// The `Projection` data structure defines things of the form `B.x`
/// or `*B` or `B[index]`. Note that it is parameterized because it is
/// shared between `Constant` and `Lvalue`. See the aliases
/// `LvalueProjection` etc below.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Projection<B> {
    pub base: B,
    pub elem: ProjectionElem,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ProjectionElem {
    Deref,

    /*
    Field(Field, Ty<'tcx>),
    Index(V),

    /// These indices are generated by slice patterns. Easiest to explain
    /// by example:
    ///
    /// ```
    /// [X, _, .._, _, _] => { offset: 0, min_length: 4, from_end: false },
    /// [_, X, .._, _, _] => { offset: 1, min_length: 4, from_end: false },
    /// [_, _, .._, X, _] => { offset: 2, min_length: 4, from_end: true },
    /// [_, _, .._, _, X] => { offset: 1, min_length: 4, from_end: true },
    /// ```
    ConstantIndex {
        /// index or -index (in Python terms), depending on from_end
        offset: u32,
        /// thing being indexed must be at least this long
        min_length: u32,
        /// counting backwards from end?
        from_end: bool,
    },

    /// These indices are generated by slice patterns.
    ///
    /// slice[from:-to] in Python terms.
    Subslice {
        from: u32,
        to: u32,
    },

    /// "Downcast" to a variant of an ADT. Currently, we only introduce
    /// this for ADTs with more than one variant. It may be better to
    /// just introduce it always, or always for enums.
    Downcast(AdtDef<'tcx>, usize
    */
}

/// Alias for projections as they appear in lvalues, where the base is an lvalue
/// and the index is an operand.
pub type LvalueProjection = Projection<Lvalue>;

/// Alias for projections as they appear in lvalues, where the base is an lvalue
/// and the index is an operand.
pub type LvalueElem = ProjectionElem;

impl Lvalue {
    pub fn deref(self) -> Lvalue {
        self.elem(ProjectionElem::Deref)
    }

    pub fn elem(self, elem: LvalueElem) -> Lvalue {
        Lvalue::Projection(Box::new(LvalueProjection {
            base: self,
            elem: elem,
        }))
    }
}

pub trait ToExpr {
    fn to_expr(&self, local_decls: &IndexVec<Local, LocalDecl>) -> P<ast::Expr>;
}

impl ToExpr for Lvalue {
    fn to_expr(&self, local_decls: &IndexVec<Local, LocalDecl>) -> P<ast::Expr> {
        match *self {
            Lvalue::Local(ref local) => {
                let local_decl = &local_decls[*local];
                AstBuilder::new().span(local_decl.source_info.span).expr().id(local_decl.ident)
            }
            Lvalue::Static(ref expr) => {
                expr.clone()
            }
            Lvalue::Projection(ref projection) => {
                projection.to_expr(local_decls)
            }
        }
    }
}

impl<B> ToExpr for Projection<B> where B: ToExpr {
    fn to_expr(&self, local_decls: &IndexVec<Local, LocalDecl>) -> P<ast::Expr> {
        let base = self.base.to_expr(local_decls);

        match self.elem {
            ProjectionElem::Deref => {
                AstBuilder::new().span(base.span).expr().deref().build(base)
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// Scopes

newtype_index!(VisibilityScope, "scope");
pub const ARGUMENT_VISIBILITY_SCOPE : VisibilityScope = VisibilityScope(0);

#[derive(Clone, Debug)]
pub struct VisibilityScopeData {
    pub span: Span,
    pub parent_scope: Option<VisibilityScope>,
}

///////////////////////////////////////////////////////////////////////////
// Operands

/// These are values that can appear inside an rvalue (or an index
/// lvalue). They are intentionally limited to prevent rvalues from
/// being nested in one another.
#[derive(Clone, Debug, PartialEq)]
pub enum Operand {
    Consume(Lvalue),
    Constant(Constant),
}

impl ToExpr for Operand {
    fn to_expr(&self, local_decls: &IndexVec<Local, LocalDecl>) -> P<ast::Expr> {
        match *self {
            Operand::Consume(ref lvalue) => lvalue.to_expr(local_decls),
            Operand::Constant(ref constant) => constant.to_expr(local_decls),
        }
    }
}

///////////////////////////////////////////////////////////////////////////
/// Rvalues

#[derive(Clone, Debug, PartialEq)]
pub enum Rvalue {
    /// x (either a move or copy, depending on type of x)
    Use(Operand),

    Ref(ast::Mutability, Lvalue),

    Tuple(Vec<Operand>),
    Struct(ast::Path, Vec<ast::Field>, Vec<Operand>, Option<Operand>),
    Range(Option<Operand>, Option<Operand>, ast::RangeLimits),
}

impl ToExpr for Rvalue {
    fn to_expr(&self, local_decls: &IndexVec<Local, LocalDecl>) -> P<ast::Expr> {
        let builder = AstBuilder::new();

        match *self {
            Rvalue::Use(ref lvalue) => {
                lvalue.to_expr(local_decls)
            }
            Rvalue::Ref(ast::Mutability::Immutable, ref arg) => {
                builder.expr().ref_().build(arg.to_expr(local_decls))
            }
            Rvalue::Ref(ast::Mutability::Mutable, ref arg) => {
                builder.expr().mut_ref().build(arg.to_expr(local_decls))
            }
            Rvalue::Tuple(ref items) => {
                builder.expr().tuple()
                    .with_exprs(items.iter().map(|item| item.to_expr(local_decls)))
                    .build()
            }
            Rvalue::Struct(ref path, ref fields, ref items, ref wth) => {
                let fields = fields.iter()
                    .zip(items)
                    .map(|(field, item)| {
                        let item = item.to_expr(local_decls);
                        ast::Field { expr: item, .. field.clone() }
                    });

                let struct_builder = builder.expr().struct_path(path.clone())
                    .with_fields(fields);

                if let Some(ref wth) = *wth {
                    let wth = wth.to_expr(local_decls);
                    struct_builder.build_with().build(wth)
                } else {
                    struct_builder.build()
                }
            }
            Rvalue::Range(ref from, ref to, ref limits) => {
                let from = from.as_ref().map(|from| from.to_expr(local_decls));
                let to = to.as_ref().map(|to| to.to_expr(local_decls));

                builder.expr().range()
                    .from_opt(from)
                    .to_opt(to, *limits)
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////
/// Constants
///
/// Two constants are equal if they are the same constant. Note that
/// this does not necessarily mean that they are "==" in Rust -- in
/// particular one must be wary of `NaN`!

#[derive(Clone, Debug, PartialEq)]
pub struct Constant {
    pub span: Span,
    pub literal: P<ast::Lit>,
}


impl ToExpr for Constant {
    fn to_expr(&self, _local_decls: &IndexVec<Local, LocalDecl>) -> P<ast::Expr> {
        AstBuilder::new().span(self.span).expr()
            .build_lit(self.literal.clone())
    }
}

///////////////////////////////////////////////////////////////////////////
// Statements

#[derive(Debug)]
pub enum Statement {
    Expr(ast::Stmt),
    Declare(Local),
    Assign {
        span: Span,
        lvalue: Lvalue,
        rvalue: Rvalue,
    },
    Call {
        span: Span,
        lvalue: Lvalue,
        fun: Operand,
        args: Vec<Operand>,
    },
    MethodCall {
        span: Span,
        lvalue: Lvalue,
        ident: ast::SpannedIdent,
        tys: Vec<P<ast::Ty>>,
        args: Vec<Operand>,
    },
    Drop {
        lvalue: Local,
        moved: bool,
    },
}

#[derive(Clone, Copy, Debug)]
pub struct ShadowedDecl {
    pub lvalue: ast::Ident,
    pub decl: Local,
}

///////////////////////////////////////////////////////////////////////////
// Code Extents

newtype_index!(CodeExtent, "extent");

#[derive(Debug)]
pub enum CodeExtentData {
    Misc(ast::NodeId),

    ParameterScope { fn_id: ast::NodeId, body_id: ast::NodeId },

    // extent of code following a `let id = expr;` binding in a block
    Remainder(BlockRemainder),
}

/// Represents a subscope of `block` for a binding that is introduced
/// by `block.stmts[first_statement_index]`. Such subscopes represent
/// a suffix of the block. Note that each subscope does not include
/// the initializer expression, if any, for the statement indexed by
/// `first_statement_index`.
///
/// For example, given `{ let (a, b) = EXPR_1; let c = EXPR_2; ... }`:
///
/// * the subscope with `first_statement_index == 0` is scope of both
///   `a` and `b`; it does not include `EXPR_1`, but does include
///   everything after that first `let`. (If you want a scope that
///   includes `EXPR_1` as well, then do not use `CodeExtentData::Remainder`,
///   but instead another `CodeExtent` that encompasses the whole block,
///   e.g. `CodeExtentData::Misc`.
///
/// * the subscope with `first_statement_index == 1` is scope of `c`,
///   and thus does not include `EXPR_2`, but covers the `...`.
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy)]
pub struct BlockRemainder {
    pub block: ast::NodeId,
    pub first_statement_index: u32,
}
