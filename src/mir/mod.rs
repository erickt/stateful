use aster::AstBuilder;
use aster::ident::ToIdent;
use data_structures::indexed_vec::{Idx, IndexVec};
use std::borrow::Cow;
use std::fmt::{self, Debug, Formatter, Write};
use std::ops::{Index, IndexMut};
use std::u32;
use syntax::abi;
use syntax::ast;
use syntax::codemap::Span;
use syntax::print::pprust;
use syntax::ptr::P;

//pub mod visit;

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

        impl Debug for $name {
            fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
                write!(fmt, "{}{}", $debug_name, self.0)
            }
        }
    )
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub ident: ast::Ident,
    pub fn_decl: P<ast::FnDecl>,
    pub unsafety: ast::Unsafety,
    pub abi: abi::Abi,
    pub generics: ast::Generics,
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
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            StateMachineKind::Generator => write!(f, "generator"),
            StateMachineKind::Async => write!(f, "async"),
        }
    }
}

/// Lowered representation of a single function.
#[derive(Debug)]
pub struct Mir {
    pub state_machine_kind: StateMachineKind,

    /// List of basic blocks. References to basic block use a newtyped index type `BasicBlock`
    /// that indexes into this vector.
    basic_blocks: IndexVec<BasicBlock, BasicBlockData>,

    /// List of visibility (lexical) scopes; these are referenced by statements
    /// and used (eventually) for debuginfo. Indexed by a `VisibilityScope`.
    pub visibility_scopes: IndexVec<VisibilityScope, VisibilityScopeData>,

    /// Return type of the function.
    pub return_ty: P<ast::Ty>,

    /// Declarations of locals.
    ///
    /// The first local is the return value pointer, followed by `arg_count`
    /// locals for the function arguments, followed by any user-declared
    /// variables and temporaries.
    pub local_decls: IndexVec<Local, LocalDecl>,

    /// Number of arguments this function takes.
    ///
    /// Starting at local 2, `arg_count` locals will be provided by the caller
    /// and can be assumed to be initialized.
    ///
    /// If this MIR was built for a constant, this will be 0.
    pub arg_count: usize,

    /// A span representing this MIR, for error reporting
    pub span: Span,

    pub fn_decl: FunctionDecl,
}

/// Where execution begins
pub const START_BLOCK: BasicBlock = BasicBlock(0);

impl Mir {
    pub fn new(state_machine_kind: StateMachineKind,
               basic_blocks: IndexVec<BasicBlock, BasicBlockData>,
               visibility_scopes: IndexVec<VisibilityScope, VisibilityScopeData>,
               local_decls: IndexVec<Local, LocalDecl>,
               span: Span,
               fn_decl: FunctionDecl) -> Self
    {
        // We need `arg_count` locals, and one for the return pointer
        let arg_count = fn_decl.inputs().len();
        assert!(local_decls.len() >= arg_count + 2,
            "expected at least {} locals, got {}", arg_count + 2, local_decls.len());
        // FIXME: The return pointer's type isn't calculated correctly yet.
        // assert_eq!(local_decls[RETURN_POINTER].ty, Some(return_ty));

        Mir {
            state_machine_kind: state_machine_kind,
            basic_blocks: basic_blocks,
            visibility_scopes: visibility_scopes,
            return_ty: fn_decl.return_ty(),
            local_decls: local_decls,
            arg_count: arg_count,
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

    /*
    #[inline]
    pub fn local_kind(&self, local: Local) -> LocalKind {
        let index = local.0 as usize;
        if index == 0 {
            debug_assert!(self.local_decls[local].mutability == ast::Mutability::Mutable,
                          "return pointer should be mutable");

            LocalKind::ReturnPointer
        } else if index == 1 {
            debug_assert!(self.local_decls[local].mutability == ast::Mutability::Immutable,
                          "coroutine arguments should be immutable");

            LocalKind::CoroutineArgs

        } else if index < self.arg_count + 2 {
            LocalKind::Arg
        /*
        } else if self.local_decls[local].name.is_some() {
            LocalKind::Var
        } else {
            debug_assert!(self.local_decls[local].mutability == Mutability::Mut,
                          "temp should be mutable");

            LocalKind::Temp
        }
        */
        } else {
            LocalKind::Var
        }
    }
    */

    /// Returns an iterator over all user-declared locals.
    #[inline]
    pub fn vars_iter<'a>(&'a self) -> Box<Iterator<Item=Local> + 'a> {
        let local_count = self.local_decls.len();
        Box::new((self.arg_count + 2..local_count).map(Local::new))
    }

    /// Returns an iterator over all function arguments.
    pub fn args_iter(&self) -> Box<Iterator<Item=Local>> {
        // NOTE: we also consider the `coroutine_args` as part of the arguments.
        Box::new((1 .. self.arg_count + 2).map(Local::new))
    }

    /*
    /// Returns an iterator over all user-defined variables and compiler-generated temporaries (all
    /// locals that are neither arguments nor the return pointer).
    #[inline]
    pub fn vars_and_temps_iter(&self) -> Box<Iterator<Item=Local>> {
        let local_count = self.local_decls.len();
        Box::new((self.arg_count + 2 .. local_count).map(Local::new))
    }
    */
}

impl Index<BasicBlock> for Mir {
    type Output = BasicBlockData;

    fn index(&self, index: BasicBlock) -> &BasicBlockData {
        &self.basic_blocks()[index]
    }
}

impl IndexMut<BasicBlock> for Mir {
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

newtype_index!(Local, "_");

pub const RETURN_POINTER: Local = Local(0);
pub const COROUTINE_ARGS: Local = Local(1);

/*
/// Classifies locals into categories. See `Mir::local_kind`.
#[derive(PartialEq, Eq, Debug)]
pub enum LocalKind {
    /// User-declared variable binding
    Var,
    /*
    /// Compiler-introduced temporary
    Temp,
    */
    /// Function argument
    Arg,
    /// Location of function's return value
    ReturnPointer,
    /// Coroutine arguments
    CoroutineArgs,
}
*/

#[derive(Debug, PartialEq)]
pub struct LocalDecl {
    /// `let mut x` vs `let x`.
    ///
    /// Temporaries and the return pointer are always mutable.
    pub mutability: ast::Mutability,

    /// Type of this local.
    pub ty: Option<P<ast::Ty>>,

    /// Name of the local, used in debuginfo and pretty-printing.
    ///
    /// Note that function arguments can also have this set to `Some(_)`
    /// to generate better debuginfo.
    pub name: ast::Ident,

    /// For user-declared variables, stores their source information.
    ///
    /// For temporaries, this is `None`.
    ///
    /// This is the primary way to differentiate between user-declared
    /// variables and compiler-generated temporaries.
    pub source_info: SourceInfo,

    pub shadowed_decl: Option<Local>,
}

impl LocalDecl {
    /// Builds a `LocalDecl` for the return pointer.
    ///
    /// This must be inserted into the `local_decls` list as the first local.
    #[inline]
    pub fn new_return_pointer(source_info: SourceInfo,
                              return_ty: Option<P<ast::Ty>>) -> LocalDecl {
        LocalDecl {
            mutability: ast::Mutability::Mutable,
            ty: return_ty,
            source_info: source_info,
            name: "return_pointer".to_ident(),
            shadowed_decl: None,
        }
    }

    /// Builds a `LocalDecl` for the coroutine arguments.
    ///
    /// This must be inserted into the `local_decls` list as the second local.
    #[inline]
    pub fn new_coroutine_args(source_info: SourceInfo,
                              return_ty: Option<P<ast::Ty>>) -> LocalDecl {
        LocalDecl {
            mutability: ast::Mutability::Immutable,
            ty: return_ty,
            source_info: source_info,
            name: "coroutine_args".to_ident(),
            shadowed_decl: None,
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// BasicBlock and Terminator

newtype_index!(BasicBlock, "bb");

#[derive(Debug)]
pub struct BasicBlockData {
    pub span: Span,
    pub name: Option<&'static str>,
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
}

impl BasicBlockData {
    pub fn new(span: Span,
               name: Option<&'static str>) -> Self {
        BasicBlockData {
            span: span,
            name: name,
            statements: vec![],
            terminator: None,
        }
    }

    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }

    pub fn terminator(&self) -> &Terminator {
        self.terminator.as_ref().expect("invalid terminator state")
    }

    /*
    pub fn terminator_mut(&mut self) -> &mut Terminator {
        self.terminator.as_mut().expect("invalid terminator state")
    }
    */
}

pub struct Terminator {
    pub source_info: SourceInfo,
    pub kind: TerminatorKind,
}

impl Debug for Terminator {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        self.kind.fmt(fmt)
    }
}

pub enum TerminatorKind {
    /// block should have one successor in the graph; we jump there
    Goto {
        target: BasicBlock,
    },

    /// Effectively a goto that has an additional edge to the after-break block. This allows
    /// dataflow to properly initialize the block following a break, even though it is not
    /// reachable.
    Break {
        target: BasicBlock,
        after_target: BasicBlock,
    },

    /// jump to branch 0 if this lvalue evaluates to true
    If {
        cond: Operand,
        targets: (BasicBlock, BasicBlock),
    },

    /// lvalue evaluates to some enum; jump depending on the branch
    Match {
        discr: Operand,
        arms: Vec<Arm>,
    },

    /// Indicates a normal return. The ReturnPointer lvalue should
    /// have been filled in by now. This should only occur in the
    /// `END_BLOCK`.
    Return,

    /// jump to target on next iteration.
    Suspend {
        destination: (Lvalue, BasicBlock),
        arg: Operand,
    },
}

impl Terminator {
    pub fn successors(&self) -> Vec<BasicBlock> {
        self.kind.successors()
    }

    /*
    pub fn successors_mut(&mut self) -> Vec<&mut BasicBlock> {
        self.kind.successors_mut()
    }
    */
}

impl Debug for TerminatorKind {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        self.fmt_head(fmt)?;
        let successors = self.successors();
        let labels = self.fmt_successor_labels();
        assert_eq!(successors.len(), labels.len());

        match successors.len() {
            0 => Ok(()),

            1 => write!(fmt, " -> {:?}", successors[0]),

            _ => {
                write!(fmt, " -> [")?;
                for (i, target) in successors.iter().enumerate() {
                    if i > 0 {
                        write!(fmt, ", ")?;
                    }
                    write!(fmt, "{}: {:?}", labels[i], target)?;
                }
                write!(fmt, "]")
            }

        }
    }
}

impl TerminatorKind {
    pub fn successors(&self) -> Vec<BasicBlock> {
        match *self {
            TerminatorKind::Goto { target } => {
                vec![target]
            }
            TerminatorKind::Break { target, after_target } => {
                // Explicitly don't add `after_target` to successors. It needs to be handled.
                vec![target, after_target]
            }
            TerminatorKind::Suspend { destination: (_, target), .. } => {
                vec![target]
            }
            TerminatorKind::Match { ref arms, .. } => {
                arms.iter().map(|arm| arm.block).collect()
            }
            TerminatorKind::If { targets: (then, else_), .. } => vec![then, else_],
            TerminatorKind::Return => vec![],
        }
    }

    /*
    pub fn successors_mut(&mut self) -> Vec<&mut BasicBlock> {
        match *self {
            TerminatorKind::Goto { ref mut target } => {
                vec![target]
            }
            TerminatorKind::Break { ref mut target, ref mut after_target } => {
                // Explicitly don't add `after_target` to successors. It needs to be handled.
                vec![target, after_target]
            }
            TerminatorKind::Suspend { destination: (_, ref mut target), .. } => {
                vec![target]
            }
            TerminatorKind::Match { ref mut arms, .. } => {
                arms.iter_mut().map(|arm| &mut arm.block).collect()
            }
            TerminatorKind::If { targets: (ref mut then, ref mut else_), .. } => {
                vec![then, else_]
            }
            TerminatorKind::Return => vec![],
        }
    }
    */

    /// Write the "head" part of the terminator; that is, its name and the data it uses to pick the
    /// successor basic block, if any. The only information not inlcuded is the list of possible
    /// successors, which may be rendered differently between the text and the graphviz format.
    pub fn fmt_head<W: Write>(&self, fmt: &mut W) -> fmt::Result {
        use self::TerminatorKind::*;
        match *self {
            Goto { .. } => write!(fmt, "goto"),
            Break { .. } => write!(fmt, "break"),
            If { cond: ref lv, .. } => write!(fmt, "if({:?})", lv),
            Match { discr: ref lv, .. } => write!(fmt, "match({:?})", lv),
            Return => write!(fmt, "return"),
            Suspend { destination: (ref destination, _), ref arg, .. } => {
                write!(fmt, "{:?} = suspend({:?})", destination, arg)
            }
        }
    }

    /// Return the list of labels for the edges to the successor basic blocks.
    pub fn fmt_successor_labels(&self) -> Vec<Cow<'static, str>> {
        use self::TerminatorKind::*;
        match *self {
            Return => vec![],
            Goto { target: _ } => vec!["".into()],
            Break { .. } => vec!["target".into(), "after_target".into()],
            If { .. } => vec!["true".into(), "false".into()],
            Match { ref arms, .. } => {
                arms.iter()
                    .map(|arm| {
                        let pats = arm.pats.iter()
                            .map(|pat| pprust::pat_to_string(pat))
                            .collect::<Vec<_>>()
                            .join("|");

                        let lvalues = arm.lvalues.iter()
                            .map(|lvalue| format!("{:?}", lvalue))
                            .collect::<Vec<_>>()
                            .join(",");

                        Cow::from(format!("{}: [{}]", pats, lvalues))
                    })
                    .collect()
            }
            Suspend { .. } => vec!["".into()],
        }
    }
}

#[derive(Debug, Clone)]
pub struct Arm {
    pub pats: Vec<P<ast::Pat>>,
    pub guard: Option<P<ast::Expr>>,
    pub lvalues: Vec<Lvalue>,
    pub block: BasicBlock,
}

///////////////////////////////////////////////////////////////////////////
// Lvalues

/// A path to a value; something that can be evaluated without
/// changing or disturbing program state.
#[derive(Clone, PartialEq)]
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
                AstBuilder::new().span(local_decl.source_info.span).expr().id(local_decl.name)
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
            /*
            ProjectionElem::Index(ref idx) => {
                idx.to_expr(local_decls)
            }
            */
        }
    }
}

impl Debug for Lvalue {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        use self::Lvalue::*;

        match *self {
            Local(id) => write!(fmt, "{:?}", id),
            Static(ref expr) => write!(fmt, "{}", pprust::expr_to_string(expr)),
            Projection(ref data) => {
                match data.elem {
                    ProjectionElem::Deref =>
                        write!(fmt, "(*{:?})", data.base),
                    /*
                    ProjectionElem::Index(ref index) =>
                        write!(fmt, "{:?}[{:?}]", data.base, index),
                    */
                }
            }
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// Scopes

newtype_index!(VisibilityScope, "scope");
pub const ARGUMENT_VISIBILITY_SCOPE : VisibilityScope = VisibilityScope(0);
pub const RETURN_PTR_VISIBILITY_SCOPE : VisibilityScope = VisibilityScope(1);
pub const COROUTINE_ARGS_VISIBILITY_SCOPE : VisibilityScope = VisibilityScope(2);

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
#[derive(Clone, PartialEq)]
pub enum Operand {
    Consume(Lvalue),
    Copy(Lvalue),
    Constant(Constant),
}

impl ToExpr for Operand {
    fn to_expr(&self, local_decls: &IndexVec<Local, LocalDecl>) -> P<ast::Expr> {
        match *self {
            Operand::Consume(ref lvalue) => lvalue.to_expr(local_decls),
            Operand::Copy(ref lvalue) => lvalue.to_expr(local_decls),
            Operand::Constant(ref constant) => constant.to_expr(local_decls),
        }
    }
}

impl Debug for Operand {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        use self::Operand::*;
        match *self {
            Constant(ref a) => write!(fmt, "{:?}", a),
            Copy(ref a) => write!(fmt, "{:?}", a),
            Consume(ref lv) => write!(fmt, "{:?}", lv),
        }
    }
}

///////////////////////////////////////////////////////////////////////////
/// Rvalues

#[derive(Clone, PartialEq)]
pub enum Rvalue {
    /// x (either a move or copy, depending on type of x)
    Use(Operand),
    Mac(ast::Mac),
    Ref(ast::Mutability, Lvalue),

    BinaryOp(ast::BinOp, Operand, Operand),
    UnaryOp(ast::UnOp, Operand),

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
            Rvalue::Mac(ref mac) => {
                builder.expr().build_mac(mac.clone())
            }
            Rvalue::Ref(ast::Mutability::Immutable, ref arg) => {
                builder.expr().ref_().build(arg.to_expr(local_decls))
            }
            Rvalue::Ref(ast::Mutability::Mutable, ref arg) => {
                builder.expr().mut_ref().build(arg.to_expr(local_decls))
            }
            Rvalue::BinaryOp(op, ref lhs, ref rhs) => {
                builder.expr().build_binary(
                    op.node,
                    lhs.to_expr(local_decls),
                    rhs.to_expr(local_decls))
            }
            Rvalue::UnaryOp(op, ref expr) => {
                builder.expr().build_unary(
                    op,
                    expr.to_expr(local_decls))
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

impl Debug for Rvalue {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        use self::Rvalue::*;

        match *self {
            Use(ref lvalue) => write!(fmt, "{:?}", lvalue),
            Mac(ref mac) => write!(fmt, "{}", pprust::mac_to_string(mac)),

            BinaryOp(binop, ref lhs, ref rhs) => {
                write!(fmt, "{}({:?}, {:?})", binop.node.to_string(), lhs, rhs)
            }
            UnaryOp(unop, ref value) => {
                write!(fmt, "{}({:?})", ast::UnOp::to_string(unop), value)
            }

            Ref(borrow_kind, ref lv) => {
                let kind_str = match borrow_kind {
                    ast::Mutability::Immutable => "",
                    ast::Mutability::Mutable => "mut ",
                };
                write!(fmt, "&{}{:?}", kind_str, lv)
            }

            Tuple(ref lvs) => {
                fn fmt_tuple(fmt: &mut Formatter, lvs: &[Operand]) -> fmt::Result {
                    let mut tuple_fmt = fmt.debug_tuple("");
                    for lv in lvs {
                        tuple_fmt.field(lv);
                    }
                    tuple_fmt.finish()
                }

                match lvs.len() {
                    0 => write!(fmt, "()"),
                    1 => write!(fmt, "({:?},)", lvs[0]),
                    _ => fmt_tuple(fmt, lvs),
                }
            }
            Struct(ref path, ref fields, ref items, ref wth) => {
                write!(fmt, "{:?} {{", path)?;

                let mut first = true;
                for (field, item) in fields.iter().zip(items.iter()) {
                    if first {
                        write!(fmt, ", ")?;
                    } else {
                        first = false;
                    }
                    if field.is_shorthand {
                        write!(fmt, "{:?}", item)?;
                    } else {
                        write!(fmt, "{:?}: {:?}", field.ident, item)?;
                    }
                }

                if let Some(ref wth) = *wth {
                    write!(fmt, " .. {:?}", wth)?;
                }

                write!(fmt, "}}")
            }
            Range(ref from, ref to, limits) => {
                if let Some(ref from) = *from {
                    write!(fmt, "{:?}", from)?;
                }

                if limits == ast::RangeLimits::HalfOpen {
                    write!(fmt, "..")?;
                } else {
                    write!(fmt, "...")?;
                }

                if let Some(ref to) = *to {
                    write!(fmt, "{:?}", to)?;
                }

                Ok(())
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

#[derive(Clone, PartialEq)]
pub struct Constant {
    pub span: Span,
    pub literal: P<ast::Lit>,
}

impl Debug for Constant {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        write!(fmt, "const {}", pprust::lit_to_string(&self.literal))
    }
}

impl ToExpr for Constant {
    fn to_expr(&self, _local_decls: &IndexVec<Local, LocalDecl>) -> P<ast::Expr> {
        AstBuilder::new().span(self.span)
            .expr()
            .build_lit(self.literal.clone())
    }
}

///////////////////////////////////////////////////////////////////////////
// Statements

pub struct Statement {
    pub source_info: SourceInfo,
    pub kind: StatementKind,
}

#[derive(Debug)]
pub enum StatementKind {
    Stmt(ast::Stmt),

    /// As opposed to MIR, we don't have an easy way breaking up irrefutable patterns, so instead
    /// we'll add a dedicated statement for that hides their destructuring.
    Let {
        pat: P<ast::Pat>,
        ty: Option<P<ast::Ty>>,
        lvalues: Vec<Lvalue>,
        rvalue: Rvalue,
    },

    /// Write the RHS Rvalue to the LHS Lvalue.
    Assign(Lvalue, Rvalue),

    /// Block ends with a call of a function
    Call {
        destination: Lvalue,
        func: Operand,
        args: Vec<Rvalue>,
    },

    /// Block ends with a call of a method
    MethodCall {
        destination: Lvalue,
        ident: ast::SpannedIdent,
        tys: Vec<P<ast::Ty>>,
        self_: Operand,
        args: Vec<Rvalue>,
    },

    /// Start a live range for the storage of the local.
    StorageLive(Lvalue),

    /// End the current live range for the storage of the local.
    StorageDead(Lvalue),

    /*
    /// No-op. Useful for deleting instructions without affecting statement indices.
    Nop,
    */
}

impl Debug for Statement {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        use self::StatementKind::*;
        match self.kind {
            Stmt(ref stmt) => {
                write!(fmt, "stmt {:?}", pprust::stmt_to_string(stmt))
            }
            Let { ref pat, ty: None, ref rvalue, .. } => {
                write!(fmt, "let {:?} = {:?}", pat, rvalue)
            }
            Let { ref pat, ty: Some(ref ty), ref rvalue, .. } => {
                write!(fmt, "let {:?}: {:?} = {:?}", pat, ty, rvalue)
            }
            Assign(ref lvalue, ref rvalue) => {
                write!(fmt, "{:?} = {:?}", lvalue, rvalue)
            }
            Call { ref destination, ref func, ref args, .. } => {
                write!(fmt, "{:?} = {:?}(", destination, func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(fmt, ",")?;
                    }
                    write!(fmt, "{:?}", arg)?;
                }
                write!(fmt, ")")
            }
            MethodCall { ref destination, ref ident, ref tys, ref self_, ref args, .. } => {
                write!(fmt,
                       "{:?} = {:?}.{}",
                       destination,
                       self_,
                       pprust::ident_to_string(ident.node))?;

                if !tys.is_empty() {
                    write!(fmt, "::<")?;
                    for (i, ty) in tys.iter().enumerate() {
                        if i != 0 {
                            write!(fmt, ", ")?;
                        }
                        write!(fmt, "{:?}", ty)?;
                    }
                    write!(fmt, ">")?;
                }

                write!(fmt, "(")?;

                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(fmt, ",")?;
                    }
                    write!(fmt, "{:?}", arg)?;
                }
                write!(fmt, ")")
            }

            StorageLive(ref lv) => write!(fmt, "StorageLive({:?})", lv),
            StorageDead(ref lv) => write!(fmt, "StorageDead({:?})", lv),
            /*
            SetDiscriminant{lvalue: ref lv, variant_index: index} => {
                write!(fmt, "discriminant({:?}) = {:?}", lv, index)
            }
            Nop => write!(fmt, "nop"),
            */
        }
    }
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

    // extent of the call-site for a function or closure (outlives
    // the parameters as well as the body).
    CallSiteScope,

    // extent of parameters passed to a function or closure (they
    // outlive its body)
    ParameterScope,

    /*
    // extent of code following a `let id = expr;` binding in a block
    Remainder(BlockRemainder),
    */
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

#[derive(Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Location {
    /// the location is within this block
    pub block: BasicBlock,

    /// the location is the start of the this statement; or, if `statement_index`
    /// == num-statements, then the start of the terminator.
    pub statement_index: usize,
}

impl Debug for Location {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        write!(fmt, "{:?}[{}]", self.block, self.statement_index)
    }
}

/*
impl Location {
    pub fn dominates(&self, other: &Location, dominators: &Dominators<BasicBlock>) -> bool {
        if self.block == other.block {
            self.statement_index <= other.statement_index
        } else {
            dominators.is_dominated_by(other.block, self.block)
        }
    }
}
*/
