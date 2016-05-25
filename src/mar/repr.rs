use std::fmt;
use std::u32;
use syntax::abi;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

/// Lowered representation of a single function.
pub struct Mar {
    pub span: Span,

    pub ident: ast::Ident,

    pub fn_decl: P<ast::FnDecl>,
    pub unsafety: ast::Unsafety,
    pub constness: ast::Constness,
    pub abi: abi::Abi,
    pub generics: ast::Generics,

    pub input_decls: Vec<(VarDecl, ast::Ident)>,

    pub var_decls: Vec<VarDeclData>,

    /// List of basic blocks. References to basic block use a newtyped index type `BasicBlock`
    /// that indexes into this vector.
    pub basic_blocks: Vec<BasicBlockData>,

    /// List of extents. References to extents use a newtyped index type `CodeExtent` that indexes
    /// into this vector.
    pub extents: Vec<CodeExtentData>,
}

impl Mar {
    pub fn all_basic_blocks(&self) -> Vec<BasicBlock> {
        (0..self.basic_blocks.len())
            .map(|i| BasicBlock::new(i))
            .collect()
    }

    pub fn basic_block_data(&self, bb: BasicBlock) -> &BasicBlockData {
        &self.basic_blocks[bb.index()]
    }

    pub fn basic_block_data_mut(&mut self, bb: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks[bb.index()]
    }

    pub fn code_extent_data(&self, extent: CodeExtent) -> &CodeExtentData {
        &self.extents[extent.index()]
    }

    pub fn var_decl_data(&self, decl: VarDecl) -> &VarDeclData {
        &self.var_decls[decl.index()]
    }
}

/// Where execution begins
pub const START_BLOCK: BasicBlock = BasicBlock(0);

/// Where execution ends.
pub const END_BLOCK: BasicBlock = BasicBlock(1);

///////////////////////////////////////////////////////////////////////////
// Variables and temps

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VarDecl(u32);

impl VarDecl {
    pub fn new(index: usize) -> Self {
        assert!(index < (u32::MAX as usize));
        VarDecl(index as u32)
    }

    /// Extract the index.
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, PartialEq)]
pub struct VarDeclData {
    pub mutability: ast::Mutability,
    pub ident: ast::Ident,
}

impl VarDeclData {
    pub fn new(mutability: ast::Mutability, ident: ast::Ident) -> Self {
        VarDeclData {
            mutability: mutability,
            ident: ident,
        }
    }
}

///////////////////////////////////////////////////////////////////////////
// BasicBlock

/// The index of a particular basic block. The index is into the `basic_blocks`
/// list of the `Mar`.
///
/// (We use a `u32` internally just to save memory.)
#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct BasicBlock(u32);

impl BasicBlock {
    pub fn new(index: usize) -> BasicBlock {
        assert!(index < (u32::MAX as usize));
        BasicBlock(index as u32)
    }

    /// Extract the index.
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Debug for BasicBlock {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "BB({})", self.0)
    }
}

///////////////////////////////////////////////////////////////////////////
// BasicBlock and Terminator

pub struct BasicBlockData {
    pub name: Option<&'static str>,
    pub decls: Vec<(VarDecl, ast::Ident)>,
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
}

impl BasicBlockData {
    pub fn new(name: Option<&'static str>,
               decls: Vec<(VarDecl, ast::Ident)>,
               terminator: Option<Terminator>) -> Self {
        BasicBlockData {
            name: name,
            decls: decls,
            statements: vec![],
            terminator: terminator,
        }
    }

    pub fn name(&self) -> Option<&'static str> {
        self.name
    }

    pub fn decls(&self) -> &[(VarDecl, ast::Ident)] {
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

pub enum Terminator {
    /// block should have one successor in the graph; we jump there
    Goto {
        target: BasicBlock
    },

    /// jump to target on next iteration.
    Yield {
        expr: P<ast::Expr>,
        target: BasicBlock,
    },

    /// jump to branch 0 if this lvalue evaluates to true
    If {
        cond: P<ast::Expr>,
        targets: (BasicBlock, BasicBlock),
    },

    /// lvalue evaluates to some enum; jump depending on the branch
    Match {
        discr: P<ast::Expr>,
        targets: Vec<Arm>,
    },

    /// Indicates a normal return. The ReturnPointer lvalue should
    /// have been filled in by now. This should only occur in the
    /// `END_BLOCK`.
    Return,
}

impl Terminator {
    pub fn successors(&self) -> Vec<BasicBlock> {
        match *self {
            Terminator::Goto { target } => vec![target],
            Terminator::Yield { target, .. } => vec![target],
            Terminator::Match { ref targets, .. } => {
                targets.iter().map(|arm| arm.block).collect()
            }
            Terminator::If { targets: (then, else_), .. } => vec![then, else_],
            Terminator::Return => vec![],
        }
    }

    pub fn successors_mut(&mut self) -> Vec<&mut BasicBlock> {
        match *self {
            Terminator::Goto { ref mut target } => vec![target],
            Terminator::Yield { ref mut target, .. } => vec![target],
            Terminator::Match { ref mut targets, .. } => {
                targets.iter_mut().map(|arm| &mut arm.block).collect()
            }
            Terminator::If { targets: (ref mut then, ref mut else_), .. } => {
                vec![then, else_]
            }
            Terminator::Return => vec![],
        }
    }
}

pub struct Arm {
    pub pats: Vec<P<ast::Pat>>,
    pub guard: Option<P<ast::Expr>>,
    pub block: BasicBlock,
}

///////////////////////////////////////////////////////////////////////////
// Statements

pub enum Statement {
    Expr(ast::Stmt),
    Let {
        span: Span,
        pat: P<ast::Pat>,
        ty: Option<P<ast::Ty>>,
        init: Option<P<ast::Expr>>,
    },
    Drop {
        span: Span,
        lvalue: ast::Ident,
        alias: Option<Alias>,
    },
}

#[derive(Clone, Copy, Debug)]
pub struct Alias {
    pub lvalue: ast::Ident,
    pub decl: VarDecl,
}

///////////////////////////////////////////////////////////////////////////
// Code Extents

/// The index of a particular basic block. The index is into the `basic_blocks`
/// list of the `Mar`.
///
/// (We use a `u32` internally just to save memory.)
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct CodeExtent(u32);

impl CodeExtent {
    pub fn new(index: usize) -> CodeExtent {
        assert!(index < (u32::MAX as usize));
        CodeExtent(index as u32)
    }

    pub fn index(&self) -> usize {
        self.0 as usize
    }
}

impl fmt::Debug for CodeExtent {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(fmt, "CodeExtent({})", self.0)
    }
}

pub enum CodeExtentData {
    Misc(ast::NodeId),

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
///   `a` and `b`; it does not include EXPR_1, but does include
///   everything after that first `let`. (If you want a scope that
///   includes EXPR_1 as well, then do not use `CodeExtentData::Remainder`,
///   but instead another `CodeExtent` that encompasses the whole block,
///   e.g. `CodeExtentData::Misc`.
///
/// * the subscope with `first_statement_index == 1` is scope of `c`,
///   and thus does not include EXPR_2, but covers the `...`.
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy)]
pub struct BlockRemainder {
    pub block: ast::NodeId,
    pub first_statement_index: u32,
}
