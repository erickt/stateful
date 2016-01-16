use std::fmt;
use std::u32;
use syntax::ast;
use syntax::codemap::Span;
use syntax::ptr::P;

/// Lowered representation of a single function.
pub struct Smir {
    pub span: Span,

    pub ident: ast::Ident,

    pub fn_decl: ast::FnDecl,

    /// List of basic blocks. References to basic block use a newtyped index type `BasicBlock`
    /// that indexes into this vector.
    pub basic_blocks: Vec<BasicBlockData>,

    /// List of extents. References to extents use a newtyped index type `CodeExtent` that indexes
    /// into this vector.
    pub extents: Vec<CodeExtentData>,
}

impl Smir {
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
}

/// Where execution begins
pub const START_BLOCK: BasicBlock = BasicBlock(0);

/// Where execution ends.
pub const END_BLOCK: BasicBlock = BasicBlock(1);

///////////////////////////////////////////////////////////////////////////
// Variables and temps

/*
pub struct VarDecl {
    pub mutability: Mutability,
    pub ident: ast::Ident,
    pub ty: Option<ast::Ty>,
}

///////////////////////////////////////////////////////////////////////////
// Mutability

pub enum Mutability {
    Mut,
    Not,
}
*/

///////////////////////////////////////////////////////////////////////////
// BasicBlock

/// The index of a particular basic block. The index is into the `basic_blocks`
/// list of the `Smir`.
///
/// (We use a `u32` internally just to save memory.)
#[derive(Copy, Clone, PartialEq, Eq)]
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
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
}

impl BasicBlockData {
    pub fn new(name: Option<&'static str>, terminator: Option<Terminator>) -> Self {
        BasicBlockData {
            name: name,
            statements: vec![],
            terminator: terminator,
        }
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

    /*
    /// lvalue evaluates to some enum; jump depending on the branch
    Match {
        discr: P<ast::Expr>,
        targets: Vec<BasicBlock>,
    },
    */

    /// Indicates a normal return. The ReturnPointer lvalue should
    /// have been filled in by now. This should only occur in the
    /// `END_BLOCK`.
    Return,
}

/*
pub struct Arm {
    pub pats: Vec<P<ast::Pat>>,
    pub guard: Option<P<ast::Expr>>,
    pub body: BasicBlock,
}
*/

///////////////////////////////////////////////////////////////////////////
// Statements

pub enum Statement {
    Expr(P<ast::Stmt>),
    Drop(Span, ast::Ident),
}

/// The index of a particular basic block. The index is into the `basic_blocks`
/// list of the `Smir`.
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

pub struct CodeExtentData;
