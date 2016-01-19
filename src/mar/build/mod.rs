use mar::repr::*;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::fold::Folder;
use syntax::ptr::P;

pub struct CFG {
    basic_blocks: Vec<BasicBlockData>,
}

pub struct Builder<'a> {
    cx: &'a ExtCtxt<'a>,
    cfg: CFG,
    scopes: Vec<scope::Scope>,
    loop_scopes: Vec<scope::LoopScope>,
    extents: Vec<CodeExtentData>,
    aliases: u32,
}

pub struct Error;

///////////////////////////////////////////////////////////////////////////
// construct() -- the main entry point for building SMIR for a function

pub fn construct(cx: &ExtCtxt, item: P<ast::Item>) -> Result<Mar, Error> {
    let item = assign_node_ids(item);

    let (fn_decl, ast_block) = match item.node {
        ast::ItemFn(ref fn_decl, _, _, _, _, ref block) => (fn_decl, block),
        _ => {
            cx.span_err(item.span, "`state_machine` may only be applied to functions");
            return Err(Error);
        }
    };

    let mut builder = Builder {
        cx: cx,
        cfg: CFG {
            basic_blocks: vec![],
        },
        scopes: vec![],
        loop_scopes: vec![],
        extents: vec![],
        aliases: 0,
    };

    let extent = builder.start_new_extent();

    assert_eq!(builder.cfg.start_new_block(Some("Start")), START_BLOCK);
    assert_eq!(builder.cfg.start_new_block(Some("End")), END_BLOCK);

    // Add node ids.

    let mut block = START_BLOCK;
    block = builder.args_and_body(extent, block, fn_decl, ast_block);

    builder.cfg.terminate(block, Terminator::Goto { target: END_BLOCK });
    builder.cfg.terminate(END_BLOCK, Terminator::Return);

    Ok(Mar {
        ident: item.ident,
        span: item.span,
        fn_decl: fn_decl.clone(),
        basic_blocks: builder.cfg.basic_blocks,
        extents: builder.extents,
    })
}

fn assign_node_ids(item: P<ast::Item>) -> P<ast::Item> {
    struct Assigner {
        next_node_id: ast::NodeId,
    }

    impl Folder for Assigner {
        fn new_id(&mut self, old_id: ast::NodeId) -> ast::NodeId {
            assert_eq!(old_id, ast::DUMMY_NODE_ID);
            let node_id = self.next_node_id;

            let next_node_id = match self.next_node_id.checked_add(1) {
                Some(next_node_id) => next_node_id,
                None => { panic!("ran out of node ids!") }
            };
            self.next_node_id = next_node_id;

            node_id
        }
    }

    let mut items = Assigner { next_node_id: 1 }.fold_item(item);
    assert_eq!(items.len(), 1);
    items.pop().unwrap()
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
        self.extents.push(CodeExtentData::Misc(0));

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
