use mar::repr::*;
use syntax::ast;
use syntax::ext::base::ExtCtxt;
use syntax::fold;
use syntax::ptr::P;

pub struct CFG {
    basic_blocks: Vec<BasicBlockData>,
    var_decls: Vec<VarDeclData>,
}

pub struct Builder<'a> {
    cx: &'a ExtCtxt<'a>,
    cfg: CFG,
    scopes: Vec<scope::Scope>,
    loop_scopes: Vec<scope::LoopScope>,
    extents: Vec<CodeExtentData>,
}

pub struct Error;

///////////////////////////////////////////////////////////////////////////
// construct() -- the main entry point for building SMIR for a function

pub fn construct(cx: &ExtCtxt, item: P<ast::Item>) -> Result<Mar, Error> {
    let item = assign_node_ids(item);

    let (fn_decl, unsafety, constness, abi, generics, ast_block) = match item.node {
        ast::ItemFn(ref fn_decl, unsafety, constness, abi, ref generics, ref block) => {
            (fn_decl, unsafety, constness, abi, generics, block)
        }
        _ => {
            cx.span_err(item.span, "`state_machine` may only be applied to functions");
            return Err(Error);
        }
    };

    let mut builder = Builder {
        cx: cx,
        cfg: CFG {
            basic_blocks: vec![],
            var_decls: vec![],
        },
        scopes: vec![],
        loop_scopes: vec![],
        extents: vec![],
    };

    let extent = builder.start_new_extent();

    assert_eq!(builder.cfg.start_new_block(Some("Start")), START_BLOCK);
    assert_eq!(builder.cfg.start_new_block(Some("End")), END_BLOCK);

    let mut block = START_BLOCK;

    builder.push_scope(extent, block);

    // Register the arguments as declarations.
    builder.add_decls_from_pats(
        item.span,
        extent,
        block,
        fn_decl.inputs.iter().map(|arg| &arg.pat));

    block = builder.ast_block(extent, block, ast_block);

    let live_decls = builder.find_live_decls();

    builder.pop_scope(extent, block);

    builder.terminate(block, Terminator::Goto { target: END_BLOCK });
    builder.terminate(END_BLOCK, Terminator::Return);

    Ok(Mar {
        ident: item.ident,
        span: item.span,
        fn_decl: fn_decl.clone(),
        unsafety: unsafety,
        constness: constness,
        abi: abi,
        generics: generics.clone(),
        input_decls: live_decls,
        basic_blocks: builder.cfg.basic_blocks,
        var_decls: builder.cfg.var_decls,
        extents: builder.extents,
    })
}

fn assign_node_ids(item: P<ast::Item>) -> P<ast::Item> {
    struct Assigner {
        next_node_id: ast::NodeId,
    }

    impl fold::Folder for Assigner {
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

        fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
            fold::noop_fold_mac(mac, self)
        }
    }

    let mut assigner = Assigner { next_node_id: 1 };
    let mut items = fold::Folder::fold_item(&mut assigner, item);
    assert_eq!(items.len(), 1);
    items.pop().unwrap()
}

impl<'a> Builder<'a> {
    pub fn start_new_extent(&mut self) -> CodeExtent {
        let extent = CodeExtent::new(self.extents.len());
        self.extents.push(CodeExtentData::Misc(0));

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
mod scope;
mod stmt;
mod transition;
