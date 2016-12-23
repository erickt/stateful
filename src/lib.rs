#![cfg_attr(feature = "unstable", feature(plugin, plugin_registrar, rustc_private, quote))]
#![cfg_attr(feature = "clippy", plugin(clippy))]
#![cfg_attr(feature = "clippy", allow(too_many_arguments))]

#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unused_variables)]

extern crate aster;
extern crate bit_vec;
extern crate dot;
extern crate rustc_plugin;
extern crate rustc_errors as errors;
#[macro_use] extern crate log;
#[macro_use] extern crate syntax;
extern crate syntax_pos;

#[macro_use] mod macros;

mod analysis;
//mod borrowck;
mod build;
mod data_structures;
//mod dataflow;
mod graphviz;
mod mir;
mod pretty;
mod transform;
mod translate;
mod ty;
mod traversal;

use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::{Annotatable, ExtCtxt, MultiModifier};
use syntax::fold;
use syntax::print::pprust;
use syntax::ptr::P;
use mir::{FunctionDecl, StateMachineKind};

fn expand_state_machine<'a, 'ecx>(cx: &'a ExtCtxt<'ecx>,
                                  _sp: Span,
                                  meta_item: &ast::MetaItem,
                                  annotatable: Annotatable,
                                  name: &str,
                                  state_machine_kind: StateMachineKind) -> Annotatable {
    let item = match annotatable {
        Annotatable::Item(item) => item,
        _ => {
            cx.span_err(
                meta_item.span,
                &format!("`{}` may only be applied to functions", name));

            return annotatable;
        }
    };

    let fn_decl_ast_block = match item.node {
        ast::ItemKind::Fn(ref fn_decl, ref unsafety, _, ref abi, ref generics, ref block) => {
            let fn_decl = FunctionDecl::new(
                item.ident,
                fn_decl.clone(),
                *unsafety,
                *abi,
                generics.clone(),
            );
            Some((fn_decl, block.clone()))
        }

        _ => None,
    };

    let (fn_decl, ast_block) = match fn_decl_ast_block {
        Some(data) => data,
        None => {
            cx.span_err(
                item.span,
                &format!("`{}` may only be applied to functions", state_machine_kind));

            return Annotatable::Item(item);
        }
    };

    let tcx = ty::TyCtxt::new(cx);

    let mut mir = build::construct_fn(
        cx,
        state_machine_kind,
        item.span,
        fn_decl,
        ast_block);

    let mut passes = transform::Passes::new();
    passes.push_hook(Box::new(transform::dump_mir::DumpMir));
    passes.push_hook(Box::new(transform::validate::Validate));
    passes.push_pass(Box::new(transform::simplify_cfg::SimplifyCfg::new()));
    passes.run_passes(tcx, &mut mir);

    let assignments = analysis::analyze_assignments(tcx, &mir);

    let item = translate::translate(cx, &mir, &assignments);
    debug!("{}", pprust::item_to_string(&item));

    Annotatable::Item(strip_node_ids(item))
}

/// Syntax extensions are not allowed to have `ast::NodeId`s, so this just strips them out.
fn strip_node_ids(item: P<ast::Item>) -> P<ast::Item> {
    struct Stripper;

    impl fold::Folder for Stripper {
        fn new_id(&mut self, _old_id: ast::NodeId) -> ast::NodeId {
            ast::DUMMY_NODE_ID
        }

        fn fold_mac(&mut self, mac: ast::Mac) -> ast::Mac {
            fold::noop_fold_mac(mac, self)
        }
    }

    let mut items = fold::Folder::fold_item(&mut Stripper, item);
    assert_eq!(items.len(), 1);
    items.pop().unwrap()
}

fn expand_generator(cx: &mut ExtCtxt,
                    span: Span,
                    meta_item: &ast::MetaItem,
                    annotatable: Annotatable) -> Annotatable {
    expand_state_machine(
        cx,
        span,
        meta_item,
        annotatable,
        "generator",
        StateMachineKind::Generator)
}

fn expand_async(cx: &mut ExtCtxt,
                span: Span,
                meta_item: &ast::MetaItem,
                annotatable: Annotatable) -> Annotatable {
    expand_state_machine(
        cx,
        span,
        meta_item,
        annotatable,
        "async",
        StateMachineKind::Async)
}

#[plugin_registrar]
#[doc(hidden)]
pub fn plugin_registrar(registry: &mut rustc_plugin::Registry) {
    let builder = aster::AstBuilder::new();

    registry.register_syntax_extension(builder.symbol("generator"),
                                       MultiModifier(Box::new(expand_generator)));

    registry.register_syntax_extension(builder.symbol("async"),
                                       MultiModifier(Box::new(expand_async)));
}
