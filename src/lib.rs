#![cfg_attr(feature = "unstable", feature(plugin, plugin_registrar, rustc_private, quote))]
#![cfg_attr(feature = "clippy", plugin(clippy))]
#![cfg_attr(feature = "clippy", allow(too_many_arguments))]

extern crate aster;
extern crate bit_vec;
extern crate rustc_plugin;
extern crate rustc_errors as errors;
#[macro_use] extern crate log;
#[macro_use] extern crate syntax;

#[macro_use] mod macros;

mod build;
mod data_structures;
mod mir;
//mod transform;
mod translate;
//mod traversal;

use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::{
    Annotatable,
    ExtCtxt,
    MultiModifier,
};
use syntax::print::pprust;
use data_structures::indexed_vec::Idx;
use mir::{FunctionDecl, Mir, StateMachineKind};

fn expand_state_machine(cx: &mut ExtCtxt,
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


    let mir = build::construct_fn(
        cx,
        state_machine_kind,
        item.span,
        fn_decl,
        ast_block);

    validate(cx, &mir);

    /*
    if let Some(item) = translate::translate(cx, &mir) {
        debug!("{}", pprust::item_to_string(&item));
        debug!("-------");
    }

    let mut pass_manager = transform::pass_manager::PassManager::new();
    pass_manager.add_pass(Box::new(mir::transform::simplify_cfg::SimplifyCfg::new()));
    pass_manager.run(&mut mir);
    */

    validate(cx, &mir);

    match translate::translate(cx, &mir) {
        Some(item) => {
            debug!("{}", pprust::item_to_string(&item));

            Annotatable::Item(item)
        }
        None => {
            // We had an error, so just return the input item for a lack of a better option.
            Annotatable::Item(item.clone())
        }
    }
}

fn validate(cx: &mut ExtCtxt, mir: &Mir) {
    let basic_blocks = mir.basic_blocks();
    for (bb, block) in basic_blocks.iter_enumerated() {
        let terminator = block.terminator();

        for succ in terminator.successors() {
            if succ.index() >= basic_blocks.len() {
                cx.span_bug(
                    mir.span,
                    &format!("block {:?} terminator does not exist: {:?} len: {:?}",
                            bb,
                            terminator.kind,
                            basic_blocks.len()));
            }
        }
    }
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
