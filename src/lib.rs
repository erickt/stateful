#![feature(plugin_registrar, rustc_private, quote)]

extern crate aster;
extern crate rustc_plugin;

#[macro_use]
extern crate syntax;

pub mod mar;

use syntax::ast;
use syntax::codemap::Span;
use syntax::ext::base::{
    Annotatable,
    ExtCtxt,
    MultiModifier,
};
pub use mar::transform::pass::MarPass;

fn expand_generator(cx: &mut ExtCtxt,
                    _sp: Span,
                    meta_item: &ast::MetaItem,
                    annotatable: Annotatable) -> Annotatable {
    let item = match annotatable {
        Annotatable::Item(item) => item,
        _ => {
            cx.span_err(
                meta_item.span,
                "`state_machine` may only be applied to functions");

            return annotatable;
        }
    };

    let mut mar = match mar::build::construct(cx, item.clone()) {
        Ok(mar) => mar,
        Err(mar::build::Error) => {
            return Annotatable::Item(item);
        }
    };

    let mut passes = vec![
        mar::transform::remove_dead_blocks::RemoveDeadBlocks::new(),
    ];

    for pass in passes.iter_mut() {
        pass.run_pass(&mut mar);
    }

    match mar::translate::translate(cx, &mar) {
        Some(item) => {
            /*
            use syntax::print::pprust;
            println!("{}", pprust::item_to_string(&item));
            */

            Annotatable::Item(item)
        }
        None => {
            // We had an error, so just return the input item for a lack of a better option.
            Annotatable::Item(item.clone())
        }
    }
}

#[plugin_registrar]
#[doc(hidden)]
pub fn plugin_registrar(registry: &mut rustc_plugin::Registry) {
    let builder = aster::AstBuilder::new();

    registry.register_syntax_extension(builder.name("generator"),
                                       MultiModifier(Box::new(expand_generator)));
}
