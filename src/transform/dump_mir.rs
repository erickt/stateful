// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

//! This pass just dumps MIR at a specified point.

use mir::Mir;
use pretty;
use std::borrow::Cow;
use std::fmt;
use super::{MirPassHook, Pass};
use ty::TyCtxt;

pub struct Disambiguator<'a> {
    pass: &'a Pass,
    is_after: bool
}

impl<'a> fmt::Display for Disambiguator<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let title = if self.is_after { "after" } else { "before" };
        if let Some(fmt) = self.pass.disambiguator() {
            write!(formatter, "{}-{}", fmt, title)
        } else {
            write!(formatter, "{}", title)
        }
    }
}

pub struct DumpMir;

impl MirPassHook for DumpMir {
    fn on_mir_pass<'a, 'tcx>(
        &mut self,
        tcx: TyCtxt<'a, 'tcx>,
        mir: &Mir,
        pass: &Pass,
        is_after: bool)
    {
        pretty::dump_mir(
            tcx,
            &*pass.name(),
            &Disambiguator {
                pass: pass,
                is_after: is_after
            },
            mir
        );
    }
}

impl<'b> Pass for DumpMir {
    fn name(&self) -> Cow<'static, str> { 
        Cow::from("DumpMir")
    }
}
