use std::ops::Deref;
use syntax::ext::base::ExtCtxt;
use syntax::parse::ParseSess;

/// The data structure to keep track of all the information that typechecker
/// generates so that so that it can be reused and doesn't have to be redone
/// later on.
#[derive(Copy, Clone)]
pub struct TyCtxt<'a, 'ecx: 'a> {
    cx: &'a ExtCtxt<'ecx>,
    pub sess: &'a ParseSess,
}

impl<'a, 'ecx: 'a> TyCtxt<'a, 'ecx> {
    pub fn new(cx: &'a ExtCtxt<'ecx>) -> Self {
        TyCtxt {
            cx: cx,
            sess: cx.parse_sess,
        }
    }
}

impl<'a, 'ecx> Deref for TyCtxt<'a, 'ecx> {
    type Target = &'a ExtCtxt<'ecx>;
    fn deref(&self) -> &Self::Target {
        &self.cx
    }
}
