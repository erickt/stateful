// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!
 * Methods for the various MIR types. These are intended for use after
 * building is complete.
 */

use aster::AstBuilder;
use mir::*;
use ty::TyCtxt;

#[derive(Clone, Debug)]
pub enum LvalueTy {
    /// Normal type.
    Ty { ty: P<ast::Ty> },
}

impl<'a, 'tcx> LvalueTy {
    pub fn to_ty(&self, tcx: TyCtxt<'a, 'tcx>) -> P<ast::Ty> {
        match *self {
            LvalueTy::Ty { ref ty } => ty.clone(),
        }
    }

    pub fn projection_ty(self, tcx: TyCtxt<'a, 'tcx>,
                         elem: &LvalueElem)
                         -> LvalueTy
    {
        match *elem {
            ProjectionElem::Deref => {
                let ty = self.to_ty(tcx);
                let ty = AstBuilder::new().ty().ref_().build_ty(ty);
                LvalueTy::Ty {
                    ty: ty,
                }
            }
            ProjectionElem::Index(_) =>
                LvalueTy::Ty {
                    ty: infer(),
                },
            /*
            ProjectionElem::Subslice { from, to } => {
                let ty = self.to_ty(tcx);
                LvalueTy::Ty {
                    ty: match ty.sty {
                        ty::TyArray(inner, size) => {
                            tcx.mk_array(inner, size-(from as usize)-(to as usize))
                        }
                        ty::TySlice(..) => ty,
                        _ => {
                            bug!("cannot subslice non-array type: `{:?}`", self)
                        }
                    }
                }
            }
            ProjectionElem::Downcast(adt_def1, index) =>
                match self.to_ty(tcx).sty {
                    ty::TyAdt(adt_def, substs) => {
                        assert!(adt_def.is_enum());
                        assert!(index < adt_def.variants.len());
                        assert_eq!(adt_def, adt_def1);
                        LvalueTy::Downcast { adt_def: adt_def,
                                             substs: substs,
                                             variant_index: index }
                    }
                    _ => {
                        bug!("cannot downcast non-ADT type: `{:?}`", self)
                    }
                },
            ProjectionElem::Field(_, fty) => LvalueTy::Ty { ty: fty }
            */
        }
    }
}

impl Lvalue {
    pub fn ty<'a, 'tcx>(&self, mir: &Mir, tcx: TyCtxt<'a, 'tcx>) -> LvalueTy {
        match *self {
            Lvalue::Local(index) =>
                LvalueTy::Ty {
                    ty: mir.local_decls[index].ty.clone().unwrap_or_else(|| infer()),
                },
            Lvalue::Static(_) =>
                LvalueTy::Ty { ty: infer() },
            Lvalue::Projection(ref proj) =>
                proj.base.ty(mir, tcx).projection_ty(tcx, &proj.elem),
        }
    }
}

fn infer() -> P<ast::Ty> {
    AstBuilder::new().ty().infer()
}
