use aster::ident::ToIdent;
use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Create a new temporary variable that has a unique name.
    ///
    /// NB: **No cleanup is scheduled for this temporary.** You should
    /// call `schedule_drop` once the temporary is initialized.
    pub fn temp<T>(&mut self, span: Span, name: T) -> Lvalue
        where T: ToIdent,
    {
        /*
        // Add a unique number to the name.
        let name = format!("{}{}", name.to_ident(), self.local_decls.len());
        let ident = name.to_ident();
        let shadowed_decl = self.find_local(ident);

        if shadowed_decl.is_some() {
            span_bug!(self.cx, span,
                      "temp is shadowing another decl? name={:?} decl={:?}",
                      name, shadowed_decl);
        }

        let source_info = self.source_info(span);
        let temp = self.local_decls.push(LocalDecl {
            mutability: ast::Mutability::Mutable,
            ident: ident,
            ty: None,
            shadowed_decl: None,
            source_info: source_info,
        });
        let lvalue = Lvalue::Local(temp);
        debug!("temp: created temp {:?} with name {:?}", lvalue, name);
        lvalue
        */

        let source_info = SourceInfo {
            span: span,
            scope: self.visibility_scope,
        };

        // Add a unique number to the name.
        let name = format!("{}{}", name.to_ident(), self.local_decls.len());
        let local = self.declare_binding(
            source_info,
            ast::Mutability::Mutable,
            name,
            None);
        let lvalue = Lvalue::Local(local);

        debug!("temp: created temp {:?}", lvalue);

        lvalue
    }

    pub fn unit_rvalue(&mut self) -> Rvalue {
        Rvalue::Tuple(vec![])
    }
}
