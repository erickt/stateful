use aster::ident::ToIdent;
use mar::build::Builder;
use mar::repr::*;
use syntax::ast;
use syntax::codemap::Span;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Create a new temporary variable that has a unique name.
    pub fn temp<T>(&mut self, span: Span, name: T) -> Lvalue
        where T: ToIdent,
    {
        // Add a unique number to the name.
        let name = format!("{}{}", name.to_ident(), self.local_decls.len());
        let local = self.declare_binding(span, ast::Mutability::Mutable, name, None);
        let lvalue = Lvalue::Local(local);

        debug!("temp: created temp {:?}", lvalue);

        lvalue
    }

    pub fn unit_rvalue(&mut self) -> Rvalue {
        Rvalue::Aggregate(AggregateKind::Tuple, vec![])
    }
}
