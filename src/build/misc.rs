use aster::ident::ToIdent;
use build::Builder;
use mir::*;
use syntax::ast;
use syntax::codemap::Span;

impl<'a, 'b: 'a> Builder<'a, 'b> {
    /// Create a new temporary variable that has a unique name.
    ///
    /// NB: **No cleanup is scheduled for this temporary.** You should
    /// call `schedule_drop` once the temporary is initialized.
    pub fn temp<T>(&mut self, block: BasicBlock, span: Span, name: T) -> Lvalue
        where T: ToIdent,
    {
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
            name: ident,
            ty: None,
            shadowed_decl: None,
            source_info: source_info,
        });
        let lvalue = Lvalue::Local(temp);

        // NOTE(stateful): As opposed to Mir, we need a StorageLive for every temp in order to
        // distinguish between a local being dropped in the same block it was defined in, or that
        // the local was already moved in an incoming block.
        self.cfg.push(block, Statement {
            source_info: source_info,
            kind: StatementKind::StorageLive(lvalue.clone())
        });

        let extent = self.extent_of_innermost_scope();
        self.schedule_drop(source_info.span, extent, &lvalue);
        debug!("temp: created temp {:?} with name {:?}", lvalue, name);
        lvalue
    }

    pub fn unit_rvalue(&mut self) -> Rvalue {
        Rvalue::Tuple(vec![])
    }
}
