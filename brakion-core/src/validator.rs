use crate::{
    errors::{validator::ValidatorError, ErrorModuleRef},
    repr::{BrakionTreeVisitor, Decl, Expr, Stmt, TypeReference, TypeReferenceKind},
    unit::Span,
};

pub struct Validator {
    error_module: ErrorModuleRef,
}

impl Validator {
    pub fn new(error_module: ErrorModuleRef) -> Self {
        Self { error_module }
    }

    pub fn check(&mut self, decls: &mut [Decl]) {
        for decl in decls.iter_mut() {
            let result = self.visit_decl(decl);

            if let Err(err) = result {
                self.error_module
                    .lock()
                    .unwrap()
                    .add_validator_error(err.0, err.1);
            }
        }
    }
}

impl BrakionTreeVisitor for Validator {
    type ExprResult = Result<TypeReferenceKind, (ValidatorError, Option<Span>)>;
    type StmtResult = Result<(), (ValidatorError, Option<Span>)>;
    type DeclResult = Result<(), (ValidatorError, Option<Span>)>;
    type TypeReferenceResult = Result<TypeReferenceKind, (ValidatorError, Option<Span>)>;

    fn visit_decl(&mut self, decl: &mut Decl) -> Self::DeclResult {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Self::StmtResult {
        todo!()
    }

    fn visit_expr(&mut self, expr: &mut Expr) -> Self::ExprResult {
        todo!()
    }

    fn visit_type_reference(
        &mut self,
        ty: &mut TypeReference,
    ) -> Self::TypeReferenceResult {
        Ok(ty.kind.clone())
    }
}
