mod expr;
mod stmt;
mod decl;

pub use expr::*;
pub use stmt::*;
pub use decl::*;

use crate::unit::Span;

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct NamespacedIdentifier {
    pub namespace: Vec<Identifier>,
    pub ident: Identifier,
}

impl NamespacedIdentifier {
    pub fn span(&self) -> Span {
        if !self.namespace.is_empty() {
            Span::from_spans(self.namespace[0].span, self.ident.span)
        } else {
            self.ident.span
        }
    }
}

pub trait BrakionTreeVisitor<T> {
    fn visit_decl(&mut self, decl: &mut Decl) -> T;
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> T;
    fn visit_expr(&mut self, expr: &mut Expr) -> T;
    fn visit_type_reference(&mut self, ty: &mut TypeReference) -> T;
}

/// Try to interpret an expression as a type reference.
/// For use with ambiguous patterns in match arms.
/// Example:
/// ```brakion
/// match foo {
///    on [bar] => { ... }
///    on [bar::Baz] => { ... }
///    on bar::Baz => { ... }
///    on void => { ... }
/// }
/// ```
pub(crate) fn expr_to_type_ref(expr: &Expr) -> Option<TypeReference> {
    let kind = match &expr.kind {
        ExprKind::Variable(name) => Some(TypeReferenceKind::Named(name.clone())),
        ExprKind::Literal(Literal::Void) => Some(TypeReferenceKind::Void),
        ExprKind::Literal(Literal::List(elems)) if elems.len() == 1 => {
            let elem_ty = expr_to_type_ref(&elems[0])?;
            Some(TypeReferenceKind::List(Box::new(elem_ty)))
        }
        _ => None,
    };

    kind.map(|kind| TypeReference {
        span: Some(expr.span),
        kind,
    })
}
