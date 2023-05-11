mod decl;
mod expr;
mod stmt;

use std::fmt::Display;

pub use decl::*;
pub use expr::*;
pub use stmt::*;

use crate::unit::Span;

/// A Brakion identifier.
/// This is used for variable names, function names, etc.
/// The span is the location of the identifier in the source code.
#[derive(Debug, Hash, PartialEq, Clone)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

/// A namespaced identifier.
/// Used for lookup of variables, functions, types, etc.
#[derive(Debug, Hash, PartialEq, Clone)]
pub struct NamespacedIdentifier {
    pub namespace: Vec<Identifier>,
    pub ident: Identifier,
}

impl NamespacedIdentifier {
    /// Build the span from the underlying identifiers.
    pub fn span(&self) -> Span {
        if !self.namespace.is_empty() {
            Span::from_spans(self.namespace[0].span, self.ident.span)
        } else {
            self.ident.span
        }
    }
}

impl Display for NamespacedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for ns in &self.namespace {
            write!(f, "{}::", ns.name)?;
        }
        write!(f, "{}", self.ident.name)
    }
}

/// A visitor for the Brakion AST.
/// This is used for traversing the AST and performing operations on it.
/// The visitor is a trait so that it can be implemented for different purposes.
/// For example, the type checker is a visitor that checks the types of expressions,
/// and the interpreter is a visitor that evaluates expressions.
///
/// For this reason, the visitor is generic over the result type of each operation.
/// For example, the type checker can return a `Result<Type, TypeError>` for an expression,
/// but a `Result<(), TypeError>` for a statement since statements don't have a type.
pub trait BrakionTreeVisitor {
    type ExprResult;
    type StmtResult;
    type DeclResult;
    type TypeReferenceResult;

    fn visit_decl(&mut self, decl: &mut Decl) -> Self::DeclResult;
    fn visit_stmt(&mut self, stmt: &mut Stmt) -> Self::StmtResult;
    fn visit_expr(&mut self, expr: &mut Expr) -> Self::ExprResult;
    fn visit_type_reference(&mut self, ty: &mut TypeReference) -> Self::TypeReferenceResult;
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
