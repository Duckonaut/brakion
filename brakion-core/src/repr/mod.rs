mod expr;
mod stmt;
mod decl;

pub use expr::*;
pub use stmt::*;
pub use decl::*;

use crate::unit::Span;

#[derive(Debug, Hash, PartialEq)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Hash, PartialEq)]
pub struct NamespacedIdentifier {
    pub namespace: Vec<Identifier>,
    pub ident: Identifier,
}

pub trait BrakionTreeVisitor {
    fn visit_decl(&mut self, decl: &Decl);
    fn visit_stmt(&mut self, stmt: &Stmt);
    fn visit_expr(&mut self, expr: &Expr);
    fn visit_type_reference(&mut self, ty: &TypeReference);
}
