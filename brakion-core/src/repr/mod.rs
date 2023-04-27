mod expr;
mod stmt;
mod decl;

pub use expr::*;
pub use stmt::*;
pub use decl::*;

use crate::unit::Span;

#[derive(Debug)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

#[derive(Debug)]
pub struct NamespacedIdentifier {
    pub namespace: Vec<Identifier>,
    pub name: Identifier,
}

