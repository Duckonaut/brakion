use crate::unit::Span;

use super::{Expr, Identifier, TypeReference};

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct Stmt {
    pub span: Span,
    pub kind: StmtKind,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum StmtKind {
    Expr(Expr),
    Block(Vec<Stmt>),
    Variable {
        name: Identifier,
        ty: TypeReference,
        value: Expr,
    },
    Assign {
        target: Expr,
        value: Expr,
    },
    If {
        condition: Expr,
        then: Box<Stmt>,
        otherwise: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    For {
        name: Identifier,
        iterable: Expr,
        body: Box<Stmt>,
    },
    Match {
        expr: Option<Identifier>,
        arms: Vec<MatchArm>,
    },
    Return(Expr),
    Break,
    Continue,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: Box<Stmt>,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum MatchPattern {
    // ambiguous patterns like `foo::Bar` or `[foo::Bar]` will be
    // parsed as expressions
    // the type checker will coerce them to types if possible.
    Expr(Expr),
    Type(TypeReference),
    Wildcard,
}
