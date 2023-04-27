use super::{Expr, NamespacedIdentifier, Identifier};

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Variable {
        name: Identifier,
        ty: NamespacedIdentifier,
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
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: Box<Stmt>,
}

#[derive(Debug)]
pub enum MatchPattern {
    Expr(Expr),
    Type(NamespacedIdentifier),
    Wildcard,
}
