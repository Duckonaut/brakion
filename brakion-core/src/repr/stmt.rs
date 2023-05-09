use super::{Expr, Identifier, TypeReference};

#[derive(Debug, Hash, PartialEq)]
pub enum Stmt {
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
}

#[derive(Debug, Hash, PartialEq)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: Box<Stmt>,
}

#[derive(Debug, Hash, PartialEq)]
pub enum MatchPattern {
    Expr(Expr),
    Type(TypeReference),
    Wildcard,
}
