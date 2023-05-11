use std::hash::Hash;

use crate::unit::Span;

use super::{NamespacedIdentifier, Identifier, TypeReference};

#[derive(Debug, Hash, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Hash, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        ty: TypeReference,
    },
    Variable(NamespacedIdentifier),
    Access {
        expr: Box<Expr>,
        field: Identifier,
    },
    Call {
        expr: Box<Expr>,
        args: Vec<Expr>,
    },
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    Constructor {
        ty: NamespacedIdentifier,
        fields: Vec<FieldConstructor>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(u64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    List(Vec<Expr>),
    Void,
}

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Literal::Int(i) => i.hash(state),
            Literal::Float(f) => f.to_bits().hash(state),
            Literal::String(s) => s.hash(state),
            Literal::Char(c) => c.hash(state),
            Literal::Bool(b) => b.hash(state),
            Literal::List(l) => l.hash(state),
            Literal::Void => {}
        }
    }
}

#[derive(Debug, Hash, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
    Is,
}

#[derive(Debug, Hash, PartialEq)]
pub enum FieldConstructor {
    Named {
        name: Identifier,
        value: Expr,
    },
    Auto(Identifier),
}

