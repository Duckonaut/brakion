use std::{fmt::Display, hash::Hash};

use crate::unit::Span;

use super::{Identifier, NamespacedIdentifier, TypeReference};

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum ExprKind {
    Literal(Literal),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    TypeBinary {
        op: TypeBinaryOp,
        expr: Box<Expr>,
        ty: TypeReference,
    },
    Variable(NamespacedIdentifier),
    Access {
        expr: Box<Expr>,
        field: Identifier,
    },
    FunctionCall {
        name: NamespacedIdentifier,
        args: Vec<Expr>,
    },
    MethodCall {
        expr: Box<Expr>,
        method: Identifier,
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

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
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
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::Neq => write!(f, "!="),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::Leq => write!(f, "<="),
            BinaryOp::Geq => write!(f, ">="),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum TypeBinaryOp {
    Is,
    As,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum FieldConstructor {
    Named { name: Identifier, value: Expr },
    Auto(Identifier),
}
