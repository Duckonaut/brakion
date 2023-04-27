use super::{NamespacedIdentifier, Identifier};

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Grouping(Box<Expr>),
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
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
    ListAccess {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    Constructor {
        ty: NamespacedIdentifier,
        fields: Vec<FieldConstructor>,
    },
}

#[derive(Debug)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    List(Vec<Expr>),
    Void,
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug)]
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
    As,
}

#[derive(Debug)]
pub enum FieldConstructor {
    Named {
        name: Identifier,
        value: Expr,
    },
    Auto(Identifier),
}

