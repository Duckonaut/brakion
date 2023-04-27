use crate::{tokens::{Token, TokenKind}, unit::Span};

// Separated out as types, easier to change to a different representation (generics?) later.
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

#[derive(Debug)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug)]
pub struct Decl {
    pub visibility: Visibility,
    pub span: Span,
    pub kind: DeclKind,
}

#[derive(Debug)]
pub enum DeclKind {
    Module {
        name: Identifier,
        body: Vec<Decl>,
    },
    Function(Function),
    Type {
        name: Identifier,
        body: TypeBody,
    },
    Trait {
        name: Identifier,
        body: TraitBody,
    },
    Impl {
        trait_name: NamespacedIdentifier,
        type_name: NamespacedIdentifier,
        body: Vec<Decl>,
    },
}

#[derive(Debug)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: NamespacedIdentifier,
}

#[derive(Debug)]
pub struct Function {
    pub signature: FunctionSignature,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub kind: ParameterKind,
}

#[derive(Debug)]
pub enum ParameterKind {
    Self_,
    PreconditionedSelf(NamespacedIdentifier),
    Typed(NamespacedIdentifier),
    Preconditioned(NamespacedIdentifier, NamespacedIdentifier),
}

#[derive(Debug)]
pub struct TypeBody {
    pub variants: Vec<TypeVariant>,
    pub methods: Vec<Function>,
}

#[derive(Debug)]
pub struct TypeVariant {
    pub name: Identifier,
    pub fields: Vec<Field>,
}

#[derive(Debug)]
pub struct Field {
    pub name: Identifier,
    pub ty: NamespacedIdentifier,
}

#[derive(Debug)]
pub struct TraitBody {
    pub methods: Vec<FunctionSignature>,
}

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
