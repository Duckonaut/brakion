use crate::unit::Span;

use super::{Identifier, NamespacedIdentifier, Stmt};

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

