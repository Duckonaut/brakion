use crate::unit::Span;

use super::{Identifier, NamespacedIdentifier, Stmt};

#[derive(Debug, Hash, PartialEq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug, Hash, PartialEq)]
pub struct Decl {
    pub visibility: Visibility,
    pub kind: DeclKind,
}

#[derive(Debug, Hash, PartialEq)]
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
        type_name: TypeReference,
        body: Vec<Function>,
    },
}

#[derive(Debug, Hash, PartialEq)]
pub struct FunctionSignature {
    pub name: Identifier,
    pub takes_self: bool,
    pub self_precondition: Option<TypeReference>,
    pub parameters: Vec<Parameter>,
    pub return_type: TypeReference,
}

#[derive(Debug, Hash, PartialEq)]
pub struct Function {
    pub signature: FunctionSignature,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Hash, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub ty: TypeReference,
    pub kind: ParameterSpec,
}

#[derive(Debug, Hash, PartialEq)]
pub enum ParameterSpec {
    Basic,
    Preconditioned(TypeReference),
}

#[derive(Debug, Hash, PartialEq)]
pub struct TypeBody {
    pub variants: Vec<TypeVariant>,
    pub methods: Vec<(Visibility, Function)>,
}

#[derive(Debug, Hash, PartialEq)]
pub struct TypeVariant {
    pub name: Identifier,
    pub fields: Vec<Field>,
}

#[derive(Debug, Hash, PartialEq)]
pub struct Field {
    pub name: Identifier,
    pub ty: TypeReference,
}

#[derive(Debug, Hash, PartialEq)]
pub struct TraitBody {
    pub methods: Vec<FunctionSignature>,
}

#[derive(Debug, Hash, PartialEq)]
pub struct TypeReference {
    pub span: Option<Span>,
    pub kind: TypeReferenceKind,
}

#[derive(Debug, Hash, PartialEq)]
pub enum TypeReferenceKind {
    Infer, // Decide at type checking time
    Void,
    // TODO: Are builtin types special?
    Named(NamespacedIdentifier),
    List(Box<TypeReference>),
    Union(Vec<TypeReference>),
}
