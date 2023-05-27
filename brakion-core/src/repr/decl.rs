use crate::{errors::validator::ValidatorError, unit::Span};

use super::{Identifier, NamespacedIdentifier, Stmt};

#[derive(Debug, Hash, PartialEq, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        matches!(self, Self::Public)
    }
}

impl Default for Visibility {
    fn default() -> Self {
        Self::Private
    }
}

#[derive(Debug, Hash, PartialEq)]
pub enum Decl {
    Module {
        visibility: Visibility,
        name: Identifier,
        body: Vec<Decl>,
    },
    Function {
        visibility: Visibility,
        function: Function,
    },
    Type {
        visibility: Visibility,
        name: Identifier,
        body: TypeBody,
    },
    Trait {
        visibility: Visibility,
        name: Identifier,
        body: TraitBody,
    },
    Impl {
        trait_name: NamespacedIdentifier,
        type_name: NamespacedIdentifier,
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

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct TypeReference {
    pub span: Option<Span>,
    pub kind: TypeReferenceKind,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum TypeReferenceKind {
    Infer, // Decide at type checking time
    Void,
    // TODO: Are builtin types special?
    Named(NamespacedIdentifier),
    List(Box<TypeReference>),
    Union(Vec<TypeReference>),
}

#[derive(Debug, PartialEq)]
pub enum NamespaceReference<'a> {
    Decl(&'a Decl),
    TypeVariant(&'a TypeVariant),
}

pub fn look_up_decl<'a>(
    decls: &'a [Decl],
    name: &NamespacedIdentifier,
) -> Option<NamespaceReference<'a>> {
    if name.namespace.is_empty() {
        decls
            .iter()
            .find(|decl| match decl {
                Decl::Module { name: id, .. } => id.name == name.ident.name,
                Decl::Function { function, .. } => function.signature.name.name == name.ident.name,
                Decl::Type { name: id, .. } => id.name == name.ident.name,
                Decl::Trait { name: id, .. } => id.name == name.ident.name,
                Decl::Impl { .. } => false,
            })
            .map(NamespaceReference::Decl)
    } else {
        let outer_namespace = &name.namespace.first().unwrap().name;
        let inner_namespace = &name.namespace[1..];

        decls
            .iter()
            .find(|decl| match decl {
                Decl::Module { name: id, .. } => id.name == *outer_namespace,
                Decl::Type { name: id, .. } => id.name == *outer_namespace,
                _ => false,
            })
            .and_then(|decl| match decl {
                Decl::Module { body, .. } => look_up_decl(
                    body,
                    &NamespacedIdentifier {
                        namespace: inner_namespace.to_vec(),
                        ident: name.ident.clone(),
                    },
                ),
                Decl::Type { body, .. } => {
                    if !inner_namespace.is_empty() {
                        return None;
                    }
                    look_up_type_variant(
                        body,
                        &NamespacedIdentifier {
                            namespace: inner_namespace.to_vec(),
                            ident: name.ident.clone(),
                        },
                    )
                }
                _ => None,
            })
    }
}

pub fn look_up_type_variant<'a>(
    type_body: &'a TypeBody,
    name: &NamespacedIdentifier,
) -> Option<NamespaceReference<'a>> {
    type_body
        .variants
        .iter()
        .find(|variant| variant.name.name == name.ident.name)
        .map(NamespaceReference::TypeVariant)
}

pub fn look_up_module<'a>(decls: &'a [Decl], name: &[String]) -> Option<&'a [Decl]> {
    if name.is_empty() {
        return Some(decls);
    }

    let outer_namespace = &name.first().unwrap();
    let inner_namespace = &name[1..];

    decls
        .iter()
        .find(|decl| match decl {
            Decl::Module { name: id, .. } => id.name == **outer_namespace,
            _ => false,
        })
        .and_then(|decl| match decl {
            Decl::Module { body, .. } => look_up_module(body, inner_namespace),
            _ => None,
        })
}

pub fn type_implements_trait(
    decls: &[Decl],
    type_name: &NamespacedIdentifier,
    trait_name: &NamespacedIdentifier,
) -> Result<bool, ValidatorError> {
    let type_name_strs = type_name
        .namespace
        .iter()
        .map(|id| id.name.clone())
        .collect::<Vec<_>>();

    let type_trait_impl = if let Some(decls) = look_up_module(decls, &type_name_strs) {
        decls.iter().find(|decl| match decl {
            Decl::Impl {
                type_name: impl_type_name,
                trait_name: impl_trait_name,
                ..
            } => impl_type_name == type_name && impl_trait_name == trait_name,
            _ => false,
        })
    } else {
        return Err(ValidatorError::UnknownType(type_name.clone()));
    };

    Ok(type_trait_impl.is_some())
}

impl TypeReferenceKind {
    pub fn is_infer(&self) -> bool {
        matches!(self, TypeReferenceKind::Infer)
    }

    /// Checks if a variable of `self` can be assigned a value of `other`.
    /// - `Infer` can be assigned anything
    /// - `Void` can be assigned a value of `Void`.
    /// - `Named` can be either a `Type` or a `Trait`. This means there are three real cases:
    ///   - Both are `Type` or both are `Trait`. In this case, the namespaces must be equal.
    ///   - `self` is a `Type` and `other` is a `Trait`. This is not allowed.
    ///   - `self` is a `Trait` and `other` is a `Type`. This is only allowed if `other`
    ///     implements `self`.
    /// - `List` can be assigned a value of `List` if the inner types are compatible.
    /// - `Union` can be assigned a value of `Union` only if  each type in `other` must be
    ///   compatible with exactly one type in `self`.
    /// - `Union` can be assigned a value of `List` if the `Union` contains a `List` type
    ///   compatible with the `List` type in `other`.
    /// - `Union` can be assigned a value of `Void` if the `Union` contains a `Void` type.
    /// - `Union` can be assigned a value of `Named` if the `Union` contains a `Named` type
    ///   compatible with the `Named` type in `other`.
    ///
    /// All other cases are not allowed.
    pub fn is_compatible(&self, other: &Self, decls: &[Decl]) -> Result<bool, ValidatorError> {
        match (self, other) {
            (Self::Infer, _) => Ok(true),
            (Self::Void, Self::Void) => Ok(true),
            (Self::Named(a), Self::Named(b)) => {
                let a_decl = look_up_decl(decls, a);
                let b_decl = look_up_decl(decls, b);

                let a_ref = if let Some(a_decl) = a_decl {
                    a_decl
                } else {
                    return Err(ValidatorError::UnknownType(a.clone()));
                };

                let b_ref = if let Some(b_decl) = b_decl {
                    b_decl
                } else {
                    return Err(ValidatorError::UnknownType(b.clone()));
                };

                match (a_ref, b_ref) {
                    (NamespaceReference::Decl(a_decl), NamespaceReference::Decl(b_decl)) => {
                        match (&a_decl, &b_decl) {
                            (Decl::Type { .. }, Decl::Type { .. }) => Ok(a_decl == b_decl),
                            (Decl::Trait { .. }, Decl::Trait { .. }) => Ok(a_decl == b_decl),
                            (Decl::Type { .. }, Decl::Trait { .. }) => Ok(false),
                            (Decl::Trait { .. }, Decl::Type { .. }) => {
                                type_implements_trait(decls, b, a)
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
                }
            }
            (Self::List(a), Self::List(b)) => a.kind.is_compatible(&b.kind, decls),
            (Self::Union(a), Self::Union(b)) => {
                for b in b.iter() {
                    let mut exactly_one = false;
                    for a in a.iter() {
                        if a.kind.is_compatible(&b.kind, decls)? {
                            if exactly_one {
                                return Ok(false);
                            } else {
                                exactly_one = true;
                            }
                        }
                    }

                    if !exactly_one {
                        return Ok(false);
                    }
                }

                Ok(true)
            }
            (Self::Union(a), b) => {
                let mut exactly_one = false;
                for a in a.iter() {
                    if a.kind.is_compatible(b, decls)? {
                        if exactly_one {
                            return Ok(false);
                        } else {
                            exactly_one = true;
                        }
                    }
                }

                Ok(exactly_one)
            }
            _ => Ok(false),
        }
    }
}
