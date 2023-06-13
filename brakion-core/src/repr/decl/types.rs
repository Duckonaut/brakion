use std::fmt::Display;

use crate::{errors::validator::ValidatorError, unit::Span};

use super::{
    look_up_decl, type_implements_trait, Decl, Function, Identifier, NamespaceReference,
    NamespacedIdentifier, Visibility,
};

#[derive(Debug, Hash, PartialEq)]
pub struct TypeBody {
    pub variants: Vec<TypeVariant>,
    pub methods: Vec<(Visibility, Function)>,
}

impl TypeBody {
    pub fn field_type(&self, name: &str) -> Option<TypeReference> {
        let mut field_types = Vec::new();

        for variant in self.variants.iter() {
            for variant_field in variant.fields.iter() {
                if variant_field.name.name == name {
                    field_types.push(variant_field.ty.clone());
                }
            }
        }

        if field_types.is_empty() {
            None
        } else if field_types.len() == 1 {
            Some(field_types[0].clone())
        } else {
            Some(TypeReference {
                span: None,
                kind: TypeReferenceKind::Union(field_types),
            })
        }
    }
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

#[derive(Debug, Hash, PartialEq, Clone)]
pub struct TypeReference {
    pub span: Option<Span>,
    pub kind: TypeReferenceKind,
}

#[derive(Debug, Hash, PartialEq, Clone, PartialOrd, Eq, Ord)]
pub enum IntSize {
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, Hash, PartialEq, Clone, PartialOrd, Eq, Ord)]
pub enum FloatSize {
    F32,
    F64,
}

#[derive(Debug, Hash, PartialEq, Clone)]
pub enum TypeReferenceKind {
    Infer,                  // Decide at type checking time
    FloatIndeterminate,     // Float of indeterminate size, decided at type checking time
    Integer(IntSize, bool), // Integer of specific size, signed or unsigned
    Float(FloatSize),
    Bool,
    Char,
    String,
    Void,
    // TODO: Are builtin types special?
    Named(NamespacedIdentifier),
    List(Box<TypeReference>),
    Union(Vec<TypeReference>),
}

impl Display for TypeReferenceKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeReferenceKind::Infer => write!(f, "_"),
            TypeReferenceKind::FloatIndeterminate => write!(f, "{{float}}"),
            TypeReferenceKind::Integer(size, signed) => {
                write!(
                    f,
                    "{}{}",
                    if *signed { "i" } else { "u" },
                    match size {
                        IntSize::I8 => "8",
                        IntSize::I16 => "16",
                        IntSize::I32 => "32",
                        IntSize::I64 => "64",
                    }
                )
            }
            TypeReferenceKind::Float(size) => write!(
                f,
                "f{}",
                match size {
                    FloatSize::F32 => "32",
                    FloatSize::F64 => "64",
                }
            ),
            TypeReferenceKind::Bool => write!(f, "bool"),
            TypeReferenceKind::Char => write!(f, "char"),
            TypeReferenceKind::String => write!(f, "string"),
            TypeReferenceKind::Void => write!(f, "void"),
            TypeReferenceKind::Named(name) => write!(f, "{}", name),
            TypeReferenceKind::List(ty) => write!(f, "[{}]", ty.kind),
            TypeReferenceKind::Union(tys) => {
                write!(f, "(")?;
                for (i, ty) in tys.iter().enumerate() {
                    if i != 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", ty.kind)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl TypeReferenceKind {
    pub fn is_infer(&self) -> bool {
        matches!(self, TypeReferenceKind::Infer)
    }

    pub fn is_void(&self) -> bool {
        matches!(self, TypeReferenceKind::Void)
    }

    pub fn is_named(&self) -> bool {
        matches!(self, TypeReferenceKind::Named(_))
    }

    pub fn is_list(&self) -> bool {
        matches!(self, TypeReferenceKind::List(_))
    }

    pub fn is_union(&self) -> bool {
        matches!(self, TypeReferenceKind::Union(_))
    }

    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            TypeReferenceKind::FloatIndeterminate
                | TypeReferenceKind::Integer(_, _)
                | TypeReferenceKind::Float(_)
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, TypeReferenceKind::Integer(_, _))
    }

    pub fn is_float(&self) -> bool {
        matches!(
            self,
            TypeReferenceKind::FloatIndeterminate | TypeReferenceKind::Float(_)
        )
    }

    pub fn is_string(&self) -> bool {
        matches!(self, TypeReferenceKind::String)
    }

    pub fn same(&self, other: &Self) -> bool {
        match (self, other) {
            (TypeReferenceKind::Infer, TypeReferenceKind::Infer) => true,
            (TypeReferenceKind::Void, TypeReferenceKind::Void) => true,
            (TypeReferenceKind::Bool, TypeReferenceKind::Bool) => true,
            (TypeReferenceKind::String, TypeReferenceKind::String) => true,
            (TypeReferenceKind::Char, TypeReferenceKind::Char) => true,
            (TypeReferenceKind::Named(a), TypeReferenceKind::Named(b)) => a.same(b),
            (TypeReferenceKind::List(a), TypeReferenceKind::List(b)) => a.kind.same(&b.kind),
            (TypeReferenceKind::Union(a), TypeReferenceKind::Union(b)) => {
                a.len() == b.len() && a.iter().all(|a| b.iter().any(|b| a.kind.same(&b.kind)))
            }
            (TypeReferenceKind::FloatIndeterminate, TypeReferenceKind::FloatIndeterminate) => true,
            (TypeReferenceKind::Integer(a, a_s), TypeReferenceKind::Integer(b, b_s)) => {
                a == b && a_s == b_s
            }
            (TypeReferenceKind::Float(a), TypeReferenceKind::Float(b)) => a == b,
            _ => false,
        }
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
                    (
                        NamespaceReference::TypeVariant(a_variant),
                        NamespaceReference::TypeVariant(b_variant),
                    ) => Ok(a_variant == b_variant),
                    (
                        NamespaceReference::Decl(Decl::Type {
                            body: TypeBody { variants, .. },
                            ..
                        }),
                        NamespaceReference::TypeVariant(b_variant),
                    ) => {
                        if variants.iter().any(|v| v == b_variant) {
                            Ok(true)
                        } else {
                            Ok(false)
                        }
                    }
                    (
                        NamespaceReference::Decl(Decl::Trait { .. }),
                        NamespaceReference::TypeVariant(b_variant),
                    ) => {
                        let type_decl = look_up_decl(decls, &b.up());
                        if let Some(NamespaceReference::Decl(Decl::Type {
                            body: TypeBody { variants, .. },
                            ..
                        })) = type_decl
                        {
                            if variants.iter().any(|v| v == b_variant)
                                && type_implements_trait(decls, &b.up(), a)?
                            {
                                Ok(true)
                            } else {
                                Err(ValidatorError::TypeVariantNotFound(
                                    b.clone(),
                                    b_variant.name.name.clone(),
                                ))
                            }
                        } else {
                            Err(ValidatorError::TypeVariantNotFound(
                                b.clone(),
                                b_variant.name.name.clone(),
                            ))
                        }
                    }
                    _ => unreachable!(),
                }
            }
            (Self::List(a), Self::List(b)) => {
                Ok(b.kind.is_infer() || a.kind.is_compatible(&b.kind, decls)?)
            }
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
            (a, b) if a == b => Ok(true),
            (TypeReferenceKind::Float(size), TypeReferenceKind::Float(other_size)) => {
                Ok(size >= other_size)
            }
            (TypeReferenceKind::Float(_), TypeReferenceKind::Integer(..)) => Ok(true),
            (TypeReferenceKind::Float(..), TypeReferenceKind::FloatIndeterminate) => Ok(true),
            (TypeReferenceKind::FloatIndeterminate, TypeReferenceKind::Integer(..)) => Ok(true),
            (
                TypeReferenceKind::Integer(size, signed),
                TypeReferenceKind::Integer(other_size, other_signed),
            ) => Ok(size >= other_size && (!signed || !other_signed)),
            _ => Ok(false),
        }
    }
}
