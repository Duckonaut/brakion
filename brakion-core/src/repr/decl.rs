use super::{Identifier, NamespacedIdentifier, Stmt};

mod functions;
mod traits;
mod types;

pub use functions::*;
pub use traits::*;
pub use types::*;

#[derive(Debug, Hash, PartialEq, Clone, Copy)]
pub enum Visibility {
    Public,
    Private,
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        matches!(self, Self::Public)
    }

    pub fn verbose(&self) -> &'static str {
        match self {
            Self::Public => "public",
            Self::Private => "private",
        }
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
    NativeFunction(NativeFunction),
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

#[derive(Debug, PartialEq)]
pub enum NamespaceReference<'a> {
    Decl(&'a Decl),
    TypeVariant(&'a TypeVariant),
    TraitMethod(&'a FunctionSignature),
    TypeMethod(&'a Function),
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
                Decl::NativeFunction(native_function) => {
                    native_function.signature.name.name == name.ident.name
                }
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
            .filter(|decl| match decl {
                Decl::Module { name: id, .. } => id.name == *outer_namespace,
                Decl::Type { name: id, .. } => id.name == *outer_namespace,
                Decl::Trait { name: id, .. } => id.name == *outer_namespace,
                _ => false,
            })
            .find_map(|decl| match decl {
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
                    let variant = look_up_type_variant(
                        body,
                        &NamespacedIdentifier {
                            namespace: inner_namespace.to_vec(),
                            ident: name.ident.clone(),
                        },
                    );

                    if let Some(variant) = variant {
                        return Some(variant);
                    }

                    let function = body
                        .methods
                        .iter()
                        .find(|function| function.1.signature.name.name == name.ident.name);

                    if let Some(function) = function {
                        return Some(NamespaceReference::TypeMethod(&function.1));
                    }

                    None
                }
                _ => None,
            })
    }
}

pub fn look_up_impl<'a>(
    decls: &'a [Decl],
    trait_name: &NamespacedIdentifier,
    type_name: &NamespacedIdentifier,
) -> Option<&'a [Function]> {
    let module = look_up_module(
        decls,
        &type_name
            .namespace
            .iter()
            .map(|n| n.name.clone())
            .collect::<Vec<_>>(),
    )?;

    let type_decl = module.iter().find(|decl| match decl {
        Decl::Impl {
            trait_name: t,
            type_name: t2,
            ..
        } => t.same(trait_name) && t2.same(type_name),
        _ => false,
    })?;

    if let Decl::Impl { body, .. } = type_decl {
        Some(body)
    } else {
        None
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
        .filter(|decl| match decl {
            Decl::Module { name: id, .. } => id.name == **outer_namespace,
            _ => false,
        })
        .find_map(|decl| match decl {
            Decl::Module { body, .. } => look_up_module(body, inner_namespace),
            _ => None,
        })
}

pub fn look_up_module_mut<'a>(decls: &'a mut [Decl], name: &[String]) -> Option<&'a mut [Decl]> {
    if name.is_empty() {
        return Some(decls);
    }

    let outer_namespace = &name.first().unwrap();
    let inner_namespace = &name[1..];

    decls
        .iter_mut()
        .filter(|decl| match decl {
            Decl::Module { name: id, .. } => id.name == **outer_namespace,
            _ => false,
        })
        .find_map(|decl| match decl {
            Decl::Module { body, .. } => look_up_module_mut(body, inner_namespace),
            _ => None,
        })
}
