use crate::{repr::NamespacedIdentifier, errors::validator::ValidatorError};

use super::{FunctionSignature, Decl, look_up_module};

#[derive(Debug, Hash, PartialEq)]
pub struct TraitBody {
    pub methods: Vec<FunctionSignature>,
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
            } => impl_type_name.same(type_name) && impl_trait_name.same(trait_name),
            _ => false,
        })
    } else {
        return Err(ValidatorError::UnknownType(type_name.clone()));
    };

    Ok(type_trait_impl.is_some())
}
