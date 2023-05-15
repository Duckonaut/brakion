use std::fmt::Display;

use crate::repr::NamespacedIdentifier;

#[derive(Debug, Clone, PartialEq)]
pub enum ValidatorError {
    TypeMismatch(String, String),
    UnknownType(NamespacedIdentifier),
}

impl Display for ValidatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidatorError::TypeMismatch(expected, actual) => {
                write!(f, "Expected type {}, found type {}", expected, actual)
            }
            ValidatorError::UnknownType(name) => write!(f, "Unknown type {}", name),
        }
    }
}
