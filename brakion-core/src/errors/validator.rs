use std::fmt::Display;

use crate::repr::{BinaryOp, NamespacedIdentifier, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub enum ValidatorError {
    UnknownType(NamespacedIdentifier),
    UnknownTrait(NamespacedIdentifier),
    UnknownVariable(NamespacedIdentifier),
    UnknownField(NamespacedIdentifier, String),
    UnknownMethod(NamespacedIdentifier, String),
    UnknownFunction(NamespacedIdentifier),
    WrongArgCount(NamespacedIdentifier, usize, usize),
    ArgTypeMismatch(NamespacedIdentifier, usize, String, String),
    TypeMismatch(String, String),
    ListTypeMismatch(String, String),
    BadUnaryOp(UnaryOp, String),
    BinaryOpTypeMismatch(BinaryOp, String, String),
    AccessOnNonType(String),
    IndexOnNonList(String),
    IndexNotInt(String, String),
    ConstructorOfVariantedType(NamespacedIdentifier),
    ConstructorFieldTypeMismatch(NamespacedIdentifier, String, String, String),
    ConstructorFieldMissing(NamespacedIdentifier, String),
    ConstructorFieldDuplicate(NamespacedIdentifier, String),
    ConstructorFieldDoesNotExist(NamespacedIdentifier, String),
    SelfInStaticFunction(String),
    DuplicateParameter(String),
    NamespacedVariable(NamespacedIdentifier),
    SelfOutsideOfTraitOrType,
    UnionTypeDuplicate(String),
    DuplicateVariant(String),
    DuplicateType(String),
    DuplicateField(String),
    DuplicateFunction(String),
    MissingTraitMethod(String),
    PreconditionCollapseFailed,
}

impl Display for ValidatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidatorError::UnknownType(name) => write!(f, "Unknown type {}", name),
            ValidatorError::UnknownTrait(name) => write!(f, "Unknown trait {}", name),
            ValidatorError::UnknownVariable(name) => write!(f, "Unknown variable {}", name),
            ValidatorError::UnknownField(name, field) => {
                write!(f, "Unknown field {} on type {}", field, name)
            }
            ValidatorError::UnknownMethod(name, method) => {
                write!(f, "Unknown method {} on type {}", method, name)
            }
            ValidatorError::UnknownFunction(name) => write!(f, "Unknown function {}", name),
            ValidatorError::WrongArgCount(name, expected, actual) => write!(
                f,
                "Wrong number of arguments for function {}. Expected {}, found {}",
                name, expected, actual
            ),
            ValidatorError::ListTypeMismatch(expected, actual) => {
                write!(
                    f,
                    "List elements must be of type {}, found type {}",
                    expected, actual
                )
            }
            ValidatorError::ArgTypeMismatch(name, index, expected, actual) => write!(
                f,
                "Argument {} of function {} must be of type {}, found type {}",
                index, name, expected, actual
            ),
            ValidatorError::TypeMismatch(expected, actual) => {
                write!(f, "Expected type {}, found type {}", expected, actual)
            }
            ValidatorError::BadUnaryOp(op, ty) => {
                write!(f, "Unary operator {} cannot be applied to type {}", op, ty)
            }
            ValidatorError::BinaryOpTypeMismatch(op, expected, actual) => {
                write!(
                    f,
                    "Binary operator {} cannot be applied to types {} and {}",
                    op, expected, actual
                )
            }
            ValidatorError::AccessOnNonType(ty) => {
                write!(f, "Cannot access members of non-type {}", ty)
            }
            ValidatorError::IndexOnNonList(ty) => {
                write!(f, "Cannot index non-list type {}", ty)
            }
            ValidatorError::IndexNotInt(ty, index) => {
                write!(f, "Cannot index type {} with non-integer {}", ty, index)
            }
            ValidatorError::ConstructorOfVariantedType(ty) => {
                write!(f, "Cannot construct type {} with constructor. It contains variants. Choose one.", ty)
            }
            ValidatorError::ConstructorFieldTypeMismatch(ty, field, expected, actual) => {
                write!(f, "Cannot construct type {}. Field {} must be of type {}, found type {}", ty, field, expected, actual)
            }
            ValidatorError::ConstructorFieldMissing(ty, field) => {
                write!(f, "Cannot construct type {}. Field {} is missing.", ty, field)
            }
            ValidatorError::ConstructorFieldDuplicate(ty, field) => {
                write!(f, "Cannot construct type {}. Field {} is duplicated.", ty, field)
            }
            ValidatorError::ConstructorFieldDoesNotExist(ty, field) => {
                write!(f, "Cannot construct type {}. Field {} does not exist.", ty, field)
            }
            ValidatorError::SelfInStaticFunction(name) => {
                write!(f, "Cannot use self in static function {}", name)
            }
            ValidatorError::DuplicateParameter(name) => {
                write!(f, "Duplicate parameter {}", name)
            }
            ValidatorError::NamespacedVariable(name) => {
                write!(f, "Variable {} cannot be namespaced. Global variables are not supported.", name)
            }
            ValidatorError::SelfOutsideOfTraitOrType => {
                write!(f, "self can only be used inside of a trait or type")
            }
            ValidatorError::UnionTypeDuplicate(name) => {
                write!(f, "Union type {} is duplicated", name)
            }
            ValidatorError::DuplicateVariant(name) => {
                write!(f, "Variant {} is duplicated", name)
            }
            ValidatorError::DuplicateType(name) => {
                write!(f, "Type {} is duplicated", name)
            }
            ValidatorError::DuplicateField(name) => {
                write!(f, "Field {} is duplicated", name)
            }
            ValidatorError::DuplicateFunction(name) => {
                write!(f, "Function {} is duplicated", name)
            }
            ValidatorError::MissingTraitMethod(name) => {
                write!(f, "Trait method {} is missing", name)
            }
            ValidatorError::PreconditionCollapseFailed => {
                write!(f, "Precondition collapse failed")
            }
        }
    }
}
