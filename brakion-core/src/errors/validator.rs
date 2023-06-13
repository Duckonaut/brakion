use std::fmt::Display;

use crate::repr::{BinaryOp, NamespacedIdentifier, UnaryOp, Visibility};

#[derive(Debug, Clone, PartialEq)]
pub enum ValidatorError {
    UnknownType(NamespacedIdentifier),
    UnknownTrait(NamespacedIdentifier),
    UnknownVariable(NamespacedIdentifier),
    UnknownField(NamespacedIdentifier, String),
    UnknownFieldInUnion(String),
    UnknownMethod(NamespacedIdentifier, String),
    UnknownFunction(NamespacedIdentifier),
    WrongArgCount(NamespacedIdentifier, usize, usize),
    ArgTypeMismatch(NamespacedIdentifier, usize, String, String),
    TypeMismatch(String, String),
    ListTypeMismatch(String, String),
    BadUnaryOp(UnaryOp, String),
    BinaryOpTypeMismatch(BinaryOp, String, String),
    AccessOnNonType(String),
    MethodOnNonType(String),
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
    IncompatibleTypes(String, String),
    InvalidAssignment,
    BadConditionType(String),
    BadIterableType(String),
    BadReturnType(String, String),
    BreakOutsideOfLoop,
    ContinueOutsideOfLoop,
    FieldlessTypeInUnion,
    ReturnTypeMismatch(String, String),
    NoReturnInFunction,
    BadTraitMethod(String),
    TypeVariantNotFound(NamespacedIdentifier, String),
    DuplicateWildcard,
    VisibilityMismatch(String, Visibility, Visibility),
    PreconditionNameMismatch(String, String),
    PreconditionReturnMismatch(String, String),
    PreconditionSelfMismatch,
    PreconditionParameterMismatch(String, String),
    PreconditionParameterTypeMismatch(String, String),
    PreconditionOnInvalidType(String),
    PreconditionParameterCountMismatch(usize, usize),
    PreconditionDuplicate(String),
    PreconditionParameterInvalid(String),
    PreconditionNotExhaustive(String),
    PreconditionWildcardDuplicate(String),
    PreconditionWildcardInvalid(String),
    NamespaceCollision(String),
    PrivateMethodCall(String),
    PrivateFunctionCall(String),
    DuplicateMainFunction,
    InvalidMainFunction,
    NoMainFunction,
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
            ValidatorError::UnknownFieldInUnion(field) => write!(
                f,
                "None of the variants of union have a field named {}",
                field
            ),
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
                index + 1,
                name,
                expected,
                actual
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
            ValidatorError::MethodOnNonType(ty) => {
                write!(f, "Cannot call methods on non-type {}", ty)
            }
            ValidatorError::IndexOnNonList(ty) => {
                write!(f, "Cannot index non-list type {}", ty)
            }
            ValidatorError::IndexNotInt(ty, index) => {
                write!(
                    f,
                    "Cannot index type {} with type {}. Try casting to an unsigned integer.",
                    ty, index
                )
            }
            ValidatorError::ConstructorOfVariantedType(ty) => {
                write!(
                    f,
                    "Cannot construct type {} with constructor. It contains variants. Choose one.",
                    ty
                )
            }
            ValidatorError::ConstructorFieldTypeMismatch(ty, field, expected, actual) => {
                write!(
                    f,
                    "Cannot construct type {}. Field {} must be of type {}, found type {}",
                    ty, field, expected, actual
                )
            }
            ValidatorError::ConstructorFieldMissing(ty, field) => {
                write!(
                    f,
                    "Cannot construct type {}. Field {} is missing.",
                    ty, field
                )
            }
            ValidatorError::ConstructorFieldDuplicate(ty, field) => {
                write!(
                    f,
                    "Cannot construct type {}. Field {} is duplicated.",
                    ty, field
                )
            }
            ValidatorError::ConstructorFieldDoesNotExist(ty, field) => {
                write!(
                    f,
                    "Cannot construct type {}. Field {} does not exist.",
                    ty, field
                )
            }
            ValidatorError::SelfInStaticFunction(name) => {
                write!(f, "Cannot use self in static function {}", name)
            }
            ValidatorError::DuplicateParameter(name) => {
                write!(f, "Duplicate parameter {}", name)
            }
            ValidatorError::NamespacedVariable(name) => {
                write!(
                    f,
                    "Variable {} cannot be namespaced. Global variables are not supported.",
                    name
                )
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
            ValidatorError::IncompatibleTypes(expected, actual) => {
                write!(f, "Cannot assign type {} to type {}", actual, expected)
            }
            ValidatorError::InvalidAssignment => {
                write!(f, "The target of this assignment is not assignable")
            }
            ValidatorError::BadConditionType(ty) => {
                write!(f, "Condition must be of type bool, found type {}", ty)
            }
            ValidatorError::BadIterableType(ty) => {
                write!(f, "Iterable must be of type list, found type {}", ty)
            }
            ValidatorError::BadReturnType(expected, actual) => {
                write!(f, "Return type must be {}, found type {}", expected, actual)
            }
            ValidatorError::BreakOutsideOfLoop => {
                write!(f, "break can only be used inside of a loop")
            }
            ValidatorError::ContinueOutsideOfLoop => {
                write!(f, "continue can only be used inside of a loop")
            }
            ValidatorError::FieldlessTypeInUnion => {
                write!(
                    f,
                    "Cannot have a fieldless type in a union and try to access a field"
                )
            }
            ValidatorError::ReturnTypeMismatch(expected, actual) => {
                write!(f, "Return type must be {}, found type {}", expected, actual)
            }
            ValidatorError::NoReturnInFunction => {
                write!(f, "Function must return a value")
            }
            ValidatorError::BadTraitMethod(method) => {
                write!(
                    f,
                    "Implementation of trait method {} is invalid: signature does not match",
                    method
                )
            }
            ValidatorError::TypeVariantNotFound(ty, variant) => {
                write!(f, "Type {} does not have variant {}", ty, variant)
            }
            ValidatorError::DuplicateWildcard => {
                write!(f, "Duplicate wildcard pattern in match")
            }
            ValidatorError::VisibilityMismatch(name, expected, actual) => {
                write!(
                    f,
                    "Visibility mismatch for {}. Expected {}, found {}",
                    name,
                    expected.verbose(),
                    actual.verbose()
                )
            }
            ValidatorError::PreconditionNameMismatch(expected, actual) => {
                write!(
                    f,
                    "Preconditioned functions must be named {}. Found {}. This means the interpreter tried to collapse functions with different names! Report bug.",
                    expected, actual
                )
            }
            ValidatorError::PreconditionReturnMismatch(expected, actual) => {
                write!(
                    f,
                    "Preconditioned functions must return {}. Found {}",
                    expected, actual
                )
            }
            ValidatorError::PreconditionSelfMismatch => {
                write!(
                    f,
                    "Preconditioned functions must take self as the first parameter"
                )
            }
            ValidatorError::PreconditionParameterMismatch(expected, actual) => {
                write!(
                    f,
                    "Preconditioned functions must take parameter {} in this position. Found {}",
                    expected, actual
                )
            }
            ValidatorError::PreconditionParameterTypeMismatch(expected, actual) => {
                write!(
                    f,
                    "Preconditioned functions must take parameter of type {} in this positon, found {}",
                    expected, actual
                )
            }
            ValidatorError::PreconditionOnInvalidType(t) => {
                write!(
                    f,
                    "Parameter type {} cannot be preconditioned. Only union types and varianted named types are supported.",
                    t
                )
            }
            ValidatorError::PreconditionParameterCountMismatch(expected, actual) => {
                write!(
                    f,
                    "Preconditioned functions must take {} parameters. Found {}",
                    expected, actual
                )
            }
            ValidatorError::PreconditionDuplicate(name) => {
                write!(
                    f,
                    "Preconditioned functions of name {} have the same precondition set twice",
                    name
                )
            }
            ValidatorError::PreconditionParameterInvalid(name) => {
                write!(
                    f,
                    "Preconditioned parameters {} cannot be safely collapsed.",
                    name
                )
            }
            ValidatorError::PreconditionNotExhaustive(name) => {
                write!(f, "Preconditions don't cover all cases for parameter {}.", name)
            }
            ValidatorError::PreconditionWildcardDuplicate(name) => {
                write!(
                    f,
                    "Preconditions of parameter {} have incompatible wildcards",
                    name
                )
            }
            ValidatorError::PreconditionWildcardInvalid(name) => {
                write!(
                    f,
                    "Preconditions of function {} have wildcards in different functions.",
                    name
                )
            }
            ValidatorError::NamespaceCollision(name) => {
                write!(f, "Namespaced reference {} is ambiguous.", name)
            }
            ValidatorError::PrivateMethodCall(name) => {
                write!(
                    f,
                    "Cannot call private method {} outside of its defining type.",
                    name
                )
            }
            ValidatorError::PrivateFunctionCall(name) => {
                write!(
                    f,
                    "Cannot call private function {} outside of its defining module.",
                    name
                )
            }
            ValidatorError::DuplicateMainFunction => {
                write!(f, "Duplicate main function")
            }
            ValidatorError::InvalidMainFunction => {
                write!(f, "Invalid main function")
            }
            ValidatorError::NoMainFunction => {
                write!(f, "No main function")
            }
        }
    }
}
