use std::fmt::Display;

use crate::repr::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    InvalidCast(String, String),
    InvalidUnary(UnaryOp),
    InvalidBinary(BinaryOp),
    IndexOutOfBounds(usize),
    UndefinedVariable(String),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::InvalidCast(from, to) => write!(f, "Cannot cast from {} to {}", from, to),
            RuntimeError::InvalidUnary(op) => write!(f, "Invalid unary operation: {}", op),
            RuntimeError::InvalidBinary(op) => write!(f, "Invalid binary operation: {}", op),
            RuntimeError::IndexOutOfBounds(i) => write!(f, "Index out of bounds: {}", i),
            RuntimeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
        }
    }
}
