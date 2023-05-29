use std::fmt::Display;

use crate::tokens::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    InvalidStart,
    UnexpectedEof,
    ExpectedDecl,
    ExpectedStmt,
    ExpectedExpr,
    ExpectedToken(TokenKind),
    ExpectedIdentifier,
    ExpectedFunction,
    ExpectedType,
    ExpectedBody,
    ExpectedLiteral,
    PubInTraitImpl,
    UnterminatedScope,
    ExpectedMatchArm,
    TooManyFunctionParameters,
    VariantMethodInterweave,
    BadCall,
}

impl ParserError {
    // Whether the parser should try to recover from this error
    pub fn is_fatal(&self) -> bool {
        match self {
            ParserError::InvalidStart => false,
            ParserError::UnexpectedEof => false,
            ParserError::ExpectedDecl => false,
            ParserError::ExpectedStmt => false,
            ParserError::ExpectedExpr => false,
            ParserError::ExpectedToken(_) => true,
            ParserError::ExpectedIdentifier => true,
            ParserError::ExpectedFunction => true,
            ParserError::ExpectedType => false,
            ParserError::ExpectedLiteral => false,
            ParserError::PubInTraitImpl => false,
            ParserError::UnterminatedScope => true,
            ParserError::ExpectedMatchArm => true,
            ParserError::TooManyFunctionParameters => true,
            ParserError::ExpectedBody => false,
            ParserError::VariantMethodInterweave => false,
            ParserError::BadCall => false,
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::InvalidStart => write!(
                f,
                "Invalid start of rule. This error should never be shown! Report this as a bug"
            ),
            ParserError::UnexpectedEof => write!(f, "Unexpected end of file"),
            ParserError::ExpectedDecl => {
                write!(f, "Expected a module, function, type or trait declaration")
            }
            ParserError::ExpectedStmt => write!(f, "Expected a statement"),
            ParserError::ExpectedExpr => write!(f, "Expected an expression"),
            ParserError::ExpectedToken(kind) => write!(f, "Expected token: `{}`", kind),
            ParserError::ExpectedIdentifier => write!(f, "Expected identifier"),
            ParserError::ExpectedFunction => write!(f, "Expected function"),
            ParserError::ExpectedType => write!(f, "Expected type"),
            ParserError::ExpectedLiteral => write!(f, "Expected literal"),
            ParserError::PubInTraitImpl => write!(
                f,
                "All trait members are implicitly public. Remove the `pub` keyword"
            ),
            ParserError::UnterminatedScope => {
                write!(f, "Unterminated scope starting from this `{{`")
            }
            ParserError::ExpectedMatchArm => write!(f, "Expected match arm"),
            ParserError::TooManyFunctionParameters => {
                write!(f, "Too many function parameters")
            }
            ParserError::ExpectedBody => write!(f, "Expected body"),
            ParserError::VariantMethodInterweave => {
                write!(f, "Type methods must be declared after all the variants")
            }
            ParserError::BadCall => {
                write!(f, "Calling a non-function and non-method is not allowed")
            }
        }
    }
}
