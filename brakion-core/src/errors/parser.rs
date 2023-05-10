use std::fmt::Display;

use crate::tokens::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    ExpectedDecl,
    ExpectedToken(TokenKind),
    ExpectedIdentifier,
    ExpectedFunction,
    ExpectedType,
    PubInTraitImpl,
    UnterminatedScope,
    ExpectedMatchArm,
    TooManyFunctionParameters,
}

impl ParserError {
    // Whether the parser should try to recover from this error
    pub fn is_fatal(&self) -> bool {
        match self {
            ParserError::ExpectedDecl => false,
            ParserError::ExpectedToken(_) => true,
            ParserError::ExpectedIdentifier => true,
            ParserError::ExpectedFunction => true,
            ParserError::ExpectedType => false,
            ParserError::PubInTraitImpl => false,
            ParserError::UnterminatedScope => true,
            ParserError::ExpectedMatchArm => true,
            ParserError::TooManyFunctionParameters => true,
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ExpectedDecl => {
                write!(f, "Expected a module, function, type or trait declaration")
            }
            ParserError::ExpectedToken(kind) => write!(f, "Expected token: `{}`", kind),
            ParserError::ExpectedIdentifier => write!(f, "Expected identifier"),
            ParserError::ExpectedFunction => write!(f, "Expected function"),
            ParserError::ExpectedType => write!(f, "Expected type"),
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
        }
    }
}
