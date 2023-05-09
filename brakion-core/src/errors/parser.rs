use std::fmt::Display;

use crate::tokens::TokenKind;

#[derive(Debug, Clone, PartialEq)]
pub enum ParserError {
    ExpectedDecl,
    ExpectedToken(TokenKind),
    ExpectedIdentifier,
    ExpectedFunction,
    PubInTraitImpl,
}

impl ParserError {
    // Whether the lexer should stop processing the file after this error.
    pub fn is_fatal(&self) -> bool {
        match self {
            ParserError::ExpectedDecl => false,
            ParserError::ExpectedToken(_) => true,
            ParserError::ExpectedIdentifier => true,
            ParserError::ExpectedFunction => true,
            ParserError::PubInTraitImpl => false,
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::ExpectedDecl => {
                write!(f, "Expected a module, function, type or trait declaration")
            }
            ParserError::ExpectedToken(kind) => write!(f, "Expected token: {}", kind),
            ParserError::ExpectedIdentifier => write!(f, "Expected identifier"),
            ParserError::ExpectedFunction => write!(f, "Expected function"),
            ParserError::PubInTraitImpl => write!(f, "All trait members are implicitly public. Remove the `pub` keyword"),
        }
    }
}
