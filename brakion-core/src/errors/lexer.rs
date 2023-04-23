use std::fmt::Display;

use crate::lexer::LineEndingStyle;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    UnexpectedEndOfFile,
    UnterminatedStringLiteral,
    UnterminatedCharLiteral,
    InvalidEscapeSequence(char),
    StringTooLong,
    NumberTooLong,
    IdentifierTooLong,
    InconsistentLineEndings(LineEndingStyle, LineEndingStyle),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedCharacter(c) => write!(f, "Unexpected character '{}'", c),
            Self::UnexpectedEndOfFile => write!(f, "Unexpected end of file"),
            Self::UnterminatedStringLiteral => write!(f, "Unterminated string literal"),
            Self::UnterminatedCharLiteral => write!(f, "Unterminated character literal"),
            Self::InvalidEscapeSequence(c) => write!(f, "Invalid escape sequence '\\{}'", c),
            Self::StringTooLong => write!(f, "String literal too long"),
            Self::NumberTooLong => write!(f, "Number too long"),
            Self::IdentifierTooLong => write!(f, "Identifier too long"),
            Self::InconsistentLineEndings(expected, actual) => {
                write!(f, "Inconsistent line endings: expected {}, encountered {}", expected, actual)
            }
        }
    }
}
