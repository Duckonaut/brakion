use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    UnexpectedEndOfFile,
    UnterminatedStringLiteral,
    UnterminatedCharLiteral,
    InvalidEscapeSequence(char),
    StringTooLong,
    IdentifierTooLong,
    InconsistentLineEndings,
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
            Self::IdentifierTooLong => write!(f, "Identifier too long"),
            Self::InconsistentLineEndings => write!(f, "Inconsistent line endings"),
        }
    }
}
