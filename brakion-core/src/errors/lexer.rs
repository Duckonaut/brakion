use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum LexerError {
    UnexpectedCharacter(char),
    UnexpectedEndOfFile,
    UnterminatedStringLiteral,
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
            Self::StringTooLong => write!(f, "String literal too long"),
            Self::IdentifierTooLong => write!(f, "Identifier too long"),
            Self::InconsistentLineEndings => write!(f, "Inconsistent line endings"),
        }
    }
}
