use std::fmt::Display;

use crate::line_endings::LineEndingStyle;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerError {
    UnexpectedCharacter(char),
    UnexpectedEndOfFile,
    UnterminatedStringLiteral,
    UnterminatedCharLiteral,
    EmptyCharLiteral,
    CharLiteralNotOneChar,
    CharLiteralTooLong,
    InvalidEscapeSequence(char),
    StringTooLong,
    NumberTooLong,
    IdentifierTooLong,
    InconsistentLineEndings(LineEndingStyle, LineEndingStyle),
}

impl LexerError {
    // Whether the lexer should stop processing the file after this error.
    pub fn is_fatal(&self) -> bool {
        match self {
            Self::UnexpectedCharacter(c) => *c == '\0', // Null terminator is probably a bad sign.
            Self::UnexpectedEndOfFile => true,          // EOF is EOF.
            Self::UnterminatedStringLiteral => true,    // String parsing is greedy, this basically
            // means EOF was encountered.
            Self::UnterminatedCharLiteral => true, // Same as above.
            Self::EmptyCharLiteral => false,       // Empty character literals are allowed.
            Self::CharLiteralNotOneChar => false, // This means the character literal is too long, but parsable
            Self::CharLiteralTooLong => true, // This means the character literal is too long to
                                              // parse. Stop to avoid infinite loops.
            Self::InvalidEscapeSequence(_) => false, // Invalid escape sequences don't mean
                                                     // everything is broken.
            Self::StringTooLong => true, // This means the string literal is too long to parse.
                                         // Stop to avoid infinite loops.
            Self::NumberTooLong => true, // Same as above.
            Self::IdentifierTooLong => true, // Same as above.
            Self::InconsistentLineEndings(_, _) => false, // An error, but probably not fatal.
        }
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnexpectedCharacter(c) => write!(f, "Unexpected character '{}'", c),
            Self::UnexpectedEndOfFile => write!(f, "Unexpected end of file"),
            Self::UnterminatedStringLiteral => write!(f, "Unterminated string literal"),
            Self::UnterminatedCharLiteral => write!(f, "Unterminated character literal"),
            Self::EmptyCharLiteral => write!(f, "Empty character literal"),
            Self::CharLiteralTooLong => write!(f, "Character literal too long"),
            Self::CharLiteralNotOneChar => write!(f, "Character literal not one character"),
            Self::InvalidEscapeSequence(c) => write!(f, "Invalid escape sequence '\\{}'", c),
            Self::StringTooLong => write!(f, "String literal too long"),
            Self::NumberTooLong => write!(f, "Number too long"),
            Self::IdentifierTooLong => write!(f, "Identifier too long"),
            Self::InconsistentLineEndings(expected, actual) => {
                write!(
                    f,
                    "Inconsistent line endings: expected {}, encountered {}",
                    expected, actual
                )
            }
        }
    }
}
