use std::{fmt::Display, hash::Hash};

use crate::unit::Span;

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Option<Span>,
}

impl Token {
    pub fn new(kind: TokenKind, span: Option<Span>) -> Self {
        Self { kind, span }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.span {
            Some(span) => write!(f, "[ {} ] {}", self.kind, span),
            None => write!(f, "[ {} ]", self.kind),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Plus,
    Semicolon,
    Slash,
    Star,
    Pipe,
    Question,

    // One or two character tokens.
    Colon,
    DoubleColon,
    Minus,
    Arrow,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Char(char),
    Integer(u64),
    Float(f64),

    // Keywords.
    Pub,
    Mod,
    Fn,
    Type,
    Trait,
    Impl,
    Var,
    And,
    Or,
    Is,
    As,
    For,
    In,
    If,
    Else,
    Match,
    On,
    While,
    Break,
    Continue,
    Return,
    True,
    False,
    Void,
    Self_,

    Eof,
    Comment(String),
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::LeftBrace => write!(f, "{{"),
            TokenKind::RightBrace => write!(f, "}}"),
            TokenKind::LeftBracket => write!(f, "["),
            TokenKind::RightBracket => write!(f, "]"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Question => write!(f, "?"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::DoubleColon => write!(f, "::"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Identifier(s) => write!(f, "ident: {s}"),
            TokenKind::String(s) => write!(f, "\"{s}\""),
            TokenKind::Char(c) => write!(f, "'{c}'"),
            TokenKind::Integer(n) => write!(f, "{n}"),
            TokenKind::Float(n) => write!(f, "{n}"),
            TokenKind::Pub => write!(f, "pub"),
            TokenKind::Mod => write!(f, "mod"),
            TokenKind::Fn => write!(f, "fn"),
            TokenKind::Type => write!(f, "type"),
            TokenKind::Trait => write!(f, "trait"),
            TokenKind::Impl => write!(f, "impl"),
            TokenKind::Var => write!(f, "var"),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::Is => write!(f, "is"),
            TokenKind::As => write!(f, "as"),
            TokenKind::For => write!(f, "for"),
            TokenKind::In => write!(f, "in"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::Match => write!(f, "match"),
            TokenKind::On => write!(f, "on"),
            TokenKind::While => write!(f, "while"),
            TokenKind::Break => write!(f, "break"),
            TokenKind::Continue => write!(f, "continue"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Void => write!(f, "void"),
            TokenKind::Self_ => write!(f, "self"),
            TokenKind::Eof => write!(f, "EOF"),
            TokenKind::Comment(s) => write!(f, "#{s}"),
        }
    }
}

impl PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenKind::Identifier(s1), TokenKind::Identifier(s2)) => s1 == s2,
            (TokenKind::String(s1), TokenKind::String(s2)) => s1 == s2,
            (TokenKind::Integer(n1), TokenKind::Integer(n2)) => n1 == n2,
            (TokenKind::Float(n1), TokenKind::Float(n2)) => {
                n1 == n2
                    || (n1.is_nan() && n2.is_nan())
                    || (n1.is_infinite() && n2.is_infinite())
                    // This works relaiably enough for relative precision
                    || (n1 - n2).abs() <= f64::EPSILON * f64::max(n1.abs(), n2.abs())
            }
            (TokenKind::Comment(s1), TokenKind::Comment(s2)) => s1 == s2,
            _ => std::mem::discriminant(self) == std::mem::discriminant(other),
        }
    }
}

impl Hash for TokenKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            TokenKind::Identifier(s) => s.hash(state),
            TokenKind::String(s) => s.hash(state),
            TokenKind::Integer(n) => n.hash(state),
            TokenKind::Float(n) => n.to_bits().hash(state),
            TokenKind::Comment(s) => s.hash(state),
            _ => {}
        }
    }
}
