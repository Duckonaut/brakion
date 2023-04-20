use crate::lexer::{Lexer, TokenProducer};
use crate::tokens::{Token, TokenKind};
use crate::unit::Unit;
use crate::Config;
use crate::ErrorModule;
use std::io::Cursor;

fn compare_token_slice_kinds(a: &[Token], b: &[TokenKind]) {
    assert_eq!(a.len(), b.len());

    for (a, b) in a.iter().zip(b.iter()) {
        assert_eq!(a.kind, *b); // Takes care of value equality as well
                                // Ignore span
    }
}

fn check_output_tokens(source: &str, expected: &[TokenKind]) {
    let errors = ErrorModule::new_ref();
    let config = Config::default();

    let unit = Unit::new(
        "<test>".to_string(),
        0,
        Box::new(Cursor::new(source.to_string())),
    );

    let mut units = vec![unit];

    let mut lexer = Lexer::new(units.iter_mut().next().unwrap(), &config, errors.clone());

    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        tokens.push(token);
    }

    if !errors.lock().unwrap().errors.is_empty() {
        errors.lock().unwrap().dump(&mut units);
        panic!("Lexer produced errors");
    }

    compare_token_slice_kinds(&tokens, expected);
}

fn check_output_tokens_with_errors(
    source: &str,
    expected: &[TokenKind],
) -> Vec<crate::errors::Error> {
    let errors = ErrorModule::new_ref();
    let config = Config::default();

    let mut unit = Unit::new(
        "<test>".to_string(),
        0,
        Box::new(Cursor::new(source.to_string())),
    );

    let mut lexer = Lexer::new(&mut unit, &config, errors.clone());

    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        tokens.push(token);
    }

    compare_token_slice_kinds(&tokens, expected);

    let err = errors.lock().unwrap().errors.clone();
    err
}

// Basic "Should work when it should work" tests

#[test]
fn test_lexer_hello_world() {
    let source = "fn main() {
        std::io::println(\"Hello, world!\");
    }";

    let expected = vec![
        TokenKind::Fn,
        TokenKind::Identifier("main".to_string()),
        TokenKind::LeftParen,
        TokenKind::RightParen,
        TokenKind::LeftBrace,
        TokenKind::Identifier("std".to_string()),
        TokenKind::DoubleColon,
        TokenKind::Identifier("io".to_string()),
        TokenKind::DoubleColon,
        TokenKind::Identifier("println".to_string()),
        TokenKind::LeftParen,
        TokenKind::String("Hello, world!".to_string()),
        TokenKind::RightParen,
        TokenKind::Semicolon,
        TokenKind::RightBrace,
        TokenKind::Eof,
    ];

    check_output_tokens(source, &expected);
}

#[test]
fn test_lexer_simples() {
    let source = "(){}[],.+;/*|?:-::->!<<==>=>!===";

    let source_with_spaces = "( ) { } [ ] , . + ; / * | ? : - :: -> ! < <= = >= > != ==";

    let expected = vec![
        TokenKind::LeftParen,
        TokenKind::RightParen,
        TokenKind::LeftBrace,
        TokenKind::RightBrace,
        TokenKind::LeftBracket,
        TokenKind::RightBracket,
        TokenKind::Comma,
        TokenKind::Dot,
        TokenKind::Plus,
        TokenKind::Semicolon,
        TokenKind::Slash,
        TokenKind::Star,
        TokenKind::Pipe,
        TokenKind::Question,
        TokenKind::Colon,
        TokenKind::Minus,
        TokenKind::DoubleColon,
        TokenKind::Arrow,
        TokenKind::Bang,
        TokenKind::Less,
        TokenKind::LessEqual,
        TokenKind::Equal,
        TokenKind::GreaterEqual,
        TokenKind::Greater,
        TokenKind::BangEqual,
        TokenKind::EqualEqual,
        TokenKind::Eof,
    ];

    check_output_tokens(source, &expected);
    check_output_tokens(source_with_spaces, &expected);
}

#[test]
fn test_lexer_numbers() {
    let source = "123 123.456 0.123";

    let expected = vec![
        TokenKind::Integer(123),
        TokenKind::Float(123.456),
        TokenKind::Float(0.123),
        TokenKind::Eof,
    ];

    check_output_tokens(source, &expected);
}

#[test]
fn test_lexer_strings() {
    let source = "\"Hello, world!\" \"Hello,\\n world!\" \"Hello,\\\" world!\"";

    let expected = vec![
        TokenKind::String("Hello, world!".to_string()),
        TokenKind::String("Hello,\\n world!".to_string()),
        TokenKind::String("Hello,\\\" world!".to_string()),
        TokenKind::Eof,
    ];

    check_output_tokens(source, &expected);
}

#[test]
fn test_lexer_chars() {
    let source = "'a' '🙂' '\\n' '\\'' '\\\\'";

    let expected = vec![
        TokenKind::Char('a'),
        TokenKind::Char('🙂'),
        TokenKind::Char('\n'),
        TokenKind::Char('\''),
        TokenKind::Char('\\'),
        TokenKind::Eof,
    ];

    check_output_tokens(source, &expected);
}

#[test]
fn test_lexer_keywords() {
    let source = "pub mod fn type trait impl var and or for in if else match on while break continue return true false void";

    let expected = vec![
        TokenKind::Pub,
        TokenKind::Mod,
        TokenKind::Fn,
        TokenKind::Type,
        TokenKind::Trait,
        TokenKind::Impl,
        TokenKind::Var,
        TokenKind::And,
        TokenKind::Or,
        TokenKind::For,
        TokenKind::In,
        TokenKind::If,
        TokenKind::Else,
        TokenKind::Match,
        TokenKind::On,
        TokenKind::While,
        TokenKind::Break,
        TokenKind::Continue,
        TokenKind::Return,
        TokenKind::True,
        TokenKind::False,
        TokenKind::Void,
        TokenKind::Eof,
    ];

    check_output_tokens(source, &expected);
}

#[test]
fn test_lexer_identifiers() {
    let source = "self a b abc a_b _a _abc ___abc Abc aBC _ABC ą _ą _Ą ł _ _0 _a0 a0";

    let expected = vec![
        TokenKind::Identifier("self".to_string()),
        TokenKind::Identifier("a".to_string()),
        TokenKind::Identifier("b".to_string()),
        TokenKind::Identifier("abc".to_string()),
        TokenKind::Identifier("a_b".to_string()),
        TokenKind::Identifier("_a".to_string()),
        TokenKind::Identifier("_abc".to_string()),
        TokenKind::Identifier("___abc".to_string()),
        TokenKind::Identifier("Abc".to_string()),
        TokenKind::Identifier("aBC".to_string()),
        TokenKind::Identifier("_ABC".to_string()),
        TokenKind::Identifier("ą".to_string()),
        TokenKind::Identifier("_ą".to_string()),
        TokenKind::Identifier("_Ą".to_string()),
        TokenKind::Identifier("ł".to_string()),
        TokenKind::Identifier("_".to_string()),
        TokenKind::Identifier("_0".to_string()),
        TokenKind::Identifier("_a0".to_string()),
        TokenKind::Identifier("a0".to_string()),
        TokenKind::Eof,
    ];

    check_output_tokens(source, &expected);
}
