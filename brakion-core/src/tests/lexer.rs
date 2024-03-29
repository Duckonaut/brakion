use crate::errors::lexer::LexerError;
use crate::errors::ErrorKind;
use crate::filters::ParserTokenFilter;
use crate::lexer::{Lexer, TokenProducer};
use crate::line_endings::LineEndingStyle;
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

fn check_output_token_kinds(source: &str, expected: &[TokenKind]) {
    let errors = ErrorModule::new();
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

    if !errors.is_empty() {
        errors.dump(&mut units);
        panic!("Lexer produced errors");
    }

    compare_token_slice_kinds(&tokens, expected);
}

fn check_output_token_span_positions(source: &str, expected: &[(usize, usize, usize, usize)]) {
    let errors = ErrorModule::new();
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

    if !errors.is_empty() {
        errors.dump(&mut units);
        panic!("Lexer produced errors");
    }

    assert_eq!(tokens.len(), expected.len() + 1); // Account for EOF

    for (token, expected) in tokens.iter().zip(expected.iter()) {
        assert_eq!(token.span.unwrap().start.line, expected.0);
        assert_eq!(token.span.unwrap().start.column, expected.1);
        assert_eq!(token.span.unwrap().end.line, expected.2);
        assert_eq!(token.span.unwrap().end.column, expected.3);
    }
}

fn check_output_token_kinds_with_errors(
    source: &str,
    expected: &[TokenKind],
) -> Vec<crate::errors::Error> {
    let errors = ErrorModule::new();
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

    errors.errors()
}

#[test]
fn empty() {
    let source = "";

    let expected = vec![TokenKind::Eof];

    check_output_token_kinds(source, &expected);
}

#[test]
fn hello_world() {
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

    check_output_token_kinds(source, &expected);
}

#[test]
fn simples() {
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

    check_output_token_kinds(source, &expected);
    check_output_token_kinds(source_with_spaces, &expected);
}

#[test]
fn numbers_short() {
    let source = "123 123456 123.456 0.123 0.12345 12345.0";

    let expected = vec![
        TokenKind::Integer(123),
        TokenKind::Integer(123456),
        TokenKind::Float(123.456),
        TokenKind::Float(0.123),
        TokenKind::Float(0.12345),
        TokenKind::Float(12345.0),
        TokenKind::Eof,
    ];

    check_output_token_kinds(source, &expected);
}

#[test]
fn integer_bounds() {
    let mut source = "".to_string();
    source += &std::u8::MAX.to_string();
    source += " ";
    source += &std::u16::MAX.to_string();
    source += " ";
    source += &std::u32::MAX.to_string();
    source += " ";
    source += &std::u64::MAX.to_string();
    source += " ";
    // Token stores u64, ignore signed min/max

    let expected = vec![
        TokenKind::Integer(std::u8::MAX as u64),
        TokenKind::Integer(std::u16::MAX as u64),
        TokenKind::Integer(std::u32::MAX as u64),
        TokenKind::Integer(std::u64::MAX),
        TokenKind::Eof,
    ];

    check_output_token_kinds(&source, &expected);
}

#[test]
fn float_bounds() {
    let mut source = "".to_string();
    // Force to use decimal point, otherwise the Display impl
    // will round stuff.
    source += format!("{:.1} ", std::f32::MAX).as_str();
    source += format!("{:.1} ", std::f32::MIN).as_str();
    source += format!("{:.1} ", std::f64::MAX).as_str();
    source += format!("{:.1} ", std::f64::MIN).as_str();

    let expected = vec![
        TokenKind::Float(std::f32::MAX as f64),
        TokenKind::Minus,
        TokenKind::Float(std::f32::MIN.abs() as f64),
        TokenKind::Float(std::f64::MAX),
        TokenKind::Minus,
        TokenKind::Float(std::f64::MIN.abs()),
        TokenKind::Eof,
    ];

    check_output_token_kinds(&source, &expected);
}

#[test]
fn integer_too_long() {
    let mut source = String::new();

    for _ in 0..2048 {
        source += "9";
    }

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(&source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::NumberTooLong)
    );
}

#[test]
fn float_too_long() {
    let mut source = "0.".to_string();

    for _ in 0..2048 {
        source += "9";
    }

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(&source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::NumberTooLong)
    );
}

#[test]
fn strings() {
    let source = "\"Hello, world!\" \"Hello,\\n world!\" \"Hello,\\\" world!\"";

    let expected = vec![
        TokenKind::String("Hello, world!".to_string()),
        TokenKind::String("Hello,\n world!".to_string()),
        TokenKind::String("Hello,\" world!".to_string()),
        TokenKind::Eof,
    ];

    check_output_token_kinds(source, &expected);
}

#[test]
fn multiline_string() {
    let source = "\"Hello, \nworld!\"";

    let expected = vec![
        TokenKind::String(
            {
                #[cfg(not(windows))]
                {
                    "Hello, \nworld!"
                }
                #[cfg(windows)]
                {
                    "Hello, \r\nworld!"
                }
            }
            .to_string(),
        ),
        TokenKind::Eof,
    ];

    check_output_token_kinds(source, &expected);

    check_output_token_span_positions(source, &[(1, 1, 2, 8)]);
}

#[test]
fn string_invalid_escapes() {
    let source = "\"\\a\" \"\\b\" \"\\c\" \"\\n\" \"\\g\"";

    let expected = vec![
        TokenKind::String("\\a".to_string()),
        TokenKind::String("\\b".to_string()),
        TokenKind::String("\\c".to_string()),
        TokenKind::String("\n".to_string()),
        TokenKind::String("\\g".to_string()),
        TokenKind::Eof,
    ];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 4);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InvalidEscapeSequence('a'))
    );
    assert_eq!(
        errors[1].kind,
        ErrorKind::LexerError(LexerError::InvalidEscapeSequence('b'))
    );
    assert_eq!(
        errors[2].kind,
        ErrorKind::LexerError(LexerError::InvalidEscapeSequence('c'))
    );
    assert_eq!(
        errors[3].kind,
        ErrorKind::LexerError(LexerError::InvalidEscapeSequence('g'))
    );
}

#[test]
fn string_abrupt_termination() {
    let source = "\"Hello, world!";

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::UnterminatedStringLiteral)
    );
}

#[test]
fn string_abrupt_escape_termination() {
    let source = "\"abc\\";

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::UnterminatedStringLiteral)
    );
}

#[test]
fn string_empty() {
    let source = "\"\"";

    let expected = vec![TokenKind::String("".to_string()), TokenKind::Eof];

    check_output_token_kinds(source, &expected);
}

#[test]
fn string_too_long() {
    let mut source = "\"".to_string();
    for _ in 0..(std::u16::MAX as usize) {
        source += "a";
    }
    source += "\"";

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(&source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::StringTooLong)
    );
}

#[test]
fn chars() {
    let source = "'a' '🙂' '\\n' '\\'' '\\\\' '\\t' '\\r' '\\0'";

    let expected = vec![
        TokenKind::Char('a'),
        TokenKind::Char('🙂'),
        TokenKind::Char('\n'),
        TokenKind::Char('\''),
        TokenKind::Char('\\'),
        TokenKind::Char('\t'),
        TokenKind::Char('\r'),
        TokenKind::Char('\0'),
        TokenKind::Eof,
    ];

    check_output_token_kinds(source, &expected);
}

#[test]
fn empty_char() {
    let source = "''";

    let expected = vec![TokenKind::Eof];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::EmptyCharLiteral)
    );
}

#[test]
fn char_invalid_escapes() {
    let source = "'\\a' '\\b' '\\c' '\\n' '\\g'";

    let expected = vec![TokenKind::Char('\n'), TokenKind::Eof];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 4);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InvalidEscapeSequence('a'))
    );
    assert_eq!(
        errors[1].kind,
        ErrorKind::LexerError(LexerError::InvalidEscapeSequence('b'))
    );
    assert_eq!(
        errors[2].kind,
        ErrorKind::LexerError(LexerError::InvalidEscapeSequence('c'))
    );
    assert_eq!(
        errors[3].kind,
        ErrorKind::LexerError(LexerError::InvalidEscapeSequence('g'))
    );
}

#[test]
fn char_abrupt_termination() {
    let source = "'a";

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::UnterminatedCharLiteral)
    );
}

#[test]
fn char_no_char() {
    let source = "'";

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::UnterminatedCharLiteral)
    );
}

#[test]
fn char_abrupt_escape_termination() {
    let source = "'\\";

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::UnterminatedCharLiteral)
    );
}

#[test]
fn char_not_one_char() {
    let source = "'ab'";

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::UnterminatedCharLiteral)
    );
}

#[test]
fn char_too_long() {
    let mut source = "'".to_string();

    for _ in 0..(std::u16::MAX as usize) {
        source += "a";
    }

    source += "'";

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(&source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::UnterminatedCharLiteral)
    );
}

#[test]
fn keywords() {
    let source = "pub mod fn type trait impl var and or is as for in if else match on while break continue return true false void self";

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
        TokenKind::Is,
        TokenKind::As,
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
        TokenKind::Self_,
        TokenKind::Eof,
    ];

    check_output_token_kinds(source, &expected);
}

#[test]
fn identifiers() {
    let source = "self a b abc a_b _a _abc ___abc Abc aBC _ABC ą _ą _Ą ł _ _0 _a0 a0";

    let expected = vec![
        TokenKind::Self_,
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

    check_output_token_kinds(source, &expected);
}

#[test]
fn identifiers_with_keywords() {
    let source = "pub puba apub apuba _pub pub_ forward before";

    let expected = vec![
        TokenKind::Pub,
        TokenKind::Identifier("puba".to_string()),
        TokenKind::Identifier("apub".to_string()),
        TokenKind::Identifier("apuba".to_string()),
        TokenKind::Identifier("_pub".to_string()),
        TokenKind::Identifier("pub_".to_string()),
        TokenKind::Identifier("forward".to_string()),
        TokenKind::Identifier("before".to_string()),
        TokenKind::Eof,
    ];

    check_output_token_kinds(source, &expected);
}

#[test]
fn identifier_too_long() {
    let mut source = "a".to_string();

    for _ in 0..(std::u16::MAX as usize) {
        source += "a";
    }

    let expected = vec![];

    let errors = check_output_token_kinds_with_errors(&source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::IdentifierTooLong)
    );
}

#[test]
fn comments() {
    let source = "# Comment at start\n # Comment ater line break \n  # Comment with a # in it\nidentifier # Comment on non-empty line \n# Comment at end";

    let expected_raw = vec![
        TokenKind::Comment(" Comment at start".to_string()),
        TokenKind::Comment(" Comment ater line break ".to_string()),
        TokenKind::Comment(" Comment with a # in it".to_string()),
        TokenKind::Identifier("identifier".to_string()),
        TokenKind::Comment(" Comment on non-empty line ".to_string()),
        TokenKind::Comment(" Comment at end".to_string()),
        TokenKind::Eof,
    ];

    check_output_token_kinds(source, &expected_raw);
}

#[test]
fn comments_filter() {
    let source = "# Comment at start\n # Comment ater line break \n  # Comment with a # in it\nidentifier # Comment on non-empty line \n# Comment at end";

    let expected = vec![
        TokenKind::Identifier("identifier".to_string()),
        TokenKind::Eof,
    ];

    // Inlined from check_output_tokens with filter

    let errors = ErrorModule::new();
    let config = Config::default();

    let unit = Unit::new(
        "<test>".to_string(),
        0,
        Box::new(Cursor::new(source.to_string())),
    );

    let mut units = vec![unit];

    let mut lexer = Lexer::new(units.iter_mut().next().unwrap(), &config, errors.clone());
    // Diff from check_output_tokens
    let mut filter = ParserTokenFilter::new(&mut lexer);

    let mut tokens = Vec::new();

    while let Some(token) = filter.next() {
        tokens.push(token);
    }

    if !errors.is_empty() {
        errors.dump(&mut units);
        panic!("Lexer produced errors");
    }

    compare_token_slice_kinds(&tokens, &expected);
}

#[test]
fn line_ending_mix() {
    let source_lf = "a\nb\r\nc\rd\n\re";
    let source_cr = "a\rb\nc\r\nd\n\re";
    let source_crlf = "a\r\nb\nc\rd\n\re";
    let source_lfcr = "a\n\rb\nc\rd\r\ne";

    let expected = vec![
        TokenKind::Identifier("a".to_string()),
        TokenKind::Identifier("b".to_string()),
        TokenKind::Identifier("c".to_string()),
        TokenKind::Identifier("d".to_string()),
        TokenKind::Identifier("e".to_string()),
        TokenKind::Eof,
    ];

    let errors = check_output_token_kinds_with_errors(source_lf, &expected);

    assert_eq!(errors.len(), 3);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Lf,
            LineEndingStyle::Crlf
        ))
    );
    assert_eq!(
        errors[1].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Lf,
            LineEndingStyle::Cr
        ))
    );
    assert_eq!(
        errors[2].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Lf,
            LineEndingStyle::Lfcr
        ))
    );

    let errors = check_output_token_kinds_with_errors(source_cr, &expected);

    assert_eq!(errors.len(), 3);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Cr,
            LineEndingStyle::Lf
        ))
    );
    assert_eq!(
        errors[1].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Cr,
            LineEndingStyle::Crlf
        ))
    );
    assert_eq!(
        errors[2].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Cr,
            LineEndingStyle::Lfcr
        ))
    );

    let errors = check_output_token_kinds_with_errors(source_crlf, &expected);

    assert_eq!(errors.len(), 3);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Crlf,
            LineEndingStyle::Lf
        ))
    );
    assert_eq!(
        errors[1].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Crlf,
            LineEndingStyle::Cr
        ))
    );
    assert_eq!(
        errors[2].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Crlf,
            LineEndingStyle::Lfcr
        ))
    );

    let errors = check_output_token_kinds_with_errors(source_lfcr, &expected);

    assert_eq!(errors.len(), 3);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Lfcr,
            LineEndingStyle::Lf
        ))
    );
    assert_eq!(
        errors[1].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Lfcr,
            LineEndingStyle::Cr
        ))
    );
    assert_eq!(
        errors[2].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Lfcr,
            LineEndingStyle::Crlf
        ))
    );
}

// Produce only one error (of a kind) per file, not for every bad line ending
#[test]
fn line_ending_mix_amount() {
    let source_lf = "a\nb\r\nc\r\nd";
    let source_cr = "a\rb\nc\nd";
    let source_crlf = "a\r\nb\nc\nd";
    let source_lfcr = "a\n\rb\nc\nd";

    let expected = vec![
        TokenKind::Identifier("a".to_string()),
        TokenKind::Identifier("b".to_string()),
        TokenKind::Identifier("c".to_string()),
        TokenKind::Identifier("d".to_string()),
        TokenKind::Eof,
    ];

    let errors = check_output_token_kinds_with_errors(source_lf, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Lf,
            LineEndingStyle::Crlf
        ))
    );

    let errors = check_output_token_kinds_with_errors(source_cr, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Cr,
            LineEndingStyle::Lf
        ))
    );

    let errors = check_output_token_kinds_with_errors(source_crlf, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Crlf,
            LineEndingStyle::Lf
        ))
    );

    let errors = check_output_token_kinds_with_errors(source_lfcr, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::InconsistentLineEndings(
            LineEndingStyle::Lfcr,
            LineEndingStyle::Lf
        ))
    );
}

#[test]
fn bad_chars() {
    let source = "a b$c";

    let expected = vec![
        TokenKind::Identifier("a".to_string()),
        TokenKind::Identifier("b".to_string()),
        TokenKind::Identifier("c".to_string()),
        TokenKind::Eof,
    ];

    let errors = check_output_token_kinds_with_errors(source, &expected);

    assert_eq!(errors.len(), 1);
    assert_eq!(
        errors[0].kind,
        ErrorKind::LexerError(LexerError::UnexpectedCharacter('$'))
    );
}
