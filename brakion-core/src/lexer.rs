use crate::config::Config;
use crate::errors::lexer::LexerError;
use crate::errors::ErrorModuleRef;
use crate::tokens::Token;
use crate::tokens::TokenKind;
use crate::unit::Location;
use crate::unit::Span;
use crate::unit::Unit;

// Macro for trying to match with multiple functions
// If a function returns a token, the token is returned
// If a function returns an error, the error is returned
// If a function returns None, the next function is tried
macro_rules! try_all_paths {
    () => {
        TokenizeResult::None
    };
    ($e:expr) => {
        $e
    };
    ($head:expr $(,$tail:expr)* $(,)?) => {
        match $head {
            TokenizeResult::Some(t) => TokenizeResult::Some(t),
            TokenizeResult::Error(e) => TokenizeResult::Error(e),
            TokenizeResult::None => try_all_paths!($($tail,)*),
        }
    };
}

pub trait TokenProducer {
    fn next(&mut self) -> Option<Token>;
}

#[derive(Debug)]
pub struct Lexer<'a> {
    // Refs to other modules
    config: &'a Config,
    unit: &'a mut Unit,
    error_module: ErrorModuleRef,
    // Current character
    current: Option<char>,
    // Positioning
    start: usize,
    current_pos: usize,
    current_pos_in_bytes: usize,
    start_line: usize,
    start_line_start_in_bytes: usize,
    current_line: usize,
    current_line_start_in_bytes: usize,
    start_column: usize,
    current_column: usize,
    // Line ending style
    line_ending_style: Option<LineEndingStyle>,
    // EOF
    emitted_eof: bool,
}

#[derive(Debug)]
enum LineEndingStyle {
    Lf,
    Crlf,
}

#[derive(Debug)]
enum TokenizeResult {
    // A token was produced
    Some(Token),
    // No valid token found
    None,
    // An error occurred
    Error(LexerError),
}

impl<'a> Lexer<'a> {
    pub fn new(unit: &'a mut Unit, config: &'a Config, error_module: ErrorModuleRef) -> Self {
        let current = unit.read();
        Self {
            config,
            unit,
            error_module,
            current,
            start: 0,
            current_pos: 0,
            current_pos_in_bytes: 0,
            start_line: 1,
            start_line_start_in_bytes: 0,
            current_line: 1,
            current_line_start_in_bytes: 0,
            start_column: 1,
            current_column: 1,
            line_ending_style: None,
            emitted_eof: false,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();
        self.start = self.current_pos;
        self.start_line = self.current_line;
        self.start_column = self.current_column;
        self.start_line_start_in_bytes = self.current_line_start_in_bytes;

        let token = try_all_paths!(
            self.single_char_token(),
            self.double_char_token(),
            self.string(),
            self.char(),
            self.comment(),
            self.number(),
            self.identifier_or_keyword(),
        );

        match token {
            TokenizeResult::Some(token) => Some(token),
            TokenizeResult::Error(error) => {
                self.error_module
                    .lock()
                    .unwrap()
                    .add_lexer_error(error, self.span());

                self.next_token()
            }
            TokenizeResult::None => match self.current {
                Some(c) => {
                    self.advance();

                    self.error_module
                        .lock()
                        .unwrap()
                        .add_lexer_error(LexerError::UnexpectedCharacter(c), self.span());

                    None
                }
                None => {
                    if self.emitted_eof {
                        None
                    } else {
                        self.emitted_eof = true;
                        Some(Token::new(TokenKind::Eof, None))
                    }
                }
            },
        }
    }

    fn handle_line_ending(&mut self) {
        if self.line_ending_style.is_none() {
            match self.current {
                Some('\r') => self.line_ending_style = Some(LineEndingStyle::Crlf),
                Some('\n') => self.line_ending_style = Some(LineEndingStyle::Lf),
                _ => unreachable!(),
            }
        }

        match self.line_ending_style {
            Some(LineEndingStyle::Crlf) => {
                match self.current {
                    Some('\r') => {
                        self.advance();
                    }
                    _ => {
                        self.advance();
                        self.error_module
                            .lock()
                            .unwrap()
                            .add_lexer_error(LexerError::InconsistentLineEndings, None)
                    }
                }

                match self.current {
                    Some('\n') => {
                        self.advance();
                    }
                    _ => {
                        self.advance();
                        self.error_module
                            .lock()
                            .unwrap()
                            .add_lexer_error(LexerError::InconsistentLineEndings, None)
                    }
                }

                self.current_line += 1;
                self.current_column = 1;
                self.current_line_start_in_bytes = self.current_pos_in_bytes;
            }
            Some(LineEndingStyle::Lf) => {
                match self.current {
                    Some('\n') => {
                        self.advance();
                    }
                    _ => {
                        self.advance();
                        self.error_module
                            .lock()
                            .unwrap()
                            .add_lexer_error(LexerError::InconsistentLineEndings, None)
                    }
                }

                self.current_line += 1;
                self.current_column = 1;
                self.current_line_start_in_bytes = self.current_pos_in_bytes;
            }
            _ => unreachable!(),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current {
            match c {
                ' ' | '\t' => {
                    self.advance();
                }
                '\n' | '\r' => {
                    self.handle_line_ending();
                    self.start_line = self.current_line;
                    self.start_column = self.current_column;
                }
                _ => break,
            }
        }
    }

    fn single_char_token(&mut self) -> TokenizeResult {
        let token = match self.current {
            Some('(') => TokenizeResult::Some(Token::new(TokenKind::LeftParen, self.span())),
            Some(')') => TokenizeResult::Some(Token::new(TokenKind::RightParen, self.span())),
            Some('{') => TokenizeResult::Some(Token::new(TokenKind::LeftBrace, self.span())),
            Some('}') => TokenizeResult::Some(Token::new(TokenKind::RightBrace, self.span())),
            Some('[') => TokenizeResult::Some(Token::new(TokenKind::LeftBracket, self.span())),
            Some(']') => TokenizeResult::Some(Token::new(TokenKind::RightBracket, self.span())),
            Some(',') => TokenizeResult::Some(Token::new(TokenKind::Comma, self.span())),
            Some('.') => TokenizeResult::Some(Token::new(TokenKind::Dot, self.span())),
            Some('+') => TokenizeResult::Some(Token::new(TokenKind::Plus, self.span())),
            Some(';') => TokenizeResult::Some(Token::new(TokenKind::Semicolon, self.span())),
            Some('/') => TokenizeResult::Some(Token::new(TokenKind::Slash, self.span())),
            Some('*') => TokenizeResult::Some(Token::new(TokenKind::Star, self.span())),
            Some('|') => TokenizeResult::Some(Token::new(TokenKind::Pipe, self.span())),
            Some('?') => TokenizeResult::Some(Token::new(TokenKind::Question, self.span())),
            _ => TokenizeResult::None,
        };

        if let TokenizeResult::Some(_) = token {
            self.advance();
        }

        token
    }

    fn double_char_token(&mut self) -> TokenizeResult {
        match self.current {
            Some(':') => TokenizeResult::Some(Token::new(
                if self.match_next(':') {
                    TokenKind::DoubleColon
                } else {
                    TokenKind::Colon
                },
                self.span(),
            )),
            Some('-') => TokenizeResult::Some(Token::new(
                if self.match_next('>') {
                    TokenKind::Arrow
                } else {
                    TokenKind::Minus
                },
                self.span(),
            )),
            Some('!') => TokenizeResult::Some(Token::new(
                if self.match_next('=') {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                },
                self.span(),
            )),
            Some('=') => TokenizeResult::Some(Token::new(
                if self.match_next('=') {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                },
                self.span(),
            )),
            Some('>') => TokenizeResult::Some(Token::new(
                if self.match_next('=') {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                },
                self.span(),
            )),
            Some('<') => TokenizeResult::Some(Token::new(
                if self.match_next('=') {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                },
                self.span(),
            )),
            _ => TokenizeResult::None,
        }
    }

    fn string(&mut self) -> TokenizeResult {
        if !self.match_cur('"') {
            return TokenizeResult::None;
        }

        let mut text = String::new();

        let mut escape_last = false;
        while let Some(c) = self.advance() {
            if c == '"' && !escape_last {
                return TokenizeResult::Some(Token::new(TokenKind::String(text), self.span()));
            }

            text.push(c);

            if self.current_pos - self.start > self.config.max_string_length {
                return TokenizeResult::Error(LexerError::StringTooLong);
            }

            if c == '\\' {
                escape_last = true;
                continue;
            }

            escape_last = false;
        }
        TokenizeResult::Error(LexerError::UnterminatedStringLiteral)
    }

    fn char(&mut self) -> TokenizeResult {
        if !self.match_cur('\'') {
            return TokenizeResult::None;
        }

        let mut c = self.advance();
        if c.is_none() {
            return TokenizeResult::Error(LexerError::UnterminatedCharLiteral);
        }

        if c == Some('\\') {
            c = self.advance();
            if c.is_none() {
                return TokenizeResult::Error(LexerError::UnterminatedCharLiteral);
            }

            c = match c.unwrap() {
                'n' => Some('\n'),
                'r' => Some('\r'),
                't' => Some('\t'),
                '0' => Some('\0'),
                '\'' => Some('\''),
                '\\' => Some('\\'),
                c => return TokenizeResult::Error(LexerError::InvalidEscapeSequence(c)),
            };
        }

        if self.match_cur('\'') {
            return TokenizeResult::Some(Token::new(TokenKind::Char(c.unwrap()), self.span()));
        }
        TokenizeResult::Error(LexerError::UnterminatedCharLiteral)
    }

    fn comment(&mut self) -> TokenizeResult {
        if !self.match_cur('#') {
            return TokenizeResult::None;
        }

        let mut comment = String::new();

        loop {
            let c = self.advance();

            if self.current == Some('\n') || self.current == Some('\r') {
                self.handle_line_ending();
                self.start_line = self.current_line;
                self.start_column = self.current_column;

                return TokenizeResult::Some(Token::new(TokenKind::Comment(comment), self.span()));
            }

            if c.is_none() {
                return TokenizeResult::Some(Token::new(TokenKind::Comment(comment), self.span()));
            }

            comment.push(c.unwrap());
        }
    }

    fn number(&mut self) -> TokenizeResult {
        if self.current.is_none() || !self.current.unwrap().is_ascii_digit() {
            return TokenizeResult::None;
        }

        // TODO: limit the length of numbers + manually parse them

        let mut number = String::new();

        while let Some(c) = self.current {
            if c.is_ascii_digit() {
                number.push(c);
                self.advance();
            } else {
                break;
            }
        }
        if let Some(c) = self.current {
            if c == '.' {
                number.push(c);
                self.advance();
                while let Some(c) = self.current {
                    if c.is_ascii_digit() {
                        number.push(c);
                        self.advance();
                    } else {
                        break;
                    }
                }
                return TokenizeResult::Some(Token::new(
                    TokenKind::Float(number.parse().unwrap()),
                    self.span(),
                ));
            }
        }
        TokenizeResult::Some(Token::new(
            TokenKind::Integer(number.parse().unwrap()),
            self.span(),
        ))
    }

    fn identifier_or_keyword(&mut self) -> TokenizeResult {
        if self.current.is_none() || !self.current.unwrap().is_alphanumeric() {
            return TokenizeResult::None;
        }

        let mut text = String::new();

        while let Some(c) = self.current {
            if c.is_alphanumeric() || c == '_' {
                text.push(c);
                self.advance();
            } else {
                break;
            }

            if self.current_pos - self.start > self.config.max_identifier_length {
                return TokenizeResult::Error(LexerError::IdentifierTooLong);
            }
        }

        let kind = match text.as_str() {
            "pub" => TokenKind::Pub,
            "mod" => TokenKind::Mod,
            "fn" => TokenKind::Fn,
            "type" => TokenKind::Type,
            "trait" => TokenKind::Trait,
            "impl" => TokenKind::Impl,
            "var" => TokenKind::Var,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "match" => TokenKind::Match,
            "on" => TokenKind::On,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "void" => TokenKind::Void,
            _ => TokenKind::Identifier(text.to_string()),
        };
        TokenizeResult::Some(Token::new(kind, self.span()))
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.current;
        self.current = self.unit.read();
        self.current_pos += 1;
        self.current_pos_in_bytes += c.map_or(0, |c| c.len_utf8());
        self.current_column += 1;
        c
    }

    fn span(&self) -> Option<Span> {
        Some(Span::new(
            self.unit.id,
            Location::new(
                self.start_line,
                self.start_line_start_in_bytes,
                self.start_column,
            ),
            Location::new(
                self.current_line,
                self.current_line_start_in_bytes,
                self.current_column,
            ),
        ))
    }

    fn match_cur(&mut self, c: char) -> bool {
        if self.current == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_next(&mut self, c: char) -> bool {
        self.advance();
        self.match_cur(c)
    }
}

impl<'a> TokenProducer for Lexer<'a> {
    fn next(&mut self) -> Option<Token> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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

        let mut unit = Unit::new(
            "<test>".to_string(),
            0,
            Box::new(Cursor::new(source.to_string())),
        );

        let mut lexer = Lexer::new(&mut unit, &config, errors);

        let mut tokens = Vec::new();

        while let Some(token) = lexer.next() {
            tokens.push(token);
        }

        compare_token_slice_kinds(&tokens, expected);
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
}
