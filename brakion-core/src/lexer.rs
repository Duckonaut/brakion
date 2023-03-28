use crate::config::Config;
use crate::tokens::Token;
use crate::tokens::TokenKind;
use crate::unit::Location;
use crate::unit::Span;
use crate::unit::Unit;

macro_rules! try_all {
    ($e1:expr, $($er:expr,)*) => {
        $e1
        $(
            .or_else(|| $er)
        )*
    }
}

pub trait TokenProducer<'u> {
    fn next(&mut self) -> Option<Token<'u>>;
}

#[derive(Debug)]
pub struct Lexer<'u, 'c> {
    config: &'c Config,
    unit: &'u mut Unit<'u>,
    current: Option<char>,
    start: usize,
    current_pos: usize,
    current_pos_in_bytes: usize,
    start_line: usize,
    current_line: usize,
    current_line_start_in_bytes: usize,
    start_column: usize,
    current_column: usize,
    line_ending_style: Option<LineEndingStyle>,
    emitted_eof: bool,
}

#[derive(Debug)]
enum LineEndingStyle {
    Lf,
    Crlf,
}

impl<'u, 'c> Lexer<'u, 'c> {
    pub fn new(unit: &'u mut Unit<'u>, config: &'c Config) -> Self {
        let current = unit.read();
        Self {
            config,
            unit,
            current,
            start: 0,
            current_pos: 0,
            current_pos_in_bytes: 0,
            start_line: 1,
            current_line: 1,
            current_line_start_in_bytes: 0,
            start_column: 1,
            current_column: 1,
            line_ending_style: None,
            emitted_eof: false,
        }
    }

    pub fn next_token(&mut self) -> Option<Token<'u>> {
        self.skip_whitespace();
        self.start = self.current_pos;
        self.start_line = self.current_line;
        self.start_column = self.current_column;

        let token = try_all!(
            self.single_char_token(),
            self.double_char_token(),
            self.string(),
            self.comment(),
            self.number(),
            self.identifier(),
        );

        match token {
            Some(token) => Some(token),
            None => match self.current {
                Some(c) => {
                    self.advance();
                    Some(Token::new(
                        TokenKind::Error(format!("Unexpected character: {c}")),
                        self.span(),
                    ))
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
                    _ => panic!("File has inconsistent line endings"), // TODO: handle error
                }

                match self.current {
                    Some('\n') => {
                        self.advance();
                    }
                    _ => panic!("File has inconsistent line endings"), // TODO: handle error
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
                    _ => panic!("File has inconsistent line endings"), // TODO: handle error
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

    fn single_char_token(&mut self) -> Option<Token<'u>> {
        let token = match self.current {
            Some('(') => Some(Token::new(TokenKind::LeftParen, self.span())),
            Some(')') => Some(Token::new(TokenKind::RightParen, self.span())),
            Some('{') => Some(Token::new(TokenKind::LeftBrace, self.span())),
            Some('}') => Some(Token::new(TokenKind::RightBrace, self.span())),
            Some('[') => Some(Token::new(TokenKind::LeftBracket, self.span())),
            Some(']') => Some(Token::new(TokenKind::RightBracket, self.span())),
            Some(',') => Some(Token::new(TokenKind::Comma, self.span())),
            Some('.') => Some(Token::new(TokenKind::Dot, self.span())),
            Some('+') => Some(Token::new(TokenKind::Plus, self.span())),
            Some(';') => Some(Token::new(TokenKind::Semicolon, self.span())),
            Some('/') => Some(Token::new(TokenKind::Slash, self.span())),
            Some('*') => Some(Token::new(TokenKind::Star, self.span())),
            Some('|') => Some(Token::new(TokenKind::Pipe, self.span())),
            _ => None,
        };

        if token.is_some() {
            self.advance();
        }

        token
    }

    fn double_char_token(&mut self) -> Option<Token<'u>> {
        match self.current {
            Some(':') => {
                if self.match_next(':') {
                    Some(Token::new(TokenKind::DoubleColon, self.span()))
                } else {
                    Some(Token::new(TokenKind::Colon, self.span()))
                }
            }
            Some('-') => {
                if self.match_next('>') {
                    Some(Token::new(TokenKind::Arrow, self.span()))
                } else {
                    Some(Token::new(TokenKind::Minus, self.span()))
                }
            }
            Some('!') => {
                if self.match_next('=') {
                    Some(Token::new(TokenKind::BangEqual, self.span()))
                } else {
                    Some(Token::new(TokenKind::Bang, self.span()))
                }
            }
            Some('=') => {
                if self.match_next('=') {
                    Some(Token::new(TokenKind::EqualEqual, self.span()))
                } else {
                    Some(Token::new(TokenKind::Equal, self.span()))
                }
            }
            Some('>') => {
                if self.match_next('=') {
                    Some(Token::new(TokenKind::GreaterEqual, self.span()))
                } else {
                    Some(Token::new(TokenKind::Greater, self.span()))
                }
            }
            Some('<') => {
                if self.match_next('=') {
                    Some(Token::new(TokenKind::LessEqual, self.span()))
                } else {
                    Some(Token::new(TokenKind::Less, self.span()))
                }
            }
            Some('@') => {
                if self.match_next('=') {
                    Some(Token::new(TokenKind::AtEqual, self.span()))
                } else {
                    Some(Token::new(TokenKind::At, self.span()))
                }
            }
            _ => None,
        }
    }

    fn string(&mut self) -> Option<Token<'u>> {
        if self.current != Some('"') {
            return None;
        }
        self.advance();

        while let Some(c) = self.advance() {
            if c == '"' {
                return Some(Token::new(
                    TokenKind::String(
                        self.unit.code[self.start + 1..self.current_pos - 1].to_string(),
                    ),
                    self.span(),
                ));
            }
            if self.current_pos - self.start > self.config.max_string_length {
                return Some(Token::new(
                    TokenKind::Error("String too long.".to_string()),
                    self.span(),
                ));
            }
        }
        Some(Token::new(
            TokenKind::Error("Unterminated string.".to_string()),
            self.span(),
        ))
    }

    fn comment(&mut self) -> Option<Token<'u>> {
        if self.current != Some('#') {
            return None;
        }
        self.advance();
        loop {
            let c = self.advance();
            if let Some(c) = c {
                if c == '\n' {
                    return Some(Token::new(
                        TokenKind::Comment(
                            self.unit.code[self.start + 1..self.current_pos].to_string(),
                        ),
                        self.span(),
                    ));
                }
            } else {
                return Some(Token::new(
                    TokenKind::Comment(
                        self.unit.code[self.start + 1..self.current_pos].to_string(),
                    ),
                    self.span(),
                ));
            }
        }
    }

    fn number(&mut self) -> Option<Token<'u>> {
        if self.current.is_none() || !self.current.unwrap().is_ascii_digit() {
            return None;
        }

        // TODO: limit the length of numbers + manually parse them

        while let Some(c) = self.current {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }
        if let Some(c) = self.current {
            if c == '.' {
                self.advance();
                while let Some(c) = self.current {
                    if c.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
                return Some(Token::new(
                    TokenKind::Float(
                        self.unit.code[self.start..self.current_pos]
                            .parse()
                            .unwrap(),
                    ),
                    self.span(),
                ));
            }
        }
        Some(Token::new(
            TokenKind::Integer(
                self.unit.code[self.start..self.current_pos]
                    .parse()
                    .unwrap(),
            ),
            self.span(),
        ))
    }

    fn identifier(&mut self) -> Option<Token<'u>> {
        if self.current.is_none() || !self.current.unwrap().is_alphanumeric() {
            return None;
        }

        while let Some(c) = self.current {
            if c.is_alphanumeric() {
                self.advance();
            } else {
                break;
            }

            if self.current_pos - self.start > self.config.max_identifier_length {
                return Some(Token::new(
                    TokenKind::Error("Identifier too long.".to_string()),
                    self.span(),
                ));
            }
        }
        let text = &self.unit.code[self.start..self.current_pos];
        let kind = match text {
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
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "return" => TokenKind::Return,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "void" => TokenKind::Void,
            _ => TokenKind::Identifier(text.to_string()),
        };
        Some(Token::new(kind, self.span()))
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.current;
        self.current = self.unit.read();
        self.current_pos += 1;
        self.current_pos_in_bytes += c.map_or(0, |c| c.len_utf8());
        self.current_column += 1;
        if c == Some('\n') {
            self.current_line += 1;
            self.current_line_start_in_bytes = self.current_pos_in_bytes;
            self.current_column = 1;
        }
        c
    }

    fn span(&self) -> Option<Span<'u>> {
        Some(Span::new(
            (*self.unit).name(),
            Location::new(
                self.start_line,
                self.current_line_start_in_bytes,
                self.start_column,
            ),
            Location::new(
                self.current_line,
                self.current_line_start_in_bytes,
                self.current_column,
            ),
        ))
    }

    fn match_next(&mut self, c: char) -> bool {
        self.advance();
        if self.current == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }
}

impl<'u, 'c> TokenProducer<'u> for Lexer<'u, 'c> {
    fn next(&mut self) -> Option<Token<'u>> {
        self.next_token()
    }
}

pub struct ParserTokenFilter<'u, T>
where
    T: TokenProducer<'u>,
{
    tokens: &'u mut T,
}

impl<'u, T> ParserTokenFilter<'u, T>
where
    T: TokenProducer<'u>,
{
    pub fn new(tokens: &'u mut T) -> Self {
        Self { tokens }
    }
}

impl<'u, T> TokenProducer<'u> for ParserTokenFilter<'u, T>
where
    T: TokenProducer<'u>,
{
    fn next(&mut self) -> Option<Token<'u>> {
        let mut token = self.tokens.next();

        while let Some(Token {
            kind: TokenKind::Comment(_),
            ..
        }) = token
        {
            token = self.tokens.next();
        }

        token
    }
}
