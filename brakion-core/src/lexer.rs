use crate::tokens::Token;
use crate::tokens::TokenKind;
use crate::unit::Location;
use crate::unit::Span;
use crate::unit::Unit;
use std::io::Read;

#[derive(Debug)]
pub struct Lexer<'u, S>
where
    S: Read,
{
    unit: &'u mut Unit<'u, S>,
    current: Option<char>,
    start: usize,
    current_pos: usize,
    start_line: usize,
    current_line: usize,
    start_column: usize,
    current_column: usize,
    emitted_eof: bool,
}

impl<'u, S> Lexer<'u, S>
where
    S: Read,
{
    pub fn new(unit: &'u mut Unit<'u, S>) -> Self {
        let current = unit.read();
        Self {
            unit,
            current,
            start: 0,
            current_pos: 0,
            start_line: 1,
            current_line: 1,
            start_column: 1,
            current_column: 1,
            emitted_eof: false,
        }
    }

    pub fn next_token(&mut self) -> Option<Token<'u>> {
        self.skip_whitespace();
        self.start = self.current_pos;
        self.start_line = self.current_line;
        self.start_column = self.current_column;
        let token = match self.current {
            Some('(') => self
                .advance()
                .map(|_| Token::new(TokenKind::LeftParen, self.span())),
            Some(')') => self
                .advance()
                .map(|_| Token::new(TokenKind::RightParen, self.span())),
            Some('{') => self
                .advance()
                .map(|_| Token::new(TokenKind::LeftBrace, self.span())),
            Some('}') => self
                .advance()
                .map(|_| Token::new(TokenKind::RightBrace, self.span())),
            Some('[') => self
                .advance()
                .map(|_| Token::new(TokenKind::LeftBracket, self.span())),
            Some(']') => self
                .advance()
                .map(|_| Token::new(TokenKind::RightBracket, self.span())),
            Some(',') => self
                .advance()
                .map(|_| Token::new(TokenKind::Comma, self.span())),
            Some('.') => self
                .advance()
                .map(|_| Token::new(TokenKind::Dot, self.span())),
            Some('+') => self
                .advance()
                .map(|_| Token::new(TokenKind::Plus, self.span())),
            Some(';') => self
                .advance()
                .map(|_| Token::new(TokenKind::Semicolon, self.span())),
            Some('/') => self
                .advance()
                .map(|_| Token::new(TokenKind::Slash, self.span())),
            Some('*') => self
                .advance()
                .map(|_| Token::new(TokenKind::Star, self.span())),
            Some(':') => {
                if self.match_next(':') {
                    self.advance()
                        .map(|_| Token::new(TokenKind::DoubleColon, self.span()))
                } else {
                    self.advance()
                        .map(|_| Token::new(TokenKind::Colon, self.span()))
                }
            },
            Some('-') => {
                if self.match_next('>') {
                    self.advance()
                        .map(|_| Token::new(TokenKind::Arrow, self.span()))
                } else {
                    self.advance()
                        .map(|_| Token::new(TokenKind::Minus, self.span()))
                }
            }
            Some('!') => {
                if self.match_next('=') {
                    self.advance()
                        .map(|_| Token::new(TokenKind::BangEqual, self.span()))
                } else {
                    self.advance()
                        .map(|_| Token::new(TokenKind::Bang, self.span()))
                }
            }
            Some('=') => {
                if self.match_next('=') {
                    self.advance()
                        .map(|_| Token::new(TokenKind::EqualEqual, self.span()))
                } else {
                    self.advance()
                        .map(|_| Token::new(TokenKind::Equal, self.span()))
                }
            }
            Some('>') => {
                if self.match_next('=') {
                    self.advance()
                        .map(|_| Token::new(TokenKind::GreaterEqual, self.span()))
                } else {
                    self.advance()
                        .map(|_| Token::new(TokenKind::Greater, self.span()))
                }
            }
            Some('<') => {
                if self.match_next('=') {
                    self.advance()
                        .map(|_| Token::new(TokenKind::LessEqual, self.span()))
                } else {
                    self.advance()
                        .map(|_| Token::new(TokenKind::Less, self.span()))
                }
            }
            Some('@') => {
                if self.match_next('=') {
                    self.advance()
                        .map(|_| Token::new(TokenKind::AtEqual, self.span()))
                } else {
                    self.advance()
                        .map(|_| Token::new(TokenKind::At, self.span()))
                }
            }
            Some('"') => self.string(),
            Some(c) if c.is_ascii_digit() => self.number(),
            Some(c) if c.is_alphabetic() => self.identifier(),
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
        };
        self.skip_whitespace();
        token
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current {
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.advance();
                    self.start_line = self.current_line;
                    self.start_column = self.current_column;
                }
                '#' => {
                    while let Some(c) = self.advance() {
                        if c == '\n' {
                            self.advance();
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
    }

    fn string(&mut self) -> Option<Token<'u>> {
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
        }
        Some(Token::new(
            TokenKind::Error("Unterminated string.".to_string()),
            self.span(),
        ))
    }

    fn number(&mut self) -> Option<Token<'u>> {
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
        while let Some(c) = self.current {
            if c.is_alphanumeric() {
                self.advance();
            } else {
                break;
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

    fn match_next(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.current;
        self.current = self.unit.read();
        self.current_pos += 1;
        self.current_column += 1;
        if c == Some('\n') {
            self.current_line += 1;
            self.current_column = 1;
        }
        c
    }

    fn span(&self) -> Option<Span<'u>> {
        Some(Span::new(
            (*self.unit).name(),
            Location::new(self.start_line, self.start_column),
            Location::new(self.current_line, self.current_column),
        ))
    }

    fn peek(&mut self) -> Option<char> {
        self.unit.peek()
    }
}

impl<'u, S> Iterator for Lexer<'u, S>
where
    S: Read,
{
    type Item = Token<'u>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
