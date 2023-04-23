use crate::config::Config;
use crate::errors::lexer::LexerError;
use crate::errors::ErrorModuleRef;
use crate::line_endings::LineEndingStyle;
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
    line_ending_style: LineEndingStyle,
    // EOF
    emitted_eof: bool,
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
            line_ending_style: LineEndingStyle::Unknown,
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

                // Try to recover
                self.next_token()
            }
            TokenizeResult::None => match self.current {
                Some(c) => {
                    self.advance();

                    self.error_module
                        .lock()
                        .unwrap()
                        .add_lexer_error(LexerError::UnexpectedCharacter(c), self.span());

                    // Try to recover, should be able to skip the unexpected character?
                    self.next_token()
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

    fn increment_line(&mut self) {
        self.current_line += 1;
        self.current_column = 1;
        self.current_line_start_in_bytes = self.current_pos_in_bytes;
    }

    fn signal_inconsistent_line_ending(&mut self, encountered: LineEndingStyle) {
        self.error_module.lock().unwrap().add_lexer_error_if_first(
            LexerError::InconsistentLineEndings(self.line_ending_style, encountered),
            None,
        );
    }

    fn handle_line_ending(&mut self) -> LineEndingStyle {
        let encountered = LineEndingStyle::get_line_ending(
            &mut *self,
            |s| s.current,
            |s| {
                s.advance();
            },
        )
        .unwrap(); // Handle line ending is only called when a line ending starts.

        self.increment_line();

        if self.line_ending_style == LineEndingStyle::Unknown {
            self.line_ending_style = encountered;
        } else if self.line_ending_style != encountered {
            self.signal_inconsistent_line_ending(encountered);
        }

        encountered
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

    fn single_char_token_case(&mut self, c: char, kind: TokenKind) -> TokenizeResult {
        if self.current == Some(c) {
            self.advance();
            TokenizeResult::Some(Token::new(kind, self.span()))
        } else {
            TokenizeResult::None
        }
    }

    fn single_char_token(&mut self) -> TokenizeResult {
        try_all_paths!(
            self.single_char_token_case('(', TokenKind::LeftParen),
            self.single_char_token_case(')', TokenKind::RightParen),
            self.single_char_token_case('{', TokenKind::LeftBrace),
            self.single_char_token_case('}', TokenKind::RightBrace),
            self.single_char_token_case('[', TokenKind::LeftBracket),
            self.single_char_token_case(']', TokenKind::RightBracket),
            self.single_char_token_case(',', TokenKind::Comma),
            self.single_char_token_case('.', TokenKind::Dot),
            self.single_char_token_case('+', TokenKind::Plus),
            self.single_char_token_case(';', TokenKind::Semicolon),
            self.single_char_token_case('/', TokenKind::Slash),
            self.single_char_token_case('*', TokenKind::Star),
            self.single_char_token_case('|', TokenKind::Pipe),
            self.single_char_token_case('?', TokenKind::Question),
        )
    }

    fn double_char_token_case(
        &mut self,
        first: char,
        second: char,
        short: TokenKind,
        long: TokenKind,
    ) -> TokenizeResult {
        if self.match_char(first) {
            if self.match_char(second) {
                TokenizeResult::Some(Token::new(long, self.span()))
            } else {
                TokenizeResult::Some(Token::new(short, self.span()))
            }
        } else {
            TokenizeResult::None
        }
    }

    fn double_char_token(&mut self) -> TokenizeResult {
        try_all_paths!(
            self.double_char_token_case(':', ':', TokenKind::Colon, TokenKind::DoubleColon),
            self.double_char_token_case('-', '>', TokenKind::Minus, TokenKind::Arrow),
            self.double_char_token_case('!', '=', TokenKind::Bang, TokenKind::BangEqual),
            self.double_char_token_case('=', '=', TokenKind::Equal, TokenKind::EqualEqual),
            self.double_char_token_case('<', '=', TokenKind::Less, TokenKind::LessEqual),
            self.double_char_token_case('>', '=', TokenKind::Greater, TokenKind::GreaterEqual),
        )
    }

    fn string(&mut self) -> TokenizeResult {
        if !self.match_char('"') {
            return TokenizeResult::None;
        }

        let mut text = String::new();

        while let Some(c) = self.current {
            if c == '\n' || c == '\r' {
                let line_ending = self.handle_line_ending();

                text.push_str(line_ending.as_str());
                continue;
            }

            if c == '"' {
                let span = self.span();
                self.advance();
                return TokenizeResult::Some(Token::new(TokenKind::String(text), span));
            }

            // Handle escape sequences, but don't escape them yet.
            if c == '\\' {
                text.push(c);
                self.advance();
                if self.current.is_none() {
                    return TokenizeResult::Error(LexerError::UnterminatedStringLiteral);
                }

                match self.current.unwrap() {
                    'n' | 'r' | 't' | '\\' | '"' | '\'' | '0' => text.push(self.current.unwrap()),
                    _ => {
                        return TokenizeResult::Error(LexerError::InvalidEscapeSequence(
                            self.current.unwrap(),
                        ))
                    }
                }
            } else {
                text.push(c);
            }

            if self.current_pos - self.start > self.config.max_string_length {
                return TokenizeResult::Error(LexerError::StringTooLong);
            }

            self.advance();
        }
        TokenizeResult::Error(LexerError::UnterminatedStringLiteral)
    }

    fn char(&mut self) -> TokenizeResult {
        if !self.match_char('\'') {
            return TokenizeResult::None;
        }

        if self.current.is_none() {
            return TokenizeResult::Error(LexerError::UnterminatedCharLiteral);
        }

        if self.current == Some('\'') {
            return TokenizeResult::Error(LexerError::EmptyCharLiteral);
        }

        let mut content = self.current.unwrap();

        if self.current == Some('\\') {
            self.advance();
            if self.current.is_none() {
                return TokenizeResult::Error(LexerError::UnterminatedCharLiteral);
            }

            // Handle escape sequences in the lexer for chars
            // As opposed to strings, where we can just handle them in the parser,
            // we need to handle them here to stora as a rust char literal.
            content = match self.current.unwrap() {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '0' => '\0',
                '\'' => '\'',
                '\\' => '\\',
                c => return TokenizeResult::Error(LexerError::InvalidEscapeSequence(c)),
            };
        }

        self.advance();

        if self.match_char('\'') {
            return TokenizeResult::Some(Token::new(TokenKind::Char(content), self.span()));
        }
        TokenizeResult::Error(LexerError::UnterminatedCharLiteral)
    }

    fn comment(&mut self) -> TokenizeResult {
        if !self.match_char('#') {
            return TokenizeResult::None;
        }

        let mut comment = String::new();

        loop {
            let c = self.current;
            self.advance();

            if self.current == Some('\n') || self.current == Some('\r') {
                comment.push(c.unwrap());

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

        // Store u64 first, then convert to f64 if longer than len(u64::MAX)
        let mut number: TokenKind = TokenKind::Integer(0);
        let mut number_len = 0;

        while let Some(c) = self.current {
            if number_len > self.config.max_number_length {
                return TokenizeResult::Error(LexerError::NumberTooLong);
            }
            if c.is_ascii_digit() {
                let digit = c.to_digit(10).unwrap();

                match number {
                    TokenKind::Integer(ref mut ni) => {
                        match ni.checked_mul(10) {
                            Some(n) => match n.checked_add(digit as u64) {
                                Some(n) => *ni = n,
                                None => number = TokenKind::Float(n as f64 + digit as f64),
                            },
                            None => {
                                number = TokenKind::Float((*ni as f64).mul_add(10.0, digit as f64))
                            }
                        };
                    }
                    TokenKind::Float(ref mut n) => {
                        *n = n.mul_add(10.0, digit as f64);
                    }
                    _ => unreachable!(),
                }
                self.advance();
                number_len += 1;
            } else {
                break;
            }
        }
        if let Some(c) = self.current {
            if c == '.' {
                self.advance();
                let mut fraction = 0.1;

                while let Some(c) = self.current {
                    if number_len > self.config.max_number_length {
                        return TokenizeResult::Error(LexerError::NumberTooLong);
                    }
                    if let TokenKind::Integer(n) = number {
                        number = TokenKind::Float(n as f64);
                    }

                    if let TokenKind::Float(ref mut n) = number {
                        if c.is_ascii_digit() {
                            let digit = c.to_digit(10).unwrap();
                            *n += digit as f64 * fraction;
                            self.advance();
                            fraction *= 0.1;
                            number_len += 1;
                        } else {
                            break;
                        }
                    } else {
                        unreachable!();
                    }
                }
                return TokenizeResult::Some(Token::new(number, self.span()));
            }
        }
        TokenizeResult::Some(Token::new(number, self.span()))
    }

    fn identifier_or_keyword(&mut self) -> TokenizeResult {
        if self.current.is_none() || !Self::is_valid_identifier_start(self.current.unwrap()) {
            return TokenizeResult::None;
        }

        let mut text = String::new();

        while let Some(c) = self.current {
            if Self::is_valid_in_identifier(c) {
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
        self.current = self.unit.read();
        self.current_pos += 1;
        self.current_pos_in_bytes += self.current.map_or(0, |c| c.len_utf8());
        self.current_column += 1;
        self.current
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

    fn match_char(&mut self, c: char) -> bool {
        if self.current == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn is_valid_in_identifier(c: char) -> bool {
        Self::is_valid_identifier_start(c) || c.is_numeric()
    }

    fn is_valid_identifier_start(c: char) -> bool {
        c.is_alphabetic() || c == '_' // TODO: Support some more unicode characters
                                      //       like emojis (or even emoji modifier sequences?)
    }
}

impl<'a> TokenProducer for Lexer<'a> {
    fn next(&mut self) -> Option<Token> {
        self.next_token()
    }
}

impl<I> TokenProducer for I
where
    I: Iterator<Item = Token>,
{
    fn next(&mut self) -> Option<Token> {
        Iterator::next(self)
    }
}
