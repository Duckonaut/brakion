use std::sync::{Mutex, Arc};

use colored::Colorize;

use crate::unit::Span;

use self::{lexer::LexerError, parser::ParserError, validator::ValidatorError, runtime::RuntimeError};

pub mod lexer;
pub mod parser;
pub mod validator;
pub mod runtime;

#[derive(Debug, Clone)]
pub struct ErrorModule {
    pub errors: Arc<Mutex<Vec<Error>>>,
}

impl ErrorModule {
    pub fn new() -> Self {
        Self { errors: Arc::new(Mutex::new(Vec::new())) }
    }

    pub fn add_error(&mut self, kind: ErrorKind, level: ErrorLevel, span: Option<Span>) {
        self.errors.lock().unwrap().push(Error { kind, level, span });
    }

    pub fn add_lexer_error(&mut self, kind: LexerError, span: Option<Span>) {
        self.add_error(ErrorKind::LexerError(kind), ErrorLevel::Error, span);
    }

    pub fn add_parser_error(&mut self, kind: ParserError, span: Option<Span>) {
        self.add_error(ErrorKind::ParserError(kind), ErrorLevel::Error, span);
    }

    pub fn add_validator_error(&mut self, kind: validator::ValidatorError, span: Option<Span>) {
        self.add_error(ErrorKind::ValidatorError(kind), ErrorLevel::Error, span);
    }

    pub fn add_runtime_error(&mut self, kind: RuntimeError, span: Option<Span>) {
        self.add_error(ErrorKind::RuntimeError(kind), ErrorLevel::Error, span);
    }

    pub fn add_error_if_first(&mut self, kind: ErrorKind, level: ErrorLevel, span: Option<Span>) {
        if !self.errors.lock().unwrap().iter().any(|e| e.kind == kind) {
            self.add_error(kind, level, span);
        }
    }

    pub fn add_lexer_error_if_first(&mut self, kind: LexerError, span: Option<Span>) {
        self.add_error_if_first(ErrorKind::LexerError(kind), ErrorLevel::Error, span);
    }

    pub fn add_parser_error_if_first(&mut self, kind: ParserError, span: Option<Span>) {
        self.add_error_if_first(ErrorKind::ParserError(kind), ErrorLevel::Error, span);
    }

    pub fn unrecoverable(&self) -> bool {
        self.errors.lock().unwrap().iter().any(|e| e.level == ErrorLevel::Error)
    }

    pub fn errors(&self) -> Vec<Error> {
        self.errors.lock().unwrap().clone()
    }

    pub fn is_empty(&self) -> bool {
        self.errors.lock().unwrap().is_empty()
    }

    pub fn dump(&self, units: &mut crate::unit::Units) {
        let mut sink = std::io::stderr().lock();

        for error in self.errors.lock().unwrap().iter() {
            self.print_error(&mut sink, error, units).unwrap();
        }
    }

    fn print_error(
        &self,
        mut f: impl std::io::Write,
        error: &Error,
        units: &mut crate::unit::Units,
    ) -> std::io::Result<()> {
        match &error.kind {
            ErrorKind::LexerError(e) => writeln!(f, "{}: {}", "lexer error".red(), e)?,
            ErrorKind::ParserError(e) => writeln!(f, "{}: {}", "parser error".red(), e)?,
            ErrorKind::ValidatorError(e) => writeln!(f, "{}: {}", "validator error".red(), e)?,
            ErrorKind::RuntimeError(e) => writeln!(f, "{}: {}", "runtime error".red(), e)?,
        };

        if error.span.is_none() {
            writeln!(f)?;
            return Ok(());
        }

        let span = error.span.unwrap();
        let unit = units.get_mut(span.unit).unwrap();
        let file = unit.name();

        writeln!(
            f,
            "{} {}:{} in {}",
            " -->".bright_blue(),
            span.start.line,
            span.start.column,
            file,
        )?;
        let line_max_len = std::cmp::max(
            span.start.line.to_string().len(),
            span.end.line.to_string().len(),
        );
        let context = unit.lines(&span);

        if context.is_empty() {
            writeln!(f, "{} {}", span.start.line, " |".bright_blue())
        } else {
            let padding = format!("{}{} ", " ".repeat(line_max_len), " |".bright_blue());

            writeln!(f, "{padding}")?;
            for (line_index, line) in context.iter().enumerate() {
                let line_index = line_index + span.start.line;
                let line_num = format!("{line_index: >line_max_len$} |").bright_blue();

                writeln!(f, "{line_num} {line}")?;
                if line_index == span.start.line && line_index == span.end.line {
                    write!(f, "{padding}")?;
                    writeln!(
                        f,
                        "{}{}",
                        " ".repeat(span.start.column - 1),
                        "^".repeat(span.end.column - span.start.column).bright_red()
                    )?;
                } else if line_index == span.start.line {
                    write!(f, "{padding}")?;
                    writeln!(
                        f,
                        "{}{}",
                        " ".repeat(span.start.column - 1),
                        "^".repeat(line.len() - span.start.column + 1).bright_red()
                    )?;
                } else if line_index == span.end.line {
                    write!(f, "{padding}")?;
                    writeln!(f, "{}", "^".repeat(span.end.column).bright_red())?;
                } else {
                    write!(f, "{padding}")?;
                    writeln!(f, "{}", "^".repeat(line.len()).bright_red())?;
                }
            }
            writeln!(f, "{padding}")?;
            Ok(())
        }
    }
}

impl Default for ErrorModule {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    LexerError(LexerError),
    ParserError(ParserError),
    ValidatorError(ValidatorError),
    RuntimeError(RuntimeError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorLevel {
    Warning,
    Error,
}

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub level: ErrorLevel,
    pub span: Option<Span>,
}
