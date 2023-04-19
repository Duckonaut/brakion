use colored::Colorize;

use crate::unit::Span;

use self::lexer::LexerError;

pub mod lexer;

pub type ErrorModuleRef = std::sync::Arc<std::sync::Mutex<ErrorModule>>;

#[derive(Debug)]
pub struct ErrorModule {
    pub errors: Vec<Error>,
}

impl ErrorModule {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn add_error(&mut self, kind: ErrorKind, level: ErrorLevel, span: Option<Span>) {
        self.errors.push(Error { kind, level, span });
    }

    pub fn add_lexer_error(&mut self, kind: LexerError, span: Option<Span>) {
        self.add_error(ErrorKind::LexerError(kind), ErrorLevel::Error, span);
    }

    pub fn unrecoverable(&self) -> bool {
        self.errors.iter().any(|e| e.level == ErrorLevel::Error)
    }

    pub fn errors(&self) -> Vec<Error> {
        self.errors.clone()
    }

    pub fn dump(&self, units: &mut crate::unit::Units) {
        let mut sink = std::io::stderr().lock();

        for error in &self.errors {
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

#[derive(Debug, Clone)]
pub enum ErrorKind {
    LexerError(LexerError),
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
