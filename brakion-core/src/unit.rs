use std::{cell::RefCell, fmt::Display, hash::Hash, io::Read};

const READ_INCREMENT: usize = 1024;

pub type UnitIdentifier = String;

/// A unit is a source file that is being compiled.
/// It is a wrapper around a `Read` object that provides
/// a buffer of the source code, lazy loading it as needed.
#[derive(Debug)]
pub struct Unit<'i, S>
where
    S: Read,
{
    pub name: &'i str,
    pub code: String,
    source: RefCell<S>,
    is_at_end: bool,
    read_pos: usize,
}

impl<'i, S> Unit<'i, S>
where
    S: Read,
{
    pub fn new(name: &'i str, source: S) -> Self {
        Self {
            name,
            code: String::new(),
            source: RefCell::new(source),
            is_at_end: false,
            read_pos: 0,
        }
    }

    pub fn name(&self) -> &'i str {
        self.name
    }

    /// Returns true if the unit has reached the end of the source code.
    pub fn is_at_end(&self) -> bool {
        self.is_at_end
    }

    /// Returns the next character in the source code.
    /// If the end of the source code has been reached, returns `None`.
    /// If the end of the buffer has been reached, loads more source code.
    pub fn read(&mut self) -> Option<char> {
        if self.is_at_end {
            return None;
        }

        if self.read_pos >= self.code.len() && self.advance_buffer() == 0 {
            return None;
        }

        let c = self.code.chars().nth(self.read_pos).unwrap();
        self.read_pos += 1;
        Some(c)
    }

    /// Returns the next character in the source code without advancing the read position.
    /// If the end of the source code has been reached, returns `None`.
    /// If the end of the buffer has been reached, loads more source code.
    pub fn peek(&mut self) -> Option<char> {
        self.peek_n(0)
    }

    /// Returns the nth character in the source code without advancing the read position.
    /// If the end of the source code has been reached, returns `None`.
    /// If the end of the buffer has been reached, loads more source code.
    pub fn peek_n(&mut self, n: usize) -> Option<char> {
        if self.is_at_end {
            return None;
        }

        if self.read_pos + n >= self.code.len() && self.advance_buffer() < n {
            return None;
        }

        let c = self.code.chars().nth(self.read_pos + n).unwrap();
        Some(c)
    }

    fn advance_buffer(&mut self) -> usize {
        let mut source = self.source.borrow_mut();
        let mut buffer = [0; READ_INCREMENT];
        let read = source.read(&mut buffer).unwrap();
        if read == 0 {
            self.is_at_end = true;
            return 0;
        }
        self.code
            .push_str(&String::from_utf8_lossy(&buffer[..read]));
        read
    }
}

impl<'i, S> PartialEq for Unit<'i, S>
where
    S: Read,
{
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'i, S> Hash for Unit<'i, S>
where
    S: Read,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, Copy, PartialEq, Hash)]
pub struct Span<'u>
{
    pub unit: &'u str,
    pub start: Location,
    pub end: Location,
}

impl<'u> Span<'u>
{
    pub fn new(unit: &'u str, start: Location, end: Location) -> Self {
        Self { unit, start, end }
    }
}

impl<'u> Display for Span<'u>
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}@{}", self.start, self.end, self.unit)
    }
}

impl<'u> Clone for Span<'u>
{
    fn clone(&self) -> Self {
        Self {
            unit: self.unit,
            start: self.start,
            end: self.end,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Location {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.line
            .cmp(&other.line)
            .then(self.column.cmp(&other.column))
    }
}
