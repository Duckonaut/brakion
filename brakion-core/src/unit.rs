use std::{
    cell::RefCell,
    fmt::Display,
    hash::Hash,
    io::{Read, Seek},
};

const READ_INCREMENT: usize = 1024;

pub type UnitIdentifier = String;

pub trait ReadSeek: Read + Seek {}

impl<T: Read + Seek> ReadSeek for T {}

/// A unit is a source file that is being compiled.
/// It is a wrapper around a `Read`-implementing struct that provides
/// a buffer of the source code, lazy loading it as needed.
pub struct Unit<'i> {
    pub name: &'i str,
    pub code: String,
    source: RefCell<Box<dyn ReadSeek>>,
    is_at_end: bool,
    read_pos: usize,
    debt: usize,
}

impl<'i> std::fmt::Debug for Unit<'i> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Unit")
            .field("name", &self.name)
            .field("code", &self.code)
            .field("source", &"RefCell<Box<dyn Read>>")
            .field("is_at_end", &self.is_at_end)
            .field("read_pos", &self.read_pos)
            .finish()
    }
}

impl<'i> Unit<'i> {
    pub fn new(name: &'i str, source: Box<dyn ReadSeek>) -> Self {
        Self {
            name,
            code: String::new(),
            source: RefCell::new(source),
            is_at_end: false,
            read_pos: 0,
            debt: 0,
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

    fn advance_buffer(&mut self) -> usize {
        let mut source = self.source.borrow_mut();
        let mut buffer = [0; READ_INCREMENT];

        if self.debt > 0 {
            source
                .seek(std::io::SeekFrom::Current(-(self.debt as i64)))
                .unwrap();

            self.debt = 0;
        }

        let read = source.read(&mut buffer).unwrap();
        if read == 0 {
            self.is_at_end = true;
            return 0;
        }
        self.code
            .push_str(match std::str::from_utf8(&buffer[..read]) {
                Ok(s) => s,
                Err(e) => match e.error_len() {
                    Some(_) => {
                        self.debt = READ_INCREMENT - e.valid_up_to();
                        std::str::from_utf8(&buffer[..e.valid_up_to()]).unwrap()
                    }
                    None => {
                        let current_pos = source
                            .seek(std::io::SeekFrom::Current(0))
                            .unwrap();

                        let end_pos = source
                            .seek(std::io::SeekFrom::End(0))
                            .unwrap();

                        let source_has_anything_left = current_pos < end_pos;

                        // Restore the position of the source
                        source
                            .seek(std::io::SeekFrom::Start(current_pos))
                            .unwrap();

                        if !source_has_anything_left {
                            panic!("Invalid UTF-8 sequence at end of file");
                        }

                        self.debt = READ_INCREMENT - e.valid_up_to();
                        std::str::from_utf8(&buffer[..e.valid_up_to()]).unwrap()
                    }
                },
            });
        read
    }
}

#[test]
fn utf8_misalignment_boundary() {
    let mut s = String::new();
    for _ in 0..(READ_INCREMENT - 2) {
        s.push('a');
    }

    let mut bytes = Vec::from(s.as_bytes());

    assert_eq!(bytes.len(), READ_INCREMENT - 2);

    // Add a 4-byte UTF-8 sequence
    bytes.push(0xF0);
    bytes.push(0x90);
    bytes.push(0x80);
    bytes.push(0x80);

    assert_eq!(bytes.len(), READ_INCREMENT + 2);

    bytes.push(b'a');

    assert_eq!(bytes.len(), READ_INCREMENT + 3);

    let unit_name = "test";

    let bytes_reader = std::io::Cursor::new(bytes);

    let mut unit = Unit::new(unit_name, Box::new(bytes_reader));

    for i in 0..(READ_INCREMENT - 2) {
        println!("{:x}", i);
        assert_eq!(unit.read(), Some('a'));
    }

    let c = unit.read().unwrap();

    assert_eq!(c.len_utf8(), 4);

    assert_eq!(unit.read(), Some('a'));
}

#[test]
#[should_panic(expected = "Invalid UTF-8 sequence at end of file")]
fn utf8_misalignment_end() {
    let mut s = String::new();
    for _ in 0..(READ_INCREMENT - 2) {
        s.push('a');
    }

    let mut bytes = Vec::from(s.as_bytes());

    assert_eq!(bytes.len(), READ_INCREMENT - 2);

    // Add a 4-byte UTF-8 sequence
    bytes.push(0xF0);
    bytes.push(0x90);

    assert_eq!(bytes.len(), READ_INCREMENT);

    let unit_name = "test";

    let bytes_reader = std::io::Cursor::new(bytes);

    let mut unit = Unit::new(unit_name, Box::new(bytes_reader));

    for i in 0..(READ_INCREMENT - 2) {
        println!("{:x}", i);
        assert_eq!(unit.read(), Some('a'));
    }

    let _ = unit.read();
}

impl<'i> PartialEq for Unit<'i> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'i> Hash for Unit<'i> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, Copy, PartialEq, Hash)]
pub struct Span<'u> {
    pub unit: &'u str,
    pub start: Location,
    pub end: Location,
}

impl<'u> Span<'u> {
    pub fn new(unit: &'u str, start: Location, end: Location) -> Self {
        Self { unit, start, end }
    }
}

impl<'u> Display for Span<'u> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}@{}", self.start, self.end, self.unit)
    }
}

impl<'u> Clone for Span<'u> {
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
    pub line_start: usize,
    pub column: usize,
}

impl Location {
    pub fn new(line: usize, line_start: usize, column: usize) -> Self {
        Self {
            line,
            line_start,
            column,
        }
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
