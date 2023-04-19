use std::{
    fmt::Display,
    hash::Hash,
    io::{Read, Seek},
};

const READ_INCREMENT: usize = 1024;

pub trait ReadSeek: Read + Seek {} // Helper trait to allow for dynamic dispatch
impl<T: Read + Seek> ReadSeek for T {}

pub type UnitIdentifier = usize; // Key into the `units` map
pub type Units = Vec<Unit>;

/// A unit is a source file that is being compiled.
/// It is a wrapper around a `Read`-implementing struct that provides
/// a buffer of the source code, lazy loading it as needed.
pub struct Unit {
    pub name: String,
    pub id: UnitIdentifier, // Kept here for convenience
    pub code: String,
    source: Box<dyn ReadSeek>,
    is_at_end: bool,
    read_pos: usize,
    debt: usize,
}

impl std::fmt::Debug for Unit {
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

impl Unit {
    pub fn new(name: String, id: UnitIdentifier, source: Box<dyn ReadSeek>) -> Self {
        Self {
            name,
            id,
            code: String::new(),
            source,
            is_at_end: false,
            read_pos: 0,
            debt: 0,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
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
        let source = &mut self.source;
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

        let (decoded, debt) = Self::utf8_safe_read(&buffer[..read], &mut *source);
        self.code.push_str(decoded);
        self.debt = debt;
        read
    }

    // Returns up to `N` characters from the source code, and the debt amount. If at the end of the
    // source code, panics.
    fn utf8_safe_read<'a>(buffer: &'a [u8], source: &mut dyn ReadSeek) -> (&'a str, usize) {
        match std::str::from_utf8(buffer) {
            Ok(s) => (s, 0),
            Err(e) => match e.error_len() {
                // panic if invalid
                // utf8 in the middle
                // of the file
                Some(_) => panic!("Invalid UTF-8 sequence in source code"),
                None => {
                    let current_pos = source.stream_position().unwrap();

                    let end_pos = source.seek(std::io::SeekFrom::End(0)).unwrap();

                    let source_has_anything_left = current_pos < end_pos;

                    // Restore the position of the source
                    source.seek(std::io::SeekFrom::Start(current_pos)).unwrap();

                    if !source_has_anything_left {
                        panic!("Invalid UTF-8 sequence at end of file");
                    }

                    (
                        std::str::from_utf8(&buffer[..e.valid_up_to()]).unwrap(),
                        buffer.len() - e.valid_up_to(),
                    )
                }
            },
        }
    }

    pub fn lines(&mut self, span: &Span) -> Vec<String> {
        assert!(span.unit == self.id);

        let start_line_byte_pos = span.start.line_start;
        let end_line_byte_pos = span.end.line_start;
        let min_bytes_to_read = end_line_byte_pos - start_line_byte_pos;

        let mut lines = String::new();

        let source = &mut self.source;
        let original_pos = source.stream_position().unwrap();

        source
            .seek(std::io::SeekFrom::Start(start_line_byte_pos as u64))
            .unwrap();

        let mut buffer = [0; READ_INCREMENT];
        let mut bytes_read = 0;

        while bytes_read < min_bytes_to_read {
            let read = source.read(&mut buffer).unwrap();
            if read == 0 {
                break;
            }

            let (s, debt) = Self::utf8_safe_read(&buffer[..read], &mut *source);

            if debt > 0 {
                source
                    .seek(std::io::SeekFrom::Current(-(debt as i64)))
                    .unwrap();
            }

            lines.push_str(s);
            bytes_read += read - debt;
        }

        if bytes_read < min_bytes_to_read {
            panic!("Unexpected end of file");
        }

        let last_end = lines.rfind('\n');

        if last_end.is_none() || (last_end.is_some() && last_end.unwrap() < end_line_byte_pos) {
            // The last line is not complete, so we need to read more. No need to go overboard
            // though, if there's nothing left in the file, we'll just return what we have.
            // If there's more than READ_INCREMENT bytes left, or if there's some debt, we'll
            // just return whatever we get.
            buffer = [0; READ_INCREMENT];
            let read = source.read(&mut buffer).unwrap();

            if read == 0 {
                return lines
                    .lines()
                    .map(|s| s.to_string())
                    .collect::<Vec<String>>();
            }

            let (s, _) = Self::utf8_safe_read(&buffer[..read], &mut *source);

            let newline_pos = s.find('\n');

            if let Some(newline_pos) = newline_pos {
                lines.push_str(&s[..newline_pos]);
            } else {
                lines.push_str(s);
            }
        }

        source.seek(std::io::SeekFrom::Start(original_pos)).unwrap();

        lines
            .lines()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
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

    let bytes_reader = std::io::Cursor::new(bytes);

    let mut unit = Unit::new("test".to_string(), 0, Box::new(bytes_reader));

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

    let bytes_reader = std::io::Cursor::new(bytes);

    let mut unit = Unit::new("test".to_string(), 0, Box::new(bytes_reader));

    for i in 0..(READ_INCREMENT - 2) {
        println!("{:x}", i);
        assert_eq!(unit.read(), Some('a'));
    }

    let _ = unit.read();
}

impl PartialEq for Unit {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Hash for Unit {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

#[derive(Debug, Copy, PartialEq, Hash)]
pub struct Span {
    pub unit: UnitIdentifier,
    pub start: Location,
    pub end: Location,
}

impl Span {
    pub fn new(unit: UnitIdentifier, start: Location, end: Location) -> Self {
        Self { unit, start, end }
    }

    pub fn from_spans(start: Span, end: Span) -> Self {
        assert_eq!(start.unit, end.unit);

        Self {
            unit: start.unit,
            start: start.start,
            end: end.end,
        }
    }

    pub fn is_in_span(&self, line: usize, column: usize) -> bool {
        (line > self.start.line || (line == self.start.line && column >= self.start.column))
            && (line < self.end.line || (line == self.end.line && column <= self.end.column))
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl Clone for Span {
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
