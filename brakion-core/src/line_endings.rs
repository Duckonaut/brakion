#[cfg(windows)]
pub const LINE_ENDING: &str = "\r\n";
#[cfg(not(windows))]
pub const LINE_ENDING: &str = "\n";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineEndingStyle {
    Unknown,
    Lf,
    Cr,
    Lfcr,
    Crlf,
}

impl LineEndingStyle {
    pub fn as_str(&self) -> &'static str {
        match self {
            LineEndingStyle::Unknown => panic!("Attempted to get string for unknown line ending style"),
            LineEndingStyle::Lf => "\n",
            LineEndingStyle::Cr => "\r",
            LineEndingStyle::Lfcr => "\n\r",
            LineEndingStyle::Crlf => "\r\n",
        }
    }
}

impl std::fmt::Display for LineEndingStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LineEndingStyle::Unknown => write!(f, "Unknown"),
            LineEndingStyle::Lf => write!(f, "LF"),
            LineEndingStyle::Cr => write!(f, "CR"),
            LineEndingStyle::Lfcr => write!(f, "LFCR"),
            LineEndingStyle::Crlf => write!(f, "CRLF"),
        }
    }
}
