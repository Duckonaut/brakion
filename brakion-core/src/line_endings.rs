use std::{iter::Peekable, str::Chars};

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
            LineEndingStyle::Unknown => "<UNKNOWN>",
            LineEndingStyle::Lf => "\n",
            LineEndingStyle::Cr => "\r",
            LineEndingStyle::Lfcr => "\n\r",
            LineEndingStyle::Crlf => "\r\n",
        }
    }

    pub fn get_line_ending<C, G, A>(
        context: &mut C,
        mut get_current: G,
        mut advance: A,
    ) -> Option<LineEndingStyle>
    where
        G: FnMut(&mut C) -> Option<char>,
        A: FnMut(&mut C),
    {
        match get_current(context) {
            Some('\r') => {
                advance(context);

                match get_current(context) {
                    Some('\n') => {
                        advance(context);
                        Some(LineEndingStyle::Crlf)
                    }
                    _ => Some(LineEndingStyle::Cr),
                }
            }
            Some('\n') => {
                advance(context);

                match get_current(context) {
                    Some('\r') => {
                        advance(context);
                        Some(LineEndingStyle::Lfcr)
                    }
                    _ => Some(LineEndingStyle::Lf),
                }
            }
            _ => None,
        }
    }

    pub fn count_line_endings(text: &str) -> usize {
        let mut count = 0;
        let mut context = text.chars().peekable();
        let get_current = |context: &mut Peekable<Chars<'_>>| context.peek().copied();
        let advance = |context: &mut Peekable<Chars<'_>>| {
            context.next();
        };

        loop {
            if LineEndingStyle::get_line_ending(&mut context, get_current, advance).is_some() {
                count += 1;
            }
            else {
                advance(&mut context);
            }

            if get_current(&mut context).is_none() {
                break;
            }
        }

        count
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
