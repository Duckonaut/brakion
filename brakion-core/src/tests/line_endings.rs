use crate::line_endings::LineEndingStyle;

enum EqCheck {
    LineEnding(Option<LineEndingStyle>),
    Char(char),
}

fn test_str_with(text: &str, checks: &[EqCheck]) {
    let mut context = text.chars().peekable();
    let get_current = |context: &mut std::iter::Peekable<std::str::Chars<'_>>| {
        context.peek().copied()
    };
    let advance = |context: &mut std::iter::Peekable<std::str::Chars<'_>>| {
        context.next();
    };

    for check in checks {
        match check {
            EqCheck::LineEnding(expected) => {
                assert_eq!(
                    LineEndingStyle::get_line_ending(&mut context, get_current, advance),
                    *expected
                );
            }
            EqCheck::Char(expected) => {
                assert_eq!(get_current(&mut context), Some(*expected));
                advance(&mut context);
            }
        }
    }
}

#[test]
fn test_get_line_ending_none() {
    test_str_with(
        "abc",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(None),
            EqCheck::Char('b'),
            EqCheck::LineEnding(None),
            EqCheck::Char('c'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_lf_start() {
    test_str_with(
        "\na",
        &[
            EqCheck::LineEnding(Some(LineEndingStyle::Lf)),
            EqCheck::Char('a'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_lf_middle() {
    test_str_with(
        "a\nb",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Lf)),
            EqCheck::Char('b'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_lf_end() {
    test_str_with(
        "a\n",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Lf)),
        ],
    );
}

#[test]
fn test_get_line_ending_crlf_start() {
    test_str_with(
        "\r\na",
        &[
            EqCheck::LineEnding(Some(LineEndingStyle::Crlf)),
            EqCheck::Char('a'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_crlf_mid() {
    test_str_with(
        "a\r\nb",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Crlf)),
            EqCheck::Char('b'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_crlf_end() {
    test_str_with(
        "a\r\n",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Crlf)),
        ],
    );
}

#[test]
fn test_get_line_ending_cr_start() {
    test_str_with(
        "\ra",
        &[
            EqCheck::LineEnding(Some(LineEndingStyle::Cr)),
            EqCheck::Char('a'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_cr_mid() {
    test_str_with(
        "a\rb",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Cr)),
            EqCheck::Char('b'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_cr_end() {
    test_str_with(
        "a\r",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Cr)),
        ],
    );
}

#[test]
fn test_get_line_ending_lfcr_start() {
    test_str_with(
        "\n\ra",
        &[
            EqCheck::LineEnding(Some(LineEndingStyle::Lfcr)),
            EqCheck::Char('a'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_lfcr_mid() {
    test_str_with(
        "a\n\rb",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Lfcr)),
            EqCheck::Char('b'),
            EqCheck::LineEnding(None),
        ],
    );
}

#[test]
fn test_get_line_ending_lfcr_end() {
    test_str_with(
        "a\n\r",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Lfcr)),
        ],
    );
}

#[test]
fn test_get_line_ending_mixed() {
    test_str_with(
        "a\n\rb\nc\rd\r\n",
        &[
            EqCheck::Char('a'),
            EqCheck::LineEnding(Some(LineEndingStyle::Lfcr)),
            EqCheck::Char('b'),
            EqCheck::LineEnding(Some(LineEndingStyle::Lf)),
            EqCheck::Char('c'),
            EqCheck::LineEnding(Some(LineEndingStyle::Cr)),
            EqCheck::Char('d'),
            EqCheck::LineEnding(Some(LineEndingStyle::Crlf)),
        ],
    );
}

#[test]
fn test_count_lf() {
    assert_eq!(LineEndingStyle::count_line_endings("\na\nb\nc\n\n"), 5);
}

#[test]
fn test_count_crlf() {
    assert_eq!(LineEndingStyle::count_line_endings("\r\na\r\nb\r\nc\r\n\r\n"), 5);
}

#[test]
fn test_count_cr() {
    assert_eq!(LineEndingStyle::count_line_endings("\ra\rb\rc\r\r"), 5);
}

#[test]
fn test_count_lfcr() {
    assert_eq!(LineEndingStyle::count_line_endings("\n\ra\n\rb\n\rc\n\r\n\r"), 5);
}

#[test]
fn test_count_mixed() {
    assert_eq!(
        LineEndingStyle::count_line_endings("\na\n\rb\nc\r\nd\r\n"),
        5
    );
}
