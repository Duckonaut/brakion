use crate::unit::{Span, Location};

mod decl;
mod expr;
mod stmt;

fn test_span(start: usize, end: usize) -> Span {
    Span::new(0, Location::new(1, 0, start), Location::new(1, 0, end))
}
