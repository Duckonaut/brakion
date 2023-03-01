use std::io::Read;

use lexer::TokenProducer;
use unit::UnitIdentifier;

mod lexer;
mod parser;
mod tokens;
mod unit;

pub fn interpret<R: Read + 'static>(unit_name: UnitIdentifier, input: R) {
    let mut unit = unit::Unit::new(&unit_name, Box::new(input));
    let mut lexer = lexer::Lexer::new(&mut unit);

    while let Some(token) = lexer.next_token() {
        println!("{token}");
    }
}
