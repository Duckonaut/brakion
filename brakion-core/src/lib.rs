use std::io::Read;

use unit::UnitIdentifier;

mod lexer;
mod parser;
mod tokens;
mod unit;

pub fn interpret<R: Read>(unit_name: UnitIdentifier, input: R) {
    let mut unit = unit::Unit::new(&unit_name, input);
    let lexer = lexer::Lexer::new(&mut unit);

    for token in lexer {
        println!("{token}");
    }
}
