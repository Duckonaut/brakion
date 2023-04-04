use std::io::Read;

use lexer::TokenProducer;
use unit::{UnitIdentifier, ReadSeek};

pub use config::Config;

mod lexer;
mod parser;
mod tokens;
mod unit;
mod config;

pub fn interpret<R: ReadSeek + 'static>(unit_name: UnitIdentifier, input: R, config: config::Config) {
    let mut unit = unit::Unit::new(&unit_name, Box::new(input));
    let mut lexer = lexer::Lexer::new(&mut unit, &config);
    let mut filtered = lexer::ParserTokenFilter::new(&mut lexer);

    while let Some(token) = filtered.next() {
        println!("{token}");
    }
}
