use std::io::Cursor;

use crate::filters::ParserTokenFilter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::unit::Unit;
use crate::{repr::*, Config, ErrorModule};

fn check_output_expr(source: &str, expected: Option<Expr>) {
    let errors = ErrorModule::new_ref();
    let config = Config::default();

    let unit = Unit::new(
        "<test>".to_string(),
        0,
        Box::new(Cursor::new(source.to_string())),
    );

    let mut units = vec![unit];

    let mut lexer = Lexer::new(units.iter_mut().next().unwrap(), &config, errors.clone());
    let filtered = ParserTokenFilter::new(&mut lexer);

    let mut parser = Parser::new(&config, filtered, errors.clone());
    let output = parser.parse_expr();

    match output {
        crate::parser::ParserResult::Ok(expr) => {
            if expected.is_none() {
                panic!("Expected no expression, got {:?}", expr);
            }
            assert_eq!(expr, expected.unwrap());
        }
        crate::parser::ParserResult::None => {
            if expected.is_some() {
                panic!("Expected an expression, got None");
            }
        }
        crate::parser::ParserResult::Err(err, _) => {
            panic!("Expected an expression, got error: {:?}", err);
        }
    }

    if !errors.lock().unwrap().errors.is_empty() {
        errors.lock().unwrap().dump(&mut units);
        panic!("Errors encountered during parsing");
    }
}

#[test]
fn empty() {
    check_output_expr("", None);
}
