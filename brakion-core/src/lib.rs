use std::sync::{Arc, Mutex};

use errors::ErrorModuleRef;
use filters::ParserTokenFilter;
use repr::BrakionTreeVisitor;
use unit::{Location, ReadSeek, Unit, UnitIdentifier, Units};

pub use config::Config;
pub use errors::ErrorModule;

pub mod config;
pub mod errors;
pub mod filters;
pub mod lexer;
pub mod line_endings;
pub mod parser;
pub mod printer;
pub mod repr;
pub mod tokens;
pub mod unit;

#[cfg(test)]
mod tests;

pub struct Brakion {
    config: Config,
    units: Units,
    error_module: ErrorModuleRef,
}

impl Brakion {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            units: Vec::new(),
            error_module: Arc::new(Mutex::new(ErrorModule::new())),
        }
    }

    pub fn add_unit<R: ReadSeek + 'static>(&mut self, name: String, input: R) {
        let id = self.units.len();
        self.units.push(Unit::new(name, id, Box::new(input)));
    }

    pub fn check(&mut self) -> Result<(), Vec<errors::Error>> {
        for unit_id in 0..self.units.len() {
            self.process_unit(unit_id);
        }

        let error_module = self.error_module.lock().unwrap();

        if error_module.unrecoverable() {
            error_module.dump(&mut self.units);
            Err(error_module.errors())
        } else {
            Ok(())
        }
    }

    fn process_unit(&mut self, unit_id: UnitIdentifier) {
        let unit = &mut self.units[unit_id];
        let unit_name = unit.name().to_string();
        let mut lexer = lexer::Lexer::new(unit, &self.config, self.error_module.clone());
        let filtered = ParserTokenFilter::new(&mut lexer);

        let mut parser = parser::Parser::new(&self.config, filtered, self.error_module.clone());

        let decls = parser.parse();

        let mut unit_module = repr::Decl {
            visibility: repr::Visibility::Public,
            kind: repr::DeclKind::Module {
                name: repr::Identifier {
                    span: unit::Span::new(unit_id, Location::new(1, 0, 1), Location::new(1, 0, 1)),
                    name: unit_name,
                },
                body: decls,
            },
        };

        let mut printer = printer::Printer::new();
        let printer_node = printer.visit_decl(&mut unit_module);

        printer_node.dump();
    }

    pub fn units(&self) -> &Units {
        &self.units
    }

    pub fn unit(&self, id: UnitIdentifier) -> &Unit {
        &self.units[id]
    }
}
