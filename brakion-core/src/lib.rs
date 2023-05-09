use std::sync::{Arc, Mutex};

use errors::ErrorModuleRef;
use filters::ParserTokenFilter;
use repr::BrakionTreeVisitor;
use unit::{ReadSeek, Unit, UnitIdentifier, Units};

pub use config::Config;
pub use errors::ErrorModule;

mod config;
mod errors;
mod filters;
mod lexer;
mod line_endings;
mod parser;
mod printer;
mod repr;
mod tokens;
mod unit;

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

        let mut printer = printer::Printer::new();

        for decl in decls {
            printer.visit_decl(&decl);
        }
    }

    pub fn units(&self) -> &Units {
        &self.units
    }

    pub fn unit(&self, id: UnitIdentifier) -> &Unit {
        &self.units[id]
    }
}
