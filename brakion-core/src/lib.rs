#![allow(clippy::result_large_err)]

use std::path::Path;

use errors::validator::ValidatorError;
use filters::ParserTokenFilter;
use interpreter::{value::Value, Interpreter};
use repr::{BrakionTreeVisitor, Decl, Identifier, Visibility};
use unit::{Location, ReadSeek, Span, Unit, UnitIdentifier, Units};

pub use config::Config;
pub use errors::ErrorModule;
use validator::Validator;

mod builtin;
pub mod config;
pub mod errors;
pub mod filters;
pub mod interpreter;
pub mod lexer;
pub mod line_endings;
pub mod parser;
pub mod printer;
pub mod repr;
pub mod tokens;
pub mod type_representation;
pub mod unit;
pub mod validator;

#[cfg(test)]
mod tests;

pub struct Brakion {
    config: Config,
    units: Units,
    error_module: ErrorModule,
    decls: Vec<Decl>,
}

impl Brakion {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            units: Vec::new(),
            error_module: ErrorModule::new(),
            decls: builtin::builtin_decls(),
        }
    }

    pub fn add_unit<R: ReadSeek + 'static>(&mut self, name: String, input: R) {
        let id = self.units.len();
        self.units.push(Unit::new(name, id, Box::new(input)));
    }

    pub fn check(&mut self) -> Result<(), Vec<errors::Error>> {
        for unit_id in 0..self.units.len() {
            let decl = self.process_unit(unit_id);

            self.decls.push(decl);

            if self.error_module.unrecoverable() {
                self.error_module.dump(&mut self.units);
                return Err(self.error_module.errors());
            }
        }

        let mut printer = printer::Printer::new();

        for decl in self.decls.iter_mut() {
            let printer_node = printer.visit_decl(decl);

            printer_node.dump();
        }

        let mut validator = Validator::new(self.error_module.clone(), &mut self.decls);

        validator.check();

        println!("POST CHECK");

        for decl in self.decls.iter_mut() {
            let printer_node = printer.visit_decl(decl);

            printer_node.dump();
        }

        if self.error_module.unrecoverable() {
            self.error_module.dump(&mut self.units);
            return Err(self.error_module.errors());
        }

        Ok(())
    }

    pub fn run(&mut self, args: &[&str]) -> Result<Value, Vec<errors::Error>> {
        for unit_id in 0..self.units.len() {
            let decl = self.process_unit(unit_id);

            self.decls.push(decl);

            if self.error_module.unrecoverable() {
                self.error_module.dump(&mut self.units);
                return Err(self.error_module.errors());
            }
        }

        let mut validator = Validator::new(self.error_module.clone(), &mut self.decls);

        validator.check();

        if self.error_module.unrecoverable() {
            self.error_module.dump(&mut self.units);
            return Err(self.error_module.errors());
        }

        let main = validator.get_main();

        if main.is_none() {
            self.error_module
                .add_validator_error(ValidatorError::NoMainFunction, None);
            self.error_module.dump(&mut self.units);
            return Err(self.error_module.errors());
        }

        let mut interpreter = Interpreter::new(self.error_module.clone(), &self.decls);

        let result = interpreter.run(&main.unwrap(), args);

        match result {
            Ok(value) => Ok(value),
            Err((error, span)) => {
                self.error_module.add_runtime_error(error, span);
                self.error_module.dump(&mut self.units);
                Err(self.error_module.errors())
            }
        }
    }

    fn process_unit(&mut self, unit_id: UnitIdentifier) -> Decl {
        let unit = &mut self.units[unit_id];
        let unit_name = unit.name().to_string();

        let mut module_names = Path::new(&unit_name)
            .components()
            .filter_map(|c| match c {
                std::path::Component::Normal(s) => s.to_str().unwrap().split('.').next(),
                _ => None,
            })
            .rev();
        let mut lexer = lexer::Lexer::new(unit, &self.config, self.error_module.clone());
        let filtered = ParserTokenFilter::new(&mut lexer);

        let mut parser = parser::Parser::new(&self.config, filtered, self.error_module.clone());

        let decls = parser.parse();

        let mut decl = Decl::Module {
            visibility: Visibility::Public,
            name: Identifier {
                span: Span::new(unit_id, Location::new(1, 0, 1), Location::new(1, 0, 1)),
                name: module_names.next().unwrap().to_string(),
            },
            body: decls,
        };

        for module_name in module_names {
            decl = Decl::Module {
                visibility: Visibility::Public,
                name: Identifier {
                    span: Span::new(unit_id, Location::new(1, 0, 1), Location::new(1, 0, 1)),
                    name: module_name.to_string(),
                },
                body: vec![decl],
            };
        }

        decl
    }

    pub fn units(&self) -> &Units {
        &self.units
    }

    pub fn unit(&self, id: UnitIdentifier) -> &Unit {
        &self.units[id]
    }
}
