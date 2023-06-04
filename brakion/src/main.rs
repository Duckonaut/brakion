use std::{io::Read, path::PathBuf};

use brakion_core::{Brakion, interpreter::value::Value};
use clap::{Parser, ValueEnum};
use colored::Colorize;

#[derive(Debug, Clone, Parser)]
#[clap(
    version = "0.1.0",
    author = "Duckonaut",
    about = "The Brakion Language"
)]
struct Args {
    /// The mode to run in
    #[clap(value_enum, help = "The mode to run in")]
    mode: Mode,
    #[clap(help = "The files to run")]
    files: Vec<PathBuf>,
    #[clap(
        allow_hyphen_values = true,
        last = true,
        help = "The arguments to pass to the program"
    )]
    extra_args: Vec<String>,
}

#[derive(Copy, Clone, PartialEq, Eq, ValueEnum, Debug)]
enum Mode {
    Check,
    Run,
}

fn main() {
    let args = Args::parse();
    let config = brakion_core::Config::default();

    let mut brakion = Brakion::new(config);

    for filepath in args.files {
        if filepath.to_str().unwrap() == "-" {
            let reader = std::io::stdin();
            let mut buffer = String::new();
            reader.lock().read_to_string(&mut buffer).unwrap();

            brakion.add_unit("stdin".to_string(), std::io::Cursor::new(buffer));
        } else {
            let file = std::fs::File::open(filepath.clone()).expect("Could not open file");

            brakion.add_unit(filepath.to_str().unwrap().to_string(), file);
        }
    }

    match args.mode {
        Mode::Check => {
            let errors = brakion.check();

            if let Err(errors) = errors {
                if errors.len() == 1 {
                    println!("{} {}", errors.len(), "error found!".red().bold());
                } else {
                    println!("{} {}", errors.len(), "errors found!".red().bold());
                }
            } else {
                println!("{}", "No errors found".green());
            }
        }
        Mode::Run => {
            let extra_args = args
                .extra_args
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>();
            let result = brakion.run(&extra_args);

            match result {
                Err(errors) => {
                    if errors.len() == 1 {
                        println!("{} {}", errors.len(), "error found!".red().bold());
                    } else {
                        println!("{} {}", errors.len(), "errors found!".red().bold());
                    }
                }
                Ok(Value::I32(i)) => {
                    std::process::exit(i);
                }
                Ok(_) => {
                    unreachable!();
                }
            }
        }
    }
}
