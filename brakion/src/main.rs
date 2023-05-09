use std::{io::Read, path::PathBuf};

use brakion_core::Brakion;
use clap::Parser;
use colored::Colorize;

#[derive(Debug, Clone, Parser)]
#[clap(
    version = "0.1.0",
    author = "Duckonaut",
    about = "The Brakion Language"
)]
struct Args {
    #[clap(help = "The file to interpret")]
    file: PathBuf,
}

fn main() {
    let args = Args::parse();
    let config = brakion_core::Config::default();

    let filepath = args.file;

    if filepath.to_str().unwrap() == "-" {
        let reader = std::io::stdin();
        let mut buffer = String::new();
        reader.lock().read_to_string(&mut buffer).unwrap();

        let mut brakion = Brakion::new(config);

        brakion.add_unit("<stdin>".to_string(), std::io::Cursor::new(buffer));

        let result = brakion.check();

        match result {
            Ok(_) => println!("{}", "No errors found".green().bold()),
            Err(errors) => {
                println!("{} errors found", errors.len().to_string().red().bold());
            }
        }
    } else {
        let file = std::fs::File::open(filepath.clone()).expect("Could not open file");
        let mut brakion = Brakion::new(config);

        brakion.add_unit(filepath.to_str().unwrap().to_string(), file);

        let result = brakion.check();

        match result {
            Ok(_) => println!("{}", "No errors found".green().bold()),
            Err(errors) => {
                println!("{} errors found", errors.len().to_string().red().bold());
            }
        }
    }
}
