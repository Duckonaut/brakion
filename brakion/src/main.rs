use std::io::stdin;

use brakion_core::interpret;
use clap::Parser;

#[derive(Debug, Clone, Parser)]
#[clap(
    version = "0.1.0",
    author = "Duckonaut",
    about = "The Brakion Language"
)]
struct Args {
    #[clap(help = "The file to interpret")]
    file: Option<String>,
}

fn main() {
    let args = Args::parse();

    if let Some(filepath) = args.file {
        let file = std::fs::File::open(filepath.clone()).expect("Could not open file");
        let reader = std::io::BufReader::new(file);
        interpret(filepath, reader);
    } else {
        let reader = std::io::BufReader::new(stdin());
        interpret("<stdin>".into(), reader);
    }
}
