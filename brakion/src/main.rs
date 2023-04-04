use std::{io::stdin, path::PathBuf};

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
    file: PathBuf,
}

fn main() {
    let args = Args::parse();
    let config = brakion_core::Config::default();

    let filepath = args.file;

    let file = std::fs::File::open(filepath.clone()).expect("Could not open file");
    let reader = std::io::BufReader::new(file);
    interpret(filepath.as_os_str().to_string_lossy().to_string(), reader, config);
}
