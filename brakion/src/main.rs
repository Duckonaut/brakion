use std::io::stdin;

use brakion_core::interpret;

fn main() {
    let reader = std::io::BufReader::new(stdin());
    interpret("<stdin>".into(), reader);
}
