use std::env;
use std::fs;

mod scanner;
mod parser;

use crate::scanner::scan_tokens;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        run_file(&args[1]);
    }
    else
    {
        println!("Usage: tidal [script]");
        return
    }
}

fn run_file(path: &String) {
    let source = fs::read_to_string(path).expect("Unable to read file");

    run(source);
}

fn run(source: String) {
    match scan_tokens(source)
    {
        Ok(tokens) => {
            for token in tokens {
                println!("{}", token);
            }
        },
        Err(e) => {
            println!("{}", e)
        }
    }
    
}
