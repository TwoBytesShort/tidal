use std::env;
use std::fs;

mod evaluator;
mod parser;
mod scanner;

use crate::parser::parse;
use crate::scanner::scan_tokens;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        run_file(&args[1]);
    } else {
        println!("Usage: tidal [script]");
        return;
    }
}

fn run_file(path: &String) {
    let source = fs::read_to_string(path).expect("Unable to read file");

    run(source);
}

fn run(source: String) {
    match scan_tokens(source) {
        Ok(tokens) => match parse(&tokens) {
            Ok(exp) => {
                println!("{}", exp);

                match exp.evaluate() {
                    Ok(value) => println!("{}", value),
                    Err(e) => println!("{}", e),
                }
            }
            Err(e) => {
                println!("{}", e)
            }
        },
        Err(e) => {
            println!("{}", e)
        }
    }
}
