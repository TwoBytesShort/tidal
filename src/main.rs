use std::env;
use std::fs;

mod evaluator;
mod parser;
mod scanner;

use crate::parser::parse;
use crate::scanner::scan_tokens;
use crate::evaluator::Environment;

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
            Ok(statements) => {
                let mut env = Environment::new();

                for stmt in statements {
                    //println!("{}", stmt);

                    match stmt.evaluate(&mut env) {
                        Err(e) => {
                            println!("{}", e);
                            return;
                        }
                        Ok(_) => (),
                    }
                }

                println!("Exited...");
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
