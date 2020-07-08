use std::env;
use std::process::exit;

use beige::{CompileOption, OutputType};

fn print_help() {
    println!("Beige Compiler v{}", env!("CARGO_PKG_VERSION"));
    println!("usage: beige <directory> [--dump <type>]");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_help();
        exit(1);
    }

    let directory = &args[1];
    let output = match args.get(2).map(|s| s.as_ref()) {
        Some("token") => OutputType::Token,
        Some("ast") => OutputType::AST,
        Some("ast2") => OutputType::TypedAST,
        Some("ir") => OutputType::IR,
        Some(s) => {
            eprintln!("Unknown dump type: `{}`", s);
            exit(1);
        }
        None => OutputType::Assembly,
    };

    let option = CompileOption::new(directory).output(output);
    beige::compile(option);
}
