#[macro_use]
mod error;
mod ast;
mod dump;
mod id;
mod span;
mod token;

mod lexer;
mod parser;

use std::fs;
use std::path::Path;

use id::IdMap;

pub fn compile(directory: impl AsRef<Path>) {
    let directory = directory.as_ref();

    let main_file = directory.join("main.beige");
    let file = IdMap::new_id(&format!("{}", main_file.display()));
    let code = fs::read_to_string(&main_file).unwrap();

    // Lex
    let tokens = lexer::lex(file, &code);
    // for token in &tokens {
    //     println!("{:<2 } {:#} {}", token.level, token.kind, token.span);
    // }

    // Parse
    let module = parser::parse(tokens, file);
    if let Some(module) = module {
        ast::dump_module(&module, 0);
    }
}
