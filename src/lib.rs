#![feature(box_patterns)]

#[macro_use]
mod error;
mod dump;
mod id;
mod scope_map;
mod span;

mod ast;
mod ir;
mod token;
mod ty;

mod gen_ir;
mod lexer;
mod parser;
mod typing;

use std::fs;
use std::path::{Path, PathBuf};

use id::IdMap;

const MAIN_MODULE_FILE: &str = "main.beige";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputType {
    Token,
    AST,
    TypedAST,
    IR,
    Assembly,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompileOption {
    directory: PathBuf,
    output: OutputType,
}

impl CompileOption {
    pub fn new(directory: impl AsRef<Path>) -> Self {
        Self {
            directory: directory.as_ref().to_path_buf(),
            output: OutputType::Assembly,
        }
    }

    pub fn output(mut self, output: OutputType) -> Self {
        self.output = output;
        self
    }
}

pub fn compile(option: CompileOption) {
    let dump_option = ast::DumpOption {
        show_type: true,
        show_span: true,
    };

    let main_file = option.directory.join(MAIN_MODULE_FILE);
    let file = IdMap::new_id(&format!("{}", main_file.display()));

    let code = match fs::read_to_string(&main_file) {
        Ok(code) => code,
        Err(err) => {
            eprintln!("Unable to read file `main.beige`: {}", err);
            return;
        }
    };

    // Lex
    let tokens = lexer::lex(file, &code);

    if option.output == OutputType::Token {
        for token in &tokens {
            println!("{:<2 } {:#} {}", token.level, token.kind, token.span);
        }
        return;
    }

    // Parse
    let module = parser::parse(tokens, file);

    if option.output == OutputType::AST {
        if let Some(module) = module {
            ast::dump_module(&module, 0, &dump_option);
        }
        return;
    }

    if module.is_none() {
        return;
    }

    // Type
    let typed_module = typing::infer_module(module.unwrap());

    if option.output == OutputType::TypedAST {
        if let Some(typed_module) = typed_module {
            ast::dump_module(&typed_module, 0, &dump_option);
        }
        return;
    }

    if error::ErrorList::has_error() {
        return;
    }

    // Generate IR
    let ir_module = gen_ir::gen_ir(typed_module.unwrap());

    if option.output == OutputType::IR {
        if let Some(ir_module) = ir_module {
            ir::dump_module(&ir_module);
        }
        return;
    }
}
