#![feature(box_patterns, box_syntax)]

#[macro_use]
mod error;
mod dump;
mod graph;
mod id;
mod scope_map;
mod span;

mod ast;
mod ir;
mod token;
mod ty;

mod codegen;
mod color;
mod dbgcodegen;
mod gen_ir;
mod lexer;
mod liveness;
mod parser;
mod regalloc;
mod typing;
mod x64codegen;

use codegen::CodeGen;
use id::IdMap;
use ir::Temp;
use log::debug;
use rustc_hash::{FxHashMap, FxHashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const MAIN_MODULE_FILE: &str = "main.beige";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputType {
    Token,
    AST,
    TypedAST,
    IR,
    Assembly1,
    Assembly2,
    ObjectFiles,
    Binary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Target {
    X86_64,
    Debug, // for debugging register allocation
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompileOption {
    pub directory: PathBuf,
    pub output: OutputType,
    pub out_dir: PathBuf,
    pub target: Target,
}

fn registers(target: Target) -> (FxHashSet<Temp>, FxHashMap<Temp, u32>) {
    match target {
        Target::X86_64 => {
            let registers = x64codegen::ALL_REGS.iter().copied().collect();
            let priority = x64codegen::ALL_REGS
                .iter()
                .copied()
                .zip(x64codegen::REG_PRIORITY.iter().copied())
                .collect();
            (registers, priority)
        }
        Target::Debug => {
            let registers = dbgcodegen::REGISTERS.iter().copied().collect();
            let priority = dbgcodegen::REGISTERS
                .iter()
                .copied()
                .zip(dbgcodegen::REG_PRIORITY.iter().copied())
                .collect();
            (registers, priority)
        }
    }
}

fn assemble(asm_files: &[PathBuf]) -> (bool, Vec<PathBuf>) {
    let mut out_paths = Vec::with_capacity(asm_files.len());
    let mut failed = false;

    for in_file in asm_files {
        let mut out_path = in_file.clone();
        out_path.set_extension("o");
        out_paths.push(out_path.clone());

        // nasm -f elf64 -o {out_file} {in_file}
        let output = Command::new("nasm")
            .arg("-f")
            .arg("elf64")
            .arg("-o")
            .arg(out_path)
            .arg(in_file)
            .spawn();

        let mut output = match output {
            Ok(output) => output,
            Err(err) => {
                println!("Unable to execute nasm: {}", err);
                failed = true;
                continue;
            }
        };

        let status = match output.wait() {
            Ok(output) => output,
            Err(err) => {
                println!("Unable to execute nasm: {}", err);
                failed = true;
                continue;
            }
        };

        if !status.success() {
            println!("nasm error: {}", status);
        }
    }

    (failed, out_paths)
}

fn link(obj_files: &[PathBuf], out_path: &Path) {
    // ld -o {out_path} -dynamic-linker /lib64/ld-linux-x86-64.so.2 /usr/lib/crt1.o /usr/lib/crti.o /usr/lib64/crtn.o -lc {obj_files...}
    let mut command = Command::new("ld");
    let mut command = command
        .arg("-o")
        .arg(out_path)
        .arg("-dynamic-linker")
        .arg("/lib64/ld-linux-x86-64.so.2")
        .arg("/usr/lib/crt1.o")
        .arg("/usr/lib/crti.o")
        .arg("/usr/lib/crtn.o")
        .arg("-lc");

    for obj_file in obj_files {
        command = command.arg(obj_file);
    }

    let mut output = match command.spawn() {
        Ok(output) => output,
        Err(err) => {
            println!("Unable to execute ld: {}", err);
            return;
        }
    };

    let status = match output.wait() {
        Ok(output) => output,
        Err(err) => {
            println!("Unable to execute ld: {}", err);
            return;
        }
    };

    if !status.success() {
        println!("ld error: {}", status);
    }
}

pub fn compile(option: CompileOption) {
    env_logger::init();

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
    debug!("Lex `{}`", file);
    let tokens = lexer::lex(file, &code);

    if option.output == OutputType::Token {
        for token in &tokens {
            println!("{:<2 } {:#} {}", token.level, token.kind, token.span);
        }
        return;
    }

    // Parse
    debug!("Parse `{}`", file);
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
    debug!("Infer type `{}`", file);
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
    debug!("Generate IR `{}`", file);
    let ir_module = gen_ir::gen_ir(typed_module.unwrap());

    if option.output == OutputType::IR {
        ir::dump_module(&ir_module);
        return;
    }

    // Select instructions
    debug!("Select instructions `{}`", file);

    let mut codegen: Box<dyn CodeGen> = match option.target {
        Target::X86_64 => Box::new(x64codegen::X64CodeGen::new()),
        Target::Debug => Box::new(dbgcodegen::DebugCodeGen::new()),
    };

    let mut module = codegen.codegen(ir_module);

    if option.output == OutputType::Assembly1 {
        println!("{}", codegen.gen_all(module));
        return;
    }

    // Register allocation
    debug!("Alloc registers `{}`", file);

    let mut functions = Vec::with_capacity(module.functions.len());
    let (registers, priority) = registers(option.target);

    for func in module.functions {
        functions.push(regalloc::regalloc(
            func,
            registers.clone(),
            priority.clone(),
        ));
    }

    module.functions = functions;

    // Generate code
    debug!("Generate code `{}`", file);
    let code = codegen.gen_all(module);
    if option.output == OutputType::Assembly2 {
        println!("{}", code);
        return;
    }

    // Assemble
    debug!("Assemble `{}`", file);
    fs::create_dir_all(&option.out_dir).unwrap();

    let mut out_path = option.out_dir.join(main_file.file_name().unwrap());
    out_path.set_extension("asm");

    if let Err(err) = fs::write(&out_path, code) {
        println!("Failed to write assembly: {}", err);
        return;
    }

    let (failed, obj_files) = assemble(&[out_path]);
    if failed {
        return;
    }

    if option.output == OutputType::ObjectFiles {
        return;
    }

    // Link
    debug!("Link");
    let binary_path = option.out_dir.join("main");
    link(&obj_files, &binary_path);
}
