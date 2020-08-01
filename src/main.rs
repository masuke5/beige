use std::path::PathBuf;
use std::str::FromStr;

use beige::{CompileOption, OutputType, Target};
use clap::Clap;

struct OutputTypeFromStr {
    inner: OutputType,
}

impl FromStr for OutputTypeFromStr {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ty = match s {
            "token" => OutputType::Token,
            "ast" => OutputType::AST,
            "ast2" => OutputType::TypedAST,
            "ir" => OutputType::IR,
            "asm" => OutputType::Assembly1,
            "asm2" => OutputType::Assembly2,
            "obj" => OutputType::ObjectFiles,
            s => return Err(format!("unknown dump type: `{}`", s)),
        };

        Ok(OutputTypeFromStr { inner: ty })
    }
}

struct TargetFromStr {
    inner: Target,
}

impl FromStr for TargetFromStr {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ty = match s {
            "x86_64" => Target::X86_64,
            "dbg" => Target::Debug,
            s => return Err(format!("unknown dump type: `{}`", s)),
        };

        Ok(TargetFromStr { inner: ty })
    }
}
#[derive(Clap)]
#[clap(author, about, version)]
struct Opts {
    #[clap(short, long)]
    dump: Option<OutputTypeFromStr>,
    #[clap(short, long)]
    target: Option<TargetFromStr>,
    #[clap(name = "DIRECTORY", parse(from_os_str))]
    directory: PathBuf,
    #[clap(short, long)]
    output: PathBuf,
}

fn main() {
    let opt = Opts::parse();
    let dump = opt.dump.map(|d| d.inner).unwrap_or(OutputType::Binary);
    let target = opt.target.map(|d| d.inner).unwrap_or(Target::X86_64);

    let option = CompileOption {
        directory: opt.directory,
        output: dump,
        out_dir: opt.output,
        target,
    };
    beige::compile(option);
}
