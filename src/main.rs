use std::path::PathBuf;
use std::str::FromStr;

use beige::{CompileOption, OutputType};
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
            "asm" => OutputType::Assembly,
            "obj" => OutputType::ObjectFiles,
            s => return Err(format!("unknown dump type: `{}`", s)),
        };

        Ok(OutputTypeFromStr { inner: ty })
    }
}

#[derive(Clap)]
#[clap(author, about, version)]
struct Opts {
    #[clap(short, long)]
    dump: Option<OutputTypeFromStr>,
    #[clap(name = "DIRECTORY", parse(from_os_str))]
    directory: PathBuf,
    #[clap(short, long)]
    output: PathBuf,
}

fn main() {
    let opt = Opts::parse();
    let dump = opt.dump.map(|d| d.inner).unwrap_or(OutputType::Binary);

    let option = CompileOption::new(opt.directory, opt.output).output(dump);
    beige::compile(option);
}
