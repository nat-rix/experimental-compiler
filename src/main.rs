pub mod aasm;
pub mod cli;
pub mod elf;
pub mod error;
pub mod parser;
pub mod x64;

use aasm::ssa::Lifetimes;
use error::{Error, GetExtraInfo, InternalError};

use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct CompilerFlags {
    pub constant_propagation: bool,
}

impl Default for CompilerFlags {
    fn default() -> Self {
        Self {
            constant_propagation: true,
        }
    }
}

impl CompilerFlags {
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&'static str, &mut bool)> {
        [("constant-propagation", &mut self.constant_propagation)].into_iter()
    }
}

struct Failer<'a> {
    content: &'a [u8],
    path: &'a std::path::Path,
}

impl<'a> Failer<'a> {
    fn get<E: GetExtraInfo, T>(&self) -> impl FnOnce(E) -> T {
        |err: E| err.fail_with(self.path, self.content)
    }

    fn unwrap<E: GetExtraInfo, T>(&self, res: Result<T, E>) -> T {
        res.unwrap_or_else(self.get())
    }
}

fn compile(in_path: &PathBuf, out_path: &PathBuf) {
    // read file
    let content = std::fs::read(in_path)
        .map_err(|err| error::InternalError::FileRead(in_path.into(), err.kind()))
        .unwrap_or_else(|err| err.fail());
    let failer = Failer {
        content: &content,
        path: in_path.as_ref(),
    };

    // create token stream
    let mut stream = parser::tokenize::TokenStream::new(&content);

    // parse AST
    let ast = failer.unwrap(parser::ast::parse_program(&mut stream));

    // abstract assembly generation
    let code_gen = failer.unwrap(aasm::CodeGen::from_ast(&ast));

    for instr in code_gen.code() {
        println!("{instr:?}");
    }
    println!("---");

    // put into SSA form
    let mut ssa = code_gen.into_ssa();

    for instr in ssa.code() {
        println!("{instr:?}");
    }
    println!("---");

    // generate lifetimes
    let mut lifetimes = Lifetimes::from_block(&ssa);
    // colorize lifetimes
    let color_count = lifetimes.colorize();

    for (i, lt) in lifetimes.as_slice().iter().enumerate() {
        println!("{i:2}: {lt:?}");
    }
    println!("---");

    ssa.rename_from_colors(&lifetimes, color_count);

    for instr in ssa.code() {
        println!("{instr:?}");
    }
    println!("---");

    // generate x86-64 code
    let mut code_gen = x64::CodeGen::default();
    code_gen
        .generate(&ssa)
        .unwrap_or_else(|err| InternalError::CodeGen(err).fail());
    let mut x64_code = vec![];
    code_gen.encode(&mut x64_code);

    // write elf file
    let mut elf_file = elf::ElfFile::create(out_path).unwrap_or_else(|err| err.fail());
    elf::write_code(&mut elf_file, &x64_code).unwrap_or_else(|err| err.fail());
}

fn main_cli() -> Result<(), error::CliError> {
    let args = cli::Args::from_os_args(std::env::args_os())?;

    if !args.skip_compiler {
        compile(args.get_input()?, args.get_output()?);
    }

    Ok(())
}

fn main() {
    if let Err(err) = main_cli() {
        InternalError::CliError(err).fail()
    }
}
