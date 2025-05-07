pub mod aasm;
pub mod error;
pub mod parser;

use aasm::ssa::Lifetimes;
use error::{Error, GetExtraInfo};

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

fn main() {
    let args = std::env::args_os().collect::<Vec<_>>();
    let [in_path, out_path] = match args.as_slice() {
        [_, a, b] => [a, b],
        [] | [_] | [_, _] => error::InternalError::TooFewArgs.fail(),
        [_, _, _, _, ..] => error::InternalError::TooManyArgs.fail(),
    };

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
    let ast = failer.unwrap(<parser::ast::Ast as parser::ast::Parse>::parse(&mut stream));

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
    let mut lifetimes = Lifetimes::generate(&ssa);
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
}
