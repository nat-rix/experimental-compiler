pub mod cli;
pub mod elf;
pub mod error;
pub mod ir;
pub mod parse;
pub mod span;

use cli::CompilerFlags;
use error::{ErrorContext, Fail, InternalError};
use std::path::PathBuf;

fn compile(
    in_path: &PathBuf,
    _out_path: &PathBuf,
    flags: &CompilerFlags,
) -> Result<(), InternalError> {
    let content = std::fs::read(in_path)
        .map_err(|err| InternalError::FileRead(in_path.clone(), err.kind()))?;
    let ectx = ErrorContext::new(in_path, &content);

    let stream = parse::tokenize::TokenIter::new(&content);

    let ast = ectx.unwrap(parse::parse_ast(stream));
    ectx.unwrap(parse::ana::check_full(&ast));

    let ir = ectx.unwrap(ir::from_ast::generete_ir_from_ast(&ast));

    if flags.debug_ir {
        println!("{ir}");
    }

    Ok(())
}

fn main_cli() -> Result<(), InternalError> {
    let args = cli::Args::from_os_args(std::env::args_os())?;

    if !args.skip_compiler {
        compile(args.get_input()?, args.get_output()?, &args.features)?;
    }

    Ok(())
}

fn main() {
    main_cli().unwrap_or_else(|err| err.fail())
}
