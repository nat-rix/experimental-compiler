pub mod cli;
pub mod elf;
pub mod error;
pub mod ir;
pub mod parse;
pub mod span;
pub mod x86_64;

use cli::CompilerFlags;
use error::{ErrorContext, Fail, InternalError};
use std::path::PathBuf;

fn compile(
    in_path: &PathBuf,
    out_path: &PathBuf,
    flags: &CompilerFlags,
) -> Result<(), InternalError> {
    let content = std::fs::read(in_path)
        .map_err(|err| InternalError::FileRead(in_path.clone(), err.kind()))?;
    let ectx = ErrorContext::new(in_path, &content);

    let stream = parse::tokenize::TokenIter::new(&content);

    let ast = ectx.unwrap(parse::parse_ast(stream));
    ectx.unwrap(parse::ana::check_full(&ast));

    let mut ir = ectx.unwrap(ir::from_ast::generete_ir_from_ast(&ast));

    let precolors = x86_64::precolor::precolorize(&mut ir);

    println!("{precolors:?}");

    ir::liveness::analysis(&mut ir);

    if flags.debug_ir {
        println!("{ir}");
    }

    for (i, node) in ir.inference.vertices.iter().enumerate() {
        println!("{i:3} : {:?}", node.color);
    }

    let mut reg_map = x86_64::regs::ColorToRegMap::from(precolors);
    reg_map.populate_from_tree(&ir);

    let mut codegen = x86_64::codegen::Codegen::new(elf::PROG_ADDR);
    codegen.gen_from_tree(&ir, &reg_map);
    codegen.fix_labels();

    let mut elf = ectx.unwrap(elf::ElfFile::create(out_path));
    ectx.unwrap(elf::write_code(&mut elf, codegen.code()));

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
