pub mod aasm;
pub mod cli;
pub mod elf;
pub mod error;
pub mod parser;
pub mod x64;

use aasm::{precolorizer::Precolorizer, ssa::Lifetimes};
use error::{Error, GetExtraInfo, InternalError};

use std::path::PathBuf;

macro_rules! build_compiler_flags {
    ($($f:ident: $n:literal = $e:literal),* $(,)?) => {
        #[derive(Debug, Clone)]
        pub struct CompilerFlags { $(pub $f: bool),* }

        impl Default for CompilerFlags {
            fn default() -> Self {
                Self { $( $f: $e ),* }
            }
        }

        impl CompilerFlags {
            pub fn iter_mut(&mut self) -> impl Iterator<Item = (&'static str, &mut bool)> {
                [ $(( $n, &mut self.$f )),* ].into_iter()
            }
        }
    };
}

build_compiler_flags! {
    constant_propagation: "constant-propagation" = true,
    compact_registers: "compact-registers" = true,
    precolorize_registers: "precolorize-registers" = true,
    eliminate_unused_calculations: "eliminate-unused-calculations" = true,
    eliminate_self_moves: "eliminate-self-moves" = true,
    coalescing: "coalescing" = true,
    debug_ast: "debug-ast" = false,
    debug_ir: "debug-ir" = false,
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

fn compile(in_path: &PathBuf, out_path: &PathBuf, flags: &CompilerFlags) {
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

    if flags.debug_ast {
        println!("AST:");
        println!("  statements:");
        for stmt in ast.iter() {
            println!("    {stmt:?}");
        }
    }

    // abstract assembly generation
    let code_gen = failer.unwrap(aasm::CodeGen::from_ast(&ast));

    // put into SSA form
    let mut ssa = code_gen.into_ssa();

    if flags.constant_propagation {
        ssa.constant_propagation();
    }
    if flags.eliminate_unused_calculations {
        ssa.eliminate_unused_calculations();
    }

    let mut precolorizer = flags.precolorize_registers.then(Precolorizer::default);
    if let Some(precolorizer) = &mut precolorizer {
        precolorizer.make_block_precolorizable(&mut ssa);
    }

    if flags.compact_registers {
        // generate lifetimes
        let mut lifetimes = Lifetimes::from_block(&ssa);
        // precolorize lifetimes
        if let Some(precolorizer) = &mut precolorizer {
            precolorizer.precolorize(&ssa, &mut lifetimes);
        }
        // colorize lifetimes
        let color_count = lifetimes.colorize();
        // register coalescing
        if flags.coalescing {
            lifetimes.coalesc(&ssa);
        }
        // rename registers according to calculated colors
        ssa.rename_from_colors(&lifetimes, color_count);
    }

    if flags.eliminate_self_moves {
        ssa.eliminate_self_moves();
    }

    if flags.debug_ir {
        println!("IR:");
        for instr in ssa.code() {
            println!("  {instr:?}");
        }
    }

    // generate x86-64 code
    let mut code_gen = x64::CodeGen::default();
    code_gen
        .generate(&ssa, precolorizer)
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
        compile(args.get_input()?, args.get_output()?, &args.features);
    }

    Ok(())
}

fn main() {
    if let Err(err) = main_cli() {
        InternalError::CliError(err).fail()
    }
}
