pub mod error;
pub mod parser;

use error::{Error, GetExtraInfo};

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

    // create token stream
    let mut stream = parser::tokenize::TokenStream::new(&content);

    // parse AST
    let ast = <parser::ast::Ast as parser::ast::Parse>::parse(&mut stream)
        .unwrap_or_else(|err| err.fail_with(&content));

    println!("{:#?}", ast.val);
}
