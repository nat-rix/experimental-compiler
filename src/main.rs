pub mod error;
pub mod parser;

fn run_app() -> Result<(), error::Error> {
    let args = std::env::args_os().collect::<Vec<_>>();
    let [in_path, out_path] = match args.as_slice() {
        [_, a, b] => [a, b],
        [] | [_] | [_, _] => return Err(error::Error::Internal(error::InternalError::TooFewArgs)),
        [_, _, _, _, ..] => return Err(error::Error::Internal(error::InternalError::TooManyArgs)),
    };
    let content = std::fs::read(in_path).map_err(|err| {
        error::Error::Internal(error::InternalError::FileRead(in_path.into(), err.kind()))
    })?;
    let stream = parser::tokenize::TokenStream::new(in_path.as_ref(), &content);
    let tokens = stream
        .iter()
        .collect::<Result<Vec<_>, _>>()
        .map_err(error::Error::Tokenize)?;

    for token in tokens {
        println!("{token:?}");
    }
    Ok(())
}

fn main() {
    run_app().unwrap_or_else(|err| err.fail())
}
