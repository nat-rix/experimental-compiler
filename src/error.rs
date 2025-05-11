use core::fmt::{Display, Formatter, Result as FmtResult};
use std::path::{Path, PathBuf};

use crate::parser::{
    ast::Ident,
    span::{SrcOffset, SrcSpan},
};

const PARSE_ERR_CODE: u8 = 42;
const SEMANTIC_ERR_CODE: u8 = 7;

#[derive(Debug, Clone)]
pub enum ExtraInfo {
    Offset(SrcOffset),
    SpanList(Vec<(String, SrcSpan)>),
    Eof,
}

impl ExtraInfo {
    pub fn new_span(span: SrcSpan) -> Self {
        Self::SpanList(vec![(String::new(), span)])
    }
}

fn get_line(content: &[u8], line: usize) -> &[u8] {
    content.split(|c| c == &b'\n').nth(line).unwrap_or(&[])
}

fn print_line(content: &[u8], line_nr: usize, col: usize) -> usize {
    let line = get_line(content, line_nr);
    let line_str = (line_nr + 1).to_string();
    eprintln!("{line_str} | {}", String::from_utf8_lossy(line));
    for _ in 0..line_str.len() + 1 {
        eprint!(" ");
    }
    eprint!("|");
    for _ in 0..col + 1 {
        eprint!(" ");
    }
    line.len() - col
}

fn print_span(span: &SrcSpan, path: &Path, content: &[u8], text: &str) {
    eprintln!("in {path:?}:{}", span.start);
    let rest = print_line(content, span.start.line, span.start.col);
    let count = if span.is_multiline() {
        for _ in 0..rest {
            eprint!("^");
        }
        eprintln!();
        if span.end.line != span.start.line + 1 {
            eprintln!("...");
        }
        print_line(content, span.end.line, 0);
        span.end.col + 1
    } else {
        span.len()
    };
    for _ in 0..count {
        eprint!("^");
    }
    if !text.is_empty() {
        eprint!(" {text}");
    }
    eprintln!();
}

pub trait GetExtraInfo: Error {
    fn extra_info(&self) -> Option<ExtraInfo>;

    fn fail_with(self, path: &Path, content: &[u8]) -> ! {
        if let Some(info) = self.extra_info() {
            match info {
                ExtraInfo::Offset(off) => {
                    eprintln!("in {path:?}:{off}");
                    print_line(content, off.line, off.col);
                    eprint!("^");
                }
                ExtraInfo::SpanList(spans) => {
                    for (text, span) in spans {
                        print_span(&span, path, content, &text);
                    }
                }
                ExtraInfo::Eof => eprintln!("at EOF"),
            }
        }
        self.fail()
    }
}

pub trait Error: Display + Sized {
    fn exit_code(&self) -> u8;

    fn fail(self) -> ! {
        eprintln!("error: {self}");
        std::process::exit(self.exit_code().into())
    }
}

#[derive(Debug, Clone)]
pub enum CodeGenError {
    StackOffsetOverflow,
    InvalidEspAccess,
}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::StackOffsetOverflow => write!(f, "stack offset overflows 32-bits"),
            Self::InvalidEspAccess => write!(
                f,
                "tried to do invalid operations with the esp registger (this error is a programming error and should never happen)"
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CliError {
    MissingInputFile,
    MissingOutputFile,
    TooManyInputFiles,
    EmptyShortArgument(char),
    EmptyLongArgument(String),
    UnknownShortArgument(char),
    UnknownLongArgument(String),
}

impl Display for CliError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::MissingInputFile => write!(f, "missing input file"),
            Self::MissingOutputFile => write!(f, "missing output file"),
            Self::TooManyInputFiles => write!(f, "more than one input file provided"),
            Self::EmptyShortArgument(arg) => {
                write!(f, "argument `-{}` missing value", arg.escape_default())
            }
            Self::EmptyLongArgument(arg) => {
                write!(f, "argument `--{}` missing value", arg.escape_default())
            }
            Self::UnknownShortArgument(arg) => {
                write!(f, "unknown argument `-{}`", arg.escape_default())
            }
            Self::UnknownLongArgument(arg) => {
                write!(f, "unknown argument `--{}`", arg.escape_default())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum InternalError {
    CliError(CliError),
    FileRead(PathBuf, std::io::ErrorKind),
    FileCreate(PathBuf, std::io::ErrorKind),
    FileWrite(PathBuf, std::io::ErrorKind),
    CodeGen(CodeGenError),
}

impl Display for InternalError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::CliError(err) => write!(f, "{err}"),
            Self::FileRead(path, kind) => write!(f, "failed to open file {path:?} ({kind})"),
            Self::FileCreate(path, kind) => write!(f, "failed to create file {path:?} ({kind})"),
            Self::FileWrite(path, kind) => write!(f, "failed to write to file {path:?} ({kind})"),
            Self::CodeGen(err) => write!(f, "code gen failed ({err})"),
        }
    }
}

impl Error for InternalError {
    fn exit_code(&self) -> u8 {
        1
    }
}

impl From<CodeGenError> for InternalError {
    fn from(value: CodeGenError) -> Self {
        Self::CodeGen(value)
    }
}

#[derive(Debug, Clone)]
pub enum TokenizeError {
    UnexpectedEof(SrcOffset),
    InvalidAsciiChar(u8, SrcOffset),
    UnexpectedAsciiChar(u8, SrcOffset),
    UnclosedBlockComment(SrcOffset),
    InvalidEmptyHexPrefix(SrcSpan),
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::UnexpectedEof(_) => write!(f, "unexpected eof"),
            Self::InvalidAsciiChar(c, _) => write!(f, "invalid ascii character `\\x{c:02x}`"),
            Self::UnexpectedAsciiChar(c, _) => write!(f, "unexpected ascii character `\\x{c:02x}`"),
            Self::UnclosedBlockComment(_) => write!(f, "unclosed block comment"),
            Self::InvalidEmptyHexPrefix(_) => write!(f, "invalid empty hex prefix"),
        }
    }
}

impl Error for TokenizeError {
    fn exit_code(&self) -> u8 {
        PARSE_ERR_CODE
    }
}

impl GetExtraInfo for TokenizeError {
    fn extra_info(&self) -> Option<ExtraInfo> {
        match self {
            Self::UnexpectedEof(pos)
            | Self::UnclosedBlockComment(pos)
            | Self::InvalidAsciiChar(_, pos)
            | Self::UnexpectedAsciiChar(_, pos) => Some(ExtraInfo::Offset(*pos)),
            Self::InvalidEmptyHexPrefix(span) => Some(ExtraInfo::new_span(*span)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    // TODO: this error has no expressiveness, split it into smaller subpieces
    TodoError(Option<SrcSpan>),
    TokensAfterMain(SrcSpan),
    Int(SrcSpan),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::TodoError(_) => write!(f, "todo parse error"),
            Self::Int(_) => write!(f, "integer parsing"),
            Self::TokensAfterMain(_) => {
                write!(f, "no tokens are allowed after the main subroutine")
            }
        }
    }
}

impl Error for ParseError {
    fn exit_code(&self) -> u8 {
        match self {
            Self::Int(_) => SEMANTIC_ERR_CODE,
            _ => PARSE_ERR_CODE,
        }
    }
}

impl GetExtraInfo for ParseError {
    fn extra_info(&self) -> Option<ExtraInfo> {
        match self {
            Self::TodoError(Some(span)) | Self::Int(span) => Some(ExtraInfo::new_span(*span)),
            Self::TodoError(None) => Some(ExtraInfo::Eof),
            Self::TokensAfterMain(span) => Some(ExtraInfo::new_span(*span)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AnyError {
    Tokenize(TokenizeError),
    Parse(ParseError),
}

impl From<TokenizeError> for AnyError {
    fn from(value: TokenizeError) -> Self {
        Self::Tokenize(value)
    }
}

impl From<ParseError> for AnyError {
    fn from(value: ParseError) -> Self {
        Self::Parse(value)
    }
}

impl Display for AnyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Tokenize(err) => write!(f, "{err}"),
            Self::Parse(err) => write!(f, "{err}"),
        }
    }
}

impl Error for AnyError {
    fn exit_code(&self) -> u8 {
        match self {
            Self::Tokenize(err) => err.exit_code(),
            Self::Parse(err) => err.exit_code(),
        }
    }
}

impl GetExtraInfo for AnyError {
    fn extra_info(&self) -> Option<ExtraInfo> {
        match self {
            Self::Tokenize(err) => err.extra_info(),
            Self::Parse(err) => err.extra_info(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum SemanticError<'a> {
    MissingReturn(SrcSpan),
    UndefinedVariable(Ident<'a>, SrcSpan),
    UnassignedVariable {
        ident: Ident<'a>,
        declaration: SrcSpan,
        usage: SrcSpan,
    },
    VariableRedefinition {
        ident: Ident<'a>,
        definition1: SrcSpan,
        definition2: SrcSpan,
    },
}

impl<'a> Display for SemanticError<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::MissingReturn(_) => write!(f, "missing return statement"),
            Self::UndefinedVariable(ident, _) => write!(f, "use of undefined variable {ident}"),
            Self::UnassignedVariable { ident, .. } => {
                write!(f, "use of variable {ident} with no assigned value")
            }
            Self::VariableRedefinition { ident, .. } => {
                write!(f, "redefinition of variable {ident}")
            }
        }
    }
}

impl<'a> GetExtraInfo for SemanticError<'a> {
    fn extra_info(&self) -> Option<ExtraInfo> {
        match self {
            Self::MissingReturn(span) => Some(ExtraInfo::new_span(*span)),
            Self::UndefinedVariable(_, span) => Some(ExtraInfo::new_span(*span)),
            Self::UnassignedVariable {
                ident,
                declaration,
                usage,
            } => Some(ExtraInfo::SpanList(vec![
                (format!("declaration of {ident}"), *declaration),
                (format!("invalid use of {ident}"), *usage),
            ])),
            Self::VariableRedefinition {
                ident,
                definition1,
                definition2,
            } => Some(ExtraInfo::SpanList(vec![
                (format!("previous definition of {ident}"), *definition1),
                (format!("invalid redefinition of {ident}"), *definition2),
            ])),
        }
    }
}

impl<'a> Error for SemanticError<'a> {
    fn exit_code(&self) -> u8 {
        SEMANTIC_ERR_CODE
    }
}
