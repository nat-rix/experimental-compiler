use core::fmt::{Display, Formatter, Result as FmtResult};
use std::path::PathBuf;

use crate::parser::span::{SrcOffset, SrcSpan};

#[derive(Debug, Clone)]
pub enum ExtraInfo {
    Offset(SrcOffset),
    Span(SrcSpan),
    Eof,
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

pub trait GetExtraInfo: Error {
    fn extra_info(&self) -> Option<ExtraInfo>;

    fn fail_with(self, content: &[u8]) -> ! {
        if let Some(info) = self.extra_info() {
            match info {
                ExtraInfo::Offset(off) => {
                    eprintln!("in {off}");
                    print_line(content, off.line, off.col);
                    eprint!("^");
                }
                ExtraInfo::Span(span) => {
                    eprintln!("in {}", span.start);
                    let rest = print_line(content, span.start.line, span.start.col);
                    let count = if span.is_multiline() {
                        for _ in 0..rest {
                            eprint!("^");
                        }
                        eprintln!();
                        eprintln!("...");
                        print_line(content, span.end.line, 0);
                        span.end.col
                    } else {
                        span.len()
                    };
                    for _ in 0..count {
                        eprint!("^");
                    }
                    eprintln!();
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
        eprintln!("{self}");
        std::process::exit(self.exit_code().into())
    }
}

#[derive(Debug, Clone)]
pub enum InternalError {
    TooFewArgs,
    TooManyArgs,
    FileRead(PathBuf, std::io::ErrorKind),
}

impl Display for InternalError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::TooFewArgs => write!(f, "too few command line arguments"),
            Self::TooManyArgs => write!(f, "too many command line arguments"),
            Self::FileRead(path, kind) => write!(f, "failed to open file {path:?} ({kind})"),
        }
    }
}

impl Error for InternalError {
    fn exit_code(&self) -> u8 {
        1
    }
}

#[derive(Debug, Clone)]
pub enum TokenizeError {
    UnexpectedEof(SrcOffset),
    InvalidAsciiChar(u8, SrcOffset),
    UnexpectedAsciiChar(u8, SrcOffset),
    UnclosedBlockComment(SrcOffset),
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::UnexpectedEof(_) => write!(f, "unexpected eof"),
            Self::InvalidAsciiChar(c, _) => write!(f, "invalid ascii character `\\x{c:02x}`"),
            Self::UnexpectedAsciiChar(c, _) => write!(f, "unexpected ascii character `\\x{c:02x}`"),
            Self::UnclosedBlockComment(_) => write!(f, "unclosed block comment"),
        }
    }
}

impl Error for TokenizeError {
    fn exit_code(&self) -> u8 {
        42
    }
}

impl GetExtraInfo for TokenizeError {
    fn extra_info(&self) -> Option<ExtraInfo> {
        match self {
            Self::UnexpectedEof(pos)
            | Self::UnclosedBlockComment(pos)
            | Self::InvalidAsciiChar(_, pos)
            | Self::UnexpectedAsciiChar(_, pos) => Some(ExtraInfo::Offset(*pos)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    // TODO: this error has no expressiveness, split it into smaller subpieces
    TodoError(Option<SrcSpan>),
    Int(SrcSpan),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::TodoError(_) => write!(f, "todo parse error"),
            Self::Int(_) => write!(f, "integer parsing"),
        }
    }
}

impl Error for ParseError {
    fn exit_code(&self) -> u8 {
        42
    }
}

impl GetExtraInfo for ParseError {
    fn extra_info(&self) -> Option<ExtraInfo> {
        match self {
            Self::TodoError(Some(span)) | Self::Int(span) => Some(ExtraInfo::Span(*span)),
            Self::TodoError(None) => Some(ExtraInfo::Eof),
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
pub enum SemanticError {}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        todo!()
    }
}

impl Error for SemanticError {
    fn exit_code(&self) -> u8 {
        7
    }
}
