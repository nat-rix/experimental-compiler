use core::fmt::{Display, Formatter, Result as FmtResult};
use std::path::PathBuf;

use crate::parser::span::{SrcPosOwned, SrcSpanOwned};

#[derive(Debug, Clone)]
enum ExtraInfo<'a> {
    Pos(&'a SrcPosOwned),
    Span(&'a SrcSpanOwned),
}

trait GetExtraInfo: Sized {
    fn extra_info(&self) -> Option<ExtraInfo> {
        None
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

impl GetExtraInfo for InternalError {}

#[derive(Debug, Clone)]
pub enum TokenizeError {
    UnexpectedEof(SrcPosOwned),
    InvalidAsciiChar(u8, SrcPosOwned),
    UnexpectedAsciiChar(u8, SrcPosOwned),
    UnclosedBlockComment(SrcPosOwned),
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

impl GetExtraInfo for TokenizeError {
    fn extra_info(&self) -> Option<ExtraInfo> {
        match self {
            Self::UnexpectedEof(pos)
            | Self::UnclosedBlockComment(pos)
            | Self::InvalidAsciiChar(_, pos)
            | Self::UnexpectedAsciiChar(_, pos) => Some(ExtraInfo::Pos(pos)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        todo!()
    }
}

impl GetExtraInfo for ParseError {
    fn extra_info(&self) -> Option<ExtraInfo> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum SemanticError {}

impl Display for SemanticError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        todo!()
    }
}

impl GetExtraInfo for SemanticError {}

#[derive(Debug, Clone)]
pub enum Error {
    Internal(InternalError),
    Parse(ParseError),
    Tokenize(TokenizeError),
    Semantic(SemanticError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        if let Some(extra) = self.extra_info() {
            match extra {
                ExtraInfo::Pos(pos) => {
                    writeln!(f, "in {pos}")?;
                }
                ExtraInfo::Span(span) => {
                    writeln!(f, "in {}", span.start_pos())?;
                }
            }
        }
        match self {
            Self::Internal(err) => write!(f, "{err}"),
            Self::Parse(err) => write!(f, "{err}"),
            Self::Tokenize(err) => write!(f, "{err}"),
            Self::Semantic(err) => write!(f, "{err}"),
        }
    }
}

impl GetExtraInfo for Error {
    fn extra_info(&self) -> Option<ExtraInfo> {
        match self {
            Self::Internal(err) => err.extra_info(),
            Self::Parse(err) => err.extra_info(),
            Self::Tokenize(err) => err.extra_info(),
            Self::Semantic(err) => err.extra_info(),
        }
    }
}

impl Error {
    pub const fn code(&self) -> u8 {
        match self {
            Self::Internal(_) => 1,
            Self::Parse(_) | Self::Tokenize(_) => 42,
            Self::Semantic(_) => 7,
        }
    }

    pub fn fail(self) -> ! {
        eprintln!("{self}");
        std::process::exit(self.code().into());
    }
}
