use core::fmt::{Display, Formatter, Result as FmtResult};
use std::path::{Path, PathBuf};

use crate::{
    parse::{
        AsgnOp, Op1, Op2, Type, TypeName,
        tokenize::{Ident, Token},
    },
    span::{Pos, Span, Spanned},
};

const PARSE_CODE: i32 = 42;
const SEM_CODE: i32 = 7;

struct SpanAnnotationSingleline<'a> {
    content: &'a [u8],
    line_str: String,
    line: usize,
    col_start: usize,
    col_end: Option<usize>,
    comment: &'a str,
}

impl<'a> Display for SpanAnnotationSingleline<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let line = self.content.split(|b| b == &b'\n').nth(self.line).unwrap();
        write!(f, "{} | ", self.line_str)?;
        for byte in line {
            write!(f, "{}", char::from(*byte))?;
        }
        writeln!(f)?;
        for _ in 0..self.line_str.len() + 1 {
            write!(f, " ")?;
        }
        write!(f, "|")?;
        for _ in 0..self.col_start + 1 {
            write!(f, " ")?;
        }
        let count = if let Some(end) = self.col_end {
            end - self.col_start + 1
        } else {
            line.len() - self.col_start
        };
        for _ in 0..count {
            write!(f, "^")?;
        }
        if !self.comment.is_empty() {
            write!(f, " {}", self.comment)?;
        }
        Ok(())
    }
}

struct SpanAnnotation<'a> {
    path: &'a Path,
    content: &'a [u8],
    ann: Annotation,
}

impl<'a> Display for SpanAnnotation<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let span = self.ann.span;
        let line = format!("{}", span.start.line + 1);
        for _ in 0..line.len() {
            write!(f, " ")?;
        }
        writeln!(
            f,
            "--> {}:{line}:{}",
            self.path.display(),
            span.start.col + 1
        )?;
        let mut ann = SpanAnnotationSingleline {
            content: self.content,
            line_str: line,
            line: span.start.line,
            col_start: span.start.col,
            col_end: Some(span.end.col),
            comment: &self.ann.comment,
        };
        if span.is_singleline() {
            return writeln!(f, "{ann}");
        }
        ann.col_end = None;
        writeln!(f, "{ann}")?;
        ann.line = span.end.line;
        ann.line_str = format!("{}", ann.line + 1);
        ann.col_start = 0;
        ann.col_end = Some(span.end.col);
        ann.comment = "";
        writeln!(f, "{ann}")
    }
}

pub trait Fail: Display + Sized {
    fn code(&self) -> i32 {
        1
    }
    fn annotations(&self) -> Vec<Annotation> {
        vec![]
    }
    fn fail(self) -> ! {
        eprintln!("error: {self}");
        std::process::exit(self.code())
    }
    fn fail_with_annotation(self, path: &Path, content: &[u8]) -> ! {
        for ann in self.annotations() {
            eprintln!("{}", SpanAnnotation { path, content, ann });
        }
        self.fail()
    }
}

pub struct ErrorContext<'path, 'content> {
    path: &'path Path,
    content: &'content [u8],
}

impl<'path, 'content> ErrorContext<'path, 'content> {
    pub fn new(path: &'path Path, content: &'content [u8]) -> Self {
        Self { path, content }
    }
    pub fn unwrap<T, E: Fail>(&self, val: Result<T, E>) -> T {
        val.unwrap_or_else(|err| err.fail_with_annotation(self.path, self.content))
    }
}

#[derive(Debug, Clone)]
pub enum CliError {
    UnknownFeature(String),
    MissingInputFile,
    MissingOutputFile,
    TooManyInputFiles,
    EmptyShortArgument(char),
    EmptyLongArgument(String),
    UnknownShortArgument(char),
    UnknownLongArgument(String),
}

impl Fail for CliError {}

impl Display for CliError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::UnknownFeature(feat) => write!(f, "unknown feature `{feat}`"),
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
pub enum CodeGenError {
    InvalidVarAccess,
    InvalidContinue,
    InvalidBreak,
}

impl Fail for CodeGenError {}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::InvalidVarAccess => write!(f, "invalid var access"),
            Self::InvalidContinue => write!(f, "invalid continue"),
            Self::InvalidBreak => write!(f, "invalid break"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum InternalError {
    CliError(CliError),
    FileRead(PathBuf, std::io::ErrorKind),
    FileCreate(PathBuf, std::io::ErrorKind),
    FileWrite(PathBuf, std::io::ErrorKind),
}

impl Fail for InternalError {}

impl Display for InternalError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::CliError(err) => write!(f, "{err}"),
            Self::FileRead(path, kind) => write!(f, "failed to open file {path:?} ({kind})"),
            Self::FileCreate(path, kind) => write!(f, "failed to create file {path:?} ({kind})"),
            Self::FileWrite(path, kind) => write!(f, "failed to write to file {path:?} ({kind})"),
        }
    }
}

impl From<CliError> for InternalError {
    fn from(err: CliError) -> Self {
        Self::CliError(err)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OverflowError {
    Dec(Span),
    Hex(Span),
}

impl Fail for OverflowError {
    fn annotations(&self) -> Vec<Annotation> {
        match self {
            Self::Dec(span) | Self::Hex(span) => vec![(*span).into()],
        }
    }
}

impl Display for OverflowError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::Dec(_) => write!(f, "decimal integer overflow"),
            Self::Hex(_) => write!(f, "hex integer overflow"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TokenizeError {
    UnclosedBlockComment(Pos),
    EmptyHex(Span),
    UnexpectedChar(u8, Pos),
}

pub struct Annotation {
    pub span: Span,
    pub comment: String,
}

impl From<Span> for Annotation {
    fn from(span: Span) -> Self {
        Self {
            span,
            comment: String::new(),
        }
    }
}

impl From<Pos> for Annotation {
    fn from(value: Pos) -> Self {
        Span::from(value).into()
    }
}

impl Fail for TokenizeError {
    fn code(&self) -> i32 {
        PARSE_CODE
    }
    fn annotations(&self) -> Vec<Annotation> {
        vec![match *self {
            Self::UnexpectedChar(_, pos) | Self::UnclosedBlockComment(pos) => pos.into(),
            Self::EmptyHex(span) => span.into(),
        }]
    }
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::UnclosedBlockComment(_) => write!(f, "unclosed block comment"),
            Self::EmptyHex(_) => write!(f, "empty hex literal"),
            Self::UnexpectedChar(chr, _) => {
                write!(f, "unexpected character `{}`", chr.escape_ascii())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError<'a> {
    Tokenize(TokenizeError),
    Unexpected(Spanned<Token<'a>>),
    LeftoverToken(Spanned<Token<'a>>),
}

impl<'a> Fail for ParseError<'a> {
    fn code(&self) -> i32 {
        match self {
            Self::Tokenize(err) => err.code(),
            _ => PARSE_CODE,
        }
    }
    fn annotations(&self) -> Vec<Annotation> {
        match self {
            Self::Tokenize(err) => err.annotations(),
            Self::Unexpected(spanned) => vec![spanned.span.into()],
            Self::LeftoverToken(spanned) => vec![spanned.span.into()],
        }
    }
}

impl<'a> Display for ParseError<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::Tokenize(err) => write!(f, "{err}"),
            Self::Unexpected(token) => write!(f, "unexpected token {:?}", token.val),
            Self::LeftoverToken(token) => write!(f, "leftover token {:?}", token.val),
        }
    }
}

impl<'a> From<TokenizeError> for ParseError<'a> {
    fn from(err: TokenizeError) -> Self {
        Self::Tokenize(err)
    }
}

#[derive(Debug, Clone)]
pub struct AsgnIncompatibleTypeError {
    pub op: AsgnOp,
    pub lhs_span: Span,
    pub rhs_span: Span,
    pub lhs_ty: TypeName,
    pub rhs_ty: Type,
}

#[derive(Debug, Clone)]
pub struct Op2IncompatibleTypeError {
    pub op: Op2,
    pub lhs_span: Span,
    pub rhs_span: Span,
    pub lhs_ty: Type,
    pub rhs_ty: Type,
}

impl<'a> From<AsgnIncompatibleTypeError> for AnaError<'a> {
    fn from(value: AsgnIncompatibleTypeError) -> Self {
        Self::AsgnOperatorIncompatibleType(Box::new(value))
    }
}

impl<'a> From<Op2IncompatibleTypeError> for AnaError<'a> {
    fn from(value: Op2IncompatibleTypeError) -> Self {
        Self::Op2OperatorIncompatibleType(Box::new(value))
    }
}

#[derive(Debug, Clone)]
pub enum AnaError<'a> {
    Overflow(OverflowError),
    OnlyMainFunction(Spanned<Ident<'a>>),
    MainMustReturnInt {
        ty: TypeName,
        ident: Spanned<Ident<'a>>,
    },
    UndeclaredVariable {
        ident: Spanned<Ident<'a>>,
    },
    UnassignedVariable {
        ident: Spanned<Ident<'a>>,
    },
    AsgnOperatorIncompatibleType(Box<AsgnIncompatibleTypeError>),
    Op2OperatorIncompatibleType(Box<Op2IncompatibleTypeError>),
    Op1IncompatibleType {
        op: Op1,
        ty: Type,
        span: Span,
    },
    ConditionTypeIncompatible {
        cond: Span,
        ty: Type,
    },
    TernaryValuesIncompatible {
        lhs_span: Span,
        rhs_span: Span,
        lhs_ty: Type,
        rhs_ty: Type,
    },
    VarRedeclaration {
        orig: Span,
        decl: Span,
        ident: Ident<'a>,
    },
    CtrlOpOutsideLoop(Span),
    WrongReturnType {
        expected: TypeName,
        got: Type,
        span: Span,
    },
    ForStepMustNotBeDecl(Span),
    ReturnCheck(Span),
}

impl<'a> Fail for AnaError<'a> {
    fn code(&self) -> i32 {
        match self {
            Self::MainMustReturnInt { .. } | Self::OnlyMainFunction(_) => PARSE_CODE,
            _ => SEM_CODE,
        }
    }
    fn annotations(&self) -> Vec<Annotation> {
        match self {
            Self::Overflow(err) => err.annotations(),
            Self::OnlyMainFunction(spanned) => vec![spanned.span.into()],
            Self::MainMustReturnInt { ty, ident } => vec![
                Annotation {
                    span: *ty.span(),
                    comment: "return type must be `int`".to_string(),
                },
                Annotation {
                    span: ident.span,
                    comment: "in this function".to_string(),
                },
            ],
            Self::UndeclaredVariable { ident } => vec![ident.span.into()],
            Self::UnassignedVariable { ident } => vec![ident.span.into()],
            Self::AsgnOperatorIncompatibleType(err) => vec![
                Annotation {
                    span: err.op.span(),
                    comment: "assign operator is incompatible".to_string(),
                },
                Annotation {
                    span: err.lhs_span,
                    comment: format!("with left hand side of type `{:?}`", err.lhs_ty),
                },
                Annotation {
                    span: err.rhs_span,
                    comment: format!("and right hand side of type `{:?}`", err.rhs_ty),
                },
            ],
            Self::Op2OperatorIncompatibleType(err) => vec![
                Annotation {
                    span: err.op.span(),
                    comment: "operator is incompatible".to_string(),
                },
                Annotation {
                    span: err.lhs_span,
                    comment: format!("with left hand side of type `{:?}`", err.lhs_ty),
                },
                Annotation {
                    span: err.rhs_span,
                    comment: format!("and right hand side of type `{:?}`", err.rhs_ty),
                },
            ],
            Self::Op1IncompatibleType { op, ty, span } => vec![
                Annotation {
                    span: op.span(),
                    comment: "unary operator is incompatible".to_string(),
                },
                Annotation {
                    span: *span,
                    comment: format!("with this expression of type `{ty:?}`"),
                },
            ],
            Self::ConditionTypeIncompatible { cond, ty } => vec![Annotation {
                span: *cond,
                comment: format!("expression has type `{ty:?}`"),
            }],
            Self::TernaryValuesIncompatible {
                lhs_span,
                rhs_span,
                lhs_ty,
                rhs_ty,
            } => vec![
                Annotation {
                    span: *lhs_span,
                    comment: format!("expression of type `{lhs_ty:?}` incompatible"),
                },
                Annotation {
                    span: *rhs_span,
                    comment: format!("with expression of type `{rhs_ty:?}`"),
                },
            ],
            Self::VarRedeclaration { orig, decl, ident } => vec![
                Annotation {
                    span: *decl,
                    comment: format!("redeclaration of variable `{ident}`"),
                },
                Annotation {
                    span: *orig,
                    comment: "that already has been defined here".to_string(),
                },
            ],
            Self::CtrlOpOutsideLoop(span) => vec![(*span).into()],
            Self::WrongReturnType {
                expected,
                got,
                span,
            } => vec![
                Annotation {
                    span: *expected.span(),
                    comment: format!("function expected return type `{expected:?}`"),
                },
                Annotation {
                    span: *span,
                    comment: format!("but got type `{got:?}`"),
                },
            ],
            Self::ForStepMustNotBeDecl(span) => vec![(*span).into()],
            Self::ReturnCheck(span) => vec![Annotation {
                span: *span,
                comment: "in this function".to_string(),
            }],
        }
    }
}

impl<'a> Display for AnaError<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::Overflow(err) => write!(f, "{err}"),
            Self::OnlyMainFunction(ident) => write!(
                f,
                "expected `main` as function name, but got `{}`",
                ident.val
            ),
            Self::MainMustReturnInt { .. } => {
                write!(f, "the main function must return `int`")
            }
            Self::UndeclaredVariable { ident } => {
                write!(f, "use of undeclared variable `{}`", ident.val)
            }
            Self::UnassignedVariable { ident } => {
                write!(f, "use of unassigned variable `{}`", ident.val)
            }
            Self::AsgnOperatorIncompatibleType(_)
            | Self::Op2OperatorIncompatibleType(_)
            | Self::TernaryValuesIncompatible { .. }
            | Self::Op1IncompatibleType { .. } => {
                write!(f, "incompatible types")
            }
            Self::ConditionTypeIncompatible { .. } => {
                write!(f, "condition must be `bool`")
            }
            Self::VarRedeclaration { ident, .. } => {
                write!(f, "redeclaration of variable `{ident}`")
            }
            Self::CtrlOpOutsideLoop(_) => write!(f, "control operation outside of loop"),
            Self::WrongReturnType { .. } => write!(f, "wrong return type"),
            Self::ForStepMustNotBeDecl(..) => write!(f, "step statement must not be a declaration"),
            Self::ReturnCheck(_) => write!(f, "not all paths return"),
        }
    }
}
