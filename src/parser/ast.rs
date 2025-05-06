use std::fmt::{Display, Formatter, Result as FmtResult};

use crate::{
    error::{AnyError, ParseError},
    parser::tokenize::{Keyword, Symbol},
};

use super::{
    span::Spanned,
    tokenize::{Token, TokenStream},
};

pub type ParseResult<'a, T> = Result<Spanned<'a, T>, AnyError<'a>>;

pub trait Parse<'a>: Sized {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self>;
}

#[derive(Debug, Clone)]
pub struct Ast<'a> {
    pub stmts: Vec<Spanned<'a, Stmt<'a>>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Decl(Spanned<'a, Ident<'a>>),
    Init(Spanned<'a, Ident<'a>>, Spanned<'a, Expr<'a>>),
    Asgn(
        Spanned<'a, LValue<'a>>,
        Spanned<'a, AsgnOp>,
        Spanned<'a, Expr<'a>>,
    ),
    Ret(Spanned<'a, Expr<'a>>),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Lit(Spanned<'a, Lit>),
    Ident(Spanned<'a, Ident<'a>>),
    Op1(Spanned<'a, Op1>, Spanned<'a, Box<Expr<'a>>>),
    Op2(
        Spanned<'a, Op2>,
        Spanned<'a, Box<Expr<'a>>>,
        Spanned<'a, Box<Expr<'a>>>,
    ),
}

#[derive(Debug, Clone)]
pub enum ExprAtom<'a> {
    Lit(Lit),
    Ident(Ident<'a>),
    Parens(Spanned<'a, Box<ExprAdd<'a>>>),
}

#[derive(Debug, Clone)]
pub enum ExprOp1<'a> {
    Atom(ExprAtom<'a>),
    Op(Spanned<'a, Op1>, Spanned<'a, Box<Self>>),
}

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum ExprMul<'a> {
    Atom(ExprOp1<'a>),
    Op(
        Spanned<'a, Op2>,
        Spanned<'a, ExprOp1<'a>>,
        Spanned<'a, Box<Self>>,
    ),
}

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
pub enum ExprAdd<'a> {
    Atom(ExprMul<'a>),
    Op(
        Spanned<'a, Op2>,
        Spanned<'a, ExprMul<'a>>,
        Spanned<'a, Box<Self>>,
    ),
}

impl<'a> From<Spanned<'a, ExprAtom<'a>>> for Spanned<'a, Expr<'a>> {
    fn from(value: Spanned<'a, ExprAtom<'a>>) -> Self {
        let val = match value.val {
            ExprAtom::Lit(lit) => Expr::Lit(Spanned {
                val: lit,
                span: value.span,
            }),
            ExprAtom::Ident(ident) => Expr::Ident(Spanned {
                val: ident,
                span: value.span,
            }),
            ExprAtom::Parens(val) => return val.map(|t| *t).into(),
        };
        Spanned {
            val,
            span: value.span,
        }
    }
}

impl<'a> From<Spanned<'a, ExprOp1<'a>>> for Spanned<'a, Expr<'a>> {
    fn from(value: Spanned<'a, ExprOp1<'a>>) -> Self {
        match value.val {
            ExprOp1::Atom(atom) => (Spanned {
                val: atom,
                span: value.span,
            })
            .into(),
            ExprOp1::Op(op, val) => Spanned {
                val: Expr::Op1(
                    op,
                    Into::<Spanned<'a, Expr<'a>>>::into(val.map(|t| *t)).map(Box::new),
                ),
                span: value.span,
            },
        }
    }
}

macro_rules! impl_expr_op2 {
    ($($t:ident),*) => {
        $(impl<'a> From<Spanned<'a, $t<'a>>> for Spanned<'a, Expr<'a>> {
            fn from(value: Spanned<'a, $t<'a>>) -> Self {
                match value.val {
                    $t::Atom(op1) => (Spanned {
                        val: op1,
                        span: value.span,
                    })
                    .into(),
                    $t::Op(op, lt, rt) => Spanned {
                        val: Expr::Op2(
                            op,
                            Into::<Spanned<'a, Expr<'a>>>::into(lt).map(Box::new),
                            Into::<Spanned<'a, Expr<'a>>>::into(rt.map(|t| *t)).map(Box::new),
                        ),
                        span: value.span,
                    },
                }
            }
        })*
    };
}

impl_expr_op2!(ExprMul, ExprAdd);

#[derive(Debug, Clone)]
pub struct LValue<'a>(pub Ident<'a>);

#[derive(Debug, Clone)]
pub enum Lit {
    Int(IntLit),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntLit(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident<'a>(pub &'a [u8]);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AsgnOp {
    Asg,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Op1;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

fn todo_err<'a>(token: Option<Spanned<'a, Token<'a>>>) -> ParseError<'a> {
    ParseError::TodoError(token.map(|token| token.span))
}

fn peek_filter<'a, T>(
    stream: &mut TokenStream<'a>,
    matcher: impl FnOnce(&Token<'a>) -> Option<T>,
) -> Result<Option<Spanned<'a, T>>, AnyError<'a>> {
    Ok(match stream.peek()?.map(|s| (matcher(&s), s)) {
        Some((Some(t), s)) => Some(Spanned {
            val: t,
            span: s.span,
        }),
        _ => None,
    })
}

fn expect_filter<'a, T, E: Into<AnyError<'a>>>(
    stream: &mut TokenStream<'a>,
    matcher: impl FnOnce(&Token<'a>) -> Option<T>,
    err: impl FnOnce(Option<Spanned<'a, Token<'a>>>) -> E,
) -> Result<Spanned<'a, T>, AnyError<'a>> {
    let token = stream.peek()?;
    match token.map(|s| (matcher(&s), s)) {
        Some((Some(t), s)) => {
            stream.advance()?;
            Ok(Spanned {
                val: t,
                span: s.span,
            })
        }
        _ => Err(err(token).into()),
    }
}

fn expect<'a, E: Into<AnyError<'a>>>(
    stream: &mut TokenStream<'a>,
    matcher: impl FnOnce(&Token<'a>) -> bool,
    err: impl FnOnce(Option<Spanned<'a, Token<'a>>>) -> E,
) -> Result<Spanned<'a, Token<'a>>, AnyError<'a>> {
    match stream.peek()? {
        Some(token) if matcher(&token) => {
            stream.advance()?;
            Ok(token)
        }
        token => Err(err(token).into()),
    }
}

fn is_next<'a>(stream: &mut TokenStream<'a>, token: &Token) -> Result<bool, AnyError<'a>> {
    Ok(stream.peek()?.as_deref().is_some_and(|t| t == token))
}

impl<'a> Parse<'a> for Ast<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        let first = expect(stream, |t| t == &Token::Keyword(Keyword::Int), todo_err)?;
        expect(stream, |t| t == &Token::Ident(b"main"), todo_err)?;
        expect(stream, |t| t == &Token::Symbol(Symbol::ParenLt), todo_err)?;
        expect(stream, |t| t == &Token::Symbol(Symbol::ParenRt), todo_err)?;
        expect(stream, |t| t == &Token::Symbol(Symbol::BraceLt), todo_err)?;
        let mut stmts = vec![];
        while !is_next(stream, &Token::Symbol(Symbol::BraceRt))? {
            stmts.push(Stmt::parse(stream)?)
        }
        let last = expect(stream, |t| t == &Token::Symbol(Symbol::BraceRt), todo_err)?;
        Ok(Spanned {
            val: Self { stmts },
            span: first.span.join(&last.span),
        })
    }
}

impl<'a> Parse<'a> for Stmt<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        let token = stream.peek()?;
        let (val, last) = match token.as_deref() {
            Some(Token::Keyword(Keyword::Int)) => {
                stream.advance()?;
                let ident = Ident::parse(stream)?;
                let stmt = if is_next(stream, &Token::Symbol(Symbol::Eq))? {
                    stream.advance()?;
                    let expr = Expr::parse(stream)?;
                    Self::Init(ident, expr)
                } else {
                    Self::Decl(ident)
                };
                let last = expect(stream, |t| t == &Token::Symbol(Symbol::Semicolon), todo_err)?;
                (stmt, last.span)
            }
            Some(Token::Ident(_)) => {
                let lvalue = LValue::parse(stream)?;
                let op = AsgnOp::parse(stream)?;
                let expr = Expr::parse(stream)?;
                let last = expect(stream, |t| t == &Token::Symbol(Symbol::Semicolon), todo_err)?;
                (Stmt::Asgn(lvalue, op, expr), last.span)
            }
            Some(Token::Keyword(Keyword::Return)) => {
                stream.advance()?;
                let expr = Expr::parse(stream)?;
                let last = expect(stream, |t| t == &Token::Symbol(Symbol::Semicolon), todo_err)?;
                (Stmt::Ret(expr), last.span)
            }
            _ => Err(todo_err(token))?,
        };
        Ok(Spanned {
            val,
            span: token.unwrap().span.join(&last),
        })
    }
}

impl<'a> Parse<'a> for Expr<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        ExprAdd::parse(stream).map(Into::into)
    }
}

fn parse_op2<'a, T: Parse<'a>, U: Parse<'a>>(
    stream: &mut TokenStream<'a>,
    atom: impl Fn(U) -> T,
    join: impl Fn(Spanned<'a, Op2>, Spanned<'a, U>, Spanned<'a, Box<T>>) -> T,
    filter: impl FnOnce(&Token<'a>) -> Option<Op2>,
) -> ParseResult<'a, T> {
    let lhs = U::parse(stream)?;
    let first = lhs.span;
    let op = peek_filter(stream, filter)?;
    if let Some(op) = op {
        stream.advance()?;
        let rhs = T::parse(stream)?;
        let last = rhs.span;
        Ok(Spanned {
            val: join(op, lhs, rhs.map(Box::new)),
            span: first.join(&last),
        })
    } else {
        Ok(lhs.map(atom))
    }
}

impl<'a> Parse<'a> for ExprAdd<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        parse_op2(stream, Self::Atom, Self::Op, |token| match token {
            Token::Symbol(Symbol::Plus) => Some(Op2::Add),
            Token::Symbol(Symbol::Minus) => Some(Op2::Sub),
            _ => None,
        })
    }
}

impl<'a> Parse<'a> for ExprMul<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        parse_op2(stream, Self::Atom, Self::Op, |token| match token {
            Token::Symbol(Symbol::Mul) => Some(Op2::Mul),
            Token::Symbol(Symbol::Div) => Some(Op2::Div),
            Token::Symbol(Symbol::Mod) => Some(Op2::Mod),
            _ => None,
        })
    }
}

impl<'a> Parse<'a> for ExprOp1<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        let op = peek_filter(stream, |token| match token {
            Token::Symbol(Symbol::Minus) => Some(Op1),
            _ => None,
        })?;
        if let Some(op) = op {
            let first = op.span;
            stream.advance()?;
            let val = Self::parse(stream)?;
            let last = val.span;
            Ok(Spanned {
                val: Self::Op(op, val.map(Box::new)),
                span: first.join(&last),
            })
        } else {
            ExprAtom::parse(stream).map(|s| s.map(Self::Atom))
        }
    }
}

fn parse_int_dec(bytes: &[u8]) -> Option<u32> {
    bytes
        .iter()
        .try_fold(0u32, |a, b| {
            a.checked_mul(10)?.checked_add(u32::from(*b - b'0'))
        })
        .filter(|val| *val <= i32::MIN as u32)
}

fn parse_int_hex(bytes: &[u8]) -> Option<u32> {
    bytes.iter().try_fold(0u32, |a, b| {
        a.checked_mul(16)?.checked_add(u32::from(match b {
            b'a'..=b'f' => *b - b'a' + 10,
            b'A'..=b'F' => *b - b'A' + 10,
            _ => *b - b'0',
        }))
    })
}

fn parse_int(token: &Token) -> Option<Option<u32>> {
    match token {
        Token::IntDec(dec) => Some(parse_int_dec(dec)),
        Token::IntHex(hex) => Some(parse_int_hex(hex)),
        _ => None,
    }
}

impl<'a> Parse<'a> for ExprAtom<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        if let Some(first) = peek_filter(stream, |t| {
            (t == &Token::Symbol(Symbol::ParenLt)).then_some(*t)
        })? {
            stream.advance()?;
            let expr = ExprAdd::parse(stream)?;
            let last = expect(stream, |t| t == &Token::Symbol(Symbol::ParenRt), todo_err)?;
            return Ok(Spanned {
                val: Self::Parens(expr.map(Box::new)),
                span: first.span.join(&last.span),
            });
        }
        if let Some(ident) = peek_filter(stream, |t| t.get_ident())? {
            stream.advance()?;
            return Ok(ident.map(Ident).map(Self::Ident));
        }
        if let Some(int) = peek_filter(stream, parse_int)? {
            stream.advance()?;
            if let Some(val) = int.val {
                return Ok((Spanned {
                    val,
                    span: int.span,
                })
                .map(IntLit)
                .map(Lit::Int)
                .map(Self::Lit));
            } else {
                Err(ParseError::Int(int.span))?
            }
        }
        Err(todo_err(stream.peek()?))?
    }
}

impl<'a> Parse<'a> for LValue<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        if is_next(stream, &Token::Symbol(Symbol::ParenLt))? {
            stream.advance()?;
            let res = Self::parse(stream)?;
            expect(stream, |t| t == &Token::Symbol(Symbol::ParenRt), todo_err)?;
            Ok(res)
        } else {
            Ident::parse(stream).map(|s| s.map(Self))
        }
    }
}

impl<'a> Parse<'a> for Ident<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        Ok(expect_filter(stream, |t| t.get_ident(), todo_err)?.map(Ident))
    }
}

impl<'a> Parse<'a> for AsgnOp {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<'a, Self> {
        expect_filter(
            stream,
            |t| match t {
                Token::Symbol(Symbol::Eq) => Some(Self::Asg),
                Token::Symbol(Symbol::AddEq) => Some(Self::Add),
                Token::Symbol(Symbol::SubEq) => Some(Self::Sub),
                Token::Symbol(Symbol::MulEq) => Some(Self::Mul),
                Token::Symbol(Symbol::DivEq) => Some(Self::Div),
                Token::Symbol(Symbol::ModEq) => Some(Self::Mod),
                _ => None,
            },
            todo_err,
        )
    }
}

impl<'a> Display for Ast<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "(Ast ")?;
        for s in &self.stmts {
            writeln!(f, "{}", s.val)?;
            write!(f, "     ")?;
        }
        write!(f, ")")
    }
}

impl<'a> Display for Stmt<'a> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::Decl(ident) => write!(f, "(Decl {})", ident.val),
            Self::Init(ident, expr) => write!(f, "(Init {} {})", ident.val, expr.val),
            Self::Asgn(lval, op, expr) => write!(f, "(Asgn {} {} {})", lval.val, op.val, expr.val),
            Self::Ret(expr) => write!(f, "(Expr {})", expr.val),
        }
    }
}

impl<'a> Display for LValue<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

impl Display for AsgnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            AsgnOp::Asg => write!(f, "="),
            AsgnOp::Add => write!(f, "+="),
            AsgnOp::Sub => write!(f, "-="),
            AsgnOp::Mul => write!(f, "*="),
            AsgnOp::Div => write!(f, "/="),
            AsgnOp::Mod => write!(f, "%="),
        }
    }
}

impl Display for Op1 {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "-")
    }
}

impl Display for Op2 {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
        }
    }
}

impl<'a> Display for Ident<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "(Ident {})", self.0.escape_ascii())
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Lit(lit) => write!(f, "{}", lit.val),
            Self::Ident(ident) => write!(f, "{}", ident.val),
            Self::Op1(op, expr) => write!(f, "(Op1 {} {})", op.val, expr.val),
            Self::Op2(op, lhs, rhs) => write!(f, "(Op2 {} {} {})", op.val, lhs.val, rhs.val),
        }
    }
}

impl Display for Lit {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Lit::Int(lit) => write!(f, "(IntLit {})", lit.0),
        }
    }
}
