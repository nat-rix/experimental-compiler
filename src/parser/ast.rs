use crate::{
    error::{AnyError, ParseError},
    parser::tokenize::{Keyword, Symbol},
};

use super::{
    span::Spanned,
    tokenize::{Token, TokenStream},
};

pub type ParseResult<T> = Result<Spanned<T>, AnyError>;

pub trait Parse<'a>: Sized {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self>;
}

#[derive(Debug, Clone)]
pub struct Ast<'a> {
    stmts: Vec<Spanned<Stmt<'a>>>,
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Decl(Ident<'a>),
    Init(Ident<'a>, Expr<'a>),
    Asgn(LValue<'a>, AsgnOp, Expr<'a>),
    Ret(Expr<'a>),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Lit(Lit),
    Ident(Ident<'a>),
    Op1(Op1, Box<Expr<'a>>),
    Op2(Op2, Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug, Clone)]
enum ExprAtom<'a> {
    Lit(Lit),
    Ident(Ident<'a>),
    Parens(Box<ExprLayerAdd<'a>>),
}

#[derive(Debug, Clone)]
enum ExprLayer1<'a> {
    Atom(ExprAtom<'a>),
    Op(Op1, Box<Self>),
}

#[derive(Debug, Clone)]
struct ExprLayer2<Atom> {
    items: Vec<(Atom, Op2)>,
    atom: Atom,
}

type ExprLayerMul<'a> = ExprLayer2<ExprLayer1<'a>>;
type ExprLayerAdd<'a> = ExprLayer2<ExprLayerMul<'a>>;

impl<'a> From<ExprAtom<'a>> for Expr<'a> {
    fn from(value: ExprAtom<'a>) -> Self {
        match value {
            ExprAtom::Lit(lit) => Expr::Lit(lit),
            ExprAtom::Ident(ident) => Expr::Ident(ident),
            ExprAtom::Parens(expr) => (*expr).into(),
        }
    }
}

impl<'a> From<ExprLayer1<'a>> for Expr<'a> {
    fn from(value: ExprLayer1<'a>) -> Self {
        match value {
            ExprLayer1::Atom(atom) => atom.into(),
            ExprLayer1::Op(op, val) => Expr::Op1(op, Box::new((*val).into())),
        }
    }
}

impl<'a, Atom: Into<Expr<'a>>> From<ExprLayer2<Atom>> for Expr<'a> {
    fn from(value: ExprLayer2<Atom>) -> Self {
        let mut iter = value.items.into_iter();
        if let Some((lhs_atom, mut lhs_op)) = iter.next() {
            let mut lhs_expr: Expr = lhs_atom.into();
            for (right_atom, right_op) in iter {
                lhs_expr = Expr::Op2(lhs_op, Box::new(lhs_expr), Box::new(right_atom.into()));
                lhs_op = right_op;
            }
            Expr::Op2(lhs_op, Box::new(lhs_expr), Box::new(value.atom.into()))
        } else {
            value.atom.into()
        }
    }
}

#[derive(Debug, Clone)]
pub struct LValue<'a>(pub Ident<'a>);

#[derive(Debug, Clone)]
pub enum Lit {
    Int(IntLit),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntLit(pub u32);

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident<'a>(pub &'a [u8]);

impl<'a> core::fmt::Debug for Ident<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "Ident({})", self.0.escape_ascii())
    }
}

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

fn todo_err<'a>(token: Option<Spanned<Token<'a>>>) -> ParseError {
    ParseError::TodoError(token.map(|token| token.span))
}

fn peek_filter<'a, T>(
    stream: &mut TokenStream<'a>,
    matcher: impl FnOnce(&Token<'a>) -> Option<T>,
) -> Result<Option<Spanned<T>>, AnyError> {
    Ok(match stream.peek()?.map(|s| (matcher(&s), s)) {
        Some((Some(t), s)) => Some(Spanned {
            val: t,
            span: s.span,
        }),
        _ => None,
    })
}

fn expect_filter<'a, T, E: Into<AnyError>>(
    stream: &mut TokenStream<'a>,
    matcher: impl FnOnce(&Token<'a>) -> Option<T>,
    err: impl FnOnce(Option<Spanned<Token<'a>>>) -> E,
) -> Result<Spanned<T>, AnyError> {
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

fn expect<'a, E: Into<AnyError>>(
    stream: &mut TokenStream<'a>,
    matcher: impl FnOnce(&Token<'a>) -> bool,
    err: impl FnOnce(Option<Spanned<Token<'a>>>) -> E,
) -> Result<Spanned<Token<'a>>, AnyError> {
    match stream.peek()? {
        Some(token) if matcher(&token) => {
            stream.advance()?;
            Ok(token)
        }
        token => Err(err(token).into()),
    }
}

fn is_next<'a>(stream: &mut TokenStream<'a>, token: &Token) -> Result<bool, AnyError> {
    Ok(stream.peek()?.as_deref().is_some_and(|t| t == token))
}

impl<'a> Parse<'a> for Ast<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
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
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
        let token = stream.peek()?;
        let (val, last) = match token.as_deref() {
            Some(Token::Keyword(Keyword::Int)) => {
                stream.advance()?;
                let ident = Ident::parse(stream)?;
                let stmt = if is_next(stream, &Token::Symbol(Symbol::Eq))? {
                    stream.advance()?;
                    let expr = Expr::parse(stream)?;
                    Self::Init(ident.val, expr.val)
                } else {
                    Self::Decl(ident.val)
                };
                let last = expect(stream, |t| t == &Token::Symbol(Symbol::Semicolon), todo_err)?;
                (stmt, last.span)
            }
            Some(Token::Ident(_)) => {
                let lvalue = LValue::parse(stream)?;
                let op = AsgnOp::parse(stream)?;
                let expr = Expr::parse(stream)?;
                let last = expect(stream, |t| t == &Token::Symbol(Symbol::Semicolon), todo_err)?;
                (Stmt::Asgn(lvalue.val, op.val, expr.val), last.span)
            }
            Some(Token::Keyword(Keyword::Return)) => {
                stream.advance()?;
                let expr = Expr::parse(stream)?;
                let last = expect(stream, |t| t == &Token::Symbol(Symbol::Semicolon), todo_err)?;
                (Stmt::Ret(expr.val), last.span)
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
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
        ExprLayerAdd::parse(stream).map(|s| s.map(Into::into))
    }
}

impl<'a, Atom: Parse<'a> + Into<Expr<'a>>> ExprLayer2<Atom> {
    fn parse_with(
        stream: &mut TokenStream<'a>,
        mut filter: impl FnMut(&Token<'a>) -> Option<Op2>,
    ) -> ParseResult<Self> {
        let mut atom_lhs = Atom::parse(stream)?;
        let start = atom_lhs.span;
        let mut items = vec![];
        while let Some(op) = peek_filter(stream, &mut filter)? {
            stream.advance()?;
            let atom_rhs = Atom::parse(stream)?;
            items.push((atom_lhs.val, op.val));
            atom_lhs = atom_rhs;
        }
        Ok(Spanned {
            val: Self {
                items,
                atom: atom_lhs.val,
            },
            span: start.join(&atom_lhs.span),
        })
    }
}

impl<'a> Parse<'a> for ExprLayerAdd<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
        Self::parse_with(stream, |token| match token {
            Token::Symbol(Symbol::Plus) => Some(Op2::Add),
            Token::Symbol(Symbol::Minus) => Some(Op2::Sub),
            _ => None,
        })
    }
}

impl<'a> Parse<'a> for ExprLayerMul<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
        Self::parse_with(stream, |token| match token {
            Token::Symbol(Symbol::Mul) => Some(Op2::Mul),
            Token::Symbol(Symbol::Div) => Some(Op2::Div),
            Token::Symbol(Symbol::Mod) => Some(Op2::Mod),
            _ => None,
        })
    }
}

impl<'a> Parse<'a> for ExprLayer1<'a> {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
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
                val: Self::Op(op.val, Box::new(val.val)),
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
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
        if let Some(first) = peek_filter(stream, |t| {
            (t == &Token::Symbol(Symbol::ParenLt)).then_some(*t)
        })? {
            stream.advance()?;
            let expr = ExprLayerAdd::parse(stream)?;
            let last = expect(stream, |t| t == &Token::Symbol(Symbol::ParenRt), todo_err)?;
            return Ok(Spanned {
                val: Self::Parens(Box::new(expr.val)),
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
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
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
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
        Ok(expect_filter(stream, |t| t.get_ident(), todo_err)?.map(Ident))
    }
}

impl<'a> Parse<'a> for AsgnOp {
    fn parse(stream: &mut TokenStream<'a>) -> ParseResult<Self> {
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
