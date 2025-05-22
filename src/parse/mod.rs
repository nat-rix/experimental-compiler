pub mod ana;
pub mod return_check;
pub mod tokenize;
pub mod type_check;

use crate::{
    error::{ParseError, TokenizeError},
    span::{Pos, Span, Spanned},
};
use tokenize::{Ident, Keyword, Token};

pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;

pub fn parse_ast<'a>(iter: tokenize::TokenIter<'a>) -> ParseResult<'a, Ast<'a>> {
    Ast::parse(&mut TokenStream::new(iter), &mut false).map(|s| s.val)
}

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    buffer: Option<Spanned<Token<'a>>>,
    iter: tokenize::TokenIter<'a>,
    last_pos: Option<Pos>,
}

impl<'a> TokenStream<'a> {
    pub fn new(iter: tokenize::TokenIter<'a>) -> Self {
        Self {
            buffer: None,
            iter,
            last_pos: None,
        }
    }

    pub fn pos(&self) -> Pos {
        self.last_pos.unwrap_or_else(|| self.iter.pos())
    }

    pub fn peek(&mut self) -> Result<Spanned<Token<'a>>, TokenizeError> {
        if let Some(buf) = &self.buffer {
            return Ok(*buf);
        }
        let buf = self.iter.next_token()?;
        self.buffer = Some(buf);
        Ok(buf)
    }

    pub fn consume(&mut self, consumed: &mut bool) -> Result<Spanned<Token<'a>>, TokenizeError> {
        *consumed = true;
        self.buffer
            .take()
            .map(Ok)
            .unwrap_or_else(|| self.iter.next_token())
            .inspect(|token| self.last_pos = Some(token.span.end))
    }
}

pub trait Parse<'a>: Sized {
    type Res;
    fn parse_opt(
        stream: &mut TokenStream<'a>,
        consumed: &mut bool,
    ) -> ParseResult<'a, Option<Self::Res>>;
    fn parse(stream: &mut TokenStream<'a>, consumed: &mut bool) -> ParseResult<'a, Self::Res> {
        Self::parse_opt(stream, consumed).and_then(|val| {
            val.ok_or_else(|| match stream.peek() {
                Ok(token) => ParseError::Unexpected(token),
                Err(err) => err.into(),
            })
        })
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for Option<T> {
    type Res = Option<T::Res>;
    fn parse_opt(
        stream: &mut TokenStream<'a>,
        consumed: &mut bool,
    ) -> ParseResult<'a, Option<Self::Res>> {
        T::parse_opt(stream, consumed).map(Some)
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for Vec<T> {
    type Res = Vec<T::Res>;
    fn parse_opt(
        stream: &mut TokenStream<'a>,
        consumed: &mut bool,
    ) -> ParseResult<'a, Option<Self::Res>> {
        let mut vec = vec![];
        while let Some(val) = T::parse_opt(stream, consumed)? {
            vec.push(val);
        }
        Ok(Some(vec))
    }
}

impl<'a, T: Parse<'a>> Parse<'a> for Box<T> {
    type Res = Box<T::Res>;
    fn parse_opt(
        stream: &mut TokenStream<'a>,
        consumed: &mut bool,
    ) -> ParseResult<'a, Option<Self::Res>> {
        T::parse_opt(stream, consumed).map(|v| v.map(Box::new))
    }
}

impl<'a> Parse<'a> for Ident<'a> {
    type Res = Spanned<Self>;
    fn parse_opt(
        stream: &mut TokenStream<'a>,
        consumed: &mut bool,
    ) -> ParseResult<'a, Option<Self::Res>> {
        let token = stream.peek()?;
        Ok(match *token {
            Token::Ident(ident) => {
                stream.consume(consumed)?;
                Some(token.map(|_| ident))
            }
            _ => None,
        })
    }
}

impl<'a> Parse<'a> for i32 {
    type Res = Spanned<Self>;
    fn parse_opt(
        stream: &mut TokenStream<'a>,
        consumed: &mut bool,
    ) -> ParseResult<'a, Option<Self::Res>> {
        let token = stream.peek()?;
        Ok(match *token {
            Token::IntConst(val) => {
                stream.consume(consumed)?;
                Some(token.map(|_| val))
            }
            _ => None,
        })
    }
}

pub struct OptParens<T>(T);

impl<'a, T: Parse<'a>> Parse<'a> for OptParens<T> {
    type Res = T::Res;
    fn parse_opt(
        stream: &mut TokenStream<'a>,
        consumed: &mut bool,
    ) -> ParseResult<'a, Option<Self::Res>> {
        if ParenL::parse_opt(stream, consumed)?.is_some() {
            let res = Self::parse(stream, consumed)?;
            ParenL::parse(stream, consumed)?;
            return Ok(Some(res));
        }
        T::parse_opt(stream, consumed)
    }
}

macro_rules! def {
    (@struct $name:ident {$( $fname:ident : $fty:ty ),*$(,)?}) => {
        def!(@struct $name [] <'static> { $( $fname : $fty ),* });
    };
    (@struct $name:ident <$lt:lifetime> {$( $fname:ident : $fty:ty ),*$(,)?}) => {
        def!(@struct $name [$lt] <$lt> { $( $fname : $fty ),* });
    };
    (@struct $name:ident [$($lt:lifetime)?] <$lt2:lifetime> {$( $fname:ident : $fty:ty ),*$(,)?}) => {
        #[derive(Debug, Clone)]
        pub struct $name $(<$lt>)? {
            $(pub $fname : <$fty as Parse <$lt2>>::Res),*
        }

        impl<'a> Parse<'a> for $name $(<$lt>)? {
            type Res = Spanned<Self>;
            fn parse_opt(stream: &mut TokenStream<'a>, consumed: &mut bool) -> ParseResult<'a, Option<Self::Res>> {
                let __start = stream.peek().ok().map(|s| s.span).unwrap_or_else(|| stream.pos().into());
                let mut this_consumed = false;
                $(
                    let $fname = if !this_consumed {
                        match <$fty as Parse<'a>>::parse_opt(stream, &mut this_consumed)? {
                            None => return Ok(None),
                            Some(v) => v,
                        }
                    } else {
                        <$fty as Parse<'a>>::parse(stream, &mut this_consumed)?
                    };
                )*
                *consumed |= this_consumed;
                let __end = stream.pos();
                Ok(Some(Spanned::new(Self { $($fname,)* }, __start.combine(__end.into()))))
            }
        }
    };

    (@enum $name:ident <$lt:lifetime> $(#[$a:meta])? { $($fname:ident($fty:ty)),* $(,)? }) => {
        def!(@enum $name [$lt] <$lt> $(#[$a])? { $($fname($fty)),* });
    };
    (@enum $name:ident $(#[$a:meta])? { $($fname:ident($fty:ty)),* $(,)? }) => {
        def!(@enum $name [] <'static> $(#[$a])? { $($fname($fty)),* });
    };
    (@enum $name:ident [$($lt:lifetime)?] <$lt2:lifetime> $(#[$a:meta])? { $($fname:ident($fty:ty)),* $(,)? }) => {
        $(#[$a])?
        #[derive(Debug, Clone)]
        pub enum $name $(<$lt>)? {
            $($fname(<$fty as Parse <$lt2>>::Res)),*
        }

        impl<'a> Parse<'a> for $name $(<$lt>)? {
            type Res = Self;
            fn parse_opt(stream: &mut TokenStream<'a>, consumed: &mut bool) -> ParseResult<'a, Option<Self::Res>> {
                $(
                    if let Some(res) = <$fty as Parse<'a>>::parse_opt(stream, consumed)? {
                        return Ok(Some(Self::$fname(res)));
                    }
                )*
                Ok(None)
            }
        }
    };

    (@tokens ($prefix:ident, $map:path) $($name:ident),*$(,)?) => {$(
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct $name;
        impl $name {
            pub const EXPECT_TOKEN: Token<'static> = ($map)($prefix::$name);
        }
        impl<'a> Parse<'a> for $name {
            type Res = Spanned<Self>;
            fn parse_opt(stream: &mut TokenStream<'a>, consumed: &mut bool) -> ParseResult<'a, Option<Self::Res>> {
                let token = stream.peek()?;
                Ok(match *token {
                    Self::EXPECT_TOKEN => {
                        stream.consume(consumed)?;
                        Some(token.map(|_| Self))
                    }
                    _ => None,
                })
            }
        }
    )*};
}

def! {@struct Ast<'a> {
    main_fun: Function<'a>,
}}

def! {@struct Function<'a> {
    return_ty: TypeName,
    name: Ident<'a>,
    paren_l: ParenL,
    paren_r: ParenR,
    block: Block<'a>,
}}

def! {@struct Block<'a> {
    brace_l: BraceL,
    stmts: Vec<Stmt<'a>>,
    brace_r: BraceR,
}}

def! {@enum Stmt<'a> {
    Simp(StmtSimp<'a>),
    Ctrl(Ctrl<'a>),
    Block(Box<Block<'a>>),
}}

def! {@struct StmtSimp<'a> {
    simp: Simp<'a>,
    semicolon: Semicolon,
}}

def! {@enum Simp<'a> {
    Asgn(Asgn<'a>),
    Decl(Decl<'a>),
}}

def! {@struct Decl<'a> {
    ty: TypeName,
    name: Ident<'a>,
    val: Option<DeclVal<'a>>,
}}

def! {@struct DeclVal<'a> {
    eq: Eq,
    expr: Expr<'a>,
}}

def! {@enum Ctrl<'a> {
    If(Box<CtrlIf<'a>>),
    While(Box<CtrlWhile<'a>>),
    For(Box<CtrlFor<'a>>),
    Continue(CtrlContinue),
    Break(CtrlBreak),
    Return(CtrlReturn<'a>),
}}

def! {@struct CtrlIf<'a> {
    keyword: If,
    paren_l: ParenL,
    cond: Expr<'a>,
    paren_r: ParenR,
    stmt: Stmt<'a>,
    else_block: Option<CtrlElse<'a>>,
}}

def! {@struct CtrlElse<'a> {
    keyword: Else,
    stmt: Stmt<'a>
}}

def! {@struct CtrlWhile<'a> {
    keyword: While,
    paren_l: ParenL,
    cond: Expr<'a>,
    paren_r: ParenR,
    stmt: Stmt<'a>,
}}

def! {@struct CtrlFor<'a> {
    keyword: For,
    paren_l: ParenL,
    op0: Option<Simp<'a>>,
    semicolon0: Semicolon,
    op1: Expr<'a>,
    semicolon1: Semicolon,
    op2: Option<Simp<'a>>,
    paren_r: ParenR,
    stmt: Stmt<'a>,
}}

def! {@struct CtrlContinue {
    keyword: Continue,
    semicolon: Semicolon,
}}

def! {@struct CtrlBreak {
    keyword: Break,
    semicolon: Semicolon,
}}

def! {@struct CtrlReturn<'a> {
    keyword: Return,
    expr: Expr<'a>,
    semicolon: Semicolon,
}}

def! {@struct Asgn<'a> {
    lvalue: OptParens<Ident<'a>>,
    op: AsgnOp,
    expr: Expr<'a>,
}}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Atom(ExprAtom<'a>),
    Op2(Box<Expr<'a>>, Op2, Box<Expr<'a>>),
    Op3(
        Box<Expr<'a>>,
        Spanned<QMark>,
        Box<Expr<'a>>,
        Spanned<Colon>,
        Box<Expr<'a>>,
    ),
}

impl<'a> Expr<'a> {
    pub fn parse_precedence(
        stream: &mut TokenStream<'a>,
        precedence: i32,
    ) -> ParseResult<'a, Self> {
        if precedence == -1 {
            return OptParens::<ExprAtom>::parse(stream, &mut false).map(Self::Atom);
        }
        let mut lhs = Self::parse_precedence(stream, precedence - 1)?;
        let mut items = vec![];
        loop {
            let old_token = stream.peek()?;
            let op = if let Some(op) = Op2::parse_opt(stream, &mut false)? {
                if op.precedence() > precedence {
                    stream.buffer = Some(old_token);
                    break;
                }
                op
            } else {
                break;
            };
            let rhs = Self::parse_precedence(stream, precedence - 1)?;
            items.push((op, rhs));
        }
        for (op, expr) in items {
            lhs = Expr::Op2(Box::new(lhs), op, Box::new(expr))
        }
        Ok(lhs)
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Atom(atom) => atom.span(),
            Self::Op3(lhs, .., rhs) | Self::Op2(lhs, .., rhs) => lhs.span().combine(rhs.span()),
        }
    }
}

impl<'a> Parse<'a> for Expr<'a> {
    type Res = Self;
    fn parse_opt(
        stream: &mut TokenStream<'a>,
        consumed: &mut bool,
    ) -> ParseResult<'a, Option<Self::Res>> {
        *consumed = true;
        let cond = Expr::parse_precedence(stream, Op2::MAX_PRECEDENCE)?;
        let Some(mark) = QMark::parse_opt(stream, consumed)? else {
            return Ok(Some(cond));
        };
        Ok(Some(Expr::Op3(
            Box::new(cond),
            mark,
            Box::new(Self::parse(stream, consumed)?),
            Colon::parse(stream, consumed)?,
            Box::new(Self::parse(stream, consumed)?),
        )))
    }
}

def! {@enum ExprAtom<'a> {
    BoolConst(BoolConst),
    IntConst(i32),
    Ident(Ident<'a>),
    Op1(ExprOp1<'a>),
}}

impl<'a> ExprAtom<'a> {
    pub fn span(&self) -> Span {
        match self {
            Self::BoolConst(s) => s.span(),
            Self::IntConst(s) => s.span,
            Self::Ident(s) => s.span,
            Self::Op1(s) => s.span,
        }
    }
}

def! {@struct ExprOp1<'a> {
    op: Op1,
    expr: Box<Expr<'a>>,
}}

def! {@enum BoolConst #[derive(Copy, PartialEq, Eq)] {
    False(False),
    True(True),
}}

impl BoolConst {
    pub fn span(&self) -> Span {
        match self {
            Self::False(s) => s.span,
            Self::True(s) => s.span,
        }
    }
}

def! {@enum Op1 #[derive(Copy, PartialEq, Eq)] {
    BNot(Tilde),
    LNot(EMark),
    Neg(Minus),
}}

impl Op1 {
    pub fn is_type_compatible(&self, ty: &Type) -> bool {
        match self {
            Self::BNot(_) | Self::Neg(_) => matches!(ty, Type::Int),
            Self::LNot(_) => matches!(ty, Type::Bool),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::BNot(s) => s.span,
            Self::LNot(s) => s.span,
            Self::Neg(s) => s.span,
        }
    }
}

def! {@enum Op2 #[derive(Copy, PartialEq, Eq)] {
    Add(Plus),
    Sub(Minus),
    Mul(Star),
    Div(Slash),
    Rem(Percent),
    Lt(Lt),
    Le(LtEq),
    Gt(Gt),
    Ge(GtEq),
    Eq(EqEq),
    Neq(EMarkEq),
    LAnd(AmpersandAmpersand),
    LOr(BarBar),
    BAnd(Ampersand),
    BXor(Hat),
    BOr(Bar),
    Shl(LtLt),
    Shr(GtGt),
}}

impl Op2 {
    pub fn get_return_ty(&self, lhs: &Type, rhs: &Type) -> Option<Type> {
        match self {
            Self::Add(_)
            | Self::Sub(_)
            | Self::Mul(_)
            | Self::Div(_)
            | Self::Rem(_)
            | Self::BAnd(_)
            | Self::BXor(_)
            | Self::BOr(_)
            | Self::Shl(_)
            | Self::Shr(_) => match (lhs, rhs) {
                (Type::Int, Type::Int) => Some(Type::Int),
                _ => None,
            },

            Self::Lt(_) | Self::Le(_) | Self::Gt(_) | Self::Ge(_) => match (lhs, rhs) {
                (Type::Int, Type::Int) => Some(Type::Bool),
                _ => None,
            },

            Self::LAnd(_) | Self::LOr(_) => match (lhs, rhs) {
                (Type::Bool, Type::Bool) => Some(Type::Bool),
                _ => None,
            },

            Self::Eq(_) | Self::Neq(_) => (lhs == rhs).then_some(Type::Bool),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Op2::Add(s) => s.span,
            Op2::Sub(s) => s.span,
            Op2::Mul(s) => s.span,
            Op2::Div(s) => s.span,
            Op2::Rem(s) => s.span,
            Op2::Lt(s) => s.span,
            Op2::Le(s) => s.span,
            Op2::Gt(s) => s.span,
            Op2::Ge(s) => s.span,
            Op2::Eq(s) => s.span,
            Op2::Neq(s) => s.span,
            Op2::LAnd(s) => s.span,
            Op2::LOr(s) => s.span,
            Op2::BAnd(s) => s.span,
            Op2::BXor(s) => s.span,
            Op2::BOr(s) => s.span,
            Op2::Shl(s) => s.span,
            Op2::Shr(s) => s.span,
        }
    }
}

impl Op2 {
    const MAX_PRECEDENCE: i32 = 8;
    pub const fn precedence(&self) -> i32 {
        match self {
            Op2::Mul(_) | Op2::Div(_) | Op2::Rem(_) => 0,
            Op2::Add(_) | Op2::Sub(_) => 1,
            Op2::Shl(_) | Op2::Shr(_) => 2,
            Op2::Lt(_) | Op2::Le(_) | Op2::Gt(_) | Op2::Ge(_) | Op2::Eq(_) | Op2::Neq(_) => 3,
            Op2::BAnd(_) => 4,
            Op2::BXor(_) => 5,
            Op2::BOr(_) => 6,
            Op2::LAnd(_) => 7,
            Op2::LOr(_) => 8,
        }
    }
}

def! {@enum AsgnOp #[derive(Copy, PartialEq, Eq)] {
    Asgn(Eq),
    Sub(MinusEq),
    Add(PlusEq),
    Mul(StarEq),
    Div(SlashEq),
    Rem(PercentEq),
    And(AmpersandEq),
    Or(BarEq),
}}

impl AsgnOp {
    pub fn is_type_compatible(&self, lhs_ty: &Type, rhs_ty: &Type) -> bool {
        if lhs_ty != rhs_ty {
            return false;
        }
        match self {
            Self::Asgn(_) => true,
            Self::Sub(_)
            | Self::Add(_)
            | Self::Mul(_)
            | Self::Div(_)
            | Self::Rem(_)
            | Self::And(_)
            | Self::Or(_) => matches!(lhs_ty, Type::Int),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Asgn(s) => s.span,
            Self::Sub(s) => s.span,
            Self::Add(s) => s.span,
            Self::Mul(s) => s.span,
            Self::Div(s) => s.span,
            Self::Rem(s) => s.span,
            Self::And(s) => s.span,
            Self::Or(s) => s.span,
        }
    }
}

def! {@enum TypeName #[derive(Copy, PartialEq, Eq)] {
    Int(Int),
    Bool(Bool),
}}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
}

impl From<TypeName> for Type {
    fn from(value: TypeName) -> Self {
        value.ty()
    }
}

impl TypeName {
    pub fn span(&self) -> &Span {
        match self {
            Self::Int(Spanned { span, .. }) | Self::Bool(Spanned { span, .. }) => span,
        }
    }

    pub const fn ty(&self) -> Type {
        match self {
            Self::Int(_) => Type::Int,
            Self::Bool(_) => Type::Bool,
        }
    }
}

def! {@tokens(Token, core::convert::identity)
    ParenL, ParenR, BraceL, BraceR, Semicolon, QMark, Colon, Tilde, Eq, EMark, Lt, Gt,
    Minus, Plus, Star, Slash, Percent, Ampersand, Bar, EqEq, EMarkEq, LtEq, Hat, HatEq,
    GtEq, MinusEq, PlusEq, StarEq, SlashEq, PercentEq, AmpersandEq, BarEq,
    AmpersandAmpersand, BarBar, LtLt, GtGt,
}

def! {@tokens(Keyword, Token::Keyword)
    Struct, If, Else, While, For, Continue, Break, Return, Assert, True, False,
    Null, Print, Read, Alloc, AllocArray, Int, Bool, Void, Char, String,
}
