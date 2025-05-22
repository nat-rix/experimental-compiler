use crate::{
    error::TokenizeError,
    span::{Pos, Span, Spanned},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Ident<'a>(pub &'a [u8]);

impl<'a> core::fmt::Display for Ident<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{}", self.0.escape_ascii())
    }
}

impl<'a> core::fmt::Debug for Ident<'a> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    Ident(Ident<'a>),
    Keyword(Keyword),
    IntConst(i32),

    /// `(`
    ParenL,
    /// `)`
    ParenR,
    /// `{`
    BraceL,
    /// `}`
    BraceR,
    /// `;`
    Semicolon,
    /// `?`
    QMark,
    /// `:`
    Colon,
    /// `~`
    Tilde,

    /// `=`
    Eq,
    /// `!`
    EMark,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `&`
    Ampersand,
    /// `|`
    Bar,
    /// `^`
    Hat,

    /// `==`
    EqEq,
    /// `!=`
    EMarkEq,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,
    /// `-=`
    MinusEq,
    /// `+=`
    PlusEq,
    /// `*=`
    StarEq,
    /// `/=`
    SlashEq,
    /// `%=`
    PercentEq,
    /// `&=`
    AmpersandEq,
    /// `|=`
    BarEq,
    /// `^`
    HatEq,

    /// `&&`
    AmpersandAmpersand,
    /// `||`
    BarBar,
    /// `<<`
    LtLt,
    /// `>>`
    GtGt,

    Eof,
}

impl<'a> From<Ident<'a>> for Token<'a> {
    fn from(value: Ident<'a>) -> Self {
        if let Ok(value) = value.try_into() {
            Self::Keyword(value)
        } else {
            Self::Ident(value)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Struct,
    If,
    Else,
    While,
    For,
    Continue,
    Break,
    Return,
    Assert,
    True,
    False,
    Null,
    Print,
    Read,
    Alloc,
    AllocArray,
    Int,
    Bool,
    Void,
    Char,
    String,
}

impl<'a> TryFrom<Ident<'a>> for Keyword {
    type Error = ();
    fn try_from(value: Ident<'a>) -> Result<Self, Self::Error> {
        Ok(match value.0 {
            b"struct" => Self::Struct,
            b"if" => Self::If,
            b"else" => Self::Else,
            b"while" => Self::While,
            b"for" => Self::For,
            b"continue" => Self::Continue,
            b"break" => Self::Break,
            b"return" => Self::Return,
            b"assert" => Self::Assert,
            b"true" => Self::True,
            b"false" => Self::False,
            b"NULL" => Self::Null,
            b"print" => Self::Print,
            b"read" => Self::Read,
            b"alloc" => Self::Alloc,
            b"alloc_array" => Self::AllocArray,
            b"int" => Self::Int,
            b"bool" => Self::Bool,
            b"void" => Self::Void,
            b"char" => Self::Char,
            b"string" => Self::String,
            _ => return Err(()),
        })
    }
}

#[derive(Debug, Clone)]
pub struct TokenIter<'a> {
    bytes: &'a [u8],
    pos: Pos,
}

impl<'a> TokenIter<'a> {
    pub const fn new(bytes: &'a [u8]) -> Self {
        Self {
            bytes,
            pos: Pos::ZERO,
        }
    }

    pub const fn pos(&self) -> Pos {
        self.pos
    }

    fn peek(&self) -> Option<u8> {
        self.bytes.first().copied()
    }

    fn next_char(&mut self) -> Option<u8> {
        let c = *self.bytes.split_off_first()?;
        if c == b'\n' {
            self.pos.increment_line();
        } else {
            self.pos.increment_col();
        }
        Some(c)
    }

    fn next_if(&mut self, p: u8) -> Option<Pos> {
        (self.peek()? == p).then_some(self.pos).inspect(|_| {
            self.next_char();
        })
    }

    fn next_alt_eq(
        &mut self,
        token_no_eq: Token<'a>,
        token_with_eq: Token<'a>,
        pos: Pos,
    ) -> Spanned<Token<'a>> {
        if let Some(end) = self.next_if(b'=') {
            Spanned::new(token_with_eq, Span::from(pos).combine(end.into()))
        } else {
            Spanned::new(token_no_eq, pos)
        }
    }

    fn next_alt_eq_double(
        &mut self,
        token_no_eq: Token<'a>,
        token_with_eq: Token<'a>,
        token_double: Token<'a>,
        double: u8,
        pos: Pos,
    ) -> Spanned<Token<'a>> {
        if let Some(end) = self.next_if(b'=') {
            Spanned::new(token_with_eq, Span::from(pos).combine(end.into()))
        } else if let Some(end) = self.next_if(double) {
            Spanned::new(token_double, Span::from(pos).combine(end.into()))
        } else {
            Spanned::new(token_no_eq, pos)
        }
    }

    fn consume_block_comment(&mut self, start: Pos) -> Result<(), TokenizeError> {
        loop {
            let pos = self.pos;
            match self
                .next_char()
                .ok_or(TokenizeError::UnclosedBlockComment(start))?
            {
                b'/' if self.next_if(b'*').is_some() => self.consume_block_comment(pos)?,
                b'*' if self.next_if(b'/').is_some() => break Ok(()),
                _ => (),
            }
        }
    }

    fn consume_line_comment(&mut self) {
        let pos = self.pos;
        while pos.line == self.pos.line && self.next_char().is_some() {}
    }

    fn consume_ident(&mut self, slice: &'a [u8], start: Pos) -> Spanned<Token<'a>> {
        let mut n = 1;
        let mut end = start;
        while let Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_') = self.peek() {
            end = self.pos;
            n += 1;
            let _ = self.next_char();
        }
        Spanned {
            span: Span { start, end },
            val: Ident(&slice[..n]).into(),
        }
    }

    fn consume_num(&mut self, mut c: u8, pos: Pos) -> Result<Spanned<Token<'a>>, TokenizeError> {
        let mut num: u32 = 0;
        Ok(if c == b'0' {
            if !matches!(self.bytes.first(), Some(b'x' | b'X')) {
                return Ok(Spanned::new(Token::IntConst(0), pos));
            }
            let _ = self.next_char();
            let mut end = self.pos;
            loop {
                let span = Span { start: pos, end };
                let c = match self.peek() {
                    Some(c @ b'0'..=b'9') => c - b'0',
                    Some(c @ b'a'..=b'f') => c - b'a',
                    Some(c @ b'A'..=b'F') => c - b'A',
                    _ if end == self.pos => return Err(TokenizeError::EmptyHex(span)),
                    _ => break Spanned::new(Token::IntConst(num.cast_signed()), span),
                };
                end = self.pos;
                let _ = self.next_char();
                num = num
                    .checked_mul(16)
                    .ok_or(TokenizeError::HexOverflow(span))?;
                num |= u32::from(c);
            }
        } else {
            let mut span = Span {
                start: pos,
                end: pos,
            };
            loop {
                num = num
                    .checked_mul(10)
                    .ok_or(TokenizeError::DecOverflow(span))?;
                num = num
                    .checked_add(u32::from(c - b'0'))
                    .ok_or(TokenizeError::DecOverflow(span))?;
                if let Some(new_c @ b'0'..=b'9') = self.peek() {
                    span.end = self.pos;
                    let _ = self.next_char();
                    c = new_c;
                } else {
                    if num > 0x8000_0000 {
                        return Err(TokenizeError::DecOverflow(span));
                    }
                    break Spanned {
                        span,
                        val: Token::IntConst(num.cast_signed()),
                    };
                }
            }
        })
    }

    pub fn next_token(&mut self) -> Result<Spanned<Token<'a>>, TokenizeError> {
        Ok(loop {
            let old_slice = self.bytes;
            let pos = self.pos;
            let Some(c) = self.next_char() else {
                return Ok(Spanned::new(Token::Eof, pos));
            };
            break match c {
                b'(' => Spanned::new(Token::ParenL, pos),
                b')' => Spanned::new(Token::ParenR, pos),
                b'{' => Spanned::new(Token::BraceL, pos),
                b'}' => Spanned::new(Token::BraceR, pos),
                b';' => Spanned::new(Token::Semicolon, pos),
                b'?' => Spanned::new(Token::QMark, pos),
                b':' => Spanned::new(Token::Colon, pos),
                b'~' => Spanned::new(Token::Tilde, pos),
                b'=' => self.next_alt_eq(Token::Eq, Token::EqEq, pos),
                b'!' => self.next_alt_eq(Token::EMark, Token::EMarkEq, pos),
                b'-' => self.next_alt_eq(Token::Minus, Token::MinusEq, pos),
                b'+' => self.next_alt_eq(Token::Plus, Token::PlusEq, pos),
                b'*' => self.next_alt_eq(Token::Star, Token::StarEq, pos),
                b'^' => self.next_alt_eq(Token::Hat, Token::HatEq, pos),
                b'/' => match self.peek() {
                    Some(b'*') => {
                        let _ = self.next_char();
                        self.consume_block_comment(pos)?;
                        continue;
                    }
                    Some(b'/') => {
                        self.consume_line_comment();
                        continue;
                    }
                    _ => self.next_alt_eq(Token::Slash, Token::SlashEq, pos),
                },
                b'%' => self.next_alt_eq(Token::Percent, Token::PercentEq, pos),
                b'&' => self.next_alt_eq_double(
                    Token::Ampersand,
                    Token::AmpersandEq,
                    Token::AmpersandAmpersand,
                    b'&',
                    pos,
                ),
                b'|' => self.next_alt_eq_double(Token::Bar, Token::BarEq, Token::BarBar, b'|', pos),
                b'<' => self.next_alt_eq_double(Token::Lt, Token::LtEq, Token::LtLt, b'<', pos),
                b'>' => self.next_alt_eq_double(Token::Gt, Token::GtEq, Token::GtGt, b'>', pos),
                b' ' | b'\r' | b'\t' | b'\n' => continue,
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.consume_ident(old_slice, pos),
                c @ b'0'..=b'9' => self.consume_num(c, pos)?,
                c => return Err(TokenizeError::UnexpectedChar(c, pos)),
            };
        })
    }
}
