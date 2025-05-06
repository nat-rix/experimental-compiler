use std::{iter::Peekable, path::Path};

use super::span::{Spanned, SrcOffset, SrcPos};
use crate::{error::TokenizeError, parser::span::SrcSpan};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Token<'a> {
    Ident(&'a [u8]),
    Keyword(Keyword),
    Symbol(Symbol),
    IntDec(&'a [u8]),
    IntHex(&'a [u8]),
}

impl<'a> Token<'a> {
    pub fn get_ident(&self) -> Option<&'a [u8]> {
        match self {
            Self::Ident(val) => Some(val),
            _ => None,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    ParenLt,
    ParenRt,
    BraceLt,
    BraceRt,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    Eq,
    Semicolon,
}

#[derive(Debug, Clone)]
struct Tokenizer<'a> {
    content: &'a [u8],
    stream: Peekable<core::slice::Iter<'a, u8>>,
    prev_off: SrcOffset,
    pos: SrcPos<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(path: &'a Path, content: &'a [u8]) -> Self {
        Self {
            content,
            stream: content.iter().peekable(),
            prev_off: Default::default(),
            pos: SrcPos {
                off: Default::default(),
                path,
            },
        }
    }

    fn pos(&self, off: SrcOffset) -> SrcPos<'a> {
        SrcPos {
            off,
            path: self.pos.path,
        }
    }

    fn advance(&mut self) -> Option<u8> {
        loop {
            let res = self.stream.next().copied();
            self.prev_off = self.pos.off;
            self.pos.off.index += 1;
            self.pos.off.col += 1;
            match res {
                Some(b'\r') => {
                    // just ignore carriage returns
                    continue;
                }
                Some(b'\n') | None => {
                    self.pos.off.line += 1;
                    self.pos.off.col = 0;
                }
                Some(_) => (),
            }
            break res;
        }
    }

    fn peek(&mut self) -> Option<u8> {
        self.stream.peek().copied().copied()
    }

    fn is_next(&mut self, c: u8) -> bool {
        if self.peek() == Some(c) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn next_op(&mut self, eq: Symbol, op: Symbol) -> Token<'a> {
        Token::Symbol(if self.is_next(b'=') { eq } else { op })
    }

    fn consume_block_comment(&mut self) -> Result<(), TokenizeError<'a>> {
        let off = self.pos.off;
        loop {
            let Some((c1, c2)) = self.advance().and_then(|c1| self.peek().map(|c2| (c1, c2)))
            else {
                return Err(TokenizeError::UnclosedBlockComment(self.pos(off)));
            };
            match &[c1, c2] {
                b"/*" => {
                    self.advance();
                    self.consume_block_comment()?;
                }
                b"*/" => {
                    self.advance();
                    break Ok(());
                }
                _ => (),
            }
        }
    }

    fn consume_line_comment(&mut self) {
        let line_nr = self.pos.off.line;
        while line_nr == self.pos.off.line {
            self.advance();
        }
    }

    pub fn consume_ident(&mut self) -> Token<'a> {
        let off = self.prev_off;
        while matches!(
            self.peek(),
            Some(b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
        ) {
            self.advance();
        }
        let ident = &self.content[off.index..self.pos.off.index];
        Token::Keyword(match ident {
            b"struct" => Keyword::Struct,
            b"if" => Keyword::If,
            b"else" => Keyword::Else,
            b"while" => Keyword::While,
            b"for" => Keyword::For,
            b"continue" => Keyword::Continue,
            b"break" => Keyword::Break,
            b"return" => Keyword::Return,
            b"assert" => Keyword::Assert,
            b"true" => Keyword::True,
            b"false" => Keyword::False,
            b"NULL" => Keyword::Null,
            b"print" => Keyword::Print,
            b"read" => Keyword::Read,
            b"alloc" => Keyword::Alloc,
            b"alloc_array" => Keyword::AllocArray,
            b"int" => Keyword::Int,
            b"bool" => Keyword::Bool,
            b"void" => Keyword::Void,
            b"char" => Keyword::Char,
            b"string" => Keyword::String,
            _ => return Token::Ident(ident),
        })
    }

    pub fn consume_num(&mut self, chr: u8) -> Token<'a> {
        let off = self.prev_off;
        if chr == b'0' && matches!(self.peek(), Some(b'x' | b'X')) {
            self.advance();
            let off = self.pos.off;
            while matches!(self.peek(), Some(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')) {
                self.advance();
            }
            Token::IntHex(&self.content[off.index..self.pos.off.index])
        } else {
            while matches!(self.peek(), Some(b'0'..=b'9')) {
                self.advance();
            }
            Token::IntDec(&self.content[off.index..self.pos.off.index])
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Spanned<'a, Token<'a>>>, TokenizeError<'a>> {
        loop {
            let start_off = self.pos.off;
            let Some(chr) = self.advance() else {
                return Ok(None);
            };
            let token = match chr {
                b'(' => Token::Symbol(Symbol::ParenLt),
                b')' => Token::Symbol(Symbol::ParenRt),
                b'{' => Token::Symbol(Symbol::BraceLt),
                b'}' => Token::Symbol(Symbol::BraceRt),
                b'+' => self.next_op(Symbol::AddEq, Symbol::Plus),
                b'-' => self.next_op(Symbol::SubEq, Symbol::Minus),
                b'*' => self.next_op(Symbol::MulEq, Symbol::Mul),
                b'/' => match self.peek() {
                    Some(b'*') => {
                        self.advance();
                        self.consume_block_comment()?;
                        continue;
                    }
                    Some(b'/') => {
                        self.advance();
                        self.consume_line_comment();
                        continue;
                    }
                    Some(b'=') => {
                        self.advance();
                        Token::Symbol(Symbol::DivEq)
                    }
                    _ => Token::Symbol(Symbol::Div),
                },
                b'%' => self.next_op(Symbol::ModEq, Symbol::Mod),
                b'=' => Token::Symbol(Symbol::Eq),
                b';' => Token::Symbol(Symbol::Semicolon),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.consume_ident(),
                b'0'..=b'9' => self.consume_num(chr),
                b' ' | b'\t' | b'\r' | b'\n' => continue,
                0x80..=0xff => {
                    return Err(TokenizeError::InvalidAsciiChar(chr, self.pos(start_off)));
                }
                _ => return Err(TokenizeError::UnexpectedAsciiChar(chr, self.pos(start_off))),
            };
            break Ok(Some(Spanned {
                val: token,
                span: SrcSpan {
                    start: start_off,
                    end: self.prev_off,
                    path: self.pos.path,
                },
            }));
        }
    }

    pub fn iter(
        mut self,
    ) -> impl Iterator<Item = Result<Spanned<'a, Token<'a>>, TokenizeError<'a>>> {
        std::iter::from_fn(move || self.next_token().transpose())
    }
}

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    tokenizer: Tokenizer<'a>,
    buffer: Option<Option<Spanned<'a, Token<'a>>>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(path: &'a Path, content: &'a [u8]) -> Self {
        Self {
            tokenizer: Tokenizer::new(path, content),
            buffer: None,
        }
    }

    pub fn peek(&mut self) -> Result<Option<Spanned<'a, Token<'a>>>, TokenizeError<'a>> {
        if self.buffer.is_none() {
            self.buffer = Some(self.tokenizer.next_token()?);
        }
        Ok(self.buffer.unwrap())
    }

    pub fn advance(&mut self) -> Result<Option<Spanned<'a, Token<'a>>>, TokenizeError<'a>> {
        if let Some(buffer) = self.buffer.take() {
            Ok(buffer)
        } else {
            self.tokenizer.next_token()
        }
    }
}
