use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

/// Offset in a source file.
#[derive(Debug, Clone, Copy, Default)]
pub struct SrcOffset {
    pub line: usize,
    pub col: usize,
    pub index: usize,
}

impl Display for SrcOffset {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

/// Position in a source file including its path.
#[derive(Debug, Clone, Copy)]
pub struct SrcPosGeneric<P> {
    pub off: SrcOffset,
    pub path: P,
}

/// Span in a source file.
/// It spans from `start` to `end` (inclusive).
#[derive(Debug, Clone, Copy)]
pub struct SrcSpanGeneric<P> {
    pub start: SrcOffset,
    pub end: SrcOffset,
    pub path: P,
}

pub type SrcPos<'a> = SrcPosGeneric<&'a Path>;
pub type SrcSpan<'a> = SrcSpanGeneric<&'a Path>;
pub type SrcPosOwned = SrcPosGeneric<PathBuf>;
pub type SrcSpanOwned = SrcSpanGeneric<PathBuf>;

impl<P: ToOwned + ?Sized> SrcPosGeneric<&P> {
    pub fn into_owned(self) -> SrcPosGeneric<<P as ToOwned>::Owned> {
        let Self { off, path } = self;
        SrcPosGeneric {
            off,
            path: path.to_owned(),
        }
    }
}

impl<P: ToOwned + ?Sized> SrcSpanGeneric<&P> {
    pub fn into_owned(self) -> SrcSpanGeneric<<P as ToOwned>::Owned> {
        let Self { start, end, path } = self;
        SrcSpanGeneric {
            start,
            end,
            path: path.to_owned(),
        }
    }
}

impl<P> SrcSpanGeneric<P> {
    pub fn start_pos<B: ?Sized>(&self) -> SrcPosGeneric<&B>
    where
        P: Borrow<B>,
    {
        SrcPosGeneric {
            off: self.start,
            path: self.path.borrow(),
        }
    }

    pub fn end_pos<B: ?Sized>(&self) -> SrcPosGeneric<&B>
    where
        P: Borrow<B>,
    {
        SrcPosGeneric {
            off: self.end,
            path: self.path.borrow(),
        }
    }

    pub fn is_multiline(&self) -> bool {
        self.start.line < self.end.line
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.end.index - self.start.index + 1
    }

    pub fn join<P2>(self, rhs: &SrcSpanGeneric<P2>) -> Self {
        Self {
            end: rhs.end,
            ..self
        }
    }
}

impl<P: Borrow<Path>> Display for SrcPosGeneric<P> {
    fn fmt(&self, f: &mut Formatter) -> core::fmt::Result {
        write!(f, "{} @ {:?}", self.off, self.path.borrow())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<'a, T> {
    pub val: T,
    pub span: SrcSpan<'a>,
}

impl<'a, T> Spanned<'a, T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<'a, U> {
        let Self { val, span } = self;
        Spanned { val: f(val), span }
    }
}

impl<'a, T> core::ops::Deref for Spanned<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.val
    }
}

impl<'a, T> core::ops::DerefMut for Spanned<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.val
    }
}
