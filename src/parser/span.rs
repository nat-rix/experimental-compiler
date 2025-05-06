use std::fmt::{Display, Formatter};

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

/// Span in a source file.
/// It spans from `start` to `end` (inclusive).
#[derive(Debug, Clone, Copy)]
pub struct SrcSpan {
    pub start: SrcOffset,
    pub end: SrcOffset,
}

impl SrcSpan {
    pub const fn is_multiline(&self) -> bool {
        self.start.line < self.end.line
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.end.index - self.start.index + 1
    }

    pub fn join(self, rhs: &Self) -> Self {
        Self {
            end: rhs.end,
            ..self
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<T> {
    pub val: T,
    pub span: SrcSpan,
}

impl<T> Spanned<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        let Self { val, span } = self;
        Spanned { val: f(val), span }
    }
}

impl<T> core::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.val
    }
}

impl<T> core::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.val
    }
}
