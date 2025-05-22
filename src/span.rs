#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub const ZERO: Pos = Pos { line: 0, col: 0 };

    pub const fn increment_col(&mut self) {
        self.col += 1;
    }

    pub const fn increment_line(&mut self) {
        self.line += 1;
        self.col = 0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn combine(self, rhs: Self) -> Self {
        Self {
            start: self.start.min(rhs.start),
            end: self.end.max(rhs.end),
        }
    }

    pub const fn is_singleline(&self) -> bool {
        self.start.line == self.end.line
    }
}

impl From<Pos> for Span {
    fn from(value: Pos) -> Self {
        Self {
            start: value,
            end: value,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}

impl<T: core::cmp::PartialEq> core::cmp::PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.val == other.val
    }
}
impl<T: core::cmp::Eq> core::cmp::Eq for Spanned<T> {}

impl<T: core::fmt::Debug> core::fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.val.fmt(f)
    }
}

impl<T> Spanned<T> {
    pub fn new(val: T, span: impl Into<Span>) -> Self {
        Self {
            val,
            span: span.into(),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            val: f(self.val),
        }
    }
}

impl<T, E> Spanned<Result<T, E>> {
    pub fn transpose(self) -> Result<Spanned<T>, Spanned<E>> {
        match self.val {
            Ok(val) => Ok(Spanned::new(val, self.span)),
            Err(err) => Err(Spanned::new(err, self.span)),
        }
    }
}

impl<T> core::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.val
    }
}

impl<T> core::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.val
    }
}
