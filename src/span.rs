use std::fmt;

use crate::id::Id;

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub file: Id,
    // Note that it starts from 0
    pub line: u32,
    pub col: u32,
}

impl Span {
    pub fn zero(file: Id) -> Self {
        Self {
            file,
            line: 0,
            col: 0,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{}:{}:{}", self.file, self.line, self.col)
        } else {
            write!(f, "{}:{}", self.line, self.col)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Self { value, span }
    }
}
