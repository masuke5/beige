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

#[derive(Debug, PartialEq, Clone)]
pub struct Spanned<T> {
    pub kind: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(kind: T, span: Span) -> Spanned<T> {
        Self { kind, span }
    }
}
