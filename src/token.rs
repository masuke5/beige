use std::borrow::Cow;
use std::fmt;

use crate::id::Id;
use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    True,
    False,
    If,
    Then,
    Else,
    Let,
    Public,
    Match,
    With,
    Do,
    Const,
    Fn,
    Type,
    Not,
    Or,
    And,
}

impl Keyword {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            "if" => Some(Self::If),
            "then" => Some(Self::Then),
            "else" => Some(Self::Else),
            "let" => Some(Self::Let),
            "pub" => Some(Self::Public),
            "match" => Some(Self::Match),
            "with" => Some(Self::With),
            "do" => Some(Self::Do),
            "const" => Some(Self::Const),
            "fn" => Some(Self::Fn),
            "type" => Some(Self::Type),
            "not" => Some(Self::Not),
            "or" => Some(Self::Or),
            "and" => Some(Self::And),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            Self::True => "true",
            Self::False => "false",
            Self::If => "if",
            Self::Then => "then",
            Self::Else => "else",
            Self::Let => "let",
            Self::Public => "pub",
            Self::Match => "match",
            Self::With => "with",
            Self::Do => "do",
            Self::Const => "const",
            Self::Fn => "fn",
            Self::Type => "type",
            Self::Not => "not",
            Self::Or => "or",
            Self::And => "and",
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Int(u64),
    Float(f64),
    String(String),
    Identifier(Id),
    Keyword(Keyword),
    Plus,
    Minus,
    Asterisk,
    Slash,
    BackSlash,
    Percent,
    VerticalBar,
    Ampersand,
    Exclamation,
    Dot,
    Colon,
    Equal,
    At,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LAngle,
    RAngle,
    LessThanOrEqual,
    GreaterThanOrEqual,
    NotEqual,
    RightArrow,
}

pub fn escape_str(s: &str) -> String {
    fn alternate_str(ch: char) -> Option<Cow<'static, str>> {
        Some(match ch {
            '"' => "\\\"".into(),
            '\\' => "\\\\".into(),
            '\0' => "\\0".into(),
            '\n' => "\\n".into(),
            '\r' => "\\r".into(),
            '\t' => "\\t".into(),
            ch if ch as u8 <= 31 => format!("\\x{:x}", ch as u8).into(),
            _ => return None,
        })
    }

    let mut escaped = String::new();

    for ch in s.chars() {
        if let Some(alternate) = alternate_str(ch) {
            escaped.push_str(&alternate)
        } else {
            escaped.push(ch);
        }
    }

    escaped
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(n) if f.alternate() => write!(f, "{}", n),
            Self::Int(_) => write!(f, "int"),
            Self::Float(n) if f.alternate() => write!(f, "{}", n),
            Self::Float(_) => write!(f, "float"),
            Self::String(s) if f.alternate() => write!(f, "\"{}\"", escape_str(&s)),
            Self::String(_) => write!(f, "string"),
            Self::Identifier(id) if f.alternate() => write!(f, "`{}`", id),
            Self::Identifier(_) => write!(f, "identifier"),
            Self::Keyword(kw) => write!(f, "{}", kw),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Asterisk => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::BackSlash => write!(f, "\\"),
            Self::Percent => write!(f, "%"),
            Self::VerticalBar => write!(f, "|"),
            Self::Ampersand => write!(f, "&"),
            Self::Exclamation => write!(f, "!"),
            Self::Dot => write!(f, "."),
            Self::Colon => write!(f, ":"),
            Self::Equal => write!(f, "="),
            Self::At => write!(f, "@"),
            Self::Semicolon => write!(f, ";"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LBracket => write!(f, "["),
            Self::RBracket => write!(f, "]"),
            Self::LAngle => write!(f, "<"),
            Self::RAngle => write!(f, ">"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::GreaterThanOrEqual => write!(f, ">="),
            Self::NotEqual => write!(f, "!="),
            Self::RightArrow => write!(f, "->"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub level: usize,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token {
            kind,
            span,
            level: 0,
        }
    }
}
