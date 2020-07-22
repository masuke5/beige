use std::convert::TryInto;

use crate::id::{Id, IdMap};
use crate::span::Span;
use crate::token::{Keyword, Token, TokenKind};

#[derive(Debug)]
struct Lexer {
    file: Id,
    chars: Vec<char>,
    current_level: usize,

    pos: usize,
    line: usize,
    col: usize,
}

impl Lexer {
    fn new(file: Id, code: &str) -> Self {
        Self {
            file,
            chars: code.chars().collect(),
            current_level: 0,
            pos: 0,
            line: 0,
            col: 0,
        }
    }

    fn current(&self) -> char {
        match self.chars.get(self.pos) {
            Some(ch) => *ch,
            None => '\0',
        }
    }

    fn current_span(&self) -> Span {
        Span {
            file: self.file,
            line: self.line as u32,
            col: self.col as u32,
        }
    }

    fn next(&self) -> char {
        match self.chars.get(self.pos + 1) {
            Some(ch) => *ch,
            None => '\0',
        }
    }

    fn advance(&mut self) {
        match self.current() {
            '\n' => {
                self.pos += 1;
                self.line += 1;
                self.col = 0;
            }
            '\0' => {}
            _ => {
                self.pos += 1;
                self.col += 1;
            }
        }
    }

    #[inline]
    fn skip_until(&mut self, mut f: impl FnMut(char) -> bool) {
        while f(self.current()) {
            if self.current() == '\0' {
                error!(&self.current_span(), "unexpected EOF");
                break;
            }

            self.advance();
        }
    }

    fn skip_whitespaces(&mut self) {
        self.skip_until(Self::is_whitespace);
    }

    fn is_whitespace(ch: char) -> bool {
        match ch {
            ' ' | '\t' | '\r' => true,
            _ => false,
        }
    }

    fn is_identifier(ch: char) -> bool {
        match ch {
            ch if ch.is_alphanumeric() => true,
            '\'' | '_' => true,
            _ => false,
        }
    }

    fn lex_integer(&mut self, radix: u32) -> u64 {
        let mut n = 0u64;
        while self.current().is_digit(radix) {
            let digit = self.current().to_digit(radix).unwrap() as u64;
            n = n * radix as u64 + digit;
            self.advance();
        }

        n
    }

    fn lex_number(&mut self) -> Option<TokenKind> {
        assert!(('0'..='9').contains(&self.current()));

        match self.current() {
            // hexadecimal number
            '0' if self.next_is('x') => {
                self.advance();
                self.advance();
                Some(TokenKind::Int(self.lex_integer(16)))
            }
            // octal number
            '0' if self.next_is('o') => {
                self.advance();
                self.advance();
                Some(TokenKind::Int(self.lex_integer(8)))
            }
            // binary number
            '0' if self.next_is('b') => {
                self.advance();
                self.advance();
                Some(TokenKind::Int(self.lex_integer(2)))
            }
            // decimal number
            _ => {
                let n = self.lex_integer(10);
                if self.current() != '.' {
                    return Some(TokenKind::Int(n));
                }

                self.advance();

                // Parse after the decimal point
                let mut n = n as f64;
                let mut count = 0;
                while self.current().is_digit(10) {
                    let digit = self.current().to_digit(10).unwrap() as f64;
                    n += digit / 10f64.powf(count as f64);

                    self.advance();
                    count += 1;
                }

                Some(TokenKind::Float(n))
            }
        }
    }

    fn lex_escape_sequence(&mut self) -> Option<char> {
        assert_eq!(self.current(), '\\');
        self.advance();

        match self.current() {
            '0' => Some('\0'),
            'n' => Some('\n'),
            't' => Some('\t'),
            'r' => Some('\r'),
            '\\' => Some('\\'),
            'x' => {
                self.advance();

                let span = self.current_span();
                let code = self.lex_integer(16);

                if let Ok(code) = code.try_into() {
                    if let Some(ch) = std::char::from_u32(code) {
                        Some(ch)
                    } else {
                        error!(&self.current_span(), "invalid character code `{:x}`", code);
                        None
                    }
                } else {
                    error!(&span, "too large character code `{:x}`", code);
                    None
                }
            }
            ch => {
                error!(&self.current_span(), "unknown escape sequence `\\{}`", ch);
                None
            }
        }
    }

    fn lex_string(&mut self) -> Option<TokenKind> {
        assert_eq!(self.current(), '"');
        self.advance();

        let mut s = String::new();
        while self.current() != '"' {
            let ch = match self.current() {
                '\\' => self.lex_escape_sequence(),
                ch => {
                    self.advance();
                    Some(ch)
                }
            };

            if let Some(ch) = ch {
                s.push(ch);
            }
        }

        self.advance();

        Some(TokenKind::String(s))
    }

    fn lex_identifier(&mut self) -> Option<TokenKind> {
        assert!(Self::is_identifier(self.current()));

        let mut s = String::new();
        while Self::is_identifier(self.current()) {
            s.push(self.current());
            self.advance();
        }

        let token = Keyword::from_str(&s).map_or_else(
            || TokenKind::Identifier(IdMap::new_id(&s)),
            TokenKind::Keyword,
        );

        Some(token)
    }

    fn symbol(&mut self, kind: TokenKind) -> Option<TokenKind> {
        self.advance();
        Some(kind)
    }

    fn large_symbol(&mut self, kind: TokenKind) -> Option<TokenKind> {
        self.advance();
        self.advance();
        Some(kind)
    }

    fn next_is(&self, ch: char) -> bool {
        self.next() == ch
    }

    fn next_token(&mut self) -> Option<TokenKind> {
        type TK = TokenKind;

        match self.current() {
            '0'..='9' => self.lex_number(),
            '"' => self.lex_string(),
            ch if Self::is_identifier(ch) => self.lex_identifier(),
            '\n' => self.symbol(TK::LF),
            '+' => self.symbol(TK::Plus),
            '-' if self.next_is('>') => self.large_symbol(TK::RightArrow),
            '-' => self.symbol(TK::Minus),
            '*' => self.symbol(TK::Asterisk),
            '/' => self.symbol(TK::Slash),
            '\\' => self.symbol(TK::BackSlash),
            '|' => self.symbol(TK::VerticalBar),
            '&' => self.symbol(TK::Ampersand),
            '!' => self.symbol(TK::Exclamation),
            '.' => self.symbol(TK::Dot),
            ':' => self.symbol(TK::Colon),
            ';' => self.symbol(TK::Semicolon),
            ',' => self.symbol(TK::Comma),
            '=' => self.symbol(TK::Equal),
            '@' => self.symbol(TK::At),
            '(' => self.symbol(TK::LParen),
            ')' => self.symbol(TK::RParen),
            '{' => self.symbol(TK::LBrace),
            '}' => self.symbol(TK::RBrace),
            '[' => self.symbol(TK::LBracket),
            ']' => self.symbol(TK::RBracket),
            '<' if self.next_is('=') => self.large_symbol(TK::LessThanOrEqual),
            '<' if self.next_is('>') => self.large_symbol(TK::NotEqual),
            '<' => self.symbol(TK::LAngle),
            '>' if self.next_is('=') => self.large_symbol(TK::GreaterThanOrEqual),
            '>' => self.symbol(TK::RAngle),
            ch => {
                self.advance();
                error!(&self.current_span(), "invalid character `{}`", ch);
                None
            }
        }
    }

    fn lex(mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        while self.current() != '\0' {
            self.skip_whitespaces();

            let span = self.current_span();
            if let Some(kind) = self.next_token() {
                match tokens.last() {
                    // Avoid to duplicate LF
                    Some(last_token)
                        if last_token.kind == TokenKind::LF && kind == TokenKind::LF => {}
                    _ => {
                        tokens.push(Token::new(kind, span));
                    }
                }
            }

            self.skip_whitespaces();
        }

        tokens.push(Token::new(TokenKind::EOF, self.current_span()));

        tokens
    }
}

// If an indent size is fixed, it will be simple to implement.
fn calc_level(tokens: &mut [Token]) {
    let mut prev_line = 0u32;
    let mut indents = vec![0];

    for token in tokens {
        // If the token is at the beginning of the line
        if token.span.line != prev_line {
            let indent = token.span.col;
            let current_indent = *indents.last().unwrap();

            if let Some(matched_indent_pos) = indents.iter().position(|i| *i == indent) {
                // If unindent
                indents.truncate(matched_indent_pos + 1);
            } else if indent == current_indent {
                // Do nothing
            } else if indent > current_indent {
                indents.push(indent);
            } else {
                error!(&token.span, "unindent does not match any indentation level");
            }
        }

        token.level = indents.len() - 1;
        prev_line = token.span.line;
    }
}

pub fn lex(file: Id, code: &str) -> Vec<Token> {
    let lexer = Lexer::new(file, code);
    let mut tokens = lexer.lex();

    calc_level(&mut tokens);

    tokens
}
