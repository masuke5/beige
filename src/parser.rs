use std::convert::TryFrom;

use lazy_static::lazy_static;

use crate::ast::*;
use crate::id::{reserved_id, Id, IdMap};
use crate::span::{Span, Spanned};
use crate::token::{Keyword, Token, TokenKind};

type T = TokenKind;
type E = ExprKind<Empty>;
type KW = Keyword;

lazy_static! {
    static ref TYPEID_INT: Id = IdMap::new_id("Int");
    static ref TYPEID_FLOAT: Id = IdMap::new_id("Float");
    static ref TYPEID_BOOL: Id = IdMap::new_id("Bool");
    static ref TYPEID_STRING: Id = IdMap::new_id("String");
    static ref TYPEID_LIST: Id = IdMap::new_id("List");
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    // Helper functions {{{

    fn curr(&self) -> &T {
        &self.tokens[self.pos].kind
    }

    fn curr_span(&self) -> &Span {
        &self.tokens[self.pos].span
    }

    fn curr_level(&self) -> usize {
        self.tokens[self.pos].level
    }

    fn prev_span(&self) -> &Span {
        &self.tokens[self.pos - 1].span
    }

    fn next(&mut self) {
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
    }

    fn eat(&mut self, token: &T) -> bool {
        if self.curr() == token {
            self.next();
            true
        } else {
            false
        }
    }

    fn eat_assert(&mut self, token: &T) {
        assert!(self.curr() == token);
        self.next();
    }

    fn expect(&mut self, token: &T) -> Option<()> {
        if self.eat(token) {
            Some(())
        } else {
            self.skip_to(&[token.clone()]);
            self.next();
            None
        }
    }

    fn expect_identifier(&mut self) -> Option<Id> {
        if let T::Identifier(id) = self.curr() {
            let id = *id;
            self.next();
            Some(id)
        } else {
            error!(self.curr_span(), "expected identifier");
            None
        }
    }

    fn skip_to(&mut self, token: &[T]) {
        while !token.contains(self.curr()) {
            if *self.curr() == T::EOF {
                break;
            }

            self.next();
        }
    }

    fn skip<T>(
        &mut self,
        func: impl FnOnce(&mut Self) -> Option<T>,
        mut is_stop: impl FnMut(&TokenKind) -> bool,
    ) -> Option<T> {
        let result = func(self);

        if result.is_none() {
            while !is_stop(self.curr()) {
                if *self.curr() == TokenKind::EOF {
                    break;
                }

                self.next();
            }
        }

        result
    }

    fn eat_lf(&mut self) {
        self.eat(&T::LF);
        assert_ne!(self.curr(), &T::LF);
    }

    // }}}

    // Common {{{

    fn parse_function_from_name(&mut self, name: Id) -> Option<UntypedFunction> {
        // Parse the parameters
        let mut params = Vec::new();
        while let T::Identifier(name) = self.curr() {
            params.push(Spanned::new(*name, self.curr_span().clone()));
            self.next();
        }

        self.expect(&T::Equal);

        let body = self.parse_expr()?;

        Some(Function {
            name,
            params,
            param_types: Vec::new(),
            body: Box::new(body),
        })
    }

    #[inline]
    fn parse_multiple_expr<E>(
        &mut self,
        mut parse: impl FnMut(&mut Self) -> Option<E>,
        delimiter: &T,
        terminator: &T,
    ) -> Vec<E> {
        let mut values = Vec::new();

        loop {
            if self.eat(terminator) {
                break;
            }

            if let Some(value) = parse(self) {
                values.push(value);
            }

            if self.eat(terminator) {
                break;
            }

            if !self.eat(delimiter) {
                error!(
                    &self.curr_span(),
                    "expected {} or {}", delimiter, terminator
                );
                break;
            }
        }

        values
    }

    // }}}

    // Expression {{{

    #[inline]
    fn parse_binop(
        &mut self,
        mut parse: impl FnMut(&mut Self) -> Option<UntypedExpr>,
        allow_join: bool,
        rules: &[(T, BinOp)],
    ) -> Option<UntypedExpr> {
        let mut lhs = parse(self)?;

        'outer: loop {
            for (token, binop) in rules {
                if self.eat(token) {
                    let span = self.prev_span().clone();
                    let expr = E::BinOp(*binop, Box::new(lhs), Box::new(parse(self)?));
                    lhs = UntypedExpr::new(expr, span);

                    if allow_join {
                        continue 'outer;
                    } else {
                        return Some(lhs);
                    }
                }
            }

            break;
        }

        Some(lhs)
    }

    #[inline]
    fn parse_unary(
        &mut self,
        mut parse: impl FnMut(&mut Self) -> Option<UntypedExpr>,
        rules: &[(T, fn(Box<UntypedExpr>) -> E)],
    ) -> Option<UntypedExpr> {
        for (token, gen_expr) in rules {
            if self.eat(token) {
                let span = self.prev_span().clone();
                let expr = parse(self)?;
                let expr = gen_expr(Box::new(expr));
                return Some(UntypedExpr::new(expr, span));
            }
        }

        parse(self)
    }

    // int, bool, string, variable, tuple, record
    // call, - (unary)
    // not
    // *, /, %
    // +, -
    // <, <=, >, >=, =, <>
    // and
    // or
    // if, do, let

    fn parse_record_field(&mut self) -> Option<(Spanned<Id>, UntypedExpr)> {
        let name_span = self.curr_span().clone();
        let name = self
            .skip(Self::expect_identifier, |t| *t == T::Colon)
            .unwrap_or(*reserved_id::UNKNOWN);
        let name = Spanned::new(name, name_span);

        self.expect(&T::Colon);

        let expr = self.skip(Self::parse_expr, |t| *t == T::Comma)?;

        Some((name, expr))
    }

    fn parse_primary(&mut self) -> Option<UntypedExpr> {
        let span = self.curr_span().clone();
        let kind = match self.curr() {
            T::Int(n) => {
                let n = *n;
                self.next();

                let n = match i64::try_from(n) {
                    Ok(n) => n,
                    Err(_) => {
                        error!(&span, "too large integer `{}` (> {})", n, std::i64::MAX);
                        return None;
                    }
                };

                E::Int(n)
            }
            T::Float(n) => {
                let n = *n;
                self.next();

                E::Float(n)
            }
            T::Keyword(KW::True) => {
                self.next();
                E::Bool(true)
            }
            T::Keyword(KW::False) => {
                self.next();
                E::Bool(false)
            }
            T::String(s) => {
                let s = s.clone();
                self.next();

                E::String(s)
            }
            T::Identifier(id) => {
                let id = *id;
                self.next();

                E::Variable(id)
            }
            T::LParen => {
                self.next();

                let expr = self.skip(Self::parse_expr, |t| *t == T::RParen || *t == T::Comma);

                // Parse as a tuple if see a comma
                if self.eat(&T::Comma) {
                    let mut exprs =
                        self.parse_multiple_expr(Self::parse_expr, &T::Comma, &T::RParen);
                    let first_expr = expr?;

                    exprs.insert(0, first_expr);

                    E::Tuple(exprs)
                } else {
                    self.expect(&T::RParen);

                    return expr;
                }
            }
            T::LBrace => {
                self.next();

                let fields =
                    self.parse_multiple_expr(Self::parse_record_field, &T::Comma, &T::RBrace);

                E::Record(fields)
            }
            _ => {
                error!(
                    &span,
                    "expected int, float, true, false, identifier, ( or {{"
                );
                return None;
            }
        };

        Some(UntypedExpr::new(kind, span))
    }

    fn token_is_arg(token: &T) -> bool {
        match token {
            T::Int(..)
            | T::Float(..)
            | T::String(..)
            | T::Identifier(..)
            | T::LParen
            | T::LBrace => true,
            _ => false,
        }
    }

    fn parse_call(&mut self) -> Option<UntypedExpr> {
        let parse = Self::parse_primary;

        let mut func = parse(self)?;
        while Self::token_is_arg(self.curr()) {
            let arg = parse(self)?;
            let span = arg.span.clone();
            let expr = E::Call(Box::new(func), Box::new(arg));
            func = UntypedExpr::new(expr, span);
        }

        Some(func)
    }

    fn parse_unaries(&mut self) -> Option<UntypedExpr> {
        self.parse_unary(
            Self::parse_call,
            &[(T::Minus, E::Negative), (T::Keyword(KW::Not), E::Not)],
        )
    }

    fn parse_mul(&mut self) -> Option<UntypedExpr> {
        self.parse_binop(
            Self::parse_unaries,
            true,
            &[
                (T::Asterisk, BinOp::Mul),
                (T::Slash, BinOp::Div),
                (T::Percent, BinOp::Mod),
            ],
        )
    }

    fn parse_add(&mut self) -> Option<UntypedExpr> {
        self.parse_binop(
            Self::parse_mul,
            true,
            &[(T::Plus, BinOp::Add), (T::Minus, BinOp::Sub)],
        )
    }

    fn parse_relational(&mut self) -> Option<UntypedExpr> {
        self.parse_binop(
            Self::parse_add,
            false,
            &[
                (T::LAngle, BinOp::LessThan),
                (T::LessThanOrEqual, BinOp::LessThanOrEqual),
                (T::RAngle, BinOp::GreaterThan),
                (T::GreaterThanOrEqual, BinOp::GreaterThanOrEqual),
                (T::Equal, BinOp::Equal),
                (T::NotEqual, BinOp::NotEqual),
            ],
        )
    }

    fn parse_and(&mut self) -> Option<UntypedExpr> {
        self.parse_binop(
            Self::parse_relational,
            true,
            &[(T::Keyword(KW::And), BinOp::And)],
        )
    }

    fn parse_or(&mut self) -> Option<UntypedExpr> {
        self.parse_binop(Self::parse_and, true, &[(T::Keyword(KW::Or), BinOp::Or)])
    }

    fn parse_if(&mut self) -> Option<UntypedExpr> {
        let span = self.curr_span().clone();
        self.eat_assert(&T::Keyword(KW::If));

        let cond = self.skip(Self::parse_expr, |t| *t == T::Keyword(KW::Then));

        self.expect(&T::Keyword(KW::Then));

        let then_clause = self.skip(Self::parse_expr, |t| {
            *t == T::Keyword(KW::Else) || *t == T::LF
        });

        let else_clause = if self.eat(&T::Keyword(KW::Else)) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };

        let cond = cond?;
        let then_clause = then_clause?;

        let expr = E::If(Box::new(cond), Box::new(then_clause), else_clause);
        Some(UntypedExpr::new(expr, span))
    }

    fn parse_do(&mut self) -> Option<UntypedExpr> {
        let span = self.curr_span().clone();
        let level = self.curr_level() + 1;
        self.eat_assert(&T::Keyword(KW::Do));

        if !self.eat(&T::LF) {
            error!(&span, "no line breaks");
            self.skip_to(&[T::LF]);
            self.next();
        }

        let mut exprs = Vec::new();

        while self.curr_level() == level {
            // Check indentation
            if self.curr_level() > level {
                error!(self.curr_span(), "too big indentation");
                return None;
            }

            let expr = self.skip(Self::parse_expr, |t| *t == T::LF);
            if let Some(expr) = expr {
                exprs.push(expr);
            }

            self.eat_lf();
        }

        if self.curr_level() > level {
            error!(self.curr_span(), "too big indentation");
        }

        if exprs.is_empty() {
            error!(&span, "do expressions must have one expression at least");
            return None;
        }

        let expr = E::Do(exprs);
        Some(UntypedExpr::new(expr, span))
    }

    fn parse_let(&mut self) -> Option<UntypedExpr> {
        let span = self.curr_span().clone();
        self.eat_assert(&T::Keyword(KW::Let));

        let name = self
            .skip(Self::expect_identifier, |t| {
                t.is_identifier() || *t == T::Equal
            })
            .unwrap_or(*reserved_id::UNKNOWN);

        // Parse as a function definition if see an identifier
        if self.curr().is_identifier() {
            let func = self.parse_function_from_name(name)?;
            let expr = E::LetFun(name, func);
            Some(UntypedExpr::new(expr, span))
        } else if self.eat(&T::Equal) {
            let expr = self.parse_expr()?;
            let expr = E::Let(name, Box::new(expr));
            Some(UntypedExpr::new(expr, span))
        } else {
            error!(self.curr_span(), "expected identifier or =");
            None
        }
    }

    fn parse_ifl(&mut self) -> Option<UntypedExpr> {
        let parse = Self::parse_or;

        match self.curr() {
            T::Keyword(KW::If) => self.parse_if(),
            T::Keyword(KW::Do) => self.parse_do(),
            T::Keyword(KW::Let) => self.parse_let(),
            _ => parse(self),
        }
    }

    fn parse_expr(&mut self) -> Option<UntypedExpr> {
        self.parse_ifl()
    }

    // }}}

    // Type {{{

    fn parse_type_record_field(&mut self) -> Option<RecordField> {
        // Parse the field name and if it fails, skip to ':'
        let name_span = self.curr_span().clone();
        let name = self
            .skip(Self::expect_identifier, |t| *t == T::Colon)
            .unwrap_or(*reserved_id::UNKNOWN);
        let name = Spanned::new(name, name_span);

        self.expect(&T::Colon);

        let ty = self.skip(Self::parse_type, |t| *t == T::Comma)?;

        Some(RecordField { name, ty })
    }

    fn parse_type_record(&mut self) -> Option<TypeKind> {
        self.eat_assert(&T::LBrace);

        let fields = self.parse_multiple_expr(Self::parse_type_record_field, &T::Comma, &T::RBrace);
        let ty = TypeKind::Record(fields);

        Some(ty)
    }

    fn parse_type_tuple(&mut self) -> Option<TypeKind> {
        self.eat_assert(&T::LParen);

        let types = self.parse_multiple_expr(Self::parse_type, &T::Comma, &T::RParen);
        let ty = TypeKind::Tuple(types);

        Some(ty)
    }

    fn parse_type(&mut self) -> Option<Type> {
        let span = self.curr_span().clone();
        let kind = match self.curr() {
            T::Identifier(name) => {
                let name = *name;
                self.next();
                match name {
                    id if id == *TYPEID_INT => TypeKind::Int,
                    id if id == *TYPEID_FLOAT => TypeKind::Float,
                    id if id == *TYPEID_BOOL => TypeKind::Bool,
                    id if id == *TYPEID_STRING => TypeKind::String,
                    // id if *id == *TYPEID_LIST => TypeKind::List,
                    name => TypeKind::Name(name),
                }
            }
            T::LParen => self.parse_type_tuple()?,
            T::LBrace => self.parse_type_record()?,
            _ => {
                error!(self.curr_span(), "expected identifier, ( or {{");
                return None;
            }
        };

        Some(Type::new(kind, span))
    }

    // }}}

    // Module {{{

    fn parse_module_item(&mut self, module: &mut UntypedModule) {
        // Parse the visibility
        let is_public = self.eat(&T::Keyword(KW::Public));
        let visibility = if is_public {
            Visibility::Public
        } else {
            Visibility::Private
        };

        match self.curr() {
            // Parse as a constant
            T::Keyword(KW::Const) => {
                self.next();

                // Parse the type name, if it fails, skip to '='
                let name = self
                    .skip(Self::expect_identifier, |t| *t == T::Equal)
                    .unwrap_or(*reserved_id::UNKNOWN);

                self.expect(&T::Equal);

                // Parse the expression
                let expr = match self.parse_expr() {
                    Some(expr) => expr,
                    None => return,
                };

                module.constants.insert(name, (visibility, expr));
            }
            // Parse as a type definition
            T::Keyword(KW::Type) => {
                self.next();

                // Parse the type name, if it fails, skip to '='
                let name = self
                    .skip(Self::expect_identifier, |t| *t == T::Equal)
                    .unwrap_or(*reserved_id::UNKNOWN);

                self.expect(&T::Equal);

                // Parse the type body
                let body = match self.parse_type() {
                    Some(ty) => ty,
                    None => return,
                };

                let tydef = TypeDef { name, body };
                module.types.insert(name, (visibility, tydef));
            }
            // Parse as a module function
            T::Keyword(KW::Fn) => {
                self.next();

                // Parse the function name and if it fails, skip to an identifier or '='
                let name = self
                    .skip(Self::expect_identifier, |t| {
                        t.is_identifier() || *t == T::Equal
                    })
                    .unwrap_or(*reserved_id::UNKNOWN);

                let func = match self.parse_function_from_name(name) {
                    Some(func) => func,
                    None => return,
                };

                module.functions.push((visibility, func));
            }
            _ => {
                error!(self.curr_span(), "expected const, type or fn");
                self.skip_to(&[
                    T::Keyword(KW::Const),
                    T::Keyword(KW::Type),
                    T::Keyword(KW::Fn),
                ]);
            }
        }
    }

    fn parse_module(&mut self, path: Id) -> Option<UntypedModule> {
        let mut module = UntypedModule::new(path);

        while *self.curr() != T::EOF {
            self.eat_lf();
            self.parse_module_item(&mut module);
            self.eat_lf();
        }

        Some(module)
    }

    // }}}

    fn parse(mut self, path: Id) -> Option<UntypedModule> {
        self.parse_module(path)
    }
}

pub fn parse(tokens: Vec<Token>, path: Id) -> Option<UntypedModule> {
    let parser = Parser::new(tokens);
    parser.parse(path)
}
