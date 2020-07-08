use rustc_hash::FxHashMap;

use crate::dump::print_indent;
use crate::id::Id;
use crate::span::{Span, Spanned};
use crate::token::escape_str;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    And,
    Or,
    // JoinList,
}

impl BinOp {
    pub fn to_symbol(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::LessThan => "<",
            Self::LessThanOrEqual => "<=",
            Self::GreaterThan => ">",
            Self::GreaterThanOrEqual => ">=",
            Self::Equal => "=",
            Self::NotEqual => "<>",
            Self::And => "and",
            Self::Or => "or",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Tuple(Vec<Expr>),
    Record(Vec<(Spanned<Id>, Expr)>),
    Variable(Id),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Let(Id, Box<Expr>),
    LetFun(Id, Function),
    Not(Box<Expr>),
    Negative(Box<Expr>),
    Call(Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Do(Vec<Expr>),
    // TODO: Add match, lambda and list expression
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct RecordField {
    pub name: Spanned<Id>,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeKind {
    Int,
    Float,
    Bool,
    String,
    Name(Id),
    Record(Vec<RecordField>),
    Tuple(Vec<Type>),
    // TODO: Add app and list and algebraic type
}

pub type Type = Spanned<TypeKind>;

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub name: Id,
    pub params: Vec<Spanned<Id>>,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeDef {
    pub name: Id,
    pub body: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Visibility {
    Private,
    Public,
}

impl Visibility {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Private => "",
            Self::Public => "pub",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub path: Id,
    pub constants: FxHashMap<Id, (Visibility, Expr)>,
    pub types: FxHashMap<Id, (Visibility, TypeDef)>,
    pub functions: FxHashMap<Id, (Visibility, Function)>,
}

impl Module {
    pub fn new(path: Id) -> Self {
        Self {
            path,
            constants: FxHashMap::default(),
            types: FxHashMap::default(),
            functions: FxHashMap::default(),
        }
    }
}

pub fn dump_expr(expr: &Expr, indent: usize) {
    type E = ExprKind;

    print_indent(indent);

    match &expr.kind {
        E::Int(n) => println!("{} [{}]", n, expr.span),
        E::Float(n) => println!("{} [{}]", n, expr.span),
        E::Bool(b) => println!("{}", if *b { "true" } else { "false" }),
        E::String(s) => println!("\"{}\", [{}]", escape_str(s), expr.span),
        E::Tuple(exprs) => {
            println!("tuple [{}]", expr.span);
            for expr in exprs {
                dump_expr(expr, indent + 1);
            }
        }
        E::Record(fields) => {
            println!("record [{}]", expr.span);
            for (name, expr) in fields {
                print_indent(indent);
                println!("{}:", name.value);
                dump_expr(expr, indent + 1);
            }
        }
        E::Variable(id) => println!("`{}`", id),
        E::BinOp(binop, lhs, rhs) => {
            println!("{} [{}]", binop.to_symbol(), expr.span);
            dump_expr(lhs, indent + 1);
            dump_expr(rhs, indent + 1);
        }
        E::Let(name, exp) => {
            println!("let {} = [{}]", name, expr.span);
            dump_expr(exp, indent + 1);
        }
        E::LetFun(name, func) => {
            print!("let {}", name);
            for param_name in &func.params {
                print!(" {}", param_name.value);
            }
            println!(" = [{}]", expr.span);

            dump_expr(&func.body, indent + 1);
        }
        E::Not(exp) => {
            println!("not [{}]", expr.span);
            dump_expr(exp, indent + 1)
        }
        E::Negative(exp) => {
            println!("neg [{}]", expr.span);
            dump_expr(exp, indent + 1)
        }
        E::Call(func, arg) => {
            println!("call [{}]", expr.span);
            dump_expr(func, indent + 1);
            dump_expr(arg, indent + 1);
        }
        E::If(cond, then_expr, else_expr) => {
            println!("if [{}]", expr.span);
            dump_expr(cond, indent + 1);

            print_indent(indent);
            println!("then");
            dump_expr(then_expr, indent + 1);

            if let Some(else_expr) = else_expr {
                print_indent(indent);
                println!("else");
                dump_expr(else_expr, indent + 1);
            }
        }
        E::Do(exprs) => {
            println!("do [{}]", expr.span);
            for expr in exprs {
                dump_expr(expr, indent + 1);
            }
        }
    }
}

pub fn dump_type(ty: &Type, indent: usize) {
    type T = TypeKind;

    print_indent(indent);

    match &ty.value {
        T::Int => println!("Int [{}]", ty.span),
        T::Float => println!("Float [{}]", ty.span),
        T::String => println!("String [{}]", ty.span),
        T::Bool => println!("Bool [{}]", ty.span),
        T::Name(name) => println!("Name `{}` [{}]", name, ty.span),
        T::Record(fields) => {
            println!("Record [{}]", ty.span);

            for field in fields {
                print_indent(indent);
                println!("{}:", field.name.value);
                dump_type(&field.ty, indent + 1);
            }
        }
        T::Tuple(types) => {
            println!("Tuple [{}]", ty.span);

            for ty in types {
                dump_type(&ty, indent + 1)
            }
        }
    }
}

pub fn dump_module(module: &Module, indent: usize) {
    print_indent(indent);

    println!("module {}", module.path);

    for (name, (visibility, expr)) in &module.constants {
        print_indent(indent + 1);
        println!("{}const {} =", visibility.as_str(), name);
        dump_expr(expr, indent + 2);
    }

    for (name, (visibility, type_def)) in &module.types {
        print_indent(indent + 1);
        println!("{}type {} =", visibility.as_str(), name);
        dump_type(&type_def.body, indent + 2);
    }

    for (name, (visibility, func)) in &module.functions {
        print_indent(indent + 1);
        print!("{}fn {}", visibility.as_str(), name);
        for param_name in &func.params {
            print!(" {}", param_name.value);
        }
        println!(" =");

        dump_expr(&func.body, indent + 2);
    }
}
