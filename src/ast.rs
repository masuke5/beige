use std::fmt::{self, Debug, Display};

use rustc_hash::FxHashMap;

use crate::dump::print_indent;
use crate::id::Id;
use crate::span::{Span, Spanned};
use crate::token::escape_str;

#[derive(Debug, PartialEq, Clone)]
pub struct Empty;

impl fmt::Display for Empty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

pub type UntypedExpr = Expr<Empty>;
pub type UntypedExprKind = ExprKind<Empty>;
pub type UntypedFunction = Function<Empty>;
pub type UntypedModule = Module<Empty>;

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
pub enum ExprKind<T> {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Tuple(Vec<Expr<T>>),
    Record(Vec<(Spanned<Id>, Expr<T>)>),
    Variable(Id),
    BinOp(BinOp, Box<Expr<T>>, Box<Expr<T>>),
    Let(Id, Box<Expr<T>>),
    LetFun(Id, Function<T>),
    Not(Box<Expr<T>>),
    Negative(Box<Expr<T>>),
    Call(Box<Expr<T>>, Box<Expr<T>>),
    If(Box<Expr<T>>, Box<Expr<T>>, Option<Box<Expr<T>>>),
    Do(Vec<Expr<T>>),
    // TODO: Add match, lambda and list expression
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr<T> {
    pub kind: ExprKind<T>,
    pub span: Span,
    pub ty: T,
}

impl<T> Expr<T> {
    pub fn new(kind: ExprKind<Empty>, span: Span) -> Expr<Empty> {
        Expr {
            kind,
            span,
            ty: Empty,
        }
    }

    pub fn with_ty(kind: ExprKind<T>, span: Span, ty: T) -> Expr<T> {
        Expr { kind, span, ty }
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
pub struct Function<T> {
    pub name: Id,
    pub params: Vec<Spanned<Id>>,
    pub param_types: Vec<T>,
    pub body: Box<Expr<T>>,
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
pub struct Module<T: Debug + PartialEq + Clone> {
    pub path: Id,
    pub constants: FxHashMap<Id, (Visibility, Expr<T>)>,
    pub types: FxHashMap<Id, (Visibility, TypeDef)>,
    pub functions: Vec<(Visibility, Function<T>)>,
}

impl<T: Debug + PartialEq + Clone> Module<T> {
    pub fn new(path: Id) -> Self {
        Self {
            path,
            constants: FxHashMap::default(),
            types: FxHashMap::default(),
            functions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DumpOption {
    pub show_type: bool,
    pub show_span: bool,
}

pub fn dump_expr<T: Display>(expr: &Expr<T>, indent: usize, option: &DumpOption) {
    type E<T> = ExprKind<T>;

    print_indent(indent);

    if option.show_type || option.show_span {
        print!("\x1b[93m");

        if option.show_type {
            print!("{{{}}} ", expr.ty);
        }

        if option.show_span {
            print!("[{}] ", expr.span);
        }

        println!("\x1b[0m");
        print_indent(indent);
    }

    match &expr.kind {
        E::Int(n) => println!("{}", n),
        E::Float(n) => println!("{}", n),
        E::Bool(b) => println!("{}", if *b { "true" } else { "false" }),
        E::String(s) => println!("\"{}\"", escape_str(s)),
        E::Tuple(exprs) => {
            println!("tuple");
            for expr in exprs {
                dump_expr(expr, indent + 1, option);
            }
        }
        E::Record(fields) => {
            println!("record");
            for (name, expr) in fields {
                print_indent(indent);
                println!("{}:", name.value);
                dump_expr(expr, indent + 1, option);
            }
        }
        E::Variable(id) => println!("`{}`", id),
        E::BinOp(binop, lhs, rhs) => {
            println!("{}", binop.to_symbol());
            dump_expr(lhs, indent + 1, option);
            dump_expr(rhs, indent + 1, option);
        }
        E::Let(name, exp) => {
            println!("let {} =", name);
            dump_expr(exp, indent + 1, option);
        }
        E::LetFun(name, func) => {
            print!("let {}", name);
            for param_name in &func.params {
                print!(" {}", param_name.value);
            }
            println!(" =");

            dump_expr(&func.body, indent + 1, option);
        }
        E::Not(exp) => {
            println!("not");
            dump_expr(exp, indent + 1, option)
        }
        E::Negative(exp) => {
            println!("neg");
            dump_expr(exp, indent + 1, option)
        }
        E::Call(func, arg) => {
            println!("call");
            dump_expr(func, indent + 1, option);
            dump_expr(arg, indent + 1, option);
        }
        E::If(cond, then_expr, else_expr) => {
            println!("if");
            dump_expr(cond, indent + 1, option);

            print_indent(indent);
            println!("then");
            dump_expr(then_expr, indent + 1, option);

            if let Some(else_expr) = else_expr {
                print_indent(indent);
                println!("else");
                dump_expr(else_expr, indent + 1, option);
            }
        }
        E::Do(exprs) => {
            println!("do");
            for expr in exprs {
                dump_expr(expr, indent + 1, option);
            }
        }
    }
}

pub fn dump_type(ty: &Type, indent: usize, option: &DumpOption) {
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
                dump_type(&field.ty, indent + 1, option);
            }
        }
        T::Tuple(types) => {
            println!("Tuple [{}]", ty.span);

            for ty in types {
                dump_type(&ty, indent + 1, option)
            }
        }
    }
}

pub fn dump_module<T: Debug + PartialEq + Clone + Display>(
    module: &Module<T>,
    indent: usize,
    option: &DumpOption,
) {
    print_indent(indent);

    println!("module {}", module.path);

    for (name, (visibility, expr)) in &module.constants {
        print_indent(indent + 1);
        println!("{}const {} =", visibility.as_str(), name);
        dump_expr(expr, indent + 2, option);
    }

    for (name, (visibility, type_def)) in &module.types {
        print_indent(indent + 1);
        println!("{}type {} =", visibility.as_str(), name);
        dump_type(&type_def.body, indent + 2, option);
    }

    for (visibility, func) in &module.functions {
        print_indent(indent + 1);
        print!("{}fn {}", visibility.as_str(), func.name);
        for param_name in &func.params {
            print!(" {}", param_name.value);
        }
        println!(" = ");

        dump_expr(&func.body, indent + 2, option);
    }
}
