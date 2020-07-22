use std::fmt;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering};

use lazy_static::lazy_static;
use rustc_hash::FxHashMap;

use crate::dump::{format_iter, print_indent};
use crate::id::Id;
use crate::token::escape_str;

macro_rules! define_unique {
    ($name:ident, $next:ident) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub struct $name(NonZeroU32);

        static $next: AtomicU32 = AtomicU32::new(1);

        impl $name {
            pub fn new() -> Self {
                let id = $next.load(Ordering::Acquire);
                let var = Self(unsafe { NonZeroU32::new_unchecked(id) });
                $next.fetch_add(1, Ordering::Acquire);
                var
            }

            pub fn raw(&self) -> u32 {
                self.0.into()
            }
        }
    };
}

define_unique!(Temp, NEXT_TEMP);
define_unique!(Label, NEXT_LABEL);

impl fmt::Display for Temp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self == *TEMP_FP {
            write!(f, "tFP")
        } else {
            write!(f, "t{}", self.raw())
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "L{}", self.raw())
    }
}

lazy_static! {
    // frame pointer
    pub static ref TEMP_FP: Temp = Temp::new();
}

#[derive(Debug, Clone, PartialEq)]
pub enum Cmp {
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
}

impl fmt::Display for Cmp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LessThan => write!(f, "<"),
            Self::LessThanOrEqual => write!(f, "<="),
            Self::Equal => write!(f, "="),
            Self::NotEqual => write!(f, "<>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(i64),
    Float(f64),
    Temp(Temp),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    AddF(Box<Expr>, Box<Expr>),
    SubF(Box<Expr>, Box<Expr>),
    MulF(Box<Expr>, Box<Expr>),
    DivF(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    Negative(Box<Expr>),
    Load(Box<Expr>),
    Addr(Label),
    Func(Option<Id>, Id),
    Call(Box<Expr>, Vec<Expr>),
    CCall(Id, Vec<Expr>),
    Seq(Vec<Stmt>, Box<Expr>),
    DSeq(Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Temp, Expr),
    Store(Expr, Expr),
    Label(Label),
    JumpIf(Cmp, Expr, Expr, Label),
    Jump(Label),
    Return(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Id,
    pub params: Vec<Temp>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub path: Id,
    pub functions: FxHashMap<Id, Function>,
    pub strings: FxHashMap<Label, String>,
    pub constants: FxHashMap<Label, Expr>,
}

impl Module {
    pub fn new(path: Id) -> Self {
        Self {
            path,
            functions: FxHashMap::default(),
            strings: FxHashMap::default(),
            constants: FxHashMap::default(),
        }
    }
}

pub fn dump_stmt(stmt: &Stmt, indent: usize) {
    assert!(indent >= 1);

    print_indent(indent);

    match stmt {
        Stmt::Expr(temp, expr) => {
            print!("{} <- ", temp);
            dump_expr(expr, indent);
        }
        Stmt::Store(dst, src) => {
            print!("[");
            dump_expr(dst, indent);
            print!("] <- ");
            dump_expr(src, indent);
        }
        Stmt::Label(label) => {
            print!("\r");
            print_indent(indent - 1);
            print!("{}:", label);
        }
        Stmt::JumpIf(cmp, lhs, rhs, label) => {
            print!("jump {} if ", label);
            dump_expr(lhs, indent);
            print!(" {} ", cmp);
            dump_expr(rhs, indent);
        }
        Stmt::Jump(label) => {
            print!("jump {}", label);
        }
        Stmt::Return(expr) => {
            print!("return ");
            dump_expr(expr, indent);
        }
    }
}

pub fn dump_expr(expr: &Expr, indent: usize) {
    fn dump_binop(symbol: &str, lhs: &Expr, rhs: &Expr, indent: usize) {
        dump_expr(lhs, indent);
        print!(" {} ", symbol);
        dump_expr(rhs, indent);
    }

    match expr {
        Expr::Int(n) => print!("{}", n),
        Expr::Float(n) => print!("{}", n),
        Expr::Temp(temp) => print!("{}", temp),
        Expr::Add(lhs, rhs) => dump_binop("+", lhs, rhs, indent),
        Expr::Sub(lhs, rhs) => dump_binop("-", lhs, rhs, indent),
        Expr::Mul(lhs, rhs) => dump_binop("*", lhs, rhs, indent),
        Expr::Div(lhs, rhs) => dump_binop("/", lhs, rhs, indent),
        Expr::Mod(lhs, rhs) => dump_binop("%", lhs, rhs, indent),
        Expr::AddF(lhs, rhs) => dump_binop("f+", lhs, rhs, indent),
        Expr::SubF(lhs, rhs) => dump_binop("f-", lhs, rhs, indent),
        Expr::MulF(lhs, rhs) => dump_binop("f*", lhs, rhs, indent),
        Expr::DivF(lhs, rhs) => dump_binop("f/", lhs, rhs, indent),
        Expr::Not(expr) => {
            print!("!(");
            dump_expr(expr, indent);
            print!(")")
        }
        Expr::Negative(expr) => {
            print!("-(");
            dump_expr(expr, indent);
            print!(")")
        }
        Expr::Load(expr) => {
            print!("[");
            dump_expr(expr, indent);
            print!("]");
        }
        Expr::Addr(label) => print!("&{}", label),
        Expr::Func(None, func) => print!("self::{}", func),
        Expr::Func(Some(module), func) => print!("{}::{}", module, func),
        Expr::Call(func, args) => {
            dump_expr(func, indent);
            print!("(");

            let mut iter = args.iter();
            if let Some(arg) = iter.next() {
                dump_expr(arg, indent);
            }

            for arg in iter {
                print!(", ");
                dump_expr(arg, indent);
            }

            print!(")");
        }
        Expr::CCall(func, args) => {
            print!("*{}*(", func);

            let mut iter = args.iter();
            if let Some(arg) = iter.next() {
                dump_expr(arg, indent);
            }

            for arg in iter {
                print!(", ");
                dump_expr(arg, indent);
            }

            print!(")");
        }
        Expr::Seq(stmts, result) => {
            println!("{{");

            for stmt in stmts {
                dump_stmt(stmt, indent + 1);
                println!();
            }

            print_indent(indent + 1);
            dump_expr(result, indent + 1);

            println!();
            print_indent(indent);
            print!("}}");
        }
        Expr::DSeq(stmts) => {
            println!("{{");

            for stmt in stmts {
                dump_stmt(stmt, indent + 1);
                println!();
            }

            print_indent(indent);
            print!("}}");
        }
    }
}

pub fn dump_module(module: &Module) {
    println!("module {}", module.path);

    for (label, string) in &module.strings {
        println!("  {}: \"{}\"", label, escape_str(string))
    }

    for (label, expr) in &module.constants {
        print!("  {}: ", label,);
        dump_expr(expr, 1);
        println!();
    }

    for (name, func) in &module.functions {
        println!("  {}({}):", name, format_iter(&func.params, ", "));
        print!("  ");
        dump_expr(&func.body, 1);
        println!();
    }
}
