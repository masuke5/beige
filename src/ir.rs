use std::fmt;
use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering};

use lazy_static::lazy_static;
use rustc_hash::FxHashMap;

use crate::dump::format_iter;
use crate::id::{Id, IdMap};
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
        write!(f, ".L{}", self.raw())
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
pub enum Op {
    // dst <- src
    Int(Temp, i64),
    Float(Temp, f64),
    Move(Temp, Temp),
    Add(Temp, Temp, Temp),
    Sub(Temp, Temp, Temp),
    Mul(Temp, Temp, Temp),
    Div(Temp, Temp, Temp),
    Mod(Temp, Temp, Temp),
    AddF(Temp, Temp, Temp),
    SubF(Temp, Temp, Temp),
    MulF(Temp, Temp, Temp),
    DivF(Temp, Temp, Temp),
    Not(Temp, Temp),
    Negative(Temp, Temp),
    Load(Temp, Temp),
    Store(Temp, Temp),
    Addr(Temp, Label),
    Func(Temp, Option<Id>, Id),
    Call(Temp, Temp, Vec<Temp>),
    CCall(Temp, Id, Vec<Temp>),
    Label(Label),
    JumpIf(Cmp, Temp, Temp, Label),
    Jump(Label),
    Return(Temp),
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(dst, n) => write!(f, "{} <- {}", dst, n),
            Self::Float(dst, n) => write!(f, "{} <- {}", dst, n),
            Self::Move(dst, src) => write!(f, "{} <- {}", dst, src),
            Self::Add(dst, lhs, rhs) => write!(f, "{} <- {} + {}", dst, lhs, rhs),
            Self::Sub(dst, lhs, rhs) => write!(f, "{} <- {} - {}", dst, lhs, rhs),
            Self::Mul(dst, lhs, rhs) => write!(f, "{} <- {} * {}", dst, lhs, rhs),
            Self::Div(dst, lhs, rhs) => write!(f, "{} <- {} / {}", dst, lhs, rhs),
            Self::Mod(dst, lhs, rhs) => write!(f, "{} <- {} % {}", dst, lhs, rhs),
            Self::AddF(dst, lhs, rhs) => write!(f, "{} <- {} +f {}", dst, lhs, rhs),
            Self::SubF(dst, lhs, rhs) => write!(f, "{} <- {} -f {}", dst, lhs, rhs),
            Self::MulF(dst, lhs, rhs) => write!(f, "{} <- {} *f {}", dst, lhs, rhs),
            Self::DivF(dst, lhs, rhs) => write!(f, "{} <- {} /f {}", dst, lhs, rhs),
            Self::Not(dst, src) => write!(f, "{} <- !{}", dst, src),
            Self::Negative(dst, src) => write!(f, "{} <- -{}", dst, src),
            Self::Load(dst, loc) => write!(f, "{} <- [{}]", dst, loc),
            Self::Store(loc, src) => write!(f, "[{}] <- {}", loc, src),
            Self::Func(dst, Some(module), func) => write!(f, "{} <- {}::{}", dst, module, func),
            Self::Func(dst, None, func) => write!(f, "{} <- $self::{}", dst, func),
            Self::Addr(dst, label) => write!(f, "{} <- &{}", dst, label),
            Self::Call(dst, func, args) => {
                write!(f, "{} <- call {}({})", dst, func, format_iter(args, ", "))
            }
            Self::CCall(dst, name, args) => {
                write!(f, "{} <- ccall {}({})", dst, name, format_iter(args, ", "))
            }
            Self::Label(label) => write!(f, "{}:", label),
            Self::JumpIf(cmp, lhs, rhs, dest) => {
                write!(f, "jump {} if {} {} {}", dest, lhs, cmp, rhs)
            }
            Self::Jump(dest) => write!(f, "jump {}", dest),
            Self::Return(src) => write!(f, "return {}", src),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Id,
    pub params: Vec<Temp>,
    pub body: Vec<Op>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub path: Id,
    pub functions: FxHashMap<Id, Function>,
    pub strings: FxHashMap<Label, String>,
    pub data: FxHashMap<Label, Vec<u8>>,
}

impl Module {
    pub fn new(path: Id) -> Self {
        Self {
            path,
            functions: FxHashMap::default(),
            strings: FxHashMap::default(),
            data: FxHashMap::default(),
        }
    }
}

pub fn dump_module(module: &Module) {
    println!("module {}", module.path);

    for (label, string) in &module.strings {
        println!("  {}: \"{}\"", label, escape_str(string))
    }

    for (label, data) in &module.data {
        println!(
            "  {}: {}",
            label,
            format_iter(data.iter().map(|n| format!("{:02X}", n)), " ")
        );
    }

    for (name, func) in &module.functions {
        println!("  {}({}):", name, format_iter(&func.params, ", "));
        for op in &func.body {
            if let Op::Label(..) = op {
                println!("  {}", op);
            } else {
                println!("    {}", op);
            }
        }
    }
}
