use std::ops::{Add, AddAssign};

use rustc_hash::FxHashMap;

use crate::ast::BinOp;
use crate::id::{Id, IdMap};
use crate::ir::{Cmp, Expr as IRExpr, Function, Label, Module, Stmt as IRStmt, Temp, TEMP_FP};
use crate::scope_map::ScopeMap;
use crate::typing::{TypedExpr, TypedExprKind, TypedFunction, TypedModule};

#[derive(Debug, Clone, PartialEq)]
enum Variable {
    Local(usize),
    Param(Temp),
}

#[derive(Debug)]
struct Generator {
    strings: FxHashMap<Label, String>,
    vars: ScopeMap<Id, Variable>,
    stack_size: usize,

    functions: FxHashMap<Id, Function>,
    scope_functions: ScopeMap<Id, Id>,
    prefix: String,
}

impl Generator {
    fn new() -> Self {
        Self {
            strings: FxHashMap::default(),
            vars: ScopeMap::new(),
            stack_size: 0,
            functions: FxHashMap::default(),
            scope_functions: ScopeMap::new(),
            prefix: String::new(),
        }
    }

    fn push_scope(&mut self) {
        self.vars.push_scope();
        self.scope_functions.push_scope();
    }

    fn pop_scope(&mut self) {
        self.vars.pop_scope();
        self.scope_functions.pop_scope();
    }

    fn find_func_in_scope(&self, name: Id) -> Option<&Function> {
        match self.scope_functions.get(&name) {
            Some(name) => Some(&self.functions[name]),
            None => None,
        }
    }

    fn create_func(&mut self, mut func: Function) {
        let original_name = func.name;
        let new_func_name = IdMap::new_id(&format!("{}{}", self.prefix, func.name));
        func.name = new_func_name;

        self.functions.insert(new_func_name, func);
        self.scope_functions.insert(original_name, new_func_name);
    }

    fn alloc_var(&mut self, name: Id) -> usize {
        let loc = self.stack_size;
        self.vars.insert(name, Variable::Local(loc));

        self.stack_size += 1;
        loc
    }

    fn gen_expr(&mut self, expr: TypedExpr) -> Option<IRExpr> {
        None
    }

    fn gen_func(&mut self, func: TypedFunction) -> Option<Function> {
        None
    }

    fn gen(mut self, module: TypedModule) -> Option<Module> {
        None
    }
}

pub fn gen_ir(module: TypedModule) -> Option<Module> {
    let generator = Generator::new();
    generator.gen(module)
}
