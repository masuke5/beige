use std::mem;

use rustc_hash::FxHashMap;

use crate::ast::{BinOp, Visibility};
use crate::id::{Id, IdMap};
use crate::ir::{
    BasicBlock, Cmp, Expr as IRExpr, Function, Label, Module, Stmt as IRStmt, Temp, TEMP_FP,
};
use crate::scope_map::ScopeMap;
use crate::typing::{TypedExpr, TypedExprKind, TypedFunction, TypedModule};

#[derive(Debug, Clone, PartialEq)]
enum Variable {
    Local(usize),
    Param(Temp),
}

#[derive(Debug)]
struct Generator {
    constants: FxHashMap<Id, Label>,
    strings: FxHashMap<Label, String>,
    vars: ScopeMap<Id, Variable>,
    stack_sizes: Vec<usize>,

    funcs: FxHashMap<Id, Option<Function>>,
    scope_funcs: ScopeMap<Id, Id>,
    prefix: String,
}

impl Generator {
    fn new() -> Self {
        Self {
            constants: FxHashMap::default(),
            strings: FxHashMap::default(),
            vars: ScopeMap::new(),
            stack_sizes: Vec::new(),
            funcs: FxHashMap::default(),
            scope_funcs: ScopeMap::new(),
            prefix: String::from("___"),
        }
    }

    fn push_scope(&mut self) {
        self.vars.push_scope();
        self.scope_funcs.push_scope();
        self.stack_sizes.push(0);
    }

    fn pop_scope(&mut self) {
        self.vars.pop_scope();
        self.scope_funcs.pop_scope();
        self.stack_sizes.pop();
    }

    fn find_func_name_in_scope(&self, name: Id) -> Option<Id> {
        match self.scope_funcs.get(&name) {
            Some(name) => Some(self.funcs[name].as_ref().map_or(*name, |func| func.name)),
            None => None,
        }
    }

    fn create_func_header(&mut self, name: Id) {
        self.funcs.insert(name, None);
        self.scope_funcs.insert(name, name);
    }

    fn create_module_func(&mut self, func: Function) {
        let func_name = func.name;
        self.funcs.insert(func_name, Some(func));
        self.scope_funcs.insert(func_name, func_name);
    }

    fn create_local_func(&mut self, mut func: Function) {
        let original_name = func.name;
        let new_func_name = IdMap::new_id(&format!("{}{}", self.prefix, func.name));
        func.name = new_func_name;

        self.funcs.insert(new_func_name, Some(func));
        self.scope_funcs.insert(original_name, new_func_name);
    }

    fn alloc_var(&mut self, name: Id) -> usize {
        let stack_size = self.stack_sizes.last_mut().unwrap();
        *stack_size += 8;
        self.vars.insert(name, Variable::Local(*stack_size));
        *stack_size
    }

    fn gen_record(&mut self, exprs: impl Iterator<Item = TypedExpr>, len: usize) -> IRExpr {
        type IE = IRExpr; // IR expression
        type IS = IRStmt;

        let addr = Temp::new();
        let mut stmts = vec![IS::Expr(
            addr,
            IE::CCall(IdMap::new_id("alloc_record"), vec![IE::Int(len as i64)]),
        )];

        for (i, expr) in exprs.map(|e| self.gen_expr(e)).enumerate() {
            // [addr + i] <- expr
            stmts.push(IS::Store(
                IE::Add(box IE::Temp(addr), box IE::Int(i as i64)),
                expr,
            ));
        }

        IE::Seq(stmts, box IE::Temp(addr))
    }

    fn gen_expr(&mut self, expr: TypedExpr) -> IRExpr {
        type AE = TypedExprKind; // AST expression
        type IE = IRExpr; // IR expression
        type IS = IRStmt;

        match expr.kind {
            AE::Int(n) => IE::Int(n),
            AE::Float(n) => IE::Float(n),
            AE::Bool(false) => IE::Int(0),
            AE::Bool(true) => IE::Int(1),
            AE::String(s) => {
                let label = Label::new();
                self.strings.insert(label, s);
                IE::Addr(label)
            }
            AE::Tuple(exprs) => {
                let len = exprs.len();
                self.gen_record(exprs.into_iter(), len)
            }
            AE::Record(fields) => {
                let len = fields.len();
                self.gen_record(fields.into_iter().map(|(_, e)| e), len)
            }
            AE::Variable(name) if self.constants.contains_key(&name) => {
                let label = self.constants[&name];
                IE::Load(box IE::Addr(label))
            }
            AE::Variable(name) if self.vars.contains_key(&name) => {
                match self.vars.get(&name).unwrap() {
                    Variable::Local(loc) => IE::Load(box IE::Sub(
                        box IE::Temp(*TEMP_FP),
                        box IE::Int(*loc as i64),
                    )),
                    Variable::Param(temp) => IE::Temp(*temp),
                }
            }
            AE::Variable(name) if self.scope_funcs.contains_key(&name) => {
                let name = self.find_func_name_in_scope(name).unwrap();
                IE::Func(None, name)
            }
            AE::Variable(name) => panic!(
                "the symbol `{}` existence should be checked when typing",
                name
            ),
            AE::BinOp(binop, lhs, rhs) => {
                let lhs = self.gen_expr(*lhs);
                let rhs = self.gen_expr(*rhs);

                match binop {
                    BinOp::Add => IE::Add(box lhs, box rhs),
                    BinOp::Sub => IE::Sub(box lhs, box rhs),
                    BinOp::Mul => IE::Mul(box lhs, box rhs),
                    BinOp::Div => IE::Div(box lhs, box rhs),
                    BinOp::Mod => IE::Mod(box lhs, box rhs),
                    _ => {
                        let result_t = Temp::new();
                        let false_l = Label::new();
                        let end_l = Label::new();

                        let cmp = match binop {
                            BinOp::LessThan => Cmp::GreaterThanOrEqual,
                            BinOp::LessThanOrEqual => Cmp::GreaterThan,
                            BinOp::GreaterThan => Cmp::LessThanOrEqual,
                            BinOp::GreaterThanOrEqual => Cmp::LessThan,
                            BinOp::Equal => Cmp::NotEqual,
                            BinOp::NotEqual => Cmp::Equal,
                            binop => panic!("unexpected binary operation `{:?}`", binop),
                        };

                        IE::Seq(
                            vec![
                                IS::JumpIf(cmp, lhs, rhs, false_l),
                                IS::Expr(result_t, IE::Int(1)),
                                IS::Jump(end_l),
                                IS::Label(false_l),
                                IS::Expr(result_t, IE::Int(0)),
                                IS::Label(end_l),
                            ],
                            box IE::Temp(result_t),
                        )
                    }
                }
            }
            AE::Let(name, expr) => {
                let expr = self.gen_expr(*expr);
                let loc = self.alloc_var(name);
                IE::DSeq(vec![IS::Store(
                    IE::Sub(box IE::Temp(*TEMP_FP), box IE::Int(loc as i64)),
                    expr,
                )])
            }
            AE::LetFun(_, func) => {
                let func = self.gen_func(func, true);
                self.create_local_func(func);

                IE::DSeq(vec![])
            }
            AE::Not(expr) => IE::Not(box self.gen_expr(*expr)),
            AE::Negative(expr) => IE::Negative(box self.gen_expr(*expr)),
            AE::Call(func, arg) => {
                let mut expr = func;
                let mut args = vec![arg];
                while let AE::Call(func, arg) = expr.kind {
                    expr = func;
                    args.push(arg);
                }

                let func = self.gen_expr(*expr);
                let args: Vec<_> = args.into_iter().rev().map(|e| self.gen_expr(*e)).collect();

                IE::Call(box func, args)
            }
            AE::If(cond, then_expr, else_expr) => {
                let result_t = Temp::new();
                let else_l = Label::new();
                let end_l = Label::new();

                let (cmp, lhs, rhs) = match cond.kind {
                    AE::BinOp(BinOp::LessThan, lhs, rhs) => {
                        let lhs = self.gen_expr(*lhs);
                        let rhs = self.gen_expr(*rhs);
                        (Cmp::GreaterThanOrEqual, lhs, rhs)
                    }
                    AE::BinOp(BinOp::LessThanOrEqual, lhs, rhs) => {
                        let lhs = self.gen_expr(*lhs);
                        let rhs = self.gen_expr(*rhs);
                        (Cmp::GreaterThan, lhs, rhs)
                    }
                    AE::BinOp(BinOp::GreaterThan, lhs, rhs) => {
                        let lhs = self.gen_expr(*lhs);
                        let rhs = self.gen_expr(*rhs);
                        (Cmp::LessThanOrEqual, lhs, rhs)
                    }
                    AE::BinOp(BinOp::GreaterThanOrEqual, lhs, rhs) => {
                        let lhs = self.gen_expr(*lhs);
                        let rhs = self.gen_expr(*rhs);
                        (Cmp::LessThan, lhs, rhs)
                    }
                    AE::BinOp(BinOp::Equal, lhs, rhs) => {
                        let lhs = self.gen_expr(*lhs);
                        let rhs = self.gen_expr(*rhs);
                        (Cmp::NotEqual, lhs, rhs)
                    }
                    AE::BinOp(BinOp::NotEqual, lhs, rhs) => {
                        let lhs = self.gen_expr(*lhs);
                        let rhs = self.gen_expr(*rhs);
                        (Cmp::Equal, lhs, rhs)
                    }
                    _ => {
                        let cond = self.gen_expr(*cond);
                        (Cmp::Equal, cond, IE::Int(0))
                    }
                };

                //   jump else_l if lhs (cmp) rhs
                //   result_t <- then_expr
                //   jump end_l
                // else_l:
                //   result_t <- else_expr
                // end_l:

                let mut stmts = vec![IS::JumpIf(cmp, lhs, rhs, else_l)];

                let then_expr = self.gen_expr(*then_expr);
                stmts.push(IS::Expr(result_t, then_expr));

                if let Some(else_expr) = else_expr {
                    let else_expr = self.gen_expr(*else_expr);

                    stmts.append(&mut vec![
                        IS::Jump(end_l),
                        IS::Label(else_l),
                        IS::Expr(result_t, else_expr),
                        IS::Label(end_l),
                    ]);
                } else {
                    stmts.append(&mut vec![
                        IS::Jump(end_l),
                        IS::Label(else_l),
                        IS::Expr(result_t, IE::Int(0)),
                        IS::Label(end_l),
                    ]);
                }

                IE::Seq(stmts, box IE::Temp(result_t))
            }
            AE::Do(exprs) => {
                let mut stmts = vec![];
                let mut result_t = None;

                let len = exprs.len();
                for (i, expr) in exprs.into_iter().enumerate() {
                    let expr = self.gen_expr(expr);
                    match expr {
                        IE::DSeq(mut new_stmts) => {
                            stmts.append(&mut new_stmts);
                        }
                        _ => {
                            let temp = Temp::new();
                            stmts.push(IS::Expr(temp, expr));

                            if i == len - 1 {
                                result_t = Some(temp);
                            }
                        }
                    }
                }

                if let Some(result_t) = result_t {
                    IE::Seq(stmts, box IE::Temp(result_t))
                } else {
                    IE::DSeq(stmts)
                }
            }
        }
    }

    fn gen_func(&mut self, func: TypedFunction, is_private: bool) -> Function {
        // Prepare to generate the function body
        self.push_scope();
        self.prefix = format!("{}{}___", self.prefix, func.name);

        // Insert parameters as variables
        let mut params = Vec::new();
        for param_name in func.params {
            let temp = Temp::new();
            self.vars.insert(param_name.value, Variable::Param(temp));
            params.push(temp);
        }

        let mut body = self.gen_expr(*func.body);

        let stack_size = *self.stack_sizes.last().unwrap();
        self.pop_scope();
        self.prefix
            .truncate(self.prefix.len() - format!("{}", func.name).len() - 3);

        flatten_expr(&mut body);
        let body = put_out_call(body);
        let bbs = generate_bb(body);

        let func = Function {
            name: func.name,
            params,
            bbs,
            stack_size,
            is_private,
        };

        func
    }

    fn gen(mut self, module: TypedModule) -> Module {
        let mut new_module = Module::new(module.path);

        self.push_scope();

        for (name, (_, expr)) in module.constants {
            let expr = self.gen_expr(expr);
            let label = Label::new();
            self.constants.insert(name, label);
            new_module.constants.insert(label, expr);
        }

        // Insert function headers
        for (_, func) in &module.functions {
            self.create_func_header(func.name);
        }

        for (visibility, func) in module.functions {
            let func = self.gen_func(func, visibility == Visibility::Private);
            self.create_module_func(func.clone());
        }

        self.pop_scope();

        // Add local functions to new_module
        for (_, func) in self.funcs {
            let func = func.unwrap();
            new_module.functions.insert(func.name, func);
        }

        new_module.strings = self.strings;

        new_module
    }
}

fn flatten_expr(expr: &mut IRExpr) {
    type E = IRExpr;
    type S = IRStmt;

    fn flatten_stmt(stmts: &mut Vec<IRStmt>, stmt: &mut IRStmt, call: bool) {
        match stmt {
            S::Expr(_, expr) | S::Return(expr) => {
                flatten(stmts, expr, call);
            }
            S::Store(dst, src) => {
                flatten(stmts, dst, true);
                flatten(stmts, src, call);
            }
            S::JumpIf(_, lhs, rhs, _) => {
                flatten(stmts, lhs, true);
                flatten(stmts, rhs, call);
            }
            _ => {}
        }
    }

    fn flatten(stmts: &mut Vec<IRStmt>, expr: &mut IRExpr, call: bool) {
        match expr {
            E::Add(lhs, rhs)
            | E::Sub(lhs, rhs)
            | E::Mul(lhs, rhs)
            | E::Div(lhs, rhs)
            | E::Mod(lhs, rhs)
            | E::AddF(lhs, rhs)
            | E::SubF(lhs, rhs)
            | E::MulF(lhs, rhs)
            | E::DivF(lhs, rhs) => {
                flatten(stmts, lhs, true);
                flatten(stmts, rhs, call);
            }
            E::Not(expr) | E::Negative(expr) | E::Load(expr) => flatten(stmts, expr, call),
            E::Call(func, args) if call => {
                flatten(stmts, func, true);
                for arg in args {
                    flatten(stmts, arg, true);
                }

                let temp = Temp::new();
                let expr = mem::replace(expr, E::Temp(temp));
                stmts.push(S::Expr(temp, expr));
            }
            E::CCall(_, args) if call => {
                for arg in args {
                    flatten(stmts, arg, true);
                }

                let temp = Temp::new();
                let expr = mem::replace(expr, E::Temp(temp));
                stmts.push(S::Expr(temp, expr));
            }
            E::Seq(inner_stmts, inner_expr) => {
                for mut stmt in inner_stmts.drain(..) {
                    flatten_stmt(stmts, &mut stmt, false);
                    stmts.push(stmt);
                }

                flatten(stmts, inner_expr, call);
                *expr = mem::replace(inner_expr, E::Int(0));
            }
            E::DSeq(inner_stmts) => {
                for mut stmt in inner_stmts.drain(..) {
                    flatten_stmt(stmts, &mut stmt, false);
                    stmts.push(stmt);
                }
            }
            _ => {}
        }
    }

    let mut stmts = Vec::new();
    flatten(&mut stmts, expr, false);
    *expr = IRExpr::Seq(stmts, box mem::replace(expr, IRExpr::Int(0)));
}

fn put_out_call(expr: IRExpr) -> IRExpr {
    fn put_out_in_stmt(stmts: &mut Vec<IRStmt>, stmt: &mut IRStmt) {
        match stmt {
            IRStmt::Expr(_, expr) | IRStmt::Return(expr) => {
                put_out(stmts, expr);
            }
            IRStmt::Store(dst, src) => {
                put_out(stmts, dst);
                put_out(stmts, src);
            }
            IRStmt::JumpIf(_, lhs, rhs, _) => {
                put_out(stmts, lhs);
                put_out(stmts, rhs);
            }
            _ => {}
        }
    }

    fn put_out(stmts: &mut Vec<IRStmt>, expr: &mut IRExpr) {
        match expr {
            IRExpr::Int(..)
            | IRExpr::Float(..)
            | IRExpr::Temp(..)
            | IRExpr::Addr(..)
            | IRExpr::Func(..) => {}
            IRExpr::Add(lhs, rhs)
            | IRExpr::Sub(lhs, rhs)
            | IRExpr::Mul(lhs, rhs)
            | IRExpr::Div(lhs, rhs)
            | IRExpr::Mod(lhs, rhs)
            | IRExpr::AddF(lhs, rhs)
            | IRExpr::SubF(lhs, rhs)
            | IRExpr::MulF(lhs, rhs)
            | IRExpr::DivF(lhs, rhs) => {
                put_out(stmts, lhs);
                put_out(stmts, rhs);
            }
            IRExpr::Not(expr) | IRExpr::Negative(expr) | IRExpr::Load(expr) => put_out(stmts, expr),
            IRExpr::Call(func, args) => {
                put_out(stmts, func);
                for arg in args {
                    match arg {
                        IRExpr::Temp(..) => {}
                        _ => {
                            let temp = Temp::new();
                            let mut arg = mem::replace(arg, IRExpr::Temp(temp));
                            put_out(stmts, &mut arg);
                            stmts.push(IRStmt::Expr(temp, arg));
                        }
                    }
                }
            }
            IRExpr::CCall(_, args) => {
                for arg in args {
                    put_out(stmts, arg);
                }

                let temp = Temp::new();
                let expr = mem::replace(expr, IRExpr::Temp(temp));
                stmts.push(IRStmt::Expr(temp, expr));
            }
            IRExpr::Seq(..) | IRExpr::DSeq(..) => panic!("no flatten"),
        }
    }

    let (old_stmts, mut expr) = match expr {
        IRExpr::Seq(stmts, expr) => (stmts, expr),
        _ => panic!(),
    };
    let mut new_stmts = Vec::with_capacity(old_stmts.len());
    for mut stmt in old_stmts {
        put_out_in_stmt(&mut new_stmts, &mut stmt);
        new_stmts.push(stmt);
    }

    put_out(&mut new_stmts, &mut expr);

    IRExpr::Seq(new_stmts, expr)
}

pub fn generate_bb(expr: IRExpr) -> Vec<BasicBlock> {
    let (stmts, result_expr) = match expr {
        IRExpr::Seq(stmts, expr) => (stmts, expr),
        _ => panic!("no flatten"),
    };

    let mut bbs = vec![BasicBlock::new(None)];

    for stmt in stmts {
        let is_head = bbs.len() == 1;

        let last_bb = bbs.last_mut().unwrap();
        if last_bb.label.is_none() && !is_head {
            last_bb.label = Some(Label::new());
        }

        match stmt {
            IRStmt::Label(label) => {
                if last_bb.stmts.is_empty() {
                    last_bb.label = Some(label);
                } else {
                    bbs.push(BasicBlock::new(Some(label)));
                }
            }
            IRStmt::Jump(..) | IRStmt::JumpIf(..) => {
                last_bb.stmts.push(stmt);
                bbs.push(BasicBlock::new(None));
            }
            stmt => {
                last_bb.stmts.push(stmt);
            }
        };
    }

    bbs.last_mut()
        .unwrap()
        .stmts
        .push(IRStmt::Return(*result_expr));

    bbs
}

pub fn gen_ir(module: TypedModule) -> Module {
    let generator = Generator::new();
    generator.gen(module)
}
