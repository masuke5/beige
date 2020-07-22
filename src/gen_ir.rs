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
    constants: FxHashMap<Id, Label>,
    strings: FxHashMap<Label, String>,
    vars: ScopeMap<Id, Variable>,
    stack_size: usize,

    funcs: FxHashMap<Id, Function>,
    scope_funcs: ScopeMap<Id, Id>,
    prefix: String,
}

impl Generator {
    fn new() -> Self {
        Self {
            constants: FxHashMap::default(),
            strings: FxHashMap::default(),
            vars: ScopeMap::new(),
            stack_size: 0,
            funcs: FxHashMap::default(),
            scope_funcs: ScopeMap::new(),
            prefix: String::new(),
        }
    }

    fn push_scope(&mut self) {
        self.vars.push_scope();
        self.scope_funcs.push_scope();
    }

    fn pop_scope(&mut self) {
        self.vars.pop_scope();
        self.scope_funcs.pop_scope();
    }

    fn find_func_in_scope(&self, name: Id) -> Option<&Function> {
        match self.scope_funcs.get(&name) {
            Some(name) => Some(&self.funcs[name]),
            None => None,
        }
    }

    fn create_func(&mut self, mut func: Function) {
        let original_name = func.name;
        let new_func_name = IdMap::new_id(&format!("{}{}", self.prefix, func.name));
        func.name = new_func_name;

        self.funcs.insert(new_func_name, func);
        self.scope_funcs.insert(original_name, new_func_name);
    }

    fn alloc_var(&mut self, name: Id) -> usize {
        let loc = self.stack_size;
        self.vars.insert(name, Variable::Local(loc));

        self.stack_size += 1;
        loc
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
                IE::Add(box IE::Temp(addr), box IE::Int(i as i64 + 1)),
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
                        box IE::Int(*loc as i64 + 1),
                    )),
                    Variable::Param(temp) => IE::Temp(*temp),
                }
            }
            AE::Variable(name) if self.scope_funcs.contains_key(&name) => {
                let func = self.find_func_in_scope(name).unwrap();
                IE::Func(None, func.name)
            }
            AE::Variable(_) => panic!("the symbol existence should be checked when typing"),
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

                        let (cmp, lhs, rhs) = match binop {
                            BinOp::LessThan => (Cmp::LessThanOrEqual, rhs, lhs),
                            BinOp::LessThanOrEqual => (Cmp::LessThan, rhs, lhs),
                            BinOp::GreaterThan => (Cmp::LessThanOrEqual, lhs, rhs),
                            BinOp::GreaterThanOrEqual => (Cmp::LessThanOrEqual, lhs, rhs),
                            BinOp::Equal => (Cmp::NotEqual, lhs, rhs),
                            BinOp::NotEqual => (Cmp::Equal, lhs, rhs),
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
                    IE::Sub(box IE::Temp(*TEMP_FP), box IE::Int(loc as i64 + 1)),
                    expr,
                )])
            }
            AE::LetFun(_, func) => {
                let func = self.gen_func(func);
                self.create_func(func);

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

                let (cmp, lhs, rhs) = match cond {
                    cond => {
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

    fn gen_func(&mut self, func: TypedFunction) -> Function {
        self.push_scope();

        // Insert parameters as variables
        let mut params = Vec::new();
        for param_name in func.params {
            let temp = Temp::new();
            self.vars.insert(param_name.value, Variable::Param(temp));
            params.push(temp);
        }

        let body = self.gen_expr(*func.body);

        self.pop_scope();

        let func = Function {
            name: func.name,
            params,
            body,
        };

        func
    }

    fn gen(mut self, module: TypedModule) -> Module {
        let mut new_module = Module::new(module.path);

        for (name, (_, expr)) in module.constants {
            let expr = self.gen_expr(expr);
            let label = Label::new();
            self.constants.insert(name, label);
            new_module.constants.insert(label, expr);
        }

        for (_, (_, func)) in module.functions {
            let func = self.gen_func(func);
            new_module.functions.insert(func.name, func);
        }

        new_module.strings = self.strings;

        new_module
    }
}

pub fn gen_ir(module: TypedModule) -> Module {
    let generator = Generator::new();
    generator.gen(module)
}
