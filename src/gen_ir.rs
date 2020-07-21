use std::ops::{Add, AddAssign};

use rustc_hash::FxHashMap;

use crate::ast::BinOp;
use crate::id::{Id, IdMap};
use crate::ir::{Cmp, Function, Label, Module, Op, Temp, TEMP_FP};
use crate::scope_map::ScopeMap;
use crate::typing::{TypedExpr, TypedExprKind, TypedFunction, TypedModule};

macro_rules! buf {
    ($($ops:expr),* $(,)*) => {
	OpBuf::new(&[$($ops),*])
    };
}

struct OpBuf {
    ops_list: Vec<Vec<Op>>,
}

impl OpBuf {
    fn new(ops: &[Op]) -> Self {
        Self {
            ops_list: vec![ops.to_vec()],
        }
    }

    fn append(&mut self, mut other: OpBuf) {
        self.ops_list.append(&mut other.ops_list);
    }

    fn push(&mut self, op: Op) {
        if self.ops_list.is_empty() {
            self.ops_list.push(Vec::new());
        }

        let ops = self.ops_list.last_mut().unwrap();
        ops.push(op);
    }

    fn pop(&mut self) -> Option<Op> {
        while self.ops_list.last()?.is_empty() {
            self.ops_list.pop();
        }

        Some(self.ops_list.last_mut()?.pop().unwrap())
    }

    fn build(self) -> Vec<Op> {
        self.ops_list.into_iter().flatten().collect()
    }
}

impl Add for OpBuf {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        self.append(other);
        self
    }
}

impl AddAssign for OpBuf {
    fn add_assign(&mut self, other: Self) {
        self.append(other);
    }
}

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

    fn reverse_jump_if(op: Op, true_l: Label, false_l: Label) -> Op {
        let reverse = |label| {
            if label == true_l {
                false_l
            } else if label == false_l {
                true_l
            } else {
                panic!(
                    "unexpected label `{}`, expected `{}` or `{}`",
                    label, true_l, false_l
                );
            }
        };

        match op {
            Op::JumpIf(cmp, lhs, rhs, label) => match cmp {
                Cmp::LessThan => Op::JumpIf(Cmp::LessThanOrEqual, rhs, lhs, reverse(label)),
                Cmp::LessThanOrEqual => Op::JumpIf(Cmp::LessThan, rhs, lhs, reverse(label)),
                Cmp::Equal => Op::JumpIf(Cmp::NotEqual, lhs, rhs, reverse(label)),
                Cmp::NotEqual => Op::JumpIf(Cmp::Equal, lhs, rhs, reverse(label)),
            },
            op => panic!("unexpected operation `{:?}`", op),
        }
    }

    fn is_logical_binop(expr: &TypedExpr) -> bool {
        match &expr.kind {
            TypedExprKind::BinOp(binop, ..) => *binop == BinOp::And || *binop == BinOp::Or,
            _ => false,
        }
    }

    fn gen_cmp_binop(
        &mut self,
        binop: BinOp,
        lhs: TypedExpr,
        rhs: TypedExpr,
        true_l: Label,
        false_l: Label,
    ) -> Option<OpBuf> {
        match binop {
            BinOp::And | BinOp::Or => {
                let mut buf = self.gen_cmp(lhs, true_l, false_l)?;
                if binop == BinOp::And {
                    let jump_op = buf.pop().unwrap();
                    buf.push(Self::reverse_jump_if(jump_op, true_l, false_l));
                }

                buf += self.gen_cmp(rhs, true_l, false_l)?;
                if binop == BinOp::And {
                    let jump_op = buf.pop().unwrap();
                    buf.push(Self::reverse_jump_if(jump_op, true_l, false_l));
                }

                Some(buf)
            }
            _ => {
                let lhs_t = Temp::new();
                let rhs_t = Temp::new();

                let mut buf = self.gen_expr(Some(lhs_t), lhs)? + self.gen_expr(Some(rhs_t), rhs)?;

                buf += match binop {
                    BinOp::LessThan => buf![Op::JumpIf(Cmp::LessThan, lhs_t, rhs_t, true_l)],
                    BinOp::LessThanOrEqual => {
                        buf![Op::JumpIf(Cmp::LessThanOrEqual, lhs_t, rhs_t, true_l)]
                    }
                    BinOp::GreaterThan => buf![Op::JumpIf(Cmp::LessThan, rhs_t, lhs_t, true_l)],
                    BinOp::GreaterThanOrEqual => {
                        buf![Op::JumpIf(Cmp::LessThanOrEqual, rhs_t, lhs_t, true_l)]
                    }
                    BinOp::Equal => buf![Op::JumpIf(Cmp::Equal, lhs_t, rhs_t, true_l)],
                    BinOp::NotEqual => buf![Op::JumpIf(Cmp::NotEqual, lhs_t, rhs_t, true_l)],
                    binop => panic!("unexpected binary operation `{:?}`", binop),
                };

                Some(buf)
            }
        }
    }

    fn gen_cmp(&mut self, expr: TypedExpr, true_l: Label, false_l: Label) -> Option<OpBuf> {
        type E = TypedExprKind;

        match expr.kind {
            E::BinOp(binop, lhs, rhs) => self.gen_cmp_binop(binop, *lhs, *rhs, true_l, false_l),
            _ => {
                let dst = Temp::new();
                let mut buf = self.gen_expr(Some(dst), expr)?;

                let zero = Temp::new();
                buf += buf![
                    Op::Int(zero, 0),
                    Op::JumpIf(Cmp::NotEqual, dst, zero, true_l),
                ];

                Some(buf)
            }
        }
    }

    fn gen_expr(&mut self, dst: Option<Temp>, expr: TypedExpr) -> Option<OpBuf> {
        type E = TypedExprKind;

        let dst = || dst.unwrap_or_else(Temp::new);

        let buf = match expr.kind {
            E::Int(n) => buf![Op::Int(dst(), n)],
            E::Float(n) => buf![Op::Float(dst(), n)],
            E::Bool(false) => buf![Op::Int(dst(), 0)],
            E::Bool(true) => buf![Op::Int(dst(), 1)],
            E::String(s) => {
                let label = Label::new();
                self.strings.insert(label, s);
                buf![Op::Addr(dst(), label)]
            }
            E::Tuple(exprs) => {
                let size = Temp::new();
                let ptr = dst();

                // size <- exprs.len()
                // ptr <- alloc_record(size)
                let mut buf = buf![
                    Op::Int(size, exprs.len() as i64),
                    Op::CCall(ptr, IdMap::new_id("alloc_record"), vec![size]),
                ];

                for (i, expr) in exprs.into_iter().enumerate() {
                    let dst = Temp::new();
                    if let Some(expr_ops) = self.gen_expr(Some(dst), expr) {
                        buf.append(expr_ops);

                        // offset <- i
                        // [ptr + offset] <- dst
                        let addr = Temp::new();
                        let offset = Temp::new();
                        buf.append(buf![
                            Op::Int(offset, i as i64),
                            Op::Add(addr, ptr, offset),
                            Op::Store(addr, dst),
                        ]);
                    }
                }

                buf
            }
            E::Record(fields) => {
                let exprs: Vec<TypedExpr> = fields.into_iter().map(|(_, expr)| expr).collect();
                let size = Temp::new();
                let ptr = dst();

                // size <- exprs.len()
                // ptr <- alloc_record(size)
                let mut buf = buf![
                    Op::Int(size, exprs.len() as i64),
                    Op::CCall(ptr, IdMap::new_id("alloc_record"), vec![size]),
                ];

                for (i, expr) in exprs.into_iter().enumerate() {
                    let dst = Temp::new();
                    if let Some(expr_ops) = self.gen_expr(Some(dst), expr) {
                        buf.append(expr_ops);

                        // offset <- i
                        // [ptr + offset] <- dst
                        let addr = Temp::new();
                        let offset = Temp::new();
                        buf.append(buf![
                            Op::Int(offset, i as i64),
                            Op::Add(addr, ptr, offset),
                            Op::Store(addr, dst),
                        ]);
                    }
                }

                buf
            }
            E::Variable(name) if self.vars.contains_key(&name) => {
                let variable = self.vars.get(&name).unwrap();

                match variable {
                    Variable::Local(loc) => {
                        let addr = Temp::new();
                        let offset = Temp::new();
                        buf![
                            Op::Int(offset, *loc as i64),
                            Op::Add(addr, *TEMP_FP, offset),
                            Op::Load(dst(), addr),
                        ]
                    }
                    Variable::Param(temp) => buf![Op::Move(dst(), *temp)],
                }
            }
            E::Variable(name) if self.scope_functions.contains_key(&name) => {
                let func = self.find_func_in_scope(name).unwrap();
                buf![Op::Func(dst(), None, func.name)]
            }
            E::Variable(..) => panic!("the symbol existence should be checked when typing"),
            E::BinOp(binop, lhs, rhs) => match binop {
                BinOp::LessThan
                | BinOp::LessThanOrEqual
                | BinOp::GreaterThan
                | BinOp::GreaterThanOrEqual
                | BinOp::Equal
                | BinOp::NotEqual => {
                    let dst = dst();

                    let true_l = Label::new();
                    let false_l = Label::new();
                    let end_l = Label::new();

                    let mut buf = self.gen_cmp_binop(binop, *lhs, *rhs, true_l, false_l)?;

                    buf += buf![
                        Op::Label(true_l),
                        Op::Int(dst, 1),
                        Op::Jump(end_l),
                        Op::Label(false_l),
                        Op::Int(dst, 0),
                        Op::Label(end_l),
                    ];

                    buf
                }
                binop => {
                    let lhs_t = Temp::new();
                    let rhs_t = Temp::new();

                    let mut buf =
                        self.gen_expr(Some(lhs_t), *lhs)? + self.gen_expr(Some(rhs_t), *rhs)?;

                    buf += match binop {
                        BinOp::Add => buf![Op::Add(dst(), lhs_t, rhs_t)],
                        BinOp::Sub => buf![Op::Sub(dst(), lhs_t, rhs_t)],
                        BinOp::Mul => buf![Op::Mul(dst(), lhs_t, rhs_t)],
                        BinOp::Div => buf![Op::Div(dst(), lhs_t, rhs_t)],
                        BinOp::Mod => buf![Op::Mod(dst(), lhs_t, rhs_t)],
                        BinOp::LessThan => buf![Op::Mod(dst(), lhs_t, rhs_t)],
                        BinOp::LessThanOrEqual => buf![Op::Mod(dst(), lhs_t, rhs_t)],
                        BinOp::GreaterThan => buf![Op::Mod(dst(), lhs_t, rhs_t)],
                        BinOp::GreaterThanOrEqual => buf![Op::Mod(dst(), lhs_t, rhs_t)],
                        BinOp::Equal => buf![Op::Mod(dst(), lhs_t, rhs_t)],
                        BinOp::NotEqual => buf![Op::Mod(dst(), lhs_t, rhs_t)],
                        binop => panic!("unexpected binary operation: `{:?}`", binop),
                    };

                    buf
                }
            },
            E::Let(name, expr) => {
                let expr_t = Temp::new();
                let mut buf = self.gen_expr(Some(expr_t), *expr)?;

                let loc = self.alloc_var(name);

                let addr = Temp::new();
                let offset = Temp::new();
                buf += buf![
                    Op::Int(offset, loc as i64),
                    Op::Add(addr, *TEMP_FP, offset),
                    Op::Store(addr, expr_t),
                ];

                buf
            }
            E::LetFun(_, func) => {
                let func = self.gen_func(func)?;
                self.create_func(func);

                buf![]
            }
            E::Not(expr) => {
                let expr_t = Temp::new();
                let buf = self.gen_expr(Some(expr_t), *expr)?;

                buf + buf![Op::Not(dst(), expr_t)]
            }
            E::Negative(expr) => {
                let expr_t = Temp::new();
                let buf = self.gen_expr(Some(expr_t), *expr)?;

                buf + buf![Op::Negative(dst(), expr_t)]
            }
            E::Call(func, arg) => {
                let func_t = Temp::new();
                let arg_t = Temp::new();
                let mut buf = self.gen_expr(Some(func_t), *func)?;
                buf += self.gen_expr(Some(arg_t), *arg)?;

                buf + buf![Op::Call(dst(), func_t, vec![arg_t])]
            }
            E::If(cond, then_expr, else_expr) => {
                let result_t = Temp::new();

                let true_l = Label::new();
                let false_l = Label::new();
                let end_l = Label::new();

                let mut buf = self.gen_cmp(*cond, true_l, false_l)?;
                let op = Self::reverse_jump_if(buf.pop().unwrap(), true_l, false_l);
                buf.push(op);

                buf += buf![Op::Label(true_l)];

                buf += self.gen_expr(Some(result_t), *then_expr)?;
                buf += buf![Op::Jump(end_l), Op::Label(false_l),];

                if let Some(else_expr) = else_expr {
                    buf += self.gen_expr(Some(result_t), *else_expr)?;
                }

                buf += buf![Op::Label(end_l)];

                buf
            }
            E::Do(exprs) => {
                let exprs_len = exprs.len();
                let result_t = Temp::new();

                let mut buf = buf![];
                for (i, expr) in exprs.into_iter().enumerate() {
                    if i == exprs_len - 1 {
                        buf += self.gen_expr(Some(result_t), expr)?;
                    } else {
                        buf += self.gen_expr(None, expr)?;
                    }
                }

                buf
            }
        };

        Some(buf)
    }

    fn gen_func(&mut self, func: TypedFunction) -> Option<Function> {
        self.push_scope();

        // Insert parameters as variables
        let mut param_temps = Vec::with_capacity(func.params.len());
        for param in &func.params {
            let temp = Temp::new();
            self.vars.insert(param.value, Variable::Param(temp));
            param_temps.push(temp);
        }

        // Generate IR of body
        let return_temp = Temp::new();
        let mut body = self.gen_expr(Some(return_temp), *func.body)?;
        body.push(Op::Return(return_temp));

        self.pop_scope();

        // Create ir::Function
        let func = Function {
            name: func.name,
            params: param_temps,
            body: body.build(),
        };

        Some(func)
    }

    fn gen(mut self, module: TypedModule) -> Option<Module> {
        let mut ir_module = Module::new(module.path);

        for (name, (_, func)) in module.functions {
            if let Some(func) = self.gen_func(func) {
                ir_module.functions.insert(name, func);
            }
        }

        Some(ir_module)
    }
}

pub fn gen_ir(module: TypedModule) -> Option<Module> {
    let generator = Generator::new();
    generator.gen(module)
}
