use crate::codegen::{format_mnemonic, CodeGen, Function, Mnemonic, Module};
use crate::ir::{Cmp, Expr, Function as IRFunction, Label, Module as IRModule, Stmt, Temp};
use lazy_static::lazy_static;
use std::iter;

// Adjust depending on the purpose of debugging
const REG_COUNT: usize = 32; // it must be at least 3

lazy_static! {
    // return value = R0
    // arguments = R0..R2
    pub static ref R0: Temp = Temp::new();
    pub static ref R1: Temp = Temp::new();
    pub static ref R2: Temp = Temp::new();
    pub static ref REGISTERS: Vec<Temp> = {
        let mut regs = vec![*R0, *R1, *R2];
        regs.extend(iter::repeat_with(|| Temp::new()).take(REG_COUNT - regs.len()));
        regs
    };
    pub static ref REG_PRIORITY: Vec<u32> = (0..REG_COUNT).map(|n| n as u32).rev().collect();
    pub static ref RV: Temp = *R0;
    pub static ref PARAMS: [Temp; 3] = [*R0, *R1, *R2];
}

fn result(gen: impl FnOnce(Temp)) -> Temp {
    let temp = Temp::new();
    gen(temp);
    temp
}

pub fn reg_name(temp: Temp) -> Option<String> {
    REGISTERS
        .iter()
        .position(|t| *t == temp)
        .map(|pos| format!("r{}", pos))
}

fn label_name(label: Label) -> String {
    format!(".L{}", label.raw())
}

pub struct DebugCodeGen {
    epilogue_label: Option<Label>,
    next_label: Option<Label>,
}

impl DebugCodeGen {
    pub fn new() -> Self {
        Self {
            next_label: None,
            epilogue_label: None,
        }
    }

    fn gen_label(label: Label) -> Mnemonic {
        Mnemonic::Label {
            text: format!("{}:", label_name(label)),
            label,
        }
    }

    fn gen_expr(&mut self, ms: &mut Vec<Mnemonic>, expr: Expr) -> Temp {
        match expr {
            // Expr::Temp(temp) if temp == *TEMP_FP => *R?,
            Expr::Temp(temp) => temp,
            Expr::Int(n) => result(|t| {
                ms.push(Mnemonic::Op {
                    text: format!("imm $d0, {}", n),
                    dst: vec![t],
                    src: vec![],
                })
            }),
            Expr::Float(n) => result(|t| {
                ms.push(Mnemonic::Op {
                    text: format!("fimm $d0, {}", n),
                    dst: vec![t],
                    src: vec![],
                })
            }),
            Expr::Add(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "add $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::Sub(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "sub $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::Mul(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "mul $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::Div(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "div $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::Mod(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "mod $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::AddF(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "fadd $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::SubF(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "fsub $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::MulF(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "fmul $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::DivF(lhs, rhs) => result(|t| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                ms.push(Mnemonic::Op {
                    text: "fdiv $d0, $s0, $s1".to_string(),
                    dst: vec![t],
                    src: vec![lhs, rhs],
                });
            }),
            Expr::Not(expr) => result(|t| {
                let expr = self.gen_expr(ms, *expr);
                ms.push(Mnemonic::Op {
                    text: "not $d0, $s0".to_string(),
                    dst: vec![t],
                    src: vec![expr],
                })
            }),
            Expr::Negative(expr) => result(|t| {
                let expr = self.gen_expr(ms, *expr);
                ms.push(Mnemonic::Op {
                    text: "neg $d0, $s0".to_string(),
                    dst: vec![t],
                    src: vec![expr],
                })
            }),
            Expr::Load(expr) => result(|t| {
                let expr = self.gen_expr(ms, *expr);
                ms.push(Mnemonic::Op {
                    text: "load $d0, $s0".to_string(),
                    dst: vec![t],
                    src: vec![expr],
                })
            }),
            Expr::Addr(label) => result(|t| {
                ms.push(Mnemonic::Op {
                    text: format!("addr $d0, {}", label_name(label)),
                    dst: vec![t],
                    src: vec![],
                })
            }),
            Expr::Func(None, name) => result(|t| {
                ms.push(Mnemonic::Op {
                    text: format!("aimm $d0, {}", name),
                    dst: vec![t],
                    src: vec![],
                })
            }),
            Expr::Func(Some(module), func) => result(|t| {
                ms.push(Mnemonic::Op {
                    text: format!("aimm $d0, {}・{}", module, func),
                    dst: vec![t],
                    src: vec![],
                })
            }),
            Expr::Call(func, args) => result(|t| {
                let func = self.gen_expr(ms, *func);
                if args.len() > 3 {
                    panic!("unsupported more than 3 arguments")
                }

                let args_count = args.len();
                for (arg, reg) in args.into_iter().zip(&*PARAMS) {
                    let arg = self.gen_expr(ms, arg);
                    self.gen_stmt(ms, Stmt::Expr(*reg, Expr::Temp(arg)));
                }

                let mut src = vec![func];
                src.extend(PARAMS.iter().copied().take(args_count));

                ms.push(Mnemonic::Op {
                    text: "call $s0".to_string(),
                    dst: PARAMS.to_vec(),
                    src,
                });
                self.gen_stmt(ms, Stmt::Expr(t, Expr::Temp(*RV)));
            }),
            Expr::CCall(func, args) => result(|t| {
                let func_t = Temp::new();
                ms.push(Mnemonic::Op {
                    text: format!("aimm $d0, {}", func),
                    dst: vec![func_t],
                    src: vec![],
                });

                if args.len() > 3 {
                    panic!("unsupported more than 3 arguments")
                }

                let args_count = args.len();
                for (arg, reg) in args.into_iter().zip(&*PARAMS) {
                    let arg = self.gen_expr(ms, arg);
                    self.gen_stmt(ms, Stmt::Expr(*reg, Expr::Temp(arg)));
                }

                let mut src = vec![func_t];
                src.extend(PARAMS.iter().copied().take(args_count));

                ms.push(Mnemonic::Op {
                    text: "call $s0".to_string(),
                    dst: PARAMS.to_vec(),
                    src,
                });
                self.gen_stmt(ms, Stmt::Expr(t, Expr::Temp(*RV)));
            }),
            Expr::Seq(..) | Expr::DSeq(..) => unreachable!(),
        }
    }

    fn gen_stmt(&mut self, ms: &mut Vec<Mnemonic>, stmt: Stmt) {
        match stmt {
            Stmt::Expr(dst, src) => {
                let src = self.gen_expr(ms, src);
                ms.push(Mnemonic::Move {
                    text: "mov $d0, $s0".to_string(),
                    dst,
                    src,
                });
            }
            Stmt::Store(addr, expr) => {
                let addr = self.gen_expr(ms, addr);
                let expr = self.gen_expr(ms, expr);
                ms.push(Mnemonic::Op {
                    text: "store $s0, $s1".to_string(),
                    dst: vec![],
                    src: vec![addr, expr],
                });
            }
            Stmt::Label(label) => ms.push(Self::gen_label(label)),
            Stmt::Jump(label) => ms.push(Mnemonic::Jump {
                text: format!("jmp {}", label_name(label)),
                dst: vec![],
                src: vec![],
                jump: vec![label],
            }),
            Stmt::JumpIf(cmp, lhs, rhs, label) => {
                let opcode = match cmp {
                    Cmp::LessThan => "b.lt",
                    Cmp::LessThanOrEqual => "b.le",
                    Cmp::Equal => "b.eq",
                    Cmp::NotEqual => "b.ne",
                };

                let lhs = self.gen_expr(ms, lhs);
                let rhs = self.gen_expr(ms, rhs);

                ms.push(Mnemonic::Jump {
                    text: format!("{} {}, $s0, $s1", opcode, label_name(label)),
                    dst: vec![],
                    src: vec![lhs, rhs],
                    jump: vec![self.next_label.unwrap(), label],
                });
            }
            Stmt::Return(expr) => {
                let expr = self.gen_expr(ms, expr);
                self.gen_stmt(ms, Stmt::Expr(*RV, Expr::Temp(expr)));
                self.gen_stmt(ms, Stmt::Jump(self.epilogue_label.unwrap()));
            }
        }
    }

    fn gen_prologue(&mut self, ms: &mut Vec<Mnemonic>, params: Vec<Temp>, stack_size: usize) {
        if params.len() > 3 {
            panic!("unsupported more than 3 arguments")
        }

        ms.push(Mnemonic::Op {
            text: format!("alloc_frame {}", stack_size),
            dst: vec![],
            src: vec![],
        });

        for (param, reg) in params.into_iter().zip(&*REGISTERS) {
            self.gen_stmt(ms, Stmt::Expr(param, Expr::Temp(*reg)));
        }
    }

    fn gen_epilogue(&mut self, ms: &mut Vec<Mnemonic>) {
        self.gen_stmt(ms, Stmt::Label(self.epilogue_label.unwrap()));
        ms.push(Mnemonic::Op {
            text: "ret".to_string(),
            dst: vec![],
            src: vec![*RV],
        });
    }

    fn gen_func(&mut self, ir_func: IRFunction) -> Function {
        let mut mnemonics = Vec::new();

        self.epilogue_label = Some(Label::new());
        self.gen_prologue(&mut mnemonics, ir_func.params, ir_func.stack_size);

        let labels: Vec<Option<Label>> = ir_func.bbs.iter().map(|bb| bb.label).collect();

        for (i, bb) in ir_func.bbs.into_iter().enumerate() {
            if let Some(label) = bb.label {
                mnemonics.push(Self::gen_label(label));
            }

            self.next_label = *labels.get(i + 1).unwrap_or(&None);

            for stmt in bb.stmts {
                self.gen_stmt(&mut mnemonics, stmt);
            }
        }

        self.gen_epilogue(&mut mnemonics);
        self.epilogue_label = None;

        Function {
            name: ir_func.name,
            mnemonics,
            is_private: ir_func.is_private,
        }
    }
}

impl CodeGen for DebugCodeGen {
    fn codegen(&mut self, ir_module: IRModule) -> Module {
        Module {
            functions: ir_module
                .functions
                .into_iter()
                .map(|(_, f)| self.gen_func(f))
                .collect(),
            strings: ir_module.strings.into_iter().collect(),
            constants: Vec::new(),
        }
    }

    fn gen_all(&mut self, module: Module) -> String {
        let mut text = String::new();

        for func in module.functions {
            text += &format!("{}:\n", func.name);
            for mnemonic in func.mnemonics {
                let s = &format_mnemonic(&mnemonic, |temp| {
                    reg_name(temp).unwrap_or_else(|| format!("{}", temp))
                });
                text += &format!("    {}\n", s);
            }

            text += "\n";
        }

        text
    }
}
