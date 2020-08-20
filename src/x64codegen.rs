use crate::codegen::*;
use crate::ir::{
    Cmp, Expr, Function as IRFunction, Label, Module as IRModule, Stmt, Temp, TEMP_FP,
};
use crate::token;
use lazy_static::lazy_static;
use rustc_hash::FxHashMap;
use std::iter;

fn escape_str(s: &str) -> String {
    token::escape_str(s)
}

fn result(gen: impl FnOnce(Temp)) -> Temp {
    let temp = Temp::new();
    gen(temp);
    temp
}

fn label_name(label: Label) -> String {
    format!(".L{}", label.raw())
}

lazy_static! {
    static ref RAX: Temp = Temp::new();
    static ref RBX: Temp = Temp::new();
    static ref RCX: Temp = Temp::new();
    static ref RDX: Temp = Temp::new();
    static ref RBP: Temp = Temp::new();
    static ref RSP: Temp = Temp::new();
    static ref RDI: Temp = Temp::new();
    static ref RSI: Temp = Temp::new();
    static ref R8: Temp = Temp::new();
    static ref R9: Temp = Temp::new();
    static ref R10: Temp = Temp::new();
    static ref R11: Temp = Temp::new();
    static ref R12: Temp = Temp::new();
    static ref R13: Temp = Temp::new();
    static ref R14: Temp = Temp::new();
    static ref R15: Temp = Temp::new();
    static ref ARG_REGS: [Temp; 6] = [*RDI, *RSI, *RDX, *RCX, *R8, *R9];
    static ref CALLEE_SAVES: [Temp; 5] = [*RBX, *R12, *R13, *R14, *R15];
    static ref CALLER_SAVES: [Temp; 2] = [*R10, *R11];
    // rax (return value) + ARG_REGS + CALLER_SAVES
    static ref CALL_DST: [Temp; 9] =
        [*RAX, *RDI, *RSI, *RDX, *RCX, *R8, *R9, *R10, *R11];
    pub static ref ALL_REGS: [Temp; 16] = [*RAX, *RBX, *RCX,*RDX, *RBP, *RSP, *RDI, *RSI, *R8, *R9, *R10, *R11, *R12, *R13, *R14, *R15];
}

pub const REG_PRIORITY: [u32; 16] = [1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0];

const REG64_NAMES: [&str; 16] = [
    "rax", "rbx", "rcx", "rdx", "rbp", "rsp", "rdi", "rsi", "r8", "r9", "r10", "r11", "r12", "r13",
    "r14", "r15",
];
// const REG32_NAMES: [&str; 16] = [
//     "eax", "ebx", "ecx", "edx", "ebp", "esp", "edi", "esi", "r8d", "r9d", "r10d", "r11d", "r12d",
//     "r13d", "r14d", "r15d",
// ];
// const REG16_NAMES: [&str; 16] = [
//     "ax", "bx", "cx", "dx", "bp", "sp", "di", "si", "r8w", "r9w", "r10w", "r11w", "r12w", "r13w",
//     "r14w", "r15w",
// ];
// const REG8_NAMES: [&str; 16] = [
//     "al", "bl", "cl", "dl", "bpl", "spl", "dil", "sil", "r8b", "r9b", "r10b", "r11b", "r12b",
//     "r13b", "r14b", "r15b",
// ];

pub fn reg64_name(temp: Temp) -> Option<&'static str> {
    if let Some(i) = ALL_REGS.iter().position(|t| *t == temp) {
        Some(REG64_NAMES[i])
    } else {
        None
    }
}

pub struct X64CodeGen {
    next_label: Option<Label>,
    epilogue_label: Option<Label>,
}

impl X64CodeGen {
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

    fn gen_effective_address(
        &mut self,
        ms: &mut Vec<Mnemonic>,
        src: &mut Vec<Temp>,
        expr: &Expr,
    ) -> Option<String> {
        match expr {
            Expr::Int(n) => Some(format!("{}", n)),
            Expr::Temp(temp) if *temp == *TEMP_FP => {
                self.gen_effective_address(ms, src, &Expr::Temp(*RBP))
            }
            Expr::Temp(temp) => {
                src.push(*temp);
                Some(format!("$s{}", src.len() - 1))
            }
            Expr::Addr(label) => Some(format!("{}", label_name(*label))),
            Expr::Add(lhs, rhs) => {
                let lhs = self.gen_effective_address(ms, src, &lhs)?;
                let rhs = self.gen_effective_address(ms, src, &rhs)?;
                Some(format!("{} + {}", lhs, rhs))
            }
            Expr::Sub(lhs, rhs) => {
                let lhs = self.gen_effective_address(ms, src, &lhs)?;
                let rhs = self.gen_effective_address(ms, src, &rhs)?;
                Some(format!("{} - {}", lhs, rhs))
            }
            Expr::Mul(lhs, rhs) => {
                let lhs = self.gen_effective_address(ms, src, &lhs)?;
                let rhs = self.gen_effective_address(ms, src, &rhs)?;
                Some(format!("{} * {}", lhs, rhs))
            }
            Expr::Div(lhs, rhs) => {
                let lhs = self.gen_effective_address(ms, src, &lhs)?;
                let rhs = self.gen_effective_address(ms, src, &rhs)?;
                Some(format!("{} / {}", lhs, rhs))
            }
            // Eject expressions that have side effects
            Expr::Call(..) | Expr::CCall(..) => {
                let temp = self.gen_expr(ms, expr.clone());
                self.gen_effective_address(ms, src, &Expr::Temp(temp))
            }
            _ => None,
        }
    }

    fn gen_expr(&mut self, ms: &mut Vec<Mnemonic>, expr: Expr) -> Temp {
        match expr {
            Expr::Temp(temp) if temp == *TEMP_FP => *RBP,
            Expr::Temp(temp) => temp,
            Expr::Int(n) => result(|dst| {
                ms.push(Mnemonic::Op {
                    text: format!("mov $d0, {}", n),
                    dst: vec![dst],
                    src: vec![],
                })
            }),
            Expr::Float(..) => unimplemented!(), // わからない
            Expr::Add(lhs, box Expr::Int(n)) => result(|dst| {
                let lhs = self.gen_expr(ms, *lhs);
                self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(lhs)));
                ms.push(Mnemonic::Op {
                    text: format!("add $d0, {}", n),
                    dst: vec![dst],
                    src: vec![dst],
                });
            }),
            Expr::Add(lhs, rhs) => result(|dst| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(lhs)));
                ms.push(Mnemonic::Op {
                    text: "add $d0, $s0".to_string(),
                    dst: vec![dst],
                    src: vec![rhs, dst],
                });
            }),
            Expr::Sub(lhs, box Expr::Int(n)) => result(|dst| {
                let lhs = self.gen_expr(ms, *lhs);
                self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(lhs)));
                ms.push(Mnemonic::Op {
                    text: format!("sub $d0, {}", n),
                    dst: vec![dst],
                    src: vec![dst],
                });
            }),
            Expr::Sub(lhs, rhs) => result(|dst| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(lhs)));
                ms.push(Mnemonic::Op {
                    text: "sub $d0, $s0".to_string(),
                    dst: vec![dst],
                    src: vec![rhs, dst],
                });
            }),
            Expr::Mul(lhs, box Expr::Int(n)) => result(|dst| {
                let lhs = self.gen_expr(ms, *lhs);
                ms.push(Mnemonic::Op {
                    text: format!("imul $d0, $s0, {}", n),
                    dst: vec![dst],
                    src: vec![lhs],
                });
            }),
            Expr::Mul(lhs, rhs) => result(|dst| {
                let lhs = self.gen_expr(ms, *lhs);
                let rhs = self.gen_expr(ms, *rhs);
                self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(lhs)));
                ms.push(Mnemonic::Op {
                    text: "imul $d0, $s0".to_string(),
                    dst: vec![dst],
                    src: vec![rhs, dst],
                });
            }),
            Expr::Div(lhs, rhs) => result(|dst| {
                let lhs = self.gen_expr(ms, *lhs);
                self.gen_stmt(ms, Stmt::Expr(*RAX, Expr::Temp(lhs)));
                self.gen_stmt(ms, Stmt::Expr(*RDX, Expr::Int(0)));
                let rhs = self.gen_expr(ms, *rhs);

                ms.push(Mnemonic::Op {
                    text: "idiv $s0".to_string(),
                    dst: vec![*RAX, *RDX],
                    src: vec![rhs, *RAX, *RDX],
                });
                self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(*RAX)));
            }),
            Expr::Not(expr) => result(|dst| {
                let expr = self.gen_expr(ms, *expr);
                self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(expr)));
                ms.push(Mnemonic::Op {
                    text: "not $d0".to_string(),
                    dst: vec![dst],
                    src: vec![dst],
                });
            }),
            Expr::Negative(expr) => result(|dst| {
                let expr = self.gen_expr(ms, *expr);
                self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(expr)));
                ms.push(Mnemonic::Op {
                    text: "neg $d0".to_string(),
                    dst: vec![dst],
                    src: vec![dst],
                });
            }),
            Expr::Addr(label) => result(|dst| {
                ms.push(Mnemonic::Op {
                    text: format!("lea $d0, {}", label_name(label)),
                    dst: vec![dst],
                    src: vec![],
                });
            }),
            Expr::Func(None, func) => result(|dst| {
                ms.push(Mnemonic::Op {
                    text: format!("lea $d0, {}", func),
                    dst: vec![dst],
                    src: vec![],
                })
            }),
            Expr::Load(addr) => result(|dst| {
                let mut src = Vec::new();
                let mut ms_to_add = Vec::new();
                let text = self.gen_effective_address(&mut ms_to_add, &mut src, &addr);

                match text {
                    Some(text) => {
                        ms.append(&mut ms_to_add);
                        ms.push(Mnemonic::Op {
                            text: format!("mov $d0, [{}]", text),
                            dst: vec![dst],
                            src,
                        });
                    }
                    None => {
                        let addr = self.gen_expr(ms, *addr);
                        ms.push(Mnemonic::Op {
                            text: "mov $d0, [$s0]".to_string(),
                            dst: vec![dst],
                            src: vec![addr],
                        });
                    }
                }
            }),
            Expr::CCall(func, args) | Expr::Call(box Expr::Func(None, func), args) => {
                result(|dst| {
                    // TODO: Add support of more than 6 arguments
                    if args.len() > 6 {
                        panic!("more than 6 arguments are unsupported");
                    }

                    let mut src = vec![*RBP, *RSP];

                    for (arg, reg) in args.into_iter().zip(ARG_REGS.iter()) {
                        let temp = self.gen_expr(ms, arg);
                        self.gen_stmt(ms, Stmt::Expr(*reg, Expr::Temp(temp)));
                        src.push(*reg);
                    }

                    ms.push(Mnemonic::Op {
                        text: format!("call {}", func),
                        dst: CALL_DST.to_vec(),
                        src,
                    });

                    self.gen_stmt(ms, Stmt::Expr(dst, Expr::Temp(*RAX)));
                })
            }
            Expr::Seq(..) | Expr::DSeq(..) => panic!("no flatten"),
            _ => unimplemented!(),
        }
    }

    fn gen_stmt(&mut self, ms: &mut Vec<Mnemonic>, stmt: Stmt) {
        match stmt {
            Stmt::Expr(dst, Expr::Temp(src)) => {
                ms.push(Mnemonic::Move {
                    text: "mov $d0, $s0".to_string(),
                    dst,
                    src,
                });
            }
            Stmt::Expr(dst, Expr::Int(n)) => {
                ms.push(Mnemonic::Op {
                    text: format!("mov $d0, {}", n),
                    dst: vec![dst],
                    src: vec![],
                });
            }
            Stmt::Expr(dst, expr) => {
                let expr = self.gen_expr(ms, expr);
                ms.push(Mnemonic::Move {
                    text: "mov $d0, $s0".to_string(),
                    dst,
                    src: expr,
                });
            }
            Stmt::Store(addr, expr) => {
                let expr = self.gen_expr(ms, expr);

                let mut src = vec![expr];
                let mut ms_to_add = Vec::new();
                let text = self.gen_effective_address(&mut ms_to_add, &mut src, &addr);

                match text {
                    Some(text) => {
                        ms.append(&mut ms_to_add);
                        ms.push(Mnemonic::Op {
                            text: format!("mov [{}], $s0", text),
                            dst: vec![],
                            src,
                        });
                    }
                    None => {
                        let addr = self.gen_expr(ms, addr);
                        ms.push(Mnemonic::Op {
                            text: "mov [$s0], $s1".to_string(),
                            dst: vec![],
                            src: vec![addr, expr],
                        })
                    }
                }
            }
            Stmt::JumpIf(cmp, lhs, rhs, label) => {
                let opcode = match cmp {
                    Cmp::LessThan => "jl",
                    Cmp::LessThanOrEqual => "jle",
                    Cmp::GreaterThan => "jg",
                    Cmp::GreaterThanOrEqual => "jge",
                    Cmp::Equal => "je",
                    Cmp::NotEqual => "jne",
                };

                match (lhs, rhs) {
                    (Expr::Load(addr), rhs) => {
                        let mut src = Vec::new();
                        let mut ms_to_add = Vec::new();
                        let text = self.gen_effective_address(&mut ms_to_add, &mut src, &addr);

                        match text {
                            Some(text) => {
                                ms.append(&mut ms_to_add);

                                let rhs = self.gen_expr(ms, rhs);
                                src.push(rhs);

                                ms.push(Mnemonic::Op {
                                    text: format!("cmp [{}], $s{}", text, src.len() - 1),
                                    dst: vec![],
                                    src,
                                });
                            }
                            None => {
                                let addr = self.gen_expr(ms, *addr);
                                let rhs = self.gen_expr(ms, rhs);
                                ms.push(Mnemonic::Op {
                                    text: "cmp [$s0], $s1".to_string(),
                                    dst: vec![],
                                    src: vec![addr, rhs],
                                });
                            }
                        }
                    }
                    (lhs, Expr::Int(n)) => {
                        let lhs = self.gen_expr(ms, lhs);
                        ms.push(Mnemonic::Op {
                            text: format!("cmp $s0, {}", n),
                            dst: vec![],
                            src: vec![lhs],
                        });
                    }
                    (lhs, Expr::Load(addr)) => {
                        let lhs = self.gen_expr(ms, lhs);

                        let mut src = vec![lhs];
                        let mut ms_to_add = Vec::new();
                        let text = self.gen_effective_address(&mut ms_to_add, &mut src, &addr);

                        match text {
                            Some(text) => {
                                ms.append(&mut ms_to_add);
                                ms.push(Mnemonic::Op {
                                    text: format!("cmp $s0, [{}]", text),
                                    dst: vec![],
                                    src,
                                });
                            }
                            None => {
                                let addr = self.gen_expr(ms, *addr);
                                ms.push(Mnemonic::Op {
                                    text: "cmp $s0, [$s1]".to_string(),
                                    dst: vec![],
                                    src: vec![lhs, addr],
                                });
                            }
                        }
                    }
                    (lhs, rhs) => {
                        let lhs = self.gen_expr(ms, lhs);
                        let rhs = self.gen_expr(ms, rhs);
                        ms.push(Mnemonic::Op {
                            text: "cmp $s0, $s1".to_string(),
                            dst: vec![],
                            src: vec![lhs, rhs],
                        });
                    }
                }

                let mut jump = vec![label];
                if let Some(next_label) = self.next_label {
                    jump.push(next_label);
                }

                ms.push(Mnemonic::Jump {
                    text: format!("{} {}", opcode, label_name(label)),
                    dst: vec![],
                    src: vec![],
                    jump,
                })
            }
            Stmt::Jump(label) => ms.push(Mnemonic::Jump {
                text: format!("jmp {}", label_name(label)),
                dst: vec![],
                src: vec![],
                jump: vec![label],
            }),
            Stmt::Return(expr) => {
                let expr = self.gen_expr(ms, expr);
                self.gen_stmt(ms, Stmt::Expr(*RAX, Expr::Temp(expr)));
                self.gen_stmt(ms, Stmt::Jump(self.epilogue_label.unwrap()));
            }
            Stmt::Label(..) => panic!("there is a label"),
        }
    }

    fn gen_prologue(
        &mut self,
        ms: &mut Vec<Mnemonic>,
        params: Vec<Temp>,
        stack_size: usize,
    ) -> Vec<Temp> {
        // push rbp
        // mov rbp, rsp
        // sub rsp, stack_size

        // Allocate stack frame
        if stack_size != 0 {
            ms.push(Mnemonic::Op {
                text: "push $s0".to_string(),
                dst: vec![*RSP],
                src: vec![*RBP, *RSP],
            });
            self.gen_stmt(ms, Stmt::Expr(*RBP, Expr::Temp(*RSP)));
            self.gen_stmt(
                ms,
                Stmt::Expr(
                    *RSP,
                    Expr::Sub(box Expr::Temp(*RSP), box Expr::Int(stack_size as i64)),
                ),
            );
        }

        // Save callee save registers
        let saved_temps: Vec<Temp> = iter::repeat_with(|| Temp::new())
            .take(CALLEE_SAVES.len())
            .collect();
        for (temp, reg) in saved_temps.iter().zip(&*CALLEE_SAVES) {
            self.gen_stmt(ms, Stmt::Expr(*temp, Expr::Temp(*reg)));
        }

        for (param, reg) in params.into_iter().zip(ARG_REGS.iter()) {
            self.gen_stmt(ms, Stmt::Expr(param, Expr::Temp(*reg)));
        }

        saved_temps
    }

    fn gen_epilogue(
        &mut self,
        ms: &mut Vec<Mnemonic>,
        epilogue_label: Label,
        stack_size: usize,
        saved_temps: Vec<Temp>,
    ) {
        ms.push(Self::gen_label(epilogue_label));

        // mov rsp, rbp
        // pop rbp

        // Restore callee save registers
        for (temp, reg) in saved_temps.iter().zip(&*CALLEE_SAVES) {
            self.gen_stmt(ms, Stmt::Expr(*reg, Expr::Temp(*temp)));
        }

        if stack_size != 0 {
            self.gen_stmt(ms, Stmt::Expr(*RSP, Expr::Temp(*RBP)));
            ms.push(Mnemonic::Op {
                text: "pop $d0".to_string(),
                dst: vec![*RBP, *RSP],
                src: vec![*RSP],
            });
        }

        ms.push(Mnemonic::Op {
            text: "ret".to_string(),
            dst: vec![],
            src: vec![*RBP, *RSP, *RAX],
        });
    }

    fn gen_func(&mut self, ir_func: IRFunction) -> Function {
        let mut mnemonics = Vec::new();

        self.epilogue_label = Some(Label::new());
        let saved_temps = self.gen_prologue(&mut mnemonics, ir_func.params, ir_func.stack_size);

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

        self.gen_epilogue(
            &mut mnemonics,
            self.epilogue_label.unwrap(),
            ir_func.stack_size,
            saved_temps,
        );
        self.epilogue_label = None;

        Function {
            name: ir_func.name,
            stack_size: ir_func.stack_size,
            mnemonics,
            is_private: ir_func.is_private,
        }
    }
}

impl CodeGen for X64CodeGen {
    fn codegen(&mut self, ir_module: IRModule) -> Module {
        let funcs: Vec<Function> = ir_module
            .functions
            .into_iter()
            .map(|(_, f)| self.gen_func(f))
            .collect();
        let strings: Vec<(Label, String)> = ir_module.strings.into_iter().collect();

        Module {
            functions: funcs,
            strings,
            constants: Vec::new(),
        }
    }

    fn spill(&mut self, mut func: Function, spilled_temps: &[Temp]) -> Function {
        fn load_from_memory(slf: &mut X64CodeGen, ms: &mut Vec<Mnemonic>, loc: usize) -> Temp {
            let t = Temp::new();
            slf.gen_stmt(
                ms,
                Stmt::Expr(
                    t,
                    Expr::Load(box Expr::Sub(
                        box Expr::Temp(*TEMP_FP),
                        box Expr::Int(loc as i64),
                    )),
                ),
            );
            t
        };

        fn save_to_memory(slf: &mut X64CodeGen, ms: &mut Vec<Mnemonic>, loc: usize, temp: Temp) {
            slf.gen_stmt(
                ms,
                Stmt::Store(
                    Expr::Sub(box Expr::Temp(*TEMP_FP), box Expr::Int(loc as i64)),
                    Expr::Temp(temp),
                ),
            );
        };

        let prev_stack_size = func.stack_size;

        // Allocate variables to save spilled temporaries
        let mut spilled_temp_locs = FxHashMap::default();
        for temp in spilled_temps {
            func.stack_size += 8;
            spilled_temp_locs.insert(*temp, func.stack_size);
        }

        let mut mnemonics = Vec::with_capacity(func.mnemonics.len());

        // Allocate stack frame if necessary
        if prev_stack_size <= 0 {
            mnemonics.push(Mnemonic::Op {
                text: "push $s0".to_string(),
                dst: vec![*RSP],
                src: vec![*RBP, *RSP],
            });
            self.gen_stmt(&mut mnemonics, Stmt::Expr(*RBP, Expr::Temp(*RSP)));
            self.gen_stmt(
                &mut mnemonics,
                Stmt::Expr(
                    *RSP,
                    Expr::Sub(box Expr::Temp(*RSP), box Expr::Int(func.stack_size as i64)),
                ),
            );
        }

        // Insert mnemonics for saving spilled temporaries
        for mut mnemonic in func.mnemonics {
            let locs: Vec<(usize, Temp)> = match &mut mnemonic {
                Mnemonic::Op { src, dst, .. } | Mnemonic::Jump { src, dst, .. } => {
                    for spilled in src.iter_mut().filter(|t| spilled_temp_locs.contains_key(t)) {
                        let loc = spilled_temp_locs[spilled];
                        *spilled = load_from_memory(self, &mut mnemonics, loc);
                    }

                    dst.iter()
                        .filter(|t| spilled_temp_locs.contains_key(t))
                        .map(|t| (spilled_temp_locs[t], *t))
                        .collect()
                }
                Mnemonic::Move { dst, src, .. } => {
                    if let Some(loc) = spilled_temp_locs.get(src) {
                        *src = load_from_memory(self, &mut mnemonics, *loc);
                    }

                    if let Some(loc) = spilled_temp_locs.get(dst) {
                        vec![(*loc, *dst)]
                    } else {
                        vec![]
                    }
                }
                _ => vec![],
            };

            mnemonics.push(mnemonic);

            for (loc, temp) in locs {
                save_to_memory(self, &mut mnemonics, loc, temp);
            }
        }

        // Remove ret to insert epilogue
        assert!(match mnemonics.last() {
            Some(Mnemonic::Op { text, .. }) if text == "ret" => true,
            _ => false,
        });
        mnemonics.pop().unwrap();

        // Restore stack frame if necessary
        if prev_stack_size <= 0 {
            self.gen_stmt(&mut mnemonics, Stmt::Expr(*RSP, Expr::Temp(*RBP)));
            mnemonics.push(Mnemonic::Op {
                text: "pop $d0".to_string(),
                dst: vec![*RBP, *RSP],
                src: vec![*RSP],
            });
        }

        mnemonics.push(Mnemonic::Op {
            text: "ret".to_string(),
            dst: vec![],
            src: vec![*RBP, *RSP, *RAX],
        });

        func.mnemonics = mnemonics;
        func
    }

    fn gen_all(&mut self, module: Module) -> String {
        let mut out = String::new();

        // Strings
        out += "section data\n";

        for (label, s) in module.strings {
            out += &format!("{}:\n", label_name(label));
            out += &format!("    db \"{}\", 0\n", escape_str(&s));
        }

        // Code
        out += "\n";
        out += "section text\n";

        for func in module.functions {
            if !func.is_private {
                out += &format!("global {}\n", func.name);
            }

            out += &format!("{}:\n", func.name);
            for mnemonic in func.mnemonics {
                out += &format!(
                    "    {}\n",
                    format_mnemonic(&mnemonic, |t| reg64_name(t)
                        .map(String::from)
                        .unwrap_or_else(|| format!("{}", t)))
                );
            }
        }

        out
    }
}
