use crate::id::Id;
use crate::ir::{Label, Module as IRModule, Temp};

#[derive(Debug, Clone, PartialEq)]
pub enum Mnemonic {
    Op {
        text: String,
        dst: Vec<Temp>,
        src: Vec<Temp>,
    },
    Jump {
        text: String,
        dst: Vec<Temp>,
        src: Vec<Temp>,
        jump: Vec<Label>,
    },
    Label {
        text: String,
        label: Label,
    },
    Move {
        text: String,
        dst: Temp,
        src: Temp,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Id,
    pub mnemonics: Vec<Mnemonic>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub strings: Vec<(Label, String)>,
    pub constants: Vec<(Label, Vec<u8>)>,
    pub functions: Vec<Function>,
}

pub trait CodeGen {
    fn codegen(&mut self, ir_module: IRModule) -> Module;
    fn gen_all(&mut self, module: Module) -> String;
}

pub fn format_mnemonic(mnemonic: &Mnemonic, mut get_name: impl FnMut(Temp) -> String) -> String {
    match mnemonic {
        Mnemonic::Op { text, dst, src } | Mnemonic::Jump { text, dst, src, .. } => {
            let mut result = text.to_string();
            for (i, temp) in dst.iter().enumerate() {
                result = result.replace(&format!("$d{}", i), &get_name(*temp));
            }
            for (i, temp) in src.iter().enumerate() {
                result = result.replace(&format!("$s{}", i), &get_name(*temp));
            }

            result
        }
        Mnemonic::Label { text, .. } => text.clone(),
        Mnemonic::Move { text, dst, src } => text
            .to_string()
            .replace("$d0", &get_name(*dst))
            .replace("$s0", &get_name(*src)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_mnemonic() {
        let r1 = Temp::new();
        let r2 = Temp::new();
        let r3 = Temp::new();
        let r4 = Temp::new();
        let label = Label::new();

        let get_name = |temp| {
            if temp == r1 {
                "r1"
            } else if temp == r2 {
                "r2"
            } else if temp == r3 {
                "r3"
            } else if temp == r4 {
                "r4"
            } else {
                panic!("unknown temporary")
            }
            .to_string()
        };

        let code = vec![
            Mnemonic::Op {
                text: "mov $d0, $s0".to_string(),
                dst: vec![r1],
                src: vec![r2],
            },
            Mnemonic::Label {
                text: ".L10000:".to_string(),
                label,
            },
            Mnemonic::Move {
                text: "mov $d0, $s0".to_string(),
                dst: r3,
                src: r1,
            },
            Mnemonic::Op {
                text: "add $d0, $s0, $s1".to_string(),
                dst: vec![r4],
                src: vec![r2, r3],
            },
            Mnemonic::Jump {
                text: "jump .L10000 if $s0 = 0".to_string(),
                dst: vec![],
                src: vec![r4],
                jump: vec![label],
            },
        ];

        let mut assembly = String::new();
        for mnemonic in &code {
            assembly += &format_mnemonic(mnemonic, get_name.clone());
            assembly.push('\n');
        }

        let mut expected = "mov r1, r2\n".to_string();
        expected += &format!(".L10000:\n");
        expected += r#"mov r3, r1
add r4, r2, r3
jump .L10000 if r4 = 0
"#;

        assert_eq!(expected, assembly);
    }
}
