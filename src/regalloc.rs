use crate::codegen::{Function, Mnemonic};
use crate::color::{color, ColorResult};
use crate::ir::{Label, Temp};
use crate::liveness::{self, BasicBlock};
use crate::x64codegen;
use rustc_hash::FxHashMap;

fn rewrite(bbs: Vec<BasicBlock>, map: &FxHashMap<Temp, Temp>) -> Vec<Mnemonic> {
    let mut mnemonics = Vec::new();

    let labels: Vec<Option<Label>> = bbs.iter().map(|bb| bb.label).collect();

    for (i, bb) in bbs.into_iter().enumerate() {
        let mut iter = bb.mnemonics.into_iter();

        // Remove redundant label
        match iter.next() {
            Some(Mnemonic::Label { .. }) if bb.label_is_redundant => {}
            Some(mnemonic) => mnemonics.push(mnemonic),
            None => {}
        };

        for mut mnemonic in iter {
            match &mut mnemonic {
                Mnemonic::Op { src, dst, .. } | Mnemonic::Jump { src, dst, .. } => {
                    for src in src {
                        *src = *map.get(src).unwrap_or(src);
                    }
                    for dst in dst {
                        *dst = *map.get(dst).unwrap_or(dst);
                    }
                }
                Mnemonic::Move { src, dst, .. } => {
                    *src = *map.get(src).unwrap_or(src);
                    *dst = *map.get(dst).unwrap_or(dst);
                }
                _ => {}
            }

            // Remove redundant jump
            if let Mnemonic::Jump { jump, .. } = &mnemonic {
                if let Some(Some(label)) = labels.get(i + 1) {
                    if jump.len() == 1 && jump[0] == *label {
                        continue;
                    }
                }
            }

            mnemonics.push(mnemonic);
        }
    }

    mnemonics
}

fn remove_redundant_moves(mnemonics: Vec<Mnemonic>) -> Vec<Mnemonic> {
    mnemonics
        .into_iter()
        .filter(|mnemonic| match mnemonic {
            Mnemonic::Move { src, dst, .. } if *src == *dst => false,
            _ => true,
        })
        .collect()
}

pub fn regalloc(mut func: Function) -> Function {
    let (bbs, igraph) = liveness::calc_igraph(func.mnemonics);
    let result = color(&bbs, igraph, x64codegen::ALL_REGS.iter().copied().collect());

    match result {
        ColorResult::Spilled(..) => panic!("spilled"),
        ColorResult::Completed(map) => {
            func.mnemonics = rewrite(bbs, &map);
            func.mnemonics = remove_redundant_moves(func.mnemonics);
            func
        }
    }
}
