use crate::codegen::{Function, Mnemonic};
use crate::color::{color, ColorResult};
use crate::ir::Temp;
use crate::liveness::{self, BasicBlock};
use crate::x64codegen;
use rustc_hash::FxHashMap;

fn rewrite(bbs: Vec<BasicBlock>, map: &FxHashMap<Temp, Temp>) -> Vec<Mnemonic> {
    let mut mnemonics = Vec::new();

    for bb in bbs {
        for mut mnemonic in bb.mnemonics {
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
