use crate::codegen::{reg_name, CodeGen, Function, Mnemonic};
use crate::color::{color, ColorResult};
use crate::dump::format_iter;
use crate::ir::{Label, Temp};
use crate::liveness::{self, BasicBlock};
use log::debug;
use rustc_hash::{FxHashMap, FxHashSet};

fn replace_operand(mut mnemonic: Mnemonic, map: &FxHashMap<Temp, Temp>) -> Mnemonic {
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

    mnemonic
}

fn rewrite(bbs: Vec<BasicBlock>, map: &FxHashMap<Temp, Temp>) -> Vec<Mnemonic> {
    let mut mnemonics = Vec::new();

    let labels: Vec<Option<Label>> = bbs.iter().map(|bb| bb.label).collect();

    for (i, bb) in bbs.into_iter().enumerate() {
        for (j, mnemonic) in bb.mnemonics.into_iter().enumerate() {
            if j == 0 && bb.label_is_redundant {
                assert!(match mnemonic {
                    Mnemonic::Label { .. } => true,
                    _ => false,
                });
                continue;
            }

            let mnemonic = replace_operand(mnemonic, map);

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

pub fn regalloc(
    mut func: Function,
    codegen: &mut dyn CodeGen,
    registers: FxHashSet<Temp>,
    priority: FxHashMap<Temp, u32>,
) -> Function {
    debug!("Allocate registers in function `{}`", func.name);

    let func_for_spill = func.clone();

    let (bbs, igraph, spill_priority) = liveness::calc_igraph(func.mnemonics);
    let result = color(
        &bbs,
        igraph,
        registers.clone(),
        priority.clone(),
        spill_priority.clone(),
    );

    match result {
        ColorResult::Spilled(spilled_temps) => {
            debug!(
                "Spilled {}",
                format_iter(spilled_temps.iter().copied().map(reg_name), ", ")
            );

            let func = codegen.spill(func_for_spill, &spilled_temps);

            // println!("============");
            // for mnemonic in &func.mnemonics {
            //     println!("{}", crate::codegen::format_mnemonic(mnemonic, reg_name));
            // }
            // println!("============");

            regalloc(func, codegen, registers, priority)
        }
        ColorResult::Completed(map) => {
            func.mnemonics = rewrite(bbs, &map);
            func.mnemonics = remove_redundant_moves(func.mnemonics);
            func
        }
    }
}
