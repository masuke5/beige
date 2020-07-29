use crate::codegen::Mnemonic;
use crate::graph::Graph;
use crate::ir::{Label, Temp};
use rustc_hash::{FxHashMap, FxHashSet};
use std::hash::Hash;

pub type InterferenceGraph = Graph<Temp>;

fn hs<T: Clone + Hash + Eq>(values: &[T]) -> FxHashSet<T> {
    values.iter().cloned().collect()
}

#[derive(Debug, PartialEq, Clone)]
pub struct Liveness {
    pub ins: FxHashSet<Temp>,
    pub outs: FxHashSet<Temp>,
}

impl Liveness {
    pub fn new() -> Self {
        Self {
            ins: FxHashSet::default(),
            outs: FxHashSet::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BasicBlock {
    pub label: Option<Label>,
    pub mnemonics: Vec<Mnemonic>,
}

impl BasicBlock {
    pub fn new(label: Option<Label>) -> Self {
        Self {
            label,
            mnemonics: Vec::new(),
        }
    }
}

fn generate_bb(mnemonics: Vec<Mnemonic>) -> Vec<BasicBlock> {
    let mut bbs = vec![BasicBlock::new(None)];

    for mnemonic in mnemonics {
        let last_bb = bbs.last_mut().unwrap();

        match mnemonic {
            Mnemonic::Label { label, .. } => {
                bbs.push(BasicBlock::new(Some(label)));
                bbs.last_mut().unwrap().mnemonics.push(mnemonic);
            }
            mnemonic => last_bb.mnemonics.push(mnemonic),
        }
    }

    bbs
}

fn gen_dataflow(bbs: &[BasicBlock]) -> Graph<usize> {
    let mut graph = Graph::new();
    let mut bb_map = FxHashMap::default();

    for (i, bb) in bbs.iter().enumerate() {
        graph.insert(i);
        if let Some(label) = bb.label {
            bb_map.insert(label, i);
        }
    }

    for (i, bb) in bbs.iter().enumerate() {
        match bb.mnemonics.last() {
            Some(Mnemonic::Jump { jump, .. }) => {
                for label in jump {
                    graph.add_edge(&i, &bb_map[label]);
                }
            }
            _ if i < graph.len() - 1 => {
                graph.add_edge(&i, &(i + 1));
            }
            _ => {}
        }
    }

    graph
}

fn gen_liveness(bbs: &[BasicBlock], graph: &Graph<usize>) -> Vec<Vec<Liveness>> {
    // Liveness analysis in basic blocks

    let mut livenesses_in_bbs = vec![Liveness::new(); bbs.len()];

    let mut gens: Vec<FxHashSet<Temp>> = Vec::new();
    let mut kills: Vec<FxHashSet<Temp>> = Vec::new();

    for bb in bbs {
        let mut gen = FxHashSet::default();
        let mut kill = FxHashSet::default();

        for mnemonic in &bb.mnemonics {
            match mnemonic {
                Mnemonic::Op { src, dst, .. } | Mnemonic::Jump { src, dst, .. } => {
                    gen = &gen | &(&hs(src) - &kill);
                    kill = &kill | &hs(dst);
                }
                Mnemonic::Move { src, dst, .. } => {
                    // The same as the above arm
                    if !kill.contains(src) {
                        gen.insert(*src);
                    }
                    kill.insert(*dst);
                }
                _ => {}
            }
        }

        gens.push(gen);
        kills.push(kill);
    }

    loop {
        let prev = livenesses_in_bbs.clone();

        for i in 0..bbs.len() {
            let ins = &gens[i] | &(&livenesses_in_bbs[i].outs - &kills[i]);
            let outs = graph
                .succ(&i)
                .map(|i| &livenesses_in_bbs[*i].ins)
                .fold(FxHashSet::default(), |outs, ins| &outs | ins);
            livenesses_in_bbs[i] = Liveness { ins, outs };
        }

        if prev == livenesses_in_bbs {
            break;
        }
    }

    // Liveness analysis in mnemonics

    let mut livenesses = Vec::new();

    for (i, bb) in bbs.iter().enumerate() {
        let mut prev_ins = livenesses_in_bbs[i].outs.clone();
        livenesses.push(Vec::new());

        for mnemonic in bb.mnemonics.iter().rev() {
            let (ins, outs) = match mnemonic {
                Mnemonic::Op { src, dst, .. } | Mnemonic::Jump { src, dst, .. } => {
                    (&hs(src) | &(&prev_ins - &hs(dst)), prev_ins.clone())
                }
                Mnemonic::Move { src, dst, .. } => {
                    (&hs(&[*src]) | &(&prev_ins - &hs(&[*dst])), prev_ins.clone())
                }
                _ => (prev_ins.clone(), prev_ins.clone()),
            };

            prev_ins = ins.clone();
            livenesses[i].push(Liveness { ins, outs });
        }

        livenesses[i] = livenesses[i].iter().cloned().rev().collect();
    }

    livenesses
}

// interference graph
pub fn calc_igraph(mnemonics: Vec<Mnemonic>) -> (Vec<BasicBlock>, InterferenceGraph) {
    let bbs = generate_bb(mnemonics);

    let graph = gen_dataflow(&bbs);

    let livenesses = gen_liveness(&bbs, &graph);

    // Create interference graph

    let mut igraph = Graph::new();

    for (i, bb) in bbs.iter().enumerate() {
        for (j, mnemonic) in bb.mnemonics.iter().enumerate() {
            let liveness = &livenesses[i][j];
            match mnemonic {
                Mnemonic::Op { dst, .. } | Mnemonic::Jump { dst, .. } => {
                    for dst in dst {
                        for out in &liveness.outs {
                            if !igraph.contains(dst) {
                                igraph.insert(*dst);
                            }
                            if !igraph.contains(out) {
                                igraph.insert(*out);
                            }

                            igraph.add_edge(dst, out);
                        }
                    }
                }
                Mnemonic::Move { dst, .. } => {
                    if !igraph.contains(dst) {
                        igraph.insert(*dst);
                    }

                    for out in &liveness.outs {
                        if !igraph.contains(out) {
                            igraph.insert(*out);
                        }
                        igraph.add_edge(dst, out);
                    }
                }
                _ => {}
            }
        }
    }

    // dump_igraph(&bbs, &livenesses, &igraph);

    (bbs, igraph)
}

#[allow(dead_code)]
fn dump_igraph(bbs: &[BasicBlock], livenesses: &Vec<Vec<Liveness>>, igraph: &Graph<Temp>) {
    use crate::codegen::format_mnemonic;
    use crate::dump::format_iter;
    use crate::x64codegen::reg64_name;
    use itertools::Itertools;

    // Return x64 register name if possible
    let temp_name = |temp| {
        reg64_name(temp)
            .map(|t| format!("{}", t))
            .unwrap_or_else(|| format!("{}", temp))
    };

    // Print basic blocks and livenesses
    let mnemonic_count: usize = bbs.iter().map(|bb| bb.mnemonics.len()).sum();
    let width = format!("{}", mnemonic_count).len();

    for (i, bb) in bbs.iter().enumerate() {
        println!("============= BB {} ===============", i);
        for (j, mnemonic) in bb.mnemonics.iter().enumerate() {
            let s = format_mnemonic(mnemonic, temp_name);
            println!("{:<width$}  \x1b[94m{}\x1b[0m", j, s, width = width);

            let liveness = &livenesses[i][j];
            println!(
                "  in:  {}",
                format_iter(liveness.ins.iter().copied().map(temp_name), ",")
            );
            println!(
                "  out: {}",
                format_iter(liveness.outs.iter().copied().map(temp_name), ",")
            );
        }
    }

    println!("============================");

    // Print interference graph
    for temp in igraph.iter().sorted_by_key(|t| t.raw()) {
        println!(
            "{}\t{}",
            temp_name(*temp),
            format_iter(
                igraph
                    .adjacent(temp)
                    .copied()
                    .map(temp_name)
                    .collect::<Vec<_>>(),
                ","
            )
        );
    }
}
