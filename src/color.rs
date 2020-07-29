use crate::codegen::Mnemonic;
use crate::graph::Graph;
use crate::ir::Temp;
use crate::liveness::{BasicBlock, InterferenceGraph};
use rustc_hash::{FxHashMap, FxHashSet};

// TODO: 効率の良い実装

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ColorResult {
    Spilled(Vec<Temp>),
    Completed(FxHashMap<Temp, Temp>),
}

struct WorkGraph {
    degrees: FxHashMap<Temp, usize>,
    adjacents: FxHashMap<Temp, FxHashSet<Temp>>,
    aliases: FxHashMap<Temp, Temp>,
}

impl WorkGraph {
    fn new() -> Self {
        Self {
            degrees: FxHashMap::default(),
            adjacents: FxHashMap::default(),
            aliases: FxHashMap::default(),
        }
    }

    fn from_graph(graph: Graph<Temp>) -> Self {
        // Initialize degree map
        let mut degrees = FxHashMap::default();
        for temp in graph.iter() {
            let degree = graph.adjacent(temp).count();
            degrees.insert(*temp, degree);
        }

        // Initialize adjacent map
        let mut adjacents = FxHashMap::default();
        for temp in graph.iter() {
            let adjacent = graph.adjacent(temp).copied().collect();
            adjacents.insert(*temp, adjacent);
        }

        Self {
            degrees,
            adjacents,
            aliases: FxHashMap::default(),
        }
    }

    fn alias(&self, temp: Temp) -> Temp {
        self.aliases.get(&temp).copied().unwrap_or(temp)
    }

    fn adjacent(&self, temp: Temp) -> impl Iterator<Item = Temp> + '_ {
        self.adjacents[&self.alias(temp)].iter().copied()
    }

    fn degree(&self, temp: Temp) -> usize {
        self.degrees[&self.alias(temp)]
    }

    fn remove(&mut self, temp: Temp) {
        let temp = self.alias(temp);

        for adj in self.adjacents.remove(&temp).unwrap() {
            self.adjacents.get_mut(&adj).unwrap().remove(&temp);
            *self.degrees.get_mut(&adj).unwrap() -= 1;
        }

        self.degrees.remove(&temp);
    }

    fn coalesce(&mut self, a: Temp, b: Temp) {
        let a = self.alias(a);
        let b = self.alias(b);

        if a == b {
            return;
        }

        if self.adjacents.get_mut(&a).unwrap().remove(&b) {
            *self.degrees.get_mut(&a).unwrap() -= 1;
        }
        self.adjacents.get_mut(&b).unwrap().remove(&a);

        for adj in self.adjacent(b).collect::<Vec<_>>() {
            if self.adjacents.get_mut(&a).unwrap().insert(adj) {
                *self.degrees.get_mut(&a).unwrap() += 1;
            } else {
                *self.degrees.get_mut(&adj).unwrap() -= 1;
            }

            self.adjacents.get_mut(&adj).unwrap().remove(&b);
        }

        self.adjacents.remove(&b);
        self.degrees.remove(&b);
        self.aliases.insert(b, a);
    }
}

struct Color {
    igraph: InterferenceGraph,
    move_graph: Graph<Temp>,
    simplified_temps: Vec<Temp>,
    removed_temps: FxHashSet<Temp>,
    registers: FxHashSet<Temp>,
    colored_temps: FxHashMap<Temp, Temp>,
    spilled_temps: Vec<Temp>,
}

impl Color {
    fn new(igraph: InterferenceGraph, registers: FxHashSet<Temp>, move_graph: Graph<Temp>) -> Self {
        Self {
            igraph,
            move_graph,
            simplified_temps: Vec::new(),
            removed_temps: FxHashSet::default(),
            spilled_temps: Vec::new(),
            colored_temps: FxHashMap::default(),
            registers,
        }
    }

    fn degree(&self, temp: Temp) -> usize {
        self.adjacent(temp).count()
    }

    fn is_precolored(&self, temp: Temp) -> bool {
        self.registers.contains(&temp)
    }

    fn is_significant_degree(&self, temp: Temp) -> bool {
        self.degree(temp) >= self.registers.len()
    }

    fn is_interference(&self, a: Temp, b: Temp) -> bool {
        self.adjacent(a).find(|t| *t == b).is_some()
    }

    fn is_move_related(&self, temp: Temp) -> bool {
        if !self.move_graph.contains(&temp) {
            false
        } else {
            self.move_graph.adjacent(&temp).count() == 0
        }
    }

    fn adjacent(&self, temp: Temp) -> impl Iterator<Item = Temp> + '_ {
        self.igraph
            .adjacent(&temp)
            .copied()
            .filter(move |t| !self.removed_temps.contains(&t))
    }

    fn simplify(&mut self) {
        // Find simplification target
        let temp = self
            .igraph
            .iter()
            .filter(|t| !self.removed_temps.contains(*t))
            .find(|temp| {
                !self.is_precolored(**temp)
                    && !self.is_significant_degree(**temp)
                    && !self.is_move_related(**temp)
            })
            .copied();
        let temp = match temp {
            Some(t) => t,
            // If a target is not found, do nothing
            None => return,
        };

        self.simplified_temps.push(temp);
        self.removed_temps.insert(temp);
    }

    fn coalesce(&mut self) {
        unimplemented!()
    }

    fn freeze(&mut self) {
        unimplemented!()
    }

    fn select_spill(&mut self) {
        unimplemented!()
    }

    fn select_colors(&mut self) {
        while let Some(temp) = self.simplified_temps.pop() {
            let mut ok_regs = self.registers.clone();
            for adj in self.igraph.adjacent(&temp) {
                if let Some(reg) = self.registers.get(adj) {
                    ok_regs.remove(reg);
                }
                if let Some(reg) = self.colored_temps.get(adj) {
                    ok_regs.remove(reg);
                }
            }

            if let Some(reg) = ok_regs.iter().next() {
                self.colored_temps.insert(temp, *reg);
            } else {
                self.spilled_temps.push(temp);
            }
        }
    }

    fn is_completed(&self) -> bool {
        self.igraph
            .iter()
            .filter(|t| !self.is_precolored(**t))
            .count()
            == self.removed_temps.len()
    }

    fn color(mut self) -> ColorResult {
        while !self.is_completed() {
            // self.dump();
            self.simplify();
            // self.coalesce();
            // self.freeze();
            // self.select_spill();
        }

        self.select_colors();

        if self.spilled_temps.is_empty() {
            ColorResult::Completed(self.colored_temps)
        } else {
            ColorResult::Spilled(self.spilled_temps)
        }
    }

    #[allow(dead_code)]
    fn dump(&self) {
        use crate::dump::format_iter;

        println!("##########################");

        println!("Simplified: {}", format_iter(&self.simplified_temps, ","));
        println!("Removed: {}", format_iter(&self.removed_temps, ","));

        println!("----------------");

        println!("Colored:");
        for (temp, reg) in &self.colored_temps {
            println!("{} -> {}", temp, reg);
        }

        println!("Spilled: {}", format_iter(&self.spilled_temps, ","));
    }
}

pub fn color(
    bbs: &[BasicBlock],
    igraph: InterferenceGraph,
    registers: FxHashSet<Temp>,
) -> ColorResult {
    let mut move_graph = Graph::new();
    for bb in bbs {
        for mnemonic in &bb.mnemonics {
            match mnemonic {
                Mnemonic::Move { src, dst, .. } => {
                    move_graph.insert(*src);
                    move_graph.insert(*dst);
                    move_graph.add_edge(src, dst);
                }
                _ => {}
            }
        }
    }

    let color = Color::new(igraph, registers, move_graph);
    color.color()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workgraph_from_graph() {
        let a = Temp::new();
        let b = Temp::new();
        let c = Temp::new();
        let d = Temp::new();

        let mut graph = Graph::new();
        graph.insert(a);
        graph.insert(b);
        graph.insert(c);
        graph.insert(d);
        graph.add_edge(&a, &c);
        graph.add_edge(&a, &b);
        graph.add_edge(&a, &d);
        graph.add_edge(&d, &c);

        let graph = WorkGraph::from_graph(graph);

        let adjacent: Vec<_> = graph.adjacent(c).collect();
        assert_eq!(2, adjacent.len());
        assert!(adjacent.contains(&a));
        assert!(adjacent.contains(&d));

        assert_eq!(3, graph.degree(a));
        assert_eq!(2, graph.degree(d));
    }

    #[test]
    fn test_workgraph_remove() {
        let a = Temp::new();
        let b = Temp::new();
        let c = Temp::new();
        let d = Temp::new();

        let mut graph = Graph::new();
        graph.insert(a);
        graph.insert(b);
        graph.insert(c);
        graph.insert(d);
        graph.add_edge(&a, &c);
        graph.add_edge(&a, &b);
        graph.add_edge(&a, &d);
        graph.add_edge(&d, &c);

        let mut graph = WorkGraph::from_graph(graph);

        graph.remove(d);

        let adjacent: Vec<_> = graph.adjacent(a).collect();
        assert_eq!(2, adjacent.len());
        assert!(adjacent.contains(&b));
        assert!(adjacent.contains(&c));

        assert_eq!(2, graph.degree(a));
        assert_eq!(1, graph.degree(c));
    }

    //   / b
    // a
    //   \ d
    #[test]
    fn test_workgraph_coalesce1() {
        let a = Temp::new();
        let b = Temp::new();
        let c = Temp::new();
        let d = Temp::new();

        let mut graph = Graph::new();
        graph.insert(a);
        graph.insert(b);
        graph.insert(c);
        graph.insert(d);
        graph.add_edge(&a, &c);
        graph.add_edge(&a, &b);
        graph.add_edge(&a, &d);
        graph.add_edge(&d, &c);

        let mut graph = WorkGraph::from_graph(graph);

        graph.coalesce(b, d);

        let adjacent: Vec<_> = graph.adjacent(b).collect();
        let adjacent_d: Vec<_> = graph.adjacent(d).collect();
        assert_eq!(2, adjacent.len());
        assert_eq!(adjacent, adjacent_d);
        assert!(adjacent.contains(&a));
        assert!(adjacent.contains(&c));

        assert_eq!(2, graph.degree(a));
        assert_eq!(2, graph.degree(b));
        assert_eq!(2, graph.degree(c));

        graph.remove(b);

        let adjacent: Vec<_> = graph.adjacent(a).collect();
        assert_eq!(1, adjacent.len());
        assert!(adjacent.contains(&c));

        assert_eq!(1, graph.degree(a));
        assert_eq!(1, graph.degree(c));
    }

    //   / b
    // a   |
    //   \ d
    #[test]
    fn test_workgraph_coalesce2() {
        let a = Temp::new();
        let b = Temp::new();
        let c = Temp::new();
        let d = Temp::new();

        let mut graph = Graph::new();
        graph.insert(a);
        graph.insert(b);
        graph.insert(c);
        graph.insert(d);
        graph.add_edge(&a, &c);
        graph.add_edge(&a, &b);
        graph.add_edge(&a, &d);
        graph.add_edge(&d, &c);
        graph.add_edge(&b, &d);

        let mut graph = WorkGraph::from_graph(graph);

        graph.coalesce(b, d);

        let adjacent: Vec<_> = graph.adjacent(b).collect();
        let adjacent_d: Vec<_> = graph.adjacent(d).collect();
        assert_eq!(2, adjacent.len());
        assert_eq!(adjacent, adjacent_d);
        assert!(adjacent.contains(&a));
        assert!(adjacent.contains(&c));

        assert_eq!(2, graph.degree(a));
        assert_eq!(2, graph.degree(b));
        assert_eq!(2, graph.degree(c));

        graph.remove(b);

        let adjacent: Vec<_> = graph.adjacent(a).collect();
        assert_eq!(1, adjacent.len());
        assert!(adjacent.contains(&c));

        assert_eq!(1, graph.degree(a));
        assert_eq!(1, graph.degree(c));
    }

    //     b
    // a
    //   \ d
    #[test]
    fn test_workgraph_coalesce3() {
        let a = Temp::new();
        let b = Temp::new();
        let c = Temp::new();
        let d = Temp::new();

        let mut graph = Graph::new();
        graph.insert(a);
        graph.insert(b);
        graph.insert(c);
        graph.insert(d);
        graph.add_edge(&a, &c);
        graph.add_edge(&a, &d);
        graph.add_edge(&d, &c);

        let mut graph = WorkGraph::from_graph(graph);

        graph.coalesce(b, d);

        let adjacent: Vec<_> = graph.adjacent(b).collect();
        let adjacent_d: Vec<_> = graph.adjacent(d).collect();
        assert_eq!(2, adjacent.len());
        assert_eq!(adjacent, adjacent_d);
        assert!(adjacent.contains(&a));
        assert!(adjacent.contains(&c));

        assert_eq!(2, graph.degree(a));
        assert_eq!(2, graph.degree(b));
        assert_eq!(2, graph.degree(c));

        graph.remove(b);

        let adjacent: Vec<_> = graph.adjacent(a).collect();
        assert_eq!(1, adjacent.len());
        assert!(adjacent.contains(&c));

        assert_eq!(1, graph.degree(a));
        assert_eq!(1, graph.degree(c));
    }

    //   / b
    // a
    //     d
    #[test]
    fn test_workgraph_coalesce4() {
        let a = Temp::new();
        let b = Temp::new();
        let c = Temp::new();
        let d = Temp::new();

        let mut graph = Graph::new();
        graph.insert(a);
        graph.insert(b);
        graph.insert(c);
        graph.insert(d);
        graph.add_edge(&a, &c);
        graph.add_edge(&a, &b);
        graph.add_edge(&d, &c);

        let mut graph = WorkGraph::from_graph(graph);

        graph.coalesce(b, d);

        let adjacent: Vec<_> = graph.adjacent(b).collect();
        let adjacent_d: Vec<_> = graph.adjacent(d).collect();
        assert_eq!(2, adjacent.len());
        assert_eq!(adjacent, adjacent_d);
        assert!(adjacent.contains(&a));
        assert!(adjacent.contains(&c));

        assert_eq!(2, graph.degree(a));
        assert_eq!(2, graph.degree(b));
        assert_eq!(2, graph.degree(c));

        graph.remove(b);

        let adjacent: Vec<_> = graph.adjacent(a).collect();
        assert_eq!(1, adjacent.len());
        assert!(adjacent.contains(&c));

        assert_eq!(1, graph.degree(a));
        assert_eq!(1, graph.degree(c));
    }
}
