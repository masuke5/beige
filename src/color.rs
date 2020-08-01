use crate::codegen::{reg_name, Mnemonic};
use crate::graph::Graph;
use crate::ir::Temp;
use crate::liveness::{BasicBlock, InterferenceGraph};
use log::debug;
use rustc_hash::{FxHashMap, FxHashSet};
use std::cmp::Reverse;

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
    coalesced_temps: FxHashMap<Temp, FxHashSet<Temp>>,
    move_adjacents: FxHashMap<Temp, FxHashSet<Temp>>,
    moves: FxHashSet<(Temp, Temp)>,
}

impl WorkGraph {
    fn from_graph(graph: Graph<Temp>, move_graph: Graph<Temp>) -> Self {
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

        let mut move_adjacents = FxHashMap::default();

        for temp in adjacents.keys() {
            move_adjacents.insert(*temp, FxHashSet::default());
        }
        for temp in move_graph.iter() {
            let adjacent = move_graph.adjacent(temp).copied().collect();
            move_adjacents.insert(*temp, adjacent);
        }

        let mut graph = Self {
            degrees,
            adjacents,
            aliases: FxHashMap::default(),
            move_adjacents,
            moves: FxHashSet::default(),
            coalesced_temps: FxHashMap::default(),
        };

        for temp in graph.iter().collect::<Vec<_>>() {
            for adj in graph.move_adjacent(temp).collect::<Vec<_>>() {
                graph.add_move_edge(temp, adj);
            }
        }

        for temp in graph.adjacents.keys() {
            graph.coalesced_temps.insert(*temp, FxHashSet::default());
        }

        graph
    }

    fn alias(&self, temp: Temp) -> Temp {
        self.aliases.get(&temp).copied().unwrap_or(temp)
    }

    fn iter(&self) -> impl Iterator<Item = Temp> + '_ {
        self.adjacents.keys().copied()
    }

    fn len(&self) -> usize {
        self.adjacents.len()
    }

    fn adjacent(&self, temp: Temp) -> impl Iterator<Item = Temp> + '_ {
        self.adjacents[&self.alias(temp)].iter().copied()
    }

    fn degree(&self, temp: Temp) -> usize {
        self.degrees[&self.alias(temp)]
    }

    fn coalesced_temps(&self, temp: Temp) -> impl Iterator<Item = Temp> + '_ {
        self.coalesced_temps[&self.alias(temp)].iter().copied()
    }

    fn remove(&mut self, temp: Temp) {
        let temp = self.alias(temp);

        for adj in self.adjacents.remove(&temp).unwrap() {
            self.adjacents.get_mut(&adj).unwrap().remove(&temp);
            *self.degrees.get_mut(&adj).unwrap() -= 1;
        }

        self.degrees.remove(&temp);

        if let Some(adj) = self.move_adjacents.remove(&temp) {
            for adj in adj {
                self.remove_move_edge(adj, temp);
            }
        }
    }

    fn add_edge(&mut self, a: Temp, b: Temp) {
        let a = self.alias(a);
        let b = self.alias(b);

        if a == b {
            return;
        }

        if self.adjacents.get_mut(&a).unwrap().insert(b) {
            *self.degrees.get_mut(&a).unwrap() += 1;
        }

        if self.adjacents.get_mut(&b).unwrap().insert(a) {
            *self.degrees.get_mut(&b).unwrap() += 1;
        }
    }

    fn coalesce(&mut self, a: Temp, b: Temp) {
        let a = self.alias(a);
        let b = self.alias(b);

        if a == b {
            return;
        }

        let adjacent: Vec<_> = self.adjacent(b).collect();
        let move_adjacent: Vec<_> = self.move_adjacent(b).collect();
        self.remove(b);

        for adj in adjacent {
            self.add_edge(a, adj);
        }

        for adj in move_adjacent {
            self.add_move_edge(a, adj);
        }

        // aliases
        // c --> b
        // ↓
        // b --> a
        // c --> a

        self.aliases.insert(b, a);

        let b_coalesced_temps = self.coalesced_temps[&b].clone();
        for b_coalesced in &b_coalesced_temps {
            self.aliases.insert(*b_coalesced, a);
        }

        // coalesced_temps
        // b --> [c]
        // ↓
        // a --> [b, c]
        // b --> []

        self.coalesced_temps.get_mut(&a).unwrap().insert(b);
        self.coalesced_temps.get_mut(&b).unwrap().clear();

        for b_coalesced in &b_coalesced_temps {
            self.coalesced_temps
                .get_mut(&a)
                .unwrap()
                .insert(*b_coalesced);
        }
    }

    fn dump(&self) {
        use crate::dump::format_iter;

        for temp in self.iter() {
            println!(
                "{: <7} adj:{} degree:{} coalesced:{} move_adj:{}",
                format!("{}", reg_name(temp)),
                format_iter(self.adjacent(temp).map(reg_name).collect::<Vec<_>>(), ","),
                self.degree(temp),
                format_iter(
                    self.coalesced_temps(temp).map(reg_name).collect::<Vec<_>>(),
                    ","
                ),
                format_iter(
                    self.move_adjacent(temp).map(reg_name).collect::<Vec<_>>(),
                    ","
                )
            );
        }

        println!("Moves:");
        for (a, b) in self.move_edges() {
            println!("  {}, {}", reg_name(a), reg_name(b));
        }

        println!("Aliases:");
        for (a, b) in &self.aliases {
            println!("  {} --> {}", reg_name(*a), reg_name(*b));
        }
    }
}

impl WorkGraph {
    fn is_move_related(&self, temp: Temp) -> bool {
        let temp = self.alias(temp);
        if let Some(adj) = self.move_adjacents.get(&temp) {
            !adj.is_empty()
        } else {
            false
        }
    }

    fn move_adjacent(&self, temp: Temp) -> impl Iterator<Item = Temp> + '_ {
        let temp = self.alias(temp);
        self.move_adjacents[&temp].iter().copied()
    }

    fn move_edges(&self) -> impl Iterator<Item = (Temp, Temp)> + '_ {
        self.moves.iter().copied()
    }

    fn add_move_edge(&mut self, a: Temp, b: Temp) {
        let a = self.alias(a);
        let b = self.alias(b);

        if a == b {
            return;
        }

        self.move_adjacents.entry(a).or_default().insert(b);
        self.move_adjacents.entry(b).or_default().insert(a);

        if !self.moves.contains(&(a, b)) && !self.moves.contains(&(b, a)) {
            self.moves.insert((a, b));
        }
    }

    fn remove_move_edge(&mut self, a: Temp, b: Temp) {
        if a == b {
            return;
        }

        if let Some(adj) = self.move_adjacents.get_mut(&a) {
            adj.remove(&b);
        }
        if let Some(adj) = self.move_adjacents.get_mut(&b) {
            adj.remove(&a);
        }
        self.moves.remove(&(a, b));
        self.moves.remove(&(b, a));
    }
}

struct Color {
    igraph: InterferenceGraph,
    graph: WorkGraph,
    move_graph: Graph<Temp>,
    simplified_temps: Vec<Temp>,
    registers: FxHashSet<Temp>,
    register_priority: FxHashMap<Temp, u32>,
    colored_temps: FxHashMap<Temp, Temp>,
    spilled_temps: Vec<Temp>,
}

impl Color {
    fn new(
        igraph: InterferenceGraph,
        registers: FxHashSet<Temp>,
        move_graph: Graph<Temp>,
        register_priority: FxHashMap<Temp, u32>,
    ) -> Self {
        Self {
            graph: WorkGraph::from_graph(igraph.clone(), move_graph.clone()),
            igraph,
            move_graph,
            simplified_temps: Vec::new(),
            spilled_temps: Vec::new(),
            colored_temps: FxHashMap::default(),
            registers,
            register_priority,
        }
    }

    fn is_precolored(&self, temp: Temp) -> bool {
        self.registers.contains(&temp)
    }

    fn is_significant_degree(&self, temp: Temp) -> bool {
        self.graph.degree(temp) >= self.registers.len()
    }

    fn is_interference(&self, a: Temp, b: Temp) -> bool {
        self.graph.adjacent(a).find(|t| *t == b).is_some()
    }

    fn can_coalesce(&self, a: Temp, b: Temp) -> bool {
        assert!(!self.is_precolored(a) || !self.is_precolored(b));

        let ab_adj: FxHashSet<_> = self
            .graph
            .adjacent(a)
            .chain(self.graph.adjacent(b))
            .collect();
        ab_adj
            .into_iter()
            .filter(|adj| self.is_significant_degree(*adj))
            .count()
            < self.registers.len()
    }

    fn simplify(&mut self) {
        // Find simplification target
        let temp = self.graph.iter().find(|temp| {
            !self.is_precolored(*temp)
                && !self.is_significant_degree(*temp)
                && !self.graph.is_move_related(*temp)
        });
        let temp = match temp {
            Some(t) => t,
            // If a target is not found, do nothing
            None => return,
        };

        self.simplified_temps.push(temp);
        self.graph.remove(temp);
    }

    fn coalesce(&mut self) {
        let (x, y) = match self
            .graph
            .move_edges()
            .filter(|(a, b)| !self.is_precolored(*a) || !self.is_precolored(*b))
            .next()
        {
            Some(m) => m,
            None => return,
        };

        if self.can_coalesce(x, y) {
            debug!("Coalesce {} and {}", reg_name(x), reg_name(y));

            if self.is_precolored(y) {
                self.graph.coalesce(y, x);
            } else {
                self.graph.coalesce(x, y);
            }
        }
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

            let mut ok_regs: Vec<Temp> = ok_regs.into_iter().collect();
            ok_regs.sort_by_key(|t| Reverse(self.register_priority[t]));

            if let Some(reg) = ok_regs.iter().next() {
                self.colored_temps.insert(temp, *reg);
            } else {
                self.spilled_temps.push(temp);
            }
        }

        for (temp, reg) in &self.graph.aliases {
            self.colored_temps.insert(*temp, *reg);
        }
    }

    fn is_completed(&self) -> bool {
        self.graph
            .iter()
            .filter(|t| !self.is_precolored(*t))
            .count()
            == 0
    }

    fn color(mut self) -> ColorResult {
        while !self.is_completed() {
            // self.dump();
            self.simplify();
            self.coalesce();
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
    register_priority: FxHashMap<Temp, u32>,
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

    let color = Color::new(igraph, registers, move_graph, register_priority);
    color.color()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;
    use std::hash::Hash;

    fn hs<T: Eq + Hash + Clone>(values: &[T]) -> HashSet<T> {
        let mut hs = HashSet::new();
        for value in values {
            hs.insert(value.clone());
        }
        hs
    }

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

        let graph = WorkGraph::from_graph(graph, Graph::new());

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

        let mut graph = WorkGraph::from_graph(graph, Graph::new());

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

        let mut graph = WorkGraph::from_graph(graph, Graph::new());

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

        let mut graph = WorkGraph::from_graph(graph, Graph::new());

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

        let mut graph = WorkGraph::from_graph(graph, Graph::new());

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

        let mut graph = WorkGraph::from_graph(graph, Graph::new());

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
    // a   | (move)
    //     d
    #[test]
    fn test_workgraph_coalesce_with_move_graph() {
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

        let mut mgraph = Graph::new();
        mgraph.insert(b);
        mgraph.insert(d);
        mgraph.add_edge(&b, &d);

        let mut graph = WorkGraph::from_graph(graph, mgraph);

        assert!(graph.is_move_related(b));
        assert_eq!(hs(&[d]), graph.move_adjacent(b).collect::<HashSet<_>>());
        assert_eq!(hs(&[(b, d)]), graph.move_edges().collect::<HashSet<_>>());

        graph.coalesce(b, d);

        assert!(!graph.is_move_related(b));
        assert!(graph.move_adjacent(b).collect::<Vec<_>>().is_empty());
        assert!(graph.move_edges().next().is_none());
    }

    //   / b      / bc
    // a   |  → a       → abc
    //     c
    #[test]
    fn test_workgraph_coalesce5() {
        let a = Temp::new();
        let b = Temp::new();
        let c = Temp::new();

        let mut graph = Graph::new();
        graph.insert(a);
        graph.insert(b);
        graph.insert(c);
        graph.add_edge(&a, &b);
        graph.add_edge(&b, &c);

        let mut graph = WorkGraph::from_graph(graph, Graph::new());

        graph.coalesce(b, c);
        graph.coalesce(a, b);

        assert_eq!(
            hs(&[b, c]),
            graph.coalesced_temps(a).collect::<HashSet<_>>()
        );
        assert_eq!(a, graph.alias(c));
    }
}
