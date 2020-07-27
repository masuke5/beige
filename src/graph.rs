use itertools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use std::fmt;
use std::hash::Hash;

pub struct Node<T: Eq + Hash + Clone> {
    pred: FxHashSet<T>,
    succ: FxHashSet<T>,
}

impl<T: Eq + Hash + Clone> Node<T> {
    pub fn new() -> Self {
        Self {
            pred: FxHashSet::default(),
            succ: FxHashSet::default(),
        }
    }
}

pub struct Graph<T: Eq + Hash + Clone> {
    nodes: FxHashMap<T, Node<T>>,
}

impl<T: Eq + Hash + Clone> Graph<T> {
    pub fn new() -> Self {
        Self {
            nodes: FxHashMap::default(),
        }
    }

    pub fn insert(&mut self, value: T) {
        self.nodes.insert(value, Node::new());
    }

    pub fn contains(&self, value: &T) -> bool {
        self.nodes.contains_key(value)
    }

    pub fn add_edge(&mut self, from: &T, to: &T) {
        if from == to {
            return;
        }

        self.nodes.get_mut(from).unwrap().succ.insert(to.clone());
        self.nodes.get_mut(to).unwrap().pred.insert(from.clone());
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.nodes.keys()
    }

    pub fn pred(&self, key: &T) -> impl Iterator<Item = &T> + '_ {
        self.nodes[key].pred.iter()
    }

    pub fn succ(&self, key: &T) -> impl Iterator<Item = &T> + '_ {
        self.nodes[key].succ.iter()
    }

    pub fn adjacent(&self, key: &T) -> impl Iterator<Item = &T> + '_ {
        self.nodes[key]
            .pred
            .iter()
            .chain(self.nodes[key].succ.iter())
            .unique()
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }
}

impl<T: fmt::Debug + Eq + Hash + Clone> fmt::Debug for Graph<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Graph {{\n")?;
        for (value, node) in &self.nodes {
            write!(f, "  {:?}\n", value)?;

            for pred in &node.pred {
                write!(f, "  <== {:?}\n", pred)?;
            }

            for succ in &node.succ {
                write!(f, "  ==> {:?}\n", succ)?;
            }
        }

        write!(f, "}}")
    }
}
