use std::cell::{Ref, RefCell};
use std::marker::PhantomData;
use typed_arena::Arena;

pub trait Node<T> {
    fn new(value: T) -> Self;
}

pub struct DirectedNode<'a, T> {
    value: T,
    pred: RefCell<Vec<&'a DirectedNode<'a, T>>>,
    succ: RefCell<Vec<&'a DirectedNode<'a, T>>>,
}

impl<'a, T> DirectedNode<'a, T> {
    pub fn add_to(&'a mut self, node: &'a mut DirectedNode<'a, T>) {
        self.succ.borrow_mut().push(node);
        node.pred.borrow_mut().push(self);
    }

    pub fn pred(&'a self) -> Ref<'a, Vec<&'a DirectedNode<'a, T>>> {
        self.pred.borrow()
    }

    pub fn succ(&'a self) -> Ref<'a, Vec<&'a DirectedNode<'a, T>>> {
        self.succ.borrow()
    }
}

impl<'a, T> Node<T> for DirectedNode<'a, T> {
    fn new(value: T) -> Self {
        Self {
            value,
            pred: RefCell::new(vec![]),
            succ: RefCell::new(vec![]),
        }
    }
}

pub struct UndirectedNode<'a, T> {
    value: T,
    adjacent: RefCell<Vec<&'a UndirectedNode<'a, T>>>,
}

impl<'a, T> UndirectedNode<'a, T> {
    pub fn add_to(&'a mut self, node: &'a mut UndirectedNode<'a, T>) {
        self.adjacent.borrow_mut().push(node);
        node.adjacent.borrow_mut().push(self);
    }

    pub fn adjacent(&'a self) -> Ref<'a, Vec<&'a UndirectedNode<'a, T>>> {
        self.adjacent.borrow()
    }
}

impl<'a, T> Node<T> for UndirectedNode<'a, T> {
    fn new(value: T) -> Self {
        Self {
            value,
            adjacent: RefCell::new(vec![]),
        }
    }
}

pub type DirectedGraph<'a, T> = Graph<'a, T, DirectedNode<'a, T>>;
pub type UndirectedGraph<'a, T> = Graph<'a, T, UndirectedNode<'a, T>>;

pub struct Graph<'a, T, N: Node<T>> {
    arena: Arena<N>,
    _phantom: &'a PhantomData<T>,
}

impl<'a, T, N: Node<T>> Graph<'a, T, N> {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
            _phantom: &PhantomData,
        }
    }

    pub fn insert(&'a mut self, value: T) -> &'a N {
        let node = self.arena.alloc(Node::new(value));
        node
    }
}
