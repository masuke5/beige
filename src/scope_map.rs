use std::collections::hash_map;
use std::hash::Hash;

use rustc_hash::FxHashMap;

#[derive(Debug, Default, Clone)]
pub struct ScopeMap<K: Hash + Eq, V> {
    maps: Vec<FxHashMap<K, V>>,
}

impl<K: Hash + Eq, V> ScopeMap<K, V> {
    pub fn new() -> Self {
        Self { maps: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.maps.push(FxHashMap::default());
    }

    pub fn pop_scope(&mut self) {
        if self.maps.is_empty() {
            panic!("there is no scope");
        }

        self.maps.pop().unwrap();
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        for map in self.maps.iter().rev() {
            if let Some(value) = map.get(key) {
                return Some(value);
            }
        }

        None
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> {
        for map in self.maps.iter_mut().rev() {
            if let Some(value) = map.get_mut(key) {
                return Some(value);
            }
        }

        None
    }

    pub fn get_with_level(&self, key: &K) -> Option<(&V, usize)> {
        let mut level = self.level();
        for map in self.maps.iter().rev() {
            if let Some(value) = map.get(key) {
                return Some((value, level));
            }

            level -= 1;
        }

        None
    }

    pub fn get_mut_with_level(&mut self, key: &K) -> Option<(&mut V, usize)> {
        let mut level = self.level();
        for map in self.maps.iter_mut().rev() {
            if let Some(value) = map.get_mut(key) {
                return Some((value, level));
            }

            level -= 1;
        }

        None
    }

    pub fn insert(&mut self, key: K, value: V) {
        if self.maps.is_empty() {
            panic!("there is no scope");
        }

        let front_map = self.maps.last_mut().unwrap();
        front_map.insert(key, value);
    }

    pub fn insert_with_level(&mut self, level: usize, key: K, value: V) {
        if self.maps.is_empty() {
            panic!("there is no scope");
        }

        let map = &mut self.maps[level - 1];
        map.insert(key, value);
    }

    pub fn contains_key(&self, key: &K) -> bool {
        for map in self.maps.iter().rev() {
            if map.contains_key(key) {
                return true;
            }
        }

        false
    }

    pub fn last_scope(&self) -> Option<&FxHashMap<K, V>> {
        self.maps.last()
    }

    pub fn level(&self) -> usize {
        self.maps.len()
    }

    pub fn iter(&self) -> ScopeMapIter<'_, K, V> {
        ScopeMapIter::new(&self.maps)
    }

    pub fn iter_mut(&mut self) -> ScopeMapIterMut<'_, K, V> {
        ScopeMapIterMut::new(&mut self.maps)
    }

    pub fn into_iter(self) -> ScopeMapIntoIter<K, V> {
        ScopeMapIntoIter::new(self.maps)
    }
}

macro_rules! fn_next {
    () => {
        fn next(&mut self) -> Option<Self::Item> {
            loop {
                if self.curr.is_none() {
                    let new_iter = self.iter.pop()?;
                    self.curr = Some(new_iter);
                    self.level -= 1;
                }

                match self.curr.as_mut().unwrap().next() {
                    Some((key, value)) => return Some((self.level, key, value)),
                    None => {
                        self.curr = None;
                        continue;
                    }
                }
            }
        }
    };
}

pub struct ScopeMapIter<'a, K, V> {
    iter: Vec<hash_map::Iter<'a, K, V>>,
    curr: Option<hash_map::Iter<'a, K, V>>,
    level: usize,
}

impl<'a, K, V> ScopeMapIter<'a, K, V> {
    fn new(maps: &'a [FxHashMap<K, V>]) -> Self {
        let mut iters = Vec::with_capacity(maps.len());
        for map in maps.iter() {
            iters.push(map.iter());
        }

        Self {
            iter: iters,
            curr: None,
            level: maps.len() + 1,
        }
    }
}

impl<'a, K, V> Iterator for ScopeMapIter<'a, K, V> {
    type Item = (usize, &'a K, &'a V);
    fn_next!();
}

pub struct ScopeMapIterMut<'a, K, V> {
    iter: Vec<hash_map::IterMut<'a, K, V>>,
    curr: Option<hash_map::IterMut<'a, K, V>>,
    level: usize,
}

impl<'a, K, V> ScopeMapIterMut<'a, K, V> {
    fn new(maps: &'a mut Vec<FxHashMap<K, V>>) -> Self {
        let level = maps.len() + 1;
        let mut iters = Vec::with_capacity(maps.len());
        for map in maps.iter_mut() {
            iters.push(map.iter_mut());
        }

        Self {
            iter: iters,
            curr: None,
            level,
        }
    }
}

impl<'a, K, V> Iterator for ScopeMapIterMut<'a, K, V> {
    type Item = (usize, &'a K, &'a mut V);
    fn_next!();
}

pub struct ScopeMapIntoIter<K, V> {
    iter: Vec<hash_map::IntoIter<K, V>>,
    curr: Option<hash_map::IntoIter<K, V>>,
    level: usize,
}

impl<K, V> ScopeMapIntoIter<K, V> {
    fn new(maps: Vec<FxHashMap<K, V>>) -> Self {
        let level = maps.len() + 1;
        let mut iters = Vec::with_capacity(maps.len());
        for map in maps.into_iter() {
            iters.push(map.into_iter());
        }

        Self {
            iter: iters,
            curr: None,
            level,
        }
    }
}

impl<K, V> Iterator for ScopeMapIntoIter<K, V> {
    type Item = (usize, K, V);
    fn_next!();
}

impl<K: Hash + Eq, V> IntoIterator for ScopeMap<K, V> {
    type Item = (usize, K, V);
    type IntoIter = ScopeMapIntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_iter()
    }
}

impl<'a, K: Hash + Eq, V> IntoIterator for &'a ScopeMap<K, V> {
    type Item = (usize, &'a K, &'a V);
    type IntoIter = ScopeMapIter<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, K: Hash + Eq, V> IntoIterator for &'a mut ScopeMap<K, V> {
    type Item = (usize, &'a K, &'a mut V);
    type IntoIter = ScopeMapIterMut<'a, K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn iter() {
        let mut hm = ScopeMap::<i32, i32>::new();
        hm.push_scope();

        let vec: Vec<(usize, &i32, &i32)> = hm.iter().collect();
        assert!(vec.is_empty());

        hm.insert(100, 10);

        let vec: Vec<(usize, &i32, &i32)> = hm.iter().collect();
        assert_eq!(vec, vec![(1, &100, &10)]);

        hm.push_scope();

        hm.insert(101, 20);
        hm.insert(102, 23);
        hm.insert(103, 28);

        let vec: Vec<(usize, &i32, &i32)> = hm.iter().collect();
        assert_eq!(
            vec,
            vec![
                (2, &101, &20),
                (2, &102, &23),
                (2, &103, &28),
                (1, &100, &10),
            ]
        );

        hm.pop_scope();

        let vec: Vec<(usize, &i32, &i32)> = hm.iter().collect();
        assert_eq!(vec, vec![(1, &100, &10)]);
    }

    #[test]
    fn into_iter() {
        let mut hm = ScopeMap::<i32, i32>::new();
        hm.push_scope();

        let vec: Vec<(usize, i32, i32)> = hm.clone().into_iter().collect();
        assert!(vec.is_empty());

        hm.insert(100, 10);

        let vec: Vec<(usize, i32, i32)> = hm.clone().into_iter().collect();
        assert_eq!(vec, vec![(1, 100, 10)]);

        hm.push_scope();

        hm.insert(101, 20);
        hm.insert(102, 23);
        hm.insert(103, 28);

        let vec: Vec<(usize, i32, i32)> = hm.clone().into_iter().collect();
        assert_eq!(
            vec,
            vec![(2, 101, 20), (2, 102, 23), (2, 103, 28), (1, 100, 10),]
        );

        hm.pop_scope();

        let vec: Vec<(usize, i32, i32)> = hm.into_iter().collect();
        assert_eq!(vec, vec![(1, 100, 10)]);
    }

    #[test]
    fn insert_with_level() {
        let mut hm = ScopeMap::<i32, i32>::new();
        hm.push_scope();
        hm.push_scope();
        hm.push_scope();
        hm.push_scope();

        hm.insert_with_level(3, 100, 200);
        assert_eq!(hm.maps[2][&100], 200);
        assert_eq!(hm.get_with_level(&100).unwrap(), (&200, 3));

        hm.pop_scope();
        hm.pop_scope();
        hm.pop_scope();
        hm.pop_scope();
    }
}
