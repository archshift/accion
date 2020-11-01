use std::hash::Hash;
use std::collections::HashMap;
use disjoint_sets::{self, UnionFind};

pub struct DisjointSet<T: Hash + Clone + Eq> {
    id_map: HashMap<T, usize>,
    inner: UnionFind<usize>,
}

impl<T: Hash + Clone + Eq> DisjointSet<T> {
    pub fn new(size: usize) -> Self {
        Self {
            id_map: HashMap::new(),
            inner: UnionFind::new(size),
        }
    }

    fn get_or_insert_id(&mut self, item: &T) -> usize {
        if let Some(id) = self.id_map.get(item) {
            *id
        } else {
            let id = self.id_map.len();
            self.id_map.insert(item.clone(), id);
            id
        }
    }

    pub fn join(&mut self, first: &T, second: &T) {
        let first_id = self.get_or_insert_id(first);
        let second_id = self.get_or_insert_id(second);
        self.inner.union(first_id, second_id);
    }
}