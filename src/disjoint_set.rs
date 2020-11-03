use std::hash::Hash;
use std::collections::HashMap;
use std::fmt;
use disjoint_sets::{self, UnionFind};

#[derive(Clone)]
pub struct DisjointSet<T: Hash + Eq + Clone> {
    vals: Vec<T>,
    id_map: HashMap<T, usize>,
    inner: UnionFind<usize>,
}

impl<T: Hash + Eq + Clone> DisjointSet<T> {
    pub fn new(size: usize) -> Self {
        Self {
            vals: Vec::new(),
            id_map: HashMap::new(),
            inner: UnionFind::new(size),
        }
    }

    fn get_or_insert_id(&mut self, item: &T) -> usize {
        if let Some(id) = self.id_map.get(item) {
            *id
        } else {
            let id = self.vals.len();
            while id >= self.inner.len() {
                self.inner.alloc();
            }

            self.vals.push(item.clone());
            self.id_map.insert(item.clone(), id);
            id
        }
    }

    pub fn join(&mut self, first: &T, second: &T) {
        let first_id = self.get_or_insert_id(first);
        let second_id = self.get_or_insert_id(second);
        self.inner.union(first_id, second_id);
    }

    pub fn groups(&self) -> Vec<Vec<T>> {
        let idx_grps = self.inner.to_vec();
        let idx_grps = &idx_grps[..self.id_map.len()];

        if let Some(max_grp) = idx_grps.iter().max() {
            let mut grps: Vec<Vec<usize>> = vec![Vec::new(); max_grp + 1];

            for (id, group) in idx_grps.iter().enumerate() {
                grps[*group].push(id);
            }

            let mut all_out = Vec::new();
            for grp in grps {
                if grp.len() != 0 {
                    let mut out = Vec::<T>::new();
                    for idx in grp {
                        out.push(self.vals[idx].clone());
                    }
                    all_out.push(out);
                }
            }
            all_out
        } else {
            Vec::new()
        }
    }
}

impl<T: Hash + Eq + Clone + fmt::Debug> fmt::Debug for DisjointSet<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::collections::HashSet;
        use std::iter::FromIterator;

        let mut tup = f.debug_tuple("DisjointSet");
        for grp in self.groups() {
            let grp_set = HashSet::<&T>::from_iter(grp.iter());
            tup.field(&grp_set);
        }
        tup.finish()
    }
}