use std::collections::HashMap;
use std::hash::Hash;

pub trait IdMapItem: Hash + Eq + Clone {}
impl<T: Hash + Eq + Clone> IdMapItem for T {}

#[derive(Copy, Clone, Eq, Hash, PartialEq, Debug, PartialOrd, Ord)]
pub struct Id(pub usize);

#[derive(Clone, Eq, PartialEq)]
pub struct IdMap<T: IdMapItem> {
    vals: Vec<T>,
    id_map: HashMap<T, Id>,
}

impl<T: IdMapItem> IdMap<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add(&mut self, item: T) -> Id {
        if let Some(id) = self.id_map.get(&item) {
            *id
        } else {
            let id = Id(self.vals.len());
            self.vals.push(item.clone());
            self.id_map.entry(item);
            id
        }
    }

    pub fn get(&self, id: Id) -> Option<&T> {
        self.vals.get(id.0)
    }

    pub fn update_with<F>(&mut self, id: Id, update: F) -> Option<T>
        where F: FnOnce(&mut T) {

        if id.0 >= self.vals.len() {
            return None
        }
        let old_item = self.vals[id.0].clone();
        self.id_map.remove(&self.vals[id.0]);
        update(&mut self.vals[id.0]);
        self.id_map.insert(self.vals[id.0].clone(), id);
        Some(old_item)
    }
    
    pub fn update(&mut self, id: Id, item: T) -> Option<T> {
        self.update_with(id, |old| *old = item)
    }

    pub fn get_id(&self, item: &T) -> Option<Id> {
        self.id_map.get(item).copied()
    }

    pub fn len(&self) -> usize {
        self.id_map.len()
    }

    pub fn iter(&self) -> impl Iterator<Item=(Id, &T)> {
        self.id_map.iter()
            .map(|(item, id)| (*id, item))
    }
}
impl<T: IdMapItem> Default for IdMap<T> {
    fn default() -> Self {
        Self {
            vals: Default::default(),
            id_map: Default::default()
        }
    }
}