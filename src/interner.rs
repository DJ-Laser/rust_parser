use std::{cell::{Cell, UnsafeCell}, fmt::Display, hash::Hash};

use hashbrown::HashSet;
use stable_deref_trait::StableDeref;
pub trait Interner<T: StableDeref> {
    fn intern(&self, value: T) -> &T;
}

pub struct HashSetInterner<T> {
    set: UnsafeCell<HashSet<T>>,

    /// If T's impl of Eq or Hash contains a call to this Interner,
    /// it could lead to infinite recursion. This lets us panic instead. 
    in_use: Cell<bool>,
}

struct SetGuard<'a, T> {
    in_use: &'a Cell<bool>, 
    set: *mut HashSet<T>
}

impl<T> HashSetInterner<T> {
    pub fn new() -> Self {
        Self {
            set: UnsafeCell::default(),
            in_use: Cell::new(false),
        }
    }

    /// Call before getting a ref to set
    fn lock(&self) -> SetGuard<'_, T> {
        if self.in_use.replace(true) {
            panic!("Tried to access HashSetInterner while it was in use. Make sure any Eq or Hash implementations don't modify the interner");
        }

        SetGuard {
            in_use: &self.in_use,
            set: self.set.get()
        }
    }
}

impl<T: Hash + Eq + StableDeref + Display> Interner<T> for HashSetInterner<T> {
    fn intern(&self, value: T) -> &T {
        let g = self.lock();
        unsafe {
            (*g.set).get_or_insert(value)
        }
    }
}

impl<'a, T> Drop for SetGuard<'a, T> {
    fn drop(&mut self) {
        self.in_use.set(false);
    }
}
