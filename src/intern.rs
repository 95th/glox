use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem;

#[derive(Default)]
pub struct StringPool {
    map: HashMap<Ptr<str>, u32>,
    vec: Vec<Ptr<str>>,
    curr_buf: String,
    full: Vec<String>,
}

impl StringPool {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn with_capacity(cap: usize) -> Self {
        let cap = cap.next_power_of_two();
        Self {
            curr_buf: String::with_capacity(cap),
            ..Default::default()
        }
    }

    pub fn intern<S: AsRef<str>>(&mut self, name: S) -> u32 {
        let name = name.as_ref();
        if let Some(&id) = self.map.get(&Ptr(name)) {
            return id;
        }
        let name = self.alloc(name);
        let id = self.map.len() as u32;
        self.map.insert(name, id);
        self.vec.push(name);

        debug_assert!(self.lookup(id) == name.get());
        debug_assert!(self.intern(name.get()) == id);

        id
    }

    pub fn lookup(&self, id: u32) -> &str {
        self.vec[id as usize].get()
    }

    fn alloc(&mut self, name: &str) -> Ptr<str> {
        let cap = self.curr_buf.capacity();
        if cap < self.curr_buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = mem::replace(&mut self.curr_buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.curr_buf.len();
            self.curr_buf.push_str(name);
            &self.curr_buf[start..]
        };

        Ptr(interned)
    }
}

struct Ptr<T: ?Sized>(*const T);

impl<T: ?Sized> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Self(self.0)
    }
}

impl<T: ?Sized> Copy for Ptr<T> {}

impl<T: ?Sized> Ptr<T> {
    fn get(&self) -> &T {
        unsafe { &*self.0 }
    }
}

impl<T: ?Sized + Default> Default for Ptr<T> {
    fn default() -> Self {
        Self(&T::default())
    }
}

impl<T: ?Sized + Hash> Hash for Ptr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get().hash(state);
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Ptr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

impl<T: ?Sized + Eq> Eq for Ptr<T> {}
