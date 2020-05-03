use crate::alloc::{Alloc, String};
use crate::chunk::Chunk;
use crate::intern::StringPool;
use crate::value::Value;
use std::fmt;
use std::rc::Rc;

pub type NativeFn<A> = fn(&[Value<A>]) -> Value<A>;

#[derive(Clone)]
pub enum Object<A: Alloc> {
    String(u32),
    Function(Rc<Function<A>>),
    NativeFn(NativeFn<A>),
    Closure(ClosureFn<A>),
}

impl<A: Alloc> Object<A> {
    pub fn new_string(s: &str, pool: &mut StringPool<A>) -> Self {
        Self::String(pool.intern(s))
    }
}

#[derive(Clone)]
pub struct Function<A: Alloc> {
    pub name: String<A>,
    pub arity: u32,
    pub chunk: Chunk<A>,
}

impl<A: Alloc> Function<A> {
    pub fn new(alloc: A) -> Self {
        Self {
            name: String::new_in(alloc),
            arity: 0,
            chunk: Chunk::new(alloc),
        }
    }
}

impl<A: Alloc> fmt::Display for Function<A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.name.is_empty() {
            write!(f, "<script>")
        } else {
            write!(f, "<fn {}>", self.name)
        }
    }
}

impl<A: Alloc> PartialEq for Object<A> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(a), Object::String(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct ClosureFn<A: Alloc> {
    pub function: Rc<Function<A>>,
}
