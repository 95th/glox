use crate::chunk::Chunk;
use crate::intern::StringPool;
use crate::value::Value;
use std::rc::Rc;

pub type NativeFn = fn(&[Value]) -> Value;

#[derive(Clone)]
pub enum Object {
    String(u32),
    Function(Rc<Function>),
    NativeFn(NativeFn),
}

impl Object {
    pub fn new_string(s: &str, pool: &mut StringPool) -> Self {
        Self::String(pool.intern(s))
    }

    pub fn new_function(name: &str, arity: u32) -> Self {
        Self::Function(Rc::new(Function {
            name: name.to_string(),
            arity,
            chunk: Chunk::new(),
        }))
    }
}

#[derive(Clone)]
pub struct Function {
    pub name: String,
    pub arity: u32,
    pub chunk: Chunk,
}

impl Function {
    pub fn new() -> Self {
        Self {
            name: String::new(),
            arity: 0,
            chunk: Chunk::new(),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::String(a), Object::String(b)) => a == b,
            _ => false,
        }
    }
}
