use crate::chunk::Chunk;
use crate::intern::StringPool;

#[derive(Clone)]
pub enum Object {
    String(u32),
    Function(Function),
}

impl Object {
    pub fn new_string(s: &str, pool: &mut StringPool) -> Self {
        Self::String(pool.intern(s))
    }

    pub fn new_function(name: &str, arity: u32, pool: &mut StringPool) -> Self {
        Self::Function(Function {
            name: pool.intern(name) as i32,
            arity,
            chunk: Chunk::new(),
        })
    }
}

#[derive(Clone)]
pub struct Function {
    pub name: i32,
    pub arity: u32,
    pub chunk: Chunk,
}

impl Function {
    pub fn new() -> Self {
        Self {
            name: -1,
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
