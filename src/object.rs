use crate::chunk::Chunk;
use crate::intern::StringPool;

#[derive(Clone)]
pub enum Object {
    String(u32),
    Function { name: u32, arity: u32, chunk: Chunk },
}

impl Object {
    pub fn new_string(s: &str, pool: &mut StringPool) -> Self {
        Self::String(pool.intern(s))
    }

    pub fn new_function(name: &str, arity: u32, pool: &mut StringPool) -> Self {
        Self::Function {
            name: pool.intern(name),
            arity,
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
