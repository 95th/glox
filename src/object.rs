use std::fmt;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Object {
    String(u32),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::String(s) => write!(f, "{}", s),
        }
    }
}
