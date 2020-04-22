use std::fmt;

macro_rules! if_matches {
    ($expression:expr, $( $pattern:pat )|+ $( if $guard: expr )?, $val: expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => Some($val),
            _ => None
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Double(f64),
}

impl Value {
    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
    }

    pub fn as_boolean(&self) -> Option<bool> {
        if_matches!(self, Self::Boolean(v), v).cloned()
    }

    pub fn as_double(&self) -> Option<f64> {
        if_matches!(self, Self::Double(v), v).cloned()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Double(d) => write!(f, "{}", d),
        }
    }
}
