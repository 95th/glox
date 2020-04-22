use std::fmt;

macro_rules! get {
    ($expression:expr, $( $pattern:pat )|+ $( if $guard: expr )?, $val: expr) => {
        match $expression {
            $( $pattern )|+ $( if $guard )? => $val,
            _ => panic! {
                "Expression didn't match. Expected: {}, Actual: {:?}",
                stringify! {
                    $( $pattern )|+ $( if $guard )?
                },
                $expression
            },
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

    pub fn as_boolean(&self) -> bool {
        get!(self, Self::Boolean(v), *v)
    }

    pub fn as_double(&self) -> f64 {
        get!(self, Self::Double(v), *v)
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
