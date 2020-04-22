use std::fmt;

macro_rules! __impl_accessors {
    ($is_fn: ident, $pat: pat, $as_fn: ident, $as_ty: ty, $val: expr) => {
        #[allow(unused_variables)]
        pub fn $is_fn(&self) -> bool {
            matches!(self, $pat)
        }

        pub fn $as_fn(&self) -> Option<$as_ty> {
            match self {
                $pat => Some($val),
                _ => None,
            }
        }
    };
}

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Double(f64),
}

impl Value {
    __impl_accessors!(is_boolean, Self::Boolean(v), as_boolean, bool, *v);
    __impl_accessors!(is_double, Self::Double(v), as_double, f64, *v);

    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
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
