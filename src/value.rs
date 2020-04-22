use std::fmt;

macro_rules! __impl_accessors {
    ($is_fn: ident, $variant: ident, $as_fn: ident, $inner_ty: ty) => {
        impl Value {
            pub fn $is_fn(&self) -> bool {
                matches!(self, Self::$variant(_))
            }

            pub fn $as_fn(&self) -> Option<$inner_ty> {
                match self {
                    Self::$variant(v) => Some(*v),
                    _ => None,
                }
            }
        }

        impl From<$inner_ty> for Value {
            fn from(val: $inner_ty) -> Self {
                Self::$variant(val)
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Nil,
    Boolean(bool),
    Double(f64),
}

__impl_accessors!(is_boolean, Boolean, as_boolean, bool);
__impl_accessors!(is_double, Double, as_double, f64);

impl Value {
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
