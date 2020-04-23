use crate::object::Object;
use std::fmt;

macro_rules! impl_is {
    ($fn: ident, $pat: pat) => {
        pub fn $fn(&self) -> bool {
            matches!(self, $pat)
        }
    };
}

macro_rules! impl_as {
    ($fn: ident, $pat: pat, $val: expr, $ty: ty) => {
        pub fn $fn(&self) -> Option<$ty> {
             match self {
                $pat => Some($val),
                _ => None,
            }
        }
    };
}

macro_rules! impl_as_mut {
    ($fn: ident, $pat: pat, $val: expr, $ty: ty) => {
        pub fn $fn(&mut self) -> Option<$ty> {
             match self {
                $pat => Some($val),
                _ => None,
            }
        }
    };
}

macro_rules! impl_from {
    ($ty: ty, $variant: ident) => {
        impl From<$ty> for Value {
            fn from(val: $ty) -> Self {
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
    Object(Object),
}

impl Value {
    impl_is!(is_nil, Self::Nil);
    impl_is!(is_boolean, Self::Boolean(_));
    impl_is!(is_double, Self::Double(_));
    impl_is!(is_object, Self::Object(_));

    impl_as!(as_boolean, Self::Boolean(x), *x, bool);
    impl_as!(as_double, Self::Double(x), *x, f64);
    impl_as!(as_obj, Self::Object(x), x, &Object);
    impl_as_mut!(as_obj_mut, Self::Object(x), x, &mut Object);
}

impl_from!(bool, Boolean);
impl_from!(f64, Double);
impl_from!(Object, Object);

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::Boolean(b) => write!(f, "{}", b),
            Self::Double(d) => write!(f, "{}", d),
            Self::Object(o) => write!(f, "Object[{:?}]", o),
        }
    }
}
