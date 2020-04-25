use crate::intern::StringPool;
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

macro_rules! impl_from {
    ($($val: ident : $ty: ty => $expr: expr),+) => {
        $(
            impl From<$ty> for Value {
                fn from($val: $ty) -> Self {
                    $expr
                }
            }
        )+
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
    impl_is!(is_string, Self::Object(Object::String(_)));

    impl_as!(as_boolean, Self::Boolean(x), *x, bool);
    impl_as!(as_double, Self::Double(x), *x, f64);
    impl_as!(as_string, Self::Object(Object::String(x)), *x, u32);
}

impl_from! {
    x: bool => Self::Boolean(x),
    x: f64 => Self::Double(x),
    x: u32 => Self::Object(Object::String(x))
}

impl Value {
    pub fn write(&self, mut f: impl fmt::Write, strings: &StringPool) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(x) => write!(f, "{}", x),
            Value::Double(x) => write!(f, "{}", x),
            Value::Object(Object::String(x)) => write!(f, "{}", strings.lookup(*x)),
        }
    }
}
