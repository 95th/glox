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

#[derive(Clone, PartialEq)]
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
    x: Object => Self::Object(x)
}

impl Value {
    pub fn write(&self, mut w: impl fmt::Write, strings: &StringPool) -> fmt::Result {
        match self {
            Value::Nil => write!(w, "nil"),
            Value::Boolean(x) => write!(w, "{}", x),
            Value::Double(x) => write!(w, "{}", x),
            Value::Object(Object::String(x)) => write!(w, "{}", strings.lookup(*x)),
            Value::Object(Object::Function(x)) => {
                write!(w, "<fn {}>", strings.lookup(x.name as u32))
            }
        }
    }
}
