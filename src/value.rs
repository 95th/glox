use crate::intern::StringPool;
use crate::object::{Function, Object};
use std::fmt;
use std::rc::Rc;

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
    impl_is!(is_object, Self::Object(_));
    impl_is!(is_string, Self::Object(Object::String(_)));
    impl_is!(is_function, Self::Object(Object::Function(_)));

    impl_as!(as_boolean, Self::Boolean(x), *x, bool);
    impl_as!(as_double, Self::Double(x), *x, f64);
    impl_as!(as_object, Self::Object(x), x, &Object);
    impl_as!(as_string, Self::Object(Object::String(x)), *x, u32);
    impl_as!(
        as_function,
        Self::Object(Object::Function(x)),
        x,
        &Rc<Function>
    );
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
            Value::Object(Object::Function(x)) => write!(w, "{}", x),
            Value::Object(Object::NativeFn(_)) => write!(w, "<native fn>"),
            Value::Object(Object::Closure(x)) => write!(w, "{}", x.function),
        }
    }
}
