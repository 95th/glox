use crate::alloc::Alloc;
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
            impl<A: Alloc> From<$ty> for Value<A> {
                fn from($val: $ty) -> Self {
                    $expr
                }
            }
        )+
    };
}

#[derive(Clone)]
pub enum Value<A: Alloc> {
    Nil,
    Boolean(bool),
    Double(f64),
    Object(Object<A>),
}

impl<A: Alloc> PartialEq for Value<A> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Double(a), Value::Double(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => a == b,
            _ => false,
        }
    }
}

impl<A: Alloc> Value<A> {
    impl_is!(is_nil, Self::Nil);
    impl_is!(is_boolean, Self::Boolean(_));
    impl_is!(is_double, Self::Double(_));
    impl_is!(is_object, Self::Object(_));
    impl_is!(is_string, Self::Object(Object::String(_)));
    impl_is!(is_function, Self::Object(Object::Function(_)));

    impl_as!(as_boolean, Self::Boolean(x), *x, bool);
    impl_as!(as_double, Self::Double(x), *x, f64);
    impl_as!(as_object, Self::Object(x), x, &Object<A>);
    impl_as!(as_string, Self::Object(Object::String(x)), *x, u32);
    impl_as!(
        as_function,
        Self::Object(Object::Function(x)),
        x,
        &Rc<Function<A>>
    );
}

impl_from! {
    x: bool => Self::Boolean(x),
    x: f64 => Self::Double(x),
    x: Object<A> => Self::Object(x)
}

impl<A: Alloc> Value<A> {
    pub fn write(&self, mut w: impl fmt::Write, strings: &StringPool<A>) -> fmt::Result {
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
