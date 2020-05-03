use crate::alloc::{Alloc, Vec};
use crate::chunk::OpCode;
use crate::compile::{Compiler, FunctionKind};
use crate::intern::StringPool;
use crate::object::{ClosureFn, NativeFn, Object};
use crate::value::Value;
use crate::Error;
use log::Level;
use num_traits::FromPrimitive;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

pub struct Vm<A: Alloc> {
    stack: Vec<Value<A>, A>,
    frames: Vec<CallFrame<A>, A>,
    strings: StringPool<A>,
    globals: HashMap<u32, Value<A>>,
    alloc: A,
}

struct CallFrame<A: Alloc> {
    closure: ClosureFn<A>,
    ip: usize,
    stack_top: usize,
}

impl<A: Alloc> Vm<A> {
    pub fn new(alloc: A) -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity_in(256, alloc),
            frames: Vec::with_capacity_in(64, alloc),
            strings: StringPool::new(alloc),
            globals: HashMap::new(),
            alloc,
        };
        vm.init();
        vm
    }

    pub fn interpret(&mut self, source: &str) -> crate::Result<()> {
        let compiler = Compiler::new(source, &mut self.strings, self.alloc);
        let function = Rc::new(compiler.compile(FunctionKind::Script)?);
        let closure = Object::Closure(ClosureFn { function });
        self.stack.push(closure.clone().into());
        self.call_value(closure.into(), 0)?;
        self.run()
    }

    fn print_stack(&self, strings: &StringPool<A>) {
        let mut buf = String::from("          ");
        for val in &self.stack {
            buf.push_str("[ ");
            val.write(&mut buf, strings).unwrap();
            buf.push_str(" ]");
        }

        trace!("{}", buf);
    }

    pub fn clear(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.globals.clear();
        self.strings.clear();
        self.init();
    }

    fn init(&mut self) {
        self.globals.clear();
        self.define_native("clock", clock);
    }
}

macro_rules! push {
    ($self: expr, $value: expr) => {
        $self.stack.push($value);
    };
}

macro_rules! pop {
    ($self: expr) => {
        $self.stack.pop().unwrap()
    };
}

macro_rules! peek {
    ($self: expr, $n: expr) => {
        $self.stack.iter().nth_back($n).unwrap()
    };
    ($self: expr) => {
        $self.stack.last().unwrap()
    };
}

macro_rules! binary_op {
    ($self: expr, $op: tt) => {{
        let b = pop!($self);
        let a = pop!($self);
        if let (Some(a), Some(b)) = (a.as_double(), b.as_double()) {
            push!($self, Value::from(a $op b));
        } else {
            push!($self, a);
            push!($self, b);
            runtime_error!($self, "Operands must be numbers.");
        }
    }}
}

macro_rules! runtime_error {
    ($self: expr, $fmt: literal $(, $($args: tt)* )?) => {
        eprintln!($fmt $(, $($args)* )? );

        for f in $self.frames.iter().rev() {
            let fun = &f.closure.function;
            eprint!("[line {}] in ", fun.chunk.lines[f.ip - 1]);
            if fun.name.is_empty() {
                eprintln!("script");
            } else {
                eprintln!("<fn {}>", fun.name);
            }
        }

        $self.stack.clear();
        return Err(Error::Runtime);
    }
}

macro_rules! frame {
    ($self: expr) => {
        $self.frames.last_mut().unwrap()
    };
}

impl<A: Alloc> Vm<A> {
    fn run(&mut self) -> crate::Result<()> {
        use OpCode::*;

        while let Some(instr) = self.read_instr() {
            if log_enabled!(Level::Trace) {
                self.print_stack(&self.strings);
                let f = frame!(self);
                f.closure
                    .function
                    .chunk
                    .disassemble_instr(f.ip - 1, &self.strings);
            }

            match instr {
                Constant => {
                    let constant = self.read_constant();
                    push!(self, constant);
                }
                Nil => push!(self, Value::Nil),
                True => push!(self, Value::Boolean(true)),
                False => push!(self, Value::Boolean(false)),
                Pop => {
                    pop!(self);
                }
                GetLocal => {
                    let slot = self.read_byte().unwrap();
                    let top = frame!(self).stack_top;
                    push!(self, self.stack[top..][slot as usize].clone());
                }
                SetLocal => {
                    let slot = self.read_byte().unwrap();
                    let top = frame!(self).stack_top;
                    self.stack[top..][slot as usize] = peek!(self).clone();
                }
                GetGlobal => {
                    let name = self.read_constant();
                    let name = name.as_string().unwrap();
                    if let Some(value) = self.globals.get(&name) {
                        push!(self, value.clone());
                    } else {
                        runtime_error!(self, "Undefined variable {}", self.strings.lookup(name));
                    }
                }
                SetGlobal => {
                    let name = self.read_constant();
                    let name = name.as_string().unwrap();
                    if self.globals.contains_key(&name) {
                        let value = peek!(self).clone();
                        self.globals.insert(name, value);
                    } else {
                        runtime_error!(self, "Undefined variable {}", self.strings.lookup(name));
                    }
                }
                DefineGlobal => {
                    let name = self.read_constant();
                    let name = name.as_string().unwrap();
                    self.globals.insert(name, pop!(self));
                }
                Equal => {
                    let b = pop!(self);
                    let a = pop!(self);
                    push!(self, Value::Boolean(a == b));
                }
                Greater => binary_op!(self, >),
                Less => binary_op!(self, <),
                Add => {
                    let b = pop!(self);
                    let a = pop!(self);
                    if let (Some(a), Some(b)) = (a.as_double(), b.as_double()) {
                        push!(self, Value::from(a + b));
                    } else if let (Some(sa), Some(sb)) = (a.as_string(), b.as_string()) {
                        let sa = self.strings.lookup(sa);
                        let sb = self.strings.lookup(sb);
                        let out = sa.to_string() + sb;
                        let out = Object::new_string(&out, &mut self.strings);
                        push!(self, out.into());
                    } else {
                        push!(self, a);
                        push!(self, b);
                        runtime_error!(self, "Operands must be two numbers or two strings.");
                    }
                }
                Subtract => binary_op!(self, -),
                Multiply => binary_op!(self, *),
                Divide => binary_op!(self, /),
                Not => {
                    let b = is_falsey(&pop!(self));
                    push!(self, Value::Boolean(b));
                }
                Negate => {
                    let value = pop!(self);
                    if let Some(value) = value.as_double() {
                        push!(self, Value::Double(-value));
                    } else {
                        push!(self, value);
                        runtime_error!(self, "Operand must be a number.");
                    }
                }
                Print => {
                    let mut s = String::new();
                    pop!(self).write(&mut s, &self.strings).unwrap();
                    println!("{}", s);
                }
                Jump => {
                    let offset = self.read_u16().unwrap();
                    frame!(self).ip += offset as usize;
                }
                JumpIfFalse => {
                    let offset = self.read_u16().unwrap();
                    if is_falsey(peek!(self)) {
                        frame!(self).ip += offset as usize;
                    }
                }
                Loop => {
                    let offset = self.read_u16().unwrap();
                    frame!(self).ip -= offset as usize;
                }
                Call => {
                    let arg_count = self.read_byte().unwrap();
                    self.call_value(peek!(self, arg_count as usize).clone(), arg_count)?;
                }
                Closure => {
                    let function = self.read_constant().as_function().unwrap().clone();
                    let closure = Object::Closure(ClosureFn { function });
                    push!(self, closure.into());
                }
                Return => {
                    let result = pop!(self);

                    let f = self.frames.pop().unwrap();

                    // Unwind the stack
                    self.stack.truncate(f.stack_top);

                    if self.frames.is_empty() {
                        return Ok(());
                    }

                    self.stack.push(result);
                }
            }
        }

        Ok(())
    }

    fn define_native(&mut self, name: &str, f: NativeFn<A>) {
        let name = self.strings.intern(name);
        self.globals.insert(name, Object::NativeFn(f).into());
    }

    fn call_value(&mut self, callee: Value<A>, arg_count: u8) -> crate::Result<()> {
        if let Value::Object(obj) = callee {
            match obj {
                Object::Closure(closure) => {
                    return self.call(closure, arg_count);
                }
                Object::NativeFn(f) => {
                    let start = self.stack.len() - arg_count as usize - 1;
                    let result = f(&self.stack[start..]);
                    self.stack.truncate(start);
                    push!(self, result);
                    return Ok(());
                }
                _ => {}
            }
        }

        runtime_error!(self, "Can only call functions and classes");
    }

    fn call(&mut self, closure: ClosureFn<A>, arg_count: u8) -> crate::Result<()> {
        if arg_count as u32 != closure.function.arity {
            runtime_error!(
                self,
                "Expected {} arguments but got {}",
                closure.function.arity,
                arg_count
            );
        }

        if self.frames.len() == self.frames.capacity() {
            runtime_error!(self, "Stack overflow",);
        }

        let frame = CallFrame {
            closure,
            ip: 0,
            stack_top: self.stack.len() - arg_count as usize - 1,
        };
        self.frames.push(frame);
        Ok(())
    }

    fn read_byte(&mut self) -> Option<u8> {
        let f = frame!(self);
        let byte = *f.closure.function.chunk.code.get(f.ip)?;
        f.ip += 1;
        Some(byte)
    }

    fn read_instr(&mut self) -> Option<OpCode> {
        OpCode::from_u8(self.read_byte()?)
    }

    fn read_constant(&mut self) -> Value<A> {
        let idx = self.read_byte().unwrap() as usize;
        frame!(self).closure.function.chunk.values[idx].clone()
    }

    fn read_u16(&mut self) -> Option<u16> {
        let f = frame!(self);
        let hi = *f.closure.function.chunk.code.get(f.ip)?;
        let lo = *f.closure.function.chunk.code.get(f.ip + 1)?;
        f.ip += 2;
        let out = (hi as u16) << 8 | lo as u16;
        Some(out)
    }
}

fn is_falsey<A: Alloc>(value: &Value<A>) -> bool {
    matches!(value, Value::Nil | Value::Boolean(false))
}

fn clock<A: Alloc>(_: &[Value<A>]) -> Value<A> {
    let sec = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_secs() as f64;
    Value::Double(sec)
}
