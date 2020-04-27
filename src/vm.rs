use crate::chunk::OpCode;
use crate::compile::{Compiler, FunctionKind};
use crate::intern::StringPool;
use crate::object::{Function, Object};
use crate::value::Value;
use crate::Error;
use log::Level;
use num_traits::FromPrimitive;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Vm {
    stack: Vec<Value>,
    frames: Vec<CallFrame>,
    strings: StringPool,
    globals: HashMap<u32, Value>,
}

struct CallFrame {
    function: Rc<Function>,
    ip: usize,
    stack_top: usize,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            strings: StringPool::new(),
            globals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> crate::Result<()> {
        let compiler = Compiler::new(source, &mut self.strings);
        let function = Rc::new(compiler.compile(FunctionKind::Script)?);
        self.stack.push(Object::Function(function.clone()).into());
        self.call_value(Object::Function(function).into(), 0)?;
        self.run()
    }

    fn print_stack(&self, strings: &StringPool) {
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
            eprint!("[line {}] in ", f.function.chunk.lines[f.ip - 1]);
            if f.function.name.is_empty() {
                eprintln!("script");
            } else {
                eprintln!("<fn {}>", f.function.name);
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

impl Vm {
    fn run(&mut self) -> crate::Result<()> {
        use OpCode::*;

        while let Some(instr) = self.read_instr() {
            if log_enabled!(Level::Trace) {
                self.print_stack(&self.strings);
                let f = frame!(self);
                f.function.chunk.disassemble_instr(f.ip - 1, &self.strings);
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
                Return => {
                    let result = pop!(self);

                    let f = self.frames.pop().unwrap();

                    // Unwind the stack
                    self.stack.truncate(f.stack_top);

                    if self.frames.is_empty() {
                        self.stack.pop();
                        return Ok(());
                    }

                    self.stack.push(result);
                }
            }
        }

        Ok(())
    }

    fn call_value(&mut self, callee: Value, arg_count: u8) -> crate::Result<()> {
        match callee {
            Value::Object(Object::Function(f)) => return self.call(f, arg_count),
            _ => {}
        }

        runtime_error!(self, "Can only call functions and classes");
    }

    fn call(&mut self, function: Rc<Function>, arg_count: u8) -> crate::Result<()> {
        if arg_count as u32 != function.arity {
            runtime_error!(
                self,
                "Expected {} arguments but got {}",
                function.arity,
                arg_count
            );
        }

        if self.frames.len() == self.frames.capacity() {
            runtime_error!(self, "Stack overflow",);
        }

        let frame = CallFrame {
            function,
            ip: 0,
            stack_top: self.stack.len() - arg_count as usize - 1,
        };
        self.frames.push(frame);
        Ok(())
    }

    fn read_byte(&mut self) -> Option<u8> {
        let f = frame!(self);
        let byte = *f.function.chunk.code.get(f.ip)?;
        f.ip += 1;
        Some(byte)
    }

    fn read_instr(&mut self) -> Option<OpCode> {
        OpCode::from_u8(self.read_byte()?)
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte().unwrap() as usize;
        frame!(self).function.chunk.values[idx].clone()
    }

    fn read_u16(&mut self) -> Option<u16> {
        let f = frame!(self);
        let hi = *f.function.chunk.code.get(f.ip)?;
        let lo = *f.function.chunk.code.get(f.ip + 1)?;
        f.ip += 2;
        let out = (hi as u16) << 8 | lo as u16;
        Some(out)
    }
}

fn is_falsey(value: &Value) -> bool {
    matches!(value, Value::Nil | Value::Boolean(false))
}
