use crate::chunk::{Chunk, OpCode};
use crate::compile::Compiler;
use crate::intern::StringPool;
use crate::value::Value;
use crate::Error;
use log::Level;
use num_traits::FromPrimitive;
use std::collections::HashMap;

pub struct Vm {
    stack: Vec<Value>,
    chunk: Chunk,
    strings: StringPool,
    globals: HashMap<u32, Value>,
    ip: usize,
}

impl Vm {
    pub fn new() -> Self {
        const STACK_SIZE: usize = 256;
        Self {
            stack: Vec::with_capacity(STACK_SIZE),
            chunk: Chunk::new(),
            strings: StringPool::new(),
            globals: HashMap::new(),
            ip: 0,
        }
    }

    pub fn interpret(&mut self, source: &str) -> crate::Result<()> {
        Compiler::new(source, &mut self.chunk, &mut self.strings).compile()?;
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
        self.chunk.clear();
        self.globals.clear();
        self.strings.clear();
        self.ip = 0;
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
        println!($fmt $(, $($args)* )? );
        let i = $self.ip - 1;
        let line = $self.chunk.lines[i];
        println!("[line: {}] in script", line);
        $self.stack.clear();
        return Err(Error::Runtime);
    }
}

impl Vm {
    fn run(&mut self) -> crate::Result<()> {
        use OpCode::*;

        while let Some(instr) = self.read_instr() {
            if log_enabled!(Level::Trace) {
                self.print_stack(&self.strings);
                self.chunk.disassemble_instr(self.ip - 1, &self.strings);
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
                        let out = self.strings.intern(out);
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
                    let b = is_falsey(pop!(self));
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
                Return => return Ok(()),
            }
        }

        Ok(())
    }

    fn read_byte(&mut self) -> Option<u8> {
        let byte = *self.chunk.code.get(self.ip)?;
        self.ip += 1;
        Some(byte)
    }

    fn read_instr(&mut self) -> Option<OpCode> {
        OpCode::from_u8(self.read_byte()?)
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte().unwrap() as usize;
        self.chunk.values[idx].clone()
    }
}

fn is_falsey(value: Value) -> bool {
    matches!(value, Value::Nil | Value::Boolean(true))
}
