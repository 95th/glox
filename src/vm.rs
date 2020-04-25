use crate::chunk::{Chunk, OpCode};
use crate::compile::Compiler;
use crate::value::Value;
use crate::Error;
use log::Level;
use num_traits::FromPrimitive;
use std::fmt::Write;

pub struct Vm {
    stack: Vec<Value>,
}

impl Vm {
    pub fn new() -> Self {
        const STACK_SIZE: usize = 256;
        Self {
            stack: Vec::with_capacity(STACK_SIZE),
        }
    }

    pub fn interpret(&mut self, source: &str) -> crate::Result<()> {
        let mut chunk = Chunk::new();
        Compiler::new(source, &mut chunk).compile()?;

        let mut session = VmSession::new(self, &chunk);
        session.run()
    }

    fn print_stack(&self) {
        let mut buf = String::from("          ");
        for val in &self.stack {
            write!(buf, "[ {} ]", val).unwrap();
        }

        trace!("{}", buf);
    }
}

struct VmSession<'a> {
    vm: &'a mut Vm,
    chunk: &'a Chunk,
    ip: usize,
}

macro_rules! push {
    ($me: expr, $value: expr) => {
        $me.vm.stack.push($value);
    };
}

macro_rules! pop {
    ($me: expr) => {
        $me.vm.stack.pop().unwrap()
    };
}

macro_rules! binary_op {
    ($me: expr, $op: tt) => {{
        let b = pop!($me);
        let a = pop!($me);
        if let (Some(a), Some(b)) = (a.as_double(), b.as_double()) {
            push!($me, Value::from(a $op b));
        } else {
            push!($me, a);
            push!($me, b);
            runtime_error!($me, "Operands must be numbers.");
        }
    }}
}

macro_rules! runtime_error {
    ($self: expr, $fmt: literal $(, $($args: tt)* )?) => {
        println!($fmt, $($args)*);
        let i = $self.ip - 1;
        let line = $self.chunk.lines[i];
        println!("[line: {}] in script", line);
        $self.reset_stack();
        return Err(Error::Runtime);
    }
}

impl<'a> VmSession<'a> {
    fn new(vm: &'a mut Vm, chunk: &'a Chunk) -> Self {
        Self { vm, chunk, ip: 0 }
    }

    fn run(&mut self) -> crate::Result<()> {
        use OpCode::*;

        self.vm.stack.clear();

        while let Some(instr) = self.read_instr() {
            if log_enabled!(Level::Trace) {
                self.vm.print_stack();
                self.chunk.disassemble_instr(self.ip - 1);
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
                Equal => {
                    let b = pop!(self);
                    let a = pop!(self);
                    push!(self, Value::Boolean(a == b));
                }
                Greater => binary_op!(self, >),
                Less => binary_op!(self, <),
                Add => {
                    let b = pop!(self);
                    let mut a = pop!(self);
                    if let (Some(a), Some(b)) = (a.as_double(), b.as_double()) {
                        push!(self, Value::from(a + b));
                    } else if let (Some(sa), Some(sb)) = (a.as_string_mut(), b.as_string()) {
                        *sa += sb;
                        push!(self, a);
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
                Print => println!("{}", pop!(self)),
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
        self.chunk.values[self.read_byte().unwrap() as usize].clone()
    }

    fn reset_stack(&mut self) {
        self.vm.stack.clear();
    }
}

fn is_falsey(value: Value) -> bool {
    matches!(value, Value::Nil | Value::Boolean(true))
}
