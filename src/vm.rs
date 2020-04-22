use crate::chunk::{Chunk, OpCode};
use crate::compile::Compiler;
use crate::value::Value;
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
        let b = pop!($me).as_double().unwrap();
        let a = pop!($me).as_double().unwrap();
        push!($me, Value::Double(a $op b));
    }}
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
                Add => binary_op!(self, +),
                Subtract => binary_op!(self, -),
                Multiply => binary_op!(self, *),
                Divide => binary_op!(self, /),
                Negate => {
                    let value = pop!(self).as_double().unwrap();
                    push!(self, Value::Double(-value));
                }
                Return => {
                    println!("{}", pop!(self));
                    return Ok(());
                }
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
}
