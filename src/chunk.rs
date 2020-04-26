use crate::value::Value;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, FromPrimitive, ToPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Constant,
    Nil,
    True,
    False,
    Pop,
    GetLocal,
    GetGlobal,
    SetLocal,
    SetGlobal,
    DefineGlobal,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump,
    JumpIfFalse,
    Return,
}

#[derive(Default)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub values: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_instr(&mut self, opcode: OpCode, line: usize) {
        self.push_chunk(opcode as _, line);
    }

    pub fn push_chunk(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    pub fn push_const(&mut self, value: Value) -> usize {
        let len = self.values.len();
        self.values.push(value);
        len
    }

    pub fn clear(&mut self) {
        self.code.clear();
        self.values.clear();
        self.lines.clear();
    }
}
