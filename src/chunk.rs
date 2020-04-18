use std::convert::TryFrom;
use std::fmt::Display;
use std::ops::{Deref, DerefMut};

#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Return,
}

impl TryFrom<u8> for OpCode {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        use OpCode::*;
        let opcode = match value {
            0 => Return,
            _ => return Err(value),
        };
        Ok(opcode)
    }
}

impl OpCode {
    pub fn instr_len(&self) -> usize {
        use OpCode::*;
        match *self {
            Return => 1,
        }
    }
}

#[derive(Default)]
pub struct Chunk(Vec<u8>);

impl Chunk {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn disassemble(&self, name: impl Display) {
        println!("== {} ==", name);

        let mut offset = 0;
        while offset < self.len() {
            offset = self.disassemble_instr(offset);
        }
    }

    fn disassemble_instr(&self, offset: usize) -> usize {
        print!("{:04}", offset);

        let opcode = match OpCode::try_from(self[offset]) {
            Ok(opcode) => opcode,
            Err(b) => {
                println!("Unknown opcode {}", b);
                return offset + 1;
            }
        };

        println!("{:?}", opcode);
        offset + opcode.instr_len()
    }
}

impl Deref for Chunk {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Chunk {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
