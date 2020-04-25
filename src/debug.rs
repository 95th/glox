use crate::chunk::{Chunk, OpCode};
use num_traits::FromPrimitive;
use std::fmt::{Display, Write};

impl Chunk {
    pub fn disassemble(&self, name: impl Display) {
        trace!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instr(offset);
        }
    }

    pub fn disassemble_instr(&self, offset: usize) -> usize {
        let mut buf = format!("{:04}", offset);

        if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            buf.push_str("   | ");
        } else {
            write!(buf, "{:4} ", self.lines[offset]).unwrap();
        }

        let opcode = match OpCode::from_u8(self.code[offset]) {
            Some(opcode) => opcode,
            None => {
                trace!("{}Unknown opcode", buf);
                return offset + 1;
            }
        };

        use OpCode::*;

        match opcode {
            Return | Negate | Add | Subtract | Multiply | Divide | Nil | True | False | Not
            | Equal | Greater | Less | Print | Pop => self.simple_instr(opcode, offset, buf),
            Constant => self.constant_instr(opcode, offset, buf),
        }
    }

    fn simple_instr(&self, opcode: OpCode, offset: usize, buf: String) -> usize {
        trace!("{}{:?}", buf, opcode);
        offset + 1
    }

    fn constant_instr(&self, opcode: OpCode, offset: usize, mut buf: String) -> usize {
        let constant = self.code[offset + 1];
        write!(buf, "{:-16?} {:4} ", opcode, constant).unwrap();
        self.write_value(constant, &mut buf);
        trace!("{}", buf);
        offset + 2
    }

    fn write_value(&self, value_idx: u8, buf: &mut String) {
        write!(buf, "{}", self.values[value_idx as usize]).unwrap();
    }
}
