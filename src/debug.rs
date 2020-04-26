use crate::chunk::{Chunk, OpCode};
use crate::intern::StringPool;
use num_traits::FromPrimitive;
use std::fmt::{Display, Write};

impl Chunk {
    pub fn disassemble(&self, name: impl Display, strings: &StringPool) {
        trace!("== {} ==", name);

        let mut offset = 0;
        while offset < self.code.len() {
            offset = self.disassemble_instr(offset, strings);
        }
    }

    pub fn disassemble_instr(&self, offset: usize, strings: &StringPool) -> usize {
        let buf = &mut format!("{:04}", offset);

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

        let offset = match opcode {
            Return | Negate | Add | Subtract | Multiply | Divide | Nil | True | False | Not
            | Equal | Greater | Less | Print | Pop => self.simple_instr(opcode, offset, buf),

            Constant | GetGlobal | SetGlobal | DefineGlobal => {
                self.constant_instr(opcode, offset, buf, strings)
            }

            SetLocal | GetLocal => self.byte_instr(opcode, buf, offset),

            Jump | JumpIfFalse => self.jump_instr(opcode, buf, 1, offset),

            Loop => self.jump_instr(opcode, buf, -1, offset),
        };

        trace!("{}", buf);
        offset
    }

    fn simple_instr(&self, opcode: OpCode, offset: usize, buf: &mut String) -> usize {
        write!(buf, "{:?}", opcode).unwrap();
        offset + 1
    }

    fn byte_instr(&self, opcode: OpCode, buf: &mut String, offset: usize) -> usize {
        let slot = self.code[offset + 1];
        write!(buf, "{:-16?} {:4}", opcode, slot).unwrap();
        offset + 2
    }

    fn jump_instr(&self, opcode: OpCode, buf: &mut String, sign: isize, offset: usize) -> usize {
        let mut jump = (self.code[offset + 1] as u16) << 8;
        jump |= self.code[offset + 2] as u16;
        write!(
            buf,
            "{:-16?} {:4} -> {}",
            opcode,
            offset,
            offset as isize + 3 + sign * jump as isize
        )
        .unwrap();
        offset + 3
    }

    fn constant_instr(
        &self,
        opcode: OpCode,
        offset: usize,
        buf: &mut String,
        strings: &StringPool,
    ) -> usize {
        let constant = self.code[offset + 1];
        write!(buf, "{:-16?} {:4} ", opcode, constant).unwrap();
        self.write_value(constant, buf, strings);
        offset + 2
    }

    fn write_value(&self, value_idx: u8, buf: &mut String, strings: &StringPool) {
        self.values[value_idx as usize].write(buf, strings).unwrap()
    }
}
