use glox::chunk::{Chunk, OpCode};
use glox::vm::Vm;

fn main() {
    env_logger::init();

    let mut vm = Vm::new();
    let mut chunk = Chunk::new();

    chunk.push_instr(OpCode::Constant, 123);
    let constant = chunk.push_const(1.2);
    chunk.push_chunk(constant as u8, 123);

    chunk.push_instr(OpCode::Constant, 123);
    let constant = chunk.push_const(3.4);
    chunk.push_chunk(constant as u8, 123);

    chunk.push_instr(OpCode::Add, 123);

    chunk.push_instr(OpCode::Constant, 123);
    let constant = chunk.push_const(5.6);
    chunk.push_chunk(constant as u8, 123);

    chunk.push_instr(OpCode::Divide, 123);
    chunk.push_instr(OpCode::Negate, 123);

    chunk.push_instr(OpCode::Return, 123);

    vm.interpret(&chunk);
}
