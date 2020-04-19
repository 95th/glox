use clap::{App, Arg};
use glox::chunk::{Chunk, OpCode};
use glox::vm::{InterpretError::*, InterpretResult, Vm};

fn main() {
    env_logger::init();
    let matches = App::new("Lox")
        .version("0.1")
        .about("Lox Runtime Environment")
        .author("95th <vargwin@gmail.com>")
        .arg(Arg::with_name("in_file").index(1))
        .get_matches();

    match matches.value_of("in_file") {
        Some(file) => run_file(file),
        None => repl(),
    }

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

    vm.interpret(&chunk).unwrap();
}

fn run_file(path: &str) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => {
            println!("Could not open file: {}", path);
            std::process::exit(74);
        }
    };
    match interpret(&source) {
        Ok(()) => {}
        Err(Compile) => std::process::exit(65),
        Err(Runtime) => std::process::exit(70),
    }
}

fn repl() {
    let mut editor = rustyline::Editor::<()>::new();
    while let Ok(line) = editor.readline("> ") {
        if line == "exit" {
            break;
        }
        match interpret(&line) {
            Ok(()) => {}
            Err(Compile) => println!("Unable to compile: {}", line),
            Err(Runtime) => println!("Runtime error"),
        }
    }
}

fn interpret(source: &str) -> InterpretResult<()> {
    todo!()
}
