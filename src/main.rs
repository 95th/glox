use clap::{App, Arg};
use glox::vm::Vm;
use glox::Error;

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
}

fn run_file(path: &str) {
    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(_) => {
            println!("Could not open file: {}", path);
            std::process::exit(74);
        }
    };
    let mut vm = Vm::new();
    match vm.interpret(&source) {
        Ok(()) => {}
        Err(Error::Compile) => std::process::exit(65),
        Err(Error::Runtime) => std::process::exit(70),
    }
}

fn repl() {
    let mut vm = Vm::new();
    let mut editor = rustyline::Editor::<()>::new();
    while let Ok(line) = editor.readline("> ") {
        if line == "exit" {
            break;
        }
        match vm.interpret(&line) {
            Ok(()) => {}
            Err(Error::Compile) => println!("Unable to compile: {}", line),
            Err(Error::Runtime) => println!("Runtime error"),
        }
    }
}
