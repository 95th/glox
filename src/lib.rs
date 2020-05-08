#[macro_use]
extern crate log;

pub mod chunk;
pub mod compile;
pub mod debug;
mod intern;
pub mod object;
pub mod value;
pub mod vm;

#[derive(Debug)]
pub enum Error {
    Compile,
    Runtime,
}

pub type Result<T> = std::result::Result<T, Error>;
