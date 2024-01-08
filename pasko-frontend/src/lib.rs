#[macro_use]
extern crate lazy_static;

pub mod utils;
#[macro_use]
pub mod span;
pub mod ast;
pub mod constant;
pub mod diagnostics;
pub mod dump;
pub mod lexer;
pub mod parser;
pub mod scope;
pub mod semantic;
pub mod symbol;
pub mod typesystem;
pub mod visitor;
