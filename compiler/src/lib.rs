#[macro_use]
extern crate lalrpop_util;

mod ast;
mod parser_test;
lalrpop_mod!(grammar);
