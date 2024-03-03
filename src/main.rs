#![allow(unused)]
mod ast;
mod lexer;
mod parser;
mod repl;
mod token;

use token::TokenType;

fn main() {
    println!("Hello This is the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::start_repl();
}
