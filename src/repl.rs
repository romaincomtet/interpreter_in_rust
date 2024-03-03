use crate::lexer::Lexer;
use std::io::{self, Write};

const PROMPT: &'static str = ">> ";

pub fn start_repl() {
    let mut buffer = String::new();
    let stdin = io::stdin(); // We get `Stdin` here.
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        match stdin.read_line(&mut buffer) {
            Ok(size_read) => {
                if size_read == 0 {
                    // Break on EOF or no input
                    break;
                };
                let l = Lexer::new(&buffer);
                for tok in l.into_iter() {
                    println!("{:?}", tok);
                }
            }
            Err(error) => {
                eprintln!("Error: {}", error);
                break;
            }
        }
    }
}
