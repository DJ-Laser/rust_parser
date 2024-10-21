mod lexer;
mod ast;

use std::io::{BufRead, stdin};

use lexer::Lexer;

/// Basic REPL
pub fn main() {
    for line in stdin().lock().lines() {
        let line = line.unwrap();
        let ast = ast::parse(Lexer::new(&line));
        println!("{:?}", ast);
    }
}
