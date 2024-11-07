mod lexer;
mod ast;

use std::io::{BufRead, stdin};

use lexer::Lexer;

/// Basic REPL
pub fn main() {
    for line in stdin().lock().lines() {
        let line = line.unwrap();

        let lexer = Lexer::new(&line);
        let ast = ast::parse(lexer);
        println!("{:?}", ast);
    }
}
