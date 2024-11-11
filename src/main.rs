mod lexer;
mod ast;
mod interner;

use std::io::{BufRead, stdin};

use interner::HashSetInterner;
use lexer::Lexer;

/// Basic REPL
pub fn main() {
    let mut strings = HashSetInterner::new();
    
    for line in stdin().lock().lines() {
        let line = line.unwrap();

        let lexer = Lexer::new(&line, &mut strings);
        let ast = ast::parse(lexer);
        println!("{:?}", ast);
    }
}
