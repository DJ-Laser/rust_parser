mod ast;
mod errors;
mod interner;
mod lexer;

use std::io::{stdin, BufRead};

use interner::HashSetInterner;
use lexer::Lexer;

/// Basic REPL
pub fn main() {
    let mut strings = HashSetInterner::new();

    for line in stdin().lock().lines() {
        let line = line.unwrap();

        let lexer = Lexer::new(&line, &mut strings);
        let res = ast::parse(lexer);
        match res {
            Ok(ast) => println!("{ast:?}"),
            Err(errors) => {
                for err in errors {
                    println!("{err}");
                }
            }
        }
    }
}
