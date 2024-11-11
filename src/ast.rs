pub mod astnodes;
pub mod controlflow;
pub mod parser;

use parser::{AstParser, OpBindingPower};

use astnodes::ExprNode;
use controlflow::Converge;

use crate::errors::ParseError;
use crate::lexer::Token;

pub fn parse<'i, T>(tokens: T) -> Result<ExprNode<'i>, Vec<ParseError<'i>>>
where
    T: Iterator<Item = Token<'i>> + Clone,
{
    let mut parser = AstParser::new(tokens);
    let ast = parser.expression(OpBindingPower::All);
    if parser.errors.is_empty() {
        Ok(ast.converge())
    } else {
        Err(parser.errors)
    }
}
