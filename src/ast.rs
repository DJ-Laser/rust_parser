pub mod astnodes;
pub mod controlflow;
pub mod parser;

use parser::AstParser;

use astnodes::ExprNode;

use crate::errors::ParseError;
use crate::lexer::Token;

pub fn parse<'i, T>(tokens: T) -> Result<ExprNode<'i>, Vec<ParseError<'i>>>
where
    T: Iterator<Item = Token<'i>> + Clone,
{
    let parser = AstParser::new(tokens);
    parser.parse()
}
