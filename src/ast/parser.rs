use crate::{
    errors::{ErrorKind, ParseError},
    lexer::{Token, TokenKind as Tk},
};

use super::{
    astnodes::{DelimiterKind, ExprNode},
    controlflow::{Converge, ExprFlow},
    expressions::OpBindingPower,
};

use std::ops::ControlFlow::Break;

pub(super) struct AstParser<'i, T: Iterator<Item = Token<'i>>> {
    pub(super) tokens: T,
    pub(super) delimiters: Vec<DelimiterKind>,
    pub(super) allow_comma: bool,
    pub(super) errors: Vec<ParseError<'i>>,
}

impl<'i, T: Iterator<Item = Token<'i>> + Clone> AstParser<'i, T> {
    pub fn new(tokens: T) -> Self {
        Self {
            tokens,
            delimiters: Vec::new(),
            allow_comma: false,
            errors: Vec::new(),
        }
    }

    pub(super) fn report_error(&mut self, token: Token<'i>, error: ErrorKind<'i>) {
        self.errors.push(ParseError::new(error, token));
    }

    pub(super) fn advance(&mut self) -> Option<Token<'i>> {
        self.tokens.next()
    }

    pub(super) fn advance_or_eof(&mut self) -> Token<'i> {
        self.advance().unwrap_or_else(Token::eof)
    }

    pub(super) fn expect(&mut self, expected_token: Tk<'i>) {
        let token = self.advance_or_eof();
        if token.kind != expected_token {
            self.report_error(
                token,
                ErrorKind::UnexpectedToken {
                    expected_token: Some(expected_token),
                },
            );
        }
    }

    pub(super) fn peek(&mut self) -> Option<Token<'i>> {
        self.tokens.clone().next()
    }

    pub(super) fn peek_or_eof(&mut self) -> Token<'i> {
        self.peek().unwrap_or_else(Token::eof)
    }

    pub(super) fn unexpected_token(&mut self, token: Token<'i>) -> ExprFlow<'i> {
        self.report_error(
            token,
            ErrorKind::UnexpectedToken {
                expected_token: None,
            },
        );
        Break(ExprNode::Error)
    }

    pub(super) fn verify_closing_delimiter(
        &mut self,
        closing_delimiter: DelimiterKind,
        lhs: ExprNode<'i>,
    ) -> ExprFlow<'i> {
        let token = self.peek_or_eof();
        match self.delimiters.last() {
            Some(expected_delimiter) if closing_delimiter == *expected_delimiter => {
                // Pop stack to expect the next delimiter
                self.delimiters.pop();
            }

            Some(expected_delimiter) => self.report_error(
                token,
                ErrorKind::MismatchedDelimiter {
                    expected_delimiter: *expected_delimiter,
                },
            ),
            None => self.report_error(token, ErrorKind::ExtraClosingDelimiter),
        };

        Break(lhs)
    }

    pub fn parse(mut self) -> Result<ExprNode<'i>, Vec<ParseError<'i>>> {
        let expr = self.expression(OpBindingPower::All);
        if !self.delimiters.is_empty() {
            self.errors.push(ParseError::new(
                ErrorKind::UnclosedDelimiter {
                    expected_delimiter: self.delimiters.pop().unwrap(),
                },
                Token::eof(),
            ));
        }

        if self.errors.is_empty() {
            Ok(expr.converge())
        } else {
            Err(self.errors)
        }
    }
}
