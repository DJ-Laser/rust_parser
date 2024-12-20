use std::fmt::{Display, Formatter};

use crate::{
    ast::astnodes::DelimiterKind,
    lexer::{Token, TokenKind},
};

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind<'i> {
    UnterminatedStringLiteral,
    ExtraClosingDelimiter,
    MismatchedDelimiter {
        expected_delimiter: DelimiterKind,
    },
    UnclosedDelimiter {
        expected_delimiter: DelimiterKind,
    },

    BadAccessIdentifier,
    UnexpectedToken {
        expected_token: Option<TokenKind<'i>>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParseError<'i> {
    error: ErrorKind<'i>,
    token: Token<'i>,
}

impl<'i> ParseError<'i> {
    pub fn new(error: ErrorKind<'i>, token: Token<'i>) -> Self {
        Self { error, token }
    }

    fn message(&self) -> String {
        match self.error {
            ErrorKind::UnterminatedStringLiteral => format!("unterminated string literal"),
            ErrorKind::ExtraClosingDelimiter => {
                format!("unexpected closing delimiter: `{}`", self.token)
            }
            ErrorKind::MismatchedDelimiter { expected_delimiter } => format!(
                "mismatched closing delimiter: `{}`",
                expected_delimiter.get_closing_token()
            ),
            ErrorKind::UnclosedDelimiter { expected_delimiter } => format!(
                "unclosed delimiter: `{}`",
                expected_delimiter.get_opening_token()
            ),
            ErrorKind::BadAccessIdentifier => {
                format!("expected identifier, got: {}", self.token)
            }
            ErrorKind::UnexpectedToken {
                expected_token: Some(ref token),
            } => {
                format!("expected token `{}`, got: `{}`", token, self.token)
            }
            ErrorKind::UnexpectedToken {
                expected_token: None,
            } => {
                format!("expected expression, got: `{}`", self.token)
            }
        }
    }
}

impl<'i> Display for ParseError<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.token.line == 0 {
            write!(f, "Error: {}", self.message())
        } else {
            write!(f, "Error on line {}: {}", self.token.line, self.message())
        }
    }
}
