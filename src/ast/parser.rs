#[allow(dead_code)]
use std::cmp::Ordering;
use std::ops::ControlFlow::{Break, Continue};

use crate::ast::controlflow::Converge;
use crate::lexer::LiteralKind;
use crate::{
    errors::{ErrorKind, ParseError},
    lexer::Token,
    lexer::TokenKind as Tk,
};

use super::astnodes::DelimiterKind;
use super::astnodes::{BinOpKind, ExprNode};
use super::controlflow::{converge_expr, ExprFlow};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(super) enum OpBindingPower {
    All,

    Grouping,
    Conditional,
    Call,
    Index,
    Unary,

    Sum,
    Factor,

    Literal,
}

impl OpBindingPower {
    fn can_bind_right(&self, right: &Self) -> bool {
        match right.cmp(self) {
            Ordering::Less => false,
            Ordering::Greater => true,
            Ordering::Equal => {
                match self {
                    // Right associative operators
                    Self::Conditional => true,
                    // Most operators are left associative
                    _ => false,
                }
            }
        }
    }
}

pub(super) struct AstParser<'i, T: Iterator<Item = Token<'i>>> {
    tokens: T,
    delimiters: Vec<DelimiterKind>,
    allow_comma: bool,
    pub errors: Vec<ParseError<'i>>,
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

    fn report_error(&mut self, token: Token<'i>, error: ErrorKind) {
        self.errors.push(ParseError::new(error, token));
    }

    fn advance(&mut self) -> Option<Token<'i>> {
        self.tokens.next()
    }

    fn advance_or_eof(&mut self) -> Token<'i> {
        self.tokens.next().unwrap_or_else(Token::eof)
    }

    fn expect(&mut self, tk: Tk) {
        let token = self.advance_or_eof();
        if token.kind != tk {
            println!("Expexted token {:?} but got {:?}", tk, token.kind);
        }
    }

    fn peek(&mut self) -> Option<Token<'i>> {
        self.tokens.clone().next()
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

    fn expression(&mut self, left_power: OpBindingPower) -> ExprFlow<'i> {
        let token = self.advance_or_eof();

        let mut lhs = match token.kind {
            Tk::OpenParen => self.group_or_tuple(),
            Tk::OpenBracket => self.array_literal(),
            Tk::Minus => {
                let expr = self.expression(OpBindingPower::Unary);
                converge_expr!(ExprNode::Negate => (expr.boxed()))
            }
            Tk::Ident(name) => Continue(ExprNode::Ident(name)),
            Tk::Literal(kind) => self.literal(token, kind),

            _ => self.unexpected_token(token),
        }?;

        while let Some(token) = self.peek() {
            lhs = match token.kind {
                Tk::OpenParen => self.call(lhs),
                Tk::OpenBracket => self.index(lhs),

                // Closing Delimiters, verify and return current parsed expression
                Tk::CloseParen => self.verify_closing_delimiter(DelimiterKind::Parentheses, lhs),
                Tk::CloseBracket => self.verify_closing_delimiter(DelimiterKind::Brackets, lhs),

                Tk::Dot => self.access(lhs),
                Tk::Comma if self.allow_comma => Break(lhs),

                // Conditional Operator "c ? a : b"
                Tk::Question => self.conditional(lhs),
                Tk::Colon => self.verify_closing_delimiter(DelimiterKind::Conditional, lhs),

                Tk::Plus => self.bin_op(BinOpKind::Add, lhs, left_power),
                Tk::Minus => self.bin_op(BinOpKind::Subtract, lhs, left_power),
                Tk::Star => self.bin_op(BinOpKind::Multiply, lhs, left_power),
                Tk::Slash => self.bin_op(BinOpKind::Divide, lhs, left_power),

                _ => {
                    let token = self.advance_or_eof();
                    self.unexpected_token(token)
                }
            }?;
        }

        Continue(lhs)
    }

    fn unexpected_token(&mut self, token: Token<'i>) -> ExprFlow<'i> {
        self.report_error(token, ErrorKind::UnexpectedToken);
        Break(ExprNode::Error)
    }

    fn literal(&mut self, token: Token<'i>, kind: LiteralKind<'i>) -> ExprFlow<'i> {
        let expr = match kind {
            LiteralKind::Int(n) => ExprNode::Int(n),
            LiteralKind::Float(n) => ExprNode::Float(n),
            LiteralKind::String { string, terminated } => {
                if terminated {
                    ExprNode::String(string)
                } else {
                    self.report_error(token, ErrorKind::UnterminatedStringLiteral);
                    return Break(ExprNode::String(string));
                }
            }
        };

        Continue(expr)
    }

    fn comma_seperated_exprs(&mut self, closing_delimiter: DelimiterKind) -> Vec<ExprNode<'i>> {
        self.delimiters.push(closing_delimiter);
        let mut expressions = Vec::new();
        let prev_allow_comma = self.allow_comma;
        self.allow_comma = true;

        loop {
            match self.peek().map(|tk| tk.kind) {
                Some(Tk::Comma) => {
                    self.advance();
                }
                Some(tk) if tk == closing_delimiter.get_closing_token() => {
                    self.advance();
                    break;
                }
                _ => {
                    let expr = self.expression(OpBindingPower::Grouping);
                    let should_break = matches!(expr, Break(_));
                    expressions.push(expr.converge());

                    if should_break {
                        break;
                    }
                }
            };
        }

        self.allow_comma = prev_allow_comma;
        expressions
    }

    fn array_literal(&mut self) -> ExprFlow<'i> {
        let elements = self.comma_seperated_exprs(DelimiterKind::Brackets);
        Continue(ExprNode::Array(elements))
    }

    fn group_or_tuple(&mut self) -> ExprFlow<'i> {
        let mut elements = self.comma_seperated_exprs(DelimiterKind::Parentheses);

        let expr = match elements.len() {
            1 => elements.pop().unwrap(),
            _ => ExprNode::Tuple(elements),
        };

        Continue(expr)
    }

    fn bin_op(
        &mut self,
        op_kind: BinOpKind,
        lhs: ExprNode<'i>,
        left_power: OpBindingPower,
    ) -> ExprFlow<'i> {
        let right_power = op_kind.get_binding_power();

        if !left_power.can_bind_right(&right_power) {
            return Break(lhs);
        }

        // Discard the op if we bind it to the expression
        self.advance();
        let rhs = self.expression(right_power);

        converge_expr!(ExprNode::BinOp => {
            right: rhs.boxed(),
        } !{
            kind: op_kind,
            left: Box::new(lhs),
        })
    }

    fn conditional(&mut self, condition: ExprNode<'i>) -> ExprFlow<'i> {
        self.expect(Tk::Question);
        self.delimiters.push(DelimiterKind::Conditional);

        // '?' and ':' group similar to parenthesis
        let true_expression = self.expression(OpBindingPower::Grouping);

        self.expect(Tk::Colon);

        // Expressions after the ':' are subject to standard bindng rules
        let false_expression = self.expression(OpBindingPower::Conditional);

        converge_expr!(ExprNode::Conditional => {
            true_expression: true_expression.boxed(),
            false_expression: false_expression.boxed(),
        } !{
            condition: Box::new(condition),
        })
    }

    fn access(&mut self, container: ExprNode<'i>) -> ExprFlow<'i> {
        self.expect(Tk::Dot);

        let token = self.advance_or_eof();
        let expr = match token.kind {
            Tk::Ident(name) => ExprNode::Access {
                container: Box::new(container),
                name: format!("{:?}", name),
            },
            _ => {
                self.report_error(token, ErrorKind::BadAccessIdentifier);
                ExprNode::Error
            }
        };

        Continue(expr)
    }

    fn index(&mut self, container: ExprNode<'i>) -> ExprFlow<'i> {
        self.expect(Tk::OpenBracket);
        let index = self.delimited_expression(DelimiterKind::Brackets);

        self.expect(Tk::CloseBracket);

        converge_expr!(ExprNode::Index => {
            index: index.boxed(),
        } !{
            container: Box::new(container),
        })
    }

    fn call(&mut self, function: ExprNode<'i>) -> ExprFlow<'i> {
        self.expect(Tk::OpenParen);
        let arguments = self.comma_seperated_exprs(DelimiterKind::Parentheses);
        Continue(ExprNode::Call {
            function: Box::new(function),
            arguments,
        })
    }

    fn delimited_expression(&mut self, opening_delimeter: DelimiterKind) -> ExprFlow<'i> {
        self.delimiters.push(opening_delimeter);
        let expr = self.expression(OpBindingPower::Grouping);
        expr
    }

    fn verify_closing_delimiter(
        &mut self,
        closing_delimiter: DelimiterKind,
        lhs: ExprNode<'i>,
    ) -> ExprFlow<'i> {
        let token = self.advance_or_eof();
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
}
