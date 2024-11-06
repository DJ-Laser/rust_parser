#![allow(dead_code)]

use std::cmp::Ordering;
use std::ops::ControlFlow;

use crate::lexer::{Token, TokenKind as Tk, LiteralKind};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum OpBindingPower {
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

#[derive(Debug)]
pub enum BinOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl BinOpKind {
    fn get_binding_power(&self) -> OpBindingPower {
        match self {
            Self::Add
            | Self::Subtract => OpBindingPower::Sum,
            Self::Multiply
            | Self::Divide => OpBindingPower::Factor,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum DelimiterKind {
    Parentheses,
    Brackets,
    Braces,
    Conditional,
}

impl DelimiterKind {
    fn get_opening_token(&self) -> Tk {
        match self {
            Self::Parentheses => Tk::OpenParen,
            Self::Brackets => Tk::OpenBracket,
            Self::Braces => todo!(),
            Self::Conditional => Tk::Question
        }
    }
    
    fn get_closing_token(&self) -> Tk {
        match self {
            Self::Parentheses => Tk::CloseParen,
            Self::Brackets => Tk::CloseBracket,
            Self::Braces => todo!(),
            Self::Conditional => Tk::Colon
        }
    }
}

#[derive(Debug)]
pub enum ExprNode {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Array(Vec<ExprNode>),
    Tuple(Vec<ExprNode>),

    Ident(String),

    Call {
        function: Box<ExprNode>,
        arguments: Vec<ExprNode>,
    },

    /// Index ex. "vec[idx - 1]"
    Index {
        container: Box<ExprNode>,
        index: Box<ExprNode>
    },

    /// Access by '.' operator
    Access {
        container: Box<ExprNode>,
        name: String
    },

    /// Unary '-'
    Negate (Box<ExprNode>),

    /// Binary operators ex. '+', '-', '*', '/'
    BinOp {
        kind: BinOpKind,
        left: Box<ExprNode>,
        right: Box<ExprNode>
    },

    Conditional {
        condition: Box<ExprNode>,
        true_expression: Box<ExprNode>,
        false_expression: Box<ExprNode>,
    }
}


struct AstParser<T: Iterator<Item = Token>> {
    tokens: T,
    delimiters: Vec<DelimiterKind>,
    allow_comma: bool
}

impl<T: Iterator<Item = Token> + Clone> AstParser<T> {
    fn new(tokens: T) -> Self {
        Self {
            tokens,
            delimiters: vec![],
            allow_comma: false
        }
    }
    
    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn expect(&mut self, tk: Tk) {
        let token = self.advance().expect("Unexpexted end of tokens");
        if token.kind != tk {
            panic!("Expexted token {:?} but got {:?}", tk, token.kind);
        }
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokens.clone().next()
    }
    
    fn expression(&mut self, left_power: OpBindingPower) -> ExprNode {
        let token = self.advance().expect("Unexpected end of tokens");
        
        let mut lhs = match token.kind {
            Tk::OpenParen => self.group_or_tuple(),
            Tk::OpenBracket => self.array_literal(),
            Tk::Minus => {
                let expr = self.expression(OpBindingPower::Unary, );
                ExprNode::Negate(Box::new(expr))
            },
            Tk::Ident(name) => ExprNode::Ident(name),
            Tk::Literal(kind) => self.literal(kind),
            t => todo!("Unexpected prefix: {:?}", t),
        };

        while let Some(token) = self.peek() {
            let node = match token.kind {
                Tk::OpenParen => self.call(lhs),
                Tk::OpenBracket => self.index(lhs),

                // Closing Delimiters, verify and return current parsed expression
                Tk::CloseParen => self.verify_closing_delimiter(DelimiterKind::Parentheses, lhs),
                Tk::CloseBracket => self.verify_closing_delimiter(DelimiterKind::Brackets, lhs),

                Tk::Dot => self.access(lhs),
                Tk::Comma if self.allow_comma => return lhs,

                // Conditional Operator "c ? a : b"
                Tk::Question => self.conditional(lhs),
                Tk::Colon => self.verify_closing_delimiter(DelimiterKind::Conditional, lhs),

                Tk::Plus => self.bin_op(BinOpKind::Add, lhs, left_power),
                Tk::Minus => self.bin_op(BinOpKind::Subtract, lhs, left_power),
                Tk::Star => self.bin_op(BinOpKind::Multiply, lhs, left_power),
                Tk::Slash => self.bin_op(BinOpKind::Divide, lhs, left_power),
                t => todo!("Unexpected op: {:?}", t),
            };

            lhs = match node {
                ControlFlow::Continue(node) => node,
                ControlFlow::Break(node) => return node,
            }
        };

        lhs
    }

    fn literal(&self, kind: LiteralKind) -> ExprNode {
        match kind {
            LiteralKind::Int(n) => ExprNode::Int(n),
            LiteralKind::Float(n) => ExprNode::Float(n),
            LiteralKind::String(s) => ExprNode::String(s),
        }
    }

    fn comma_seperated_exprs(&mut self, closing_delimiter: DelimiterKind) -> Vec<ExprNode> {
        self.delimiters.push(closing_delimiter);
        let mut expressions = Vec::new();
        let prev_allow_comma = self.allow_comma;
        self.allow_comma = true;

        loop {
            match self.peek().map(|tk| tk.kind) {
                Some(Tk::Comma) => { self.advance(); },
                Some(tk) if tk == closing_delimiter.get_closing_token() => {
                    self.advance();
                    break;
                },
                _ => expressions.push(
                    self.expression(OpBindingPower::Grouping)
                ),
            };
        };

        self.allow_comma = prev_allow_comma;
        self.expect_delimiter(closing_delimiter);
        expressions
    }

    fn array_literal(&mut self) -> ExprNode {
        let elements = self.comma_seperated_exprs(DelimiterKind::Brackets);
        ExprNode::Array(elements)
    }

    fn group_or_tuple(&mut self) -> ExprNode {
        let mut elements = self.comma_seperated_exprs(DelimiterKind::Parentheses);

        match elements.len() {
            1 => elements.pop().unwrap(),
            _ => ExprNode::Tuple(elements)
        }
    }

    fn bin_op(&mut self, op_kind: BinOpKind, lhs: ExprNode,
        left_power: OpBindingPower) -> ControlFlow<ExprNode, ExprNode> {
        let right_power = op_kind.get_binding_power();

        if !left_power.can_bind_right(&right_power) {
            return ControlFlow::Break(lhs);
        }

        // Discard the op if we bind it to the expression
        self.advance();
        let rhs = self.expression(right_power);
        
        ControlFlow::Continue(
            ExprNode::BinOp {
                kind: op_kind,
                left: Box::new(lhs),
                right: Box::new(rhs),
            }
        )
    }

    fn conditional(&mut self, condition: ExprNode) -> ControlFlow<ExprNode, ExprNode> {
        self.expect(Tk::Question);
        self.delimiters.push(DelimiterKind::Conditional);

        // '?' and ':' group similar to parenthesis
        let true_expression = self.expression(OpBindingPower::Grouping);

        self.expect(Tk::Colon);
        self.expect_delimiter(DelimiterKind::Conditional);
        
        // Expressions after the ':' are subject to standard bindng rules
        let false_expression = self.expression(OpBindingPower::Conditional);
        
        ControlFlow::Continue(
            ExprNode::Conditional {
                condition: Box::new(condition),
                true_expression: Box::new(true_expression),
                false_expression: Box::new(false_expression)
            }
        )
    }

    fn access(&mut self, container: ExprNode) -> ControlFlow<ExprNode, ExprNode> {
        self.expect(Tk::Dot);

        let name = match self.advance().map(|tk| tk.kind) {
            Some(Tk::Ident(name)) => name,
            Some(t) => panic!("Expected an identifier, got {:?}", t),
            None => panic!("Expected an identifier"),
        };

        ControlFlow::Continue(
            ExprNode::Access {
                container: Box::new(container),
                name: format!("{:?}", name),
            }
        )
    }

    fn index(&mut self, container: ExprNode) -> ControlFlow<ExprNode, ExprNode>{
        self.expect(Tk::OpenBracket);
        let index = self.delimited_expression(DelimiterKind::Brackets);

        self.expect(Tk::CloseBracket);

        ControlFlow::Break(
            ExprNode::Index {
                container: Box::new(container),
                index: Box::new(index)
            }
        )
    }

    fn call(&mut self, function: ExprNode) -> ControlFlow<ExprNode, ExprNode> {
        self.expect(Tk::OpenParen);
        let arguments = self.comma_seperated_exprs(DelimiterKind::Parentheses);
        ControlFlow::Continue(
            ExprNode::Call {
                function: Box::new(function),
                arguments
            }
        )
    }

    fn delimited_expression(&mut self, opening_delimeter: DelimiterKind) -> ExprNode {
        self.delimiters.push(opening_delimeter);
        let expr = self.expression(OpBindingPower::Grouping);
        self.expect_delimiter(opening_delimeter);
        expr
    }

    fn expect_delimiter(&mut self, expected_delimiter: DelimiterKind) {
        match self.delimiters.pop() {
            Some(closing_delimiter) if closing_delimiter == expected_delimiter => (),
            Some(closing_delimiter) => {
                panic!("Expected delimiter '{}' but got '{}'", expected_delimiter.get_closing_token(), closing_delimiter.get_closing_token())
            },
            // Should never happen because we only expect delimiters that we push
            None => {
                panic!("Internal Error: expected delimiter '{}' but got None", expected_delimiter.get_closing_token());
            }
        };
    }

    fn verify_closing_delimiter(&mut self, closing_delimiter: DelimiterKind, lhs: ExprNode) -> ControlFlow<ExprNode, ExprNode> {
        match self.delimiters.last() {
            Some(expected_delimiter) if closing_delimiter == *expected_delimiter => ControlFlow::Break(lhs),

            _ => {
                panic!("Mismatched closing delimiter '{}'", closing_delimiter.get_closing_token());
            }
        }
    }
}


pub fn parse<T>(tokens: T) -> ExprNode
        where T: Iterator<Item = Token> + Clone {
    let mut parser = AstParser::new(tokens);
    parser.expression(OpBindingPower::All)
}
