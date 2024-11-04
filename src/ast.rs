#![allow(dead_code)]

use std::cmp::Ordering;
use std::ops::ControlFlow;

use crate::lexer::{Token, TokenKind as Tk, LiteralKind};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum OpBindingPower {
    All,
    Grouping,

    Conditional,

    Sum,
    Factor,

    Unary,
    Access,
    Index,
    
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

#[derive(Debug)]
pub enum ExprNode {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),

    Ident(String),

    /// Index ex. "vec[idx - 1]"
    Index {
        container: Box<ExprNode>,
        index: Box<ExprNode>
    },

    /// Access by '.' operator
    Access {
        object: Box<ExprNode>,
        name: Box<ExprNode>
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
        true_value: Box<ExprNode>,
        flase_value: Box<ExprNode>,
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
    fn get_opening_char(&self) -> char {
        match self {
            Self::Parentheses => '(',
            Self::Brackets => '[',
            Self::Braces => '{',
            Self::Conditional => '?'
        }
    }

    fn get_closing_char(&self) -> char {
        match self {
            Self::Parentheses => ')',
            Self::Brackets => ']',
            Self::Braces => '}',
            Self::Conditional => ':'
        }
    }
}

struct AstParser<T: Iterator<Item = Token>> {
    tokens: T,
    delimiters: Vec<DelimiterKind>
}

impl<T: Iterator<Item = Token> + Clone> AstParser<T> {
    fn new(tokens: T) -> Self {
        Self {
            tokens,
            delimiters: vec![]
        }
    }
    
    fn advance(&mut self) -> Option<Token> {
        self.tokens.next()
    }

    fn peek(&mut self) -> Option<Token> {
        self.tokens.clone().next()
    }
    
    fn expression(&mut self, left_power: OpBindingPower) -> ExprNode {
        let token = self.advance().expect("Unexpected end of tokens");
        
        let mut lhs = match token.kind {
            Tk::OpenParen => self.delimited_expression(DelimiterKind::Parentheses),
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
                Tk::OpenBracket => self.index(lhs),

                // Closing Delimiters, verify and return current parsed expression
                Tk::CloseParen => self.verify_closing_delimiter(DelimiterKind::Parentheses, lhs),
                Tk::CloseBracket => self.verify_closing_delimiter(DelimiterKind::Brackets, lhs),

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
            LiteralKind::Bool(b) => ExprNode::Bool(b),
            LiteralKind::String(s) => ExprNode::String(s),
        }
    }

    fn bin_op(&mut self, op_kind: BinOpKind, lhs: ExprNode,
        left_power: OpBindingPower) -> ControlFlow<ExprNode, ExprNode> {
        let right_power = op_kind.get_binding_power();

        if !left_power.can_bind_right(&right_power) {
            return ControlFlow::Break(lhs);
        }

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

    fn index(&mut self, container: ExprNode) -> ControlFlow<ExprNode, ExprNode>{
        self.advance();

        let index = self.delimited_expression(DelimiterKind::Brackets);
        ControlFlow::Break(
            ExprNode::Index {
                container: Box::new(container),
                index: Box::new(index)
            }
        )
    }

    fn delimited_expression(&mut self, opening_delimeter: DelimiterKind) -> ExprNode {
        self.delimiters.push(opening_delimeter);
        self.expression(OpBindingPower::Grouping)
    }

    fn verify_closing_delimiter(&mut self, closing_delimiter: DelimiterKind, lhs: ExprNode) -> ControlFlow<ExprNode, ExprNode> {
        self.advance();

        match self.delimiters.pop() {
            Some(expected_delimiter) if closing_delimiter == expected_delimiter => ControlFlow::Break(lhs),

            _ => {
                panic!("Mismatched closing delimiter '{}'", closing_delimiter.get_closing_char());
            }
        }
    }
}


pub fn parse<T>(tokens: T) -> ExprNode
        where T: Iterator<Item = Token> + Clone {
    let mut parser = AstParser::new(tokens);
    parser.expression(OpBindingPower::All)
}
