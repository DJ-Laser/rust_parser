#![allow(dead_code)]
use crate::lexer::TokenKind as Tk;

use super::parser::OpBindingPower;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl BinOpKind {
    pub(super) fn get_binding_power(&self) -> OpBindingPower {
        match self {
            Self::Add | Self::Subtract => OpBindingPower::Sum,
            Self::Multiply | Self::Divide => OpBindingPower::Factor,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DelimiterKind {
    Parentheses,
    Brackets,
    Braces,
    Conditional,
}

impl DelimiterKind {
    pub fn get_opening_token(&self) -> Tk {
        match self {
            Self::Parentheses => Tk::OpenParen,
            Self::Brackets => Tk::OpenBracket,
            Self::Braces => todo!(),
            Self::Conditional => Tk::Question,
        }
    }

    pub fn get_closing_token(&self) -> Tk {
        match self {
            Self::Parentheses => Tk::CloseParen,
            Self::Brackets => Tk::CloseBracket,
            Self::Braces => todo!(),
            Self::Conditional => Tk::Colon,
        }
    }
}

#[derive(Debug)]
pub enum ExprNode<'i> {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(&'i str),
    Array(Vec<ExprNode<'i>>),
    Tuple(Vec<ExprNode<'i>>),

    Ident(String),

    Call {
        function: Box<ExprNode<'i>>,
        arguments: Vec<ExprNode<'i>>,
    },

    /// Index ex. "vec[idx - 1]"
    Index {
        container: Box<ExprNode<'i>>,
        index: Box<ExprNode<'i>>,
    },

    /// Access by '.' operator
    Access {
        container: Box<ExprNode<'i>>,
        name: String,
    },

    /// Unary '-'
    Negate(Box<ExprNode<'i>>),

    /// Binary operators ex. '+', '-', '*', '/'
    BinOp {
        kind: BinOpKind,
        left: Box<ExprNode<'i>>,
        right: Box<ExprNode<'i>>,
    },

    Conditional {
        condition: Box<ExprNode<'i>>,
        true_expression: Box<ExprNode<'i>>,
        false_expression: Box<ExprNode<'i>>,
    },

    Error,

    Hello {
        a: usize,
    },

    Goodbye(usize),
}
