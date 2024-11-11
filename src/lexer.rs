use std::str::Chars;
use std::fmt::{Display, Formatter};

use crate::interner::Interner;

use self::TokenKind as Tk;

#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum LiteralKind<'i> {
    Int(i64),
    Float(f64),
    String(&'i str),
}

impl<'i> Display for LiteralKind<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(num) => write!(f, "{}", num),
            Self::Float(num) => write!(f, "{}", num),
            Self::String(string) => write!(f, "{:?}", string),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Keyword {
    Else,
    False,
    Fn,
    For,
    If,
    Let,
    Return,
    True,
    While,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Else => write!(f, "else"),
            Self::False => write!(f, "false"),
            Self::Fn => write!(f, "fn"),
            Self::For => write!(f, "for"),
            Self::If => write!(f, "if"),
            Self::Let => write!(f, "let"),
            Self::Return => write!(f, "return"),
            Self::True => write!(f, "true"),
            Self::While => write!(f, "while")
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum TokenKind<'i> {
    /// A number or string
    Literal(LiteralKind<'i>),
    
    /// A variable or name
    Ident(String),

    // A language keyword
    Keyword(Keyword),

    /// '.'
    Dot,

    /// ','
    Comma,

    /// '?'
    Question,

    /// '.'
    Colon,
    
    /// '+'
    Plus,
    /// '-'
    Minus,
    /// '*'
    Star,
    /// '/'
    Slash,
    
    /// '('
    OpenParen,
    /// ')'
    CloseParen,
    
    /// '['
    OpenBracket,
    /// ']'
    CloseBracket,
}

impl<'i> Display for TokenKind<'i> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(kind) => write!(f, "{}", kind),
            Self::Keyword(keyword) => write!(f, "{}", keyword),
            Self::Ident(name) => write!(f, "{}", name),
            Self::Dot => write!(f, "."),
            Self::Comma => write!(f, ","),
            Self::Question => write!(f, "?"),
            Self::Colon => write!(f, ":"),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, ")"),
            Self::OpenBracket => write!(f, "["),
            Self::CloseBracket => write!(f, "]"),
        }
    }
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Token<'i> {
    pub kind: TokenKind<'i>,
    pub line: usize,
}

impl<'i> Token<'i> {
    fn new(kind: TokenKind<'i>, line: usize) -> Self {
        Self { kind, line }
    }
}

pub struct Lexer<'prgm, 'i, I: Interner<String>> {
    program: Chars<'prgm>,
    interner: &'i I,
    line: usize,
}

impl<'prgm, 'i, I: Interner<String>> Lexer<'prgm, 'i, I> {
    pub fn new(program: &'prgm str, interner: &'i mut I) -> Self {
        let program = program.chars();
        Self {
            program, interner, line: 1
        }
    }
    
    fn advance(&mut self) -> Option<char> {
        self.program.next()
    }
    
    fn peek(&mut self) -> Option<char> {
        self.program.clone().next()
    }
    
    fn peek_next(&mut self) -> Option<char> {
        self.program.clone().nth(1)
    }

    pub fn advance_token(&mut self) -> Option<Token<'i>> {
        self.whitespace();
        
        let token = match self.advance()? {
            '"' => self.string(),
            digit @ '0'..='9' => self.number(digit),

            c @ ('a'..='z' | 'A'..='Z') => self.ident_or_keyword(c),

            '/' => match self.peek() {
                Some('/') => todo!("Comments"),
                Some('*') => todo!("Block Comments"),
                _ => Tk::Slash,
            },

            '.' => Tk::Dot,
            ',' => Tk::Comma,

            '?' => Tk::Question,
            ':' => Tk::Colon,
            
            '+' => Tk::Plus,
            '-' => Tk::Minus,
            '*' => Tk::Star,

            '(' => Tk::OpenParen,
            ')' => Tk::CloseParen,
            '[' => Tk::OpenBracket,
            ']' => Tk::CloseBracket,
            
            c => panic!("Unexpected symbol: {}", c),
        };
        
        Some(Token::new(token, self.line))
    }
    
    fn whitespace(&mut self) {
        while self.peek().map_or(false, char::is_whitespace) {
            // Advance to skip whitespace
            if matches!(self.advance(), Some('\n')) {
                self.line += 1;
            }
        }
    }
    
    fn number(&mut self, first_digit: char) -> TokenKind<'i> {     
        let mut digits = first_digit.to_string();
        
        self.int_digits(&mut digits);
        match self.peek() {
            Some('.') if matches!(self.peek_next(), Some('0'..='9')) => {
                self.advance();
                digits.push('.');
            },

            _ => {
                let num: i64 = digits.parse().expect("Lexer should have validated the int");
                return Tk::Literal(LiteralKind::Int(num));
            }
        }
        self.int_digits(&mut digits);
        
        let num: f64 = digits.parse().expect("Lexer should have validated the float");
        Tk::Literal(LiteralKind::Float(num))
    }

    fn int_digits(&mut self, digits: &mut String) {
        loop {
            match self.peek() {
                Some(c @ '0'..='9') => {
                    self.advance();
                    digits.push(c);
                },

                _ => break,
            }
        }
    }
    
    fn string(&mut self) -> TokenKind<'i> {
        let mut text = String::new();
        
        loop {
            match self.advance() {
                Some('"') => break,
                // if let guards aren't stabilized :(
                   Some('\\') if self.peek().is_some() => {
                    let c = self.advance().expect("checked is_some() on peek()");
                    text.push(c);
                },
                // increment the line counter in multiline strings
                Some(c @ '\n') => {
                    self.line += 1;
                    text.push(c)
                }
                Some(c) => text.push(c),
                // EOF or escaped EOF
                _ => panic!("Unterminated string"),
            }
        }
        
        let interned = self.interner.intern(text);
        Tk::Literal(LiteralKind::String(interned))
    }

    fn ident_or_keyword(&mut self, first_char: char) -> TokenKind<'i> {
        let mut ident = String::from(first_char);

        // Match all keywords
        match first_char {
            'f' => {
                if let Some(c) = self.peek() {
                    self.advance();
                    ident.push(c);
                    match c {
                        'a' if self.match_keyword_suffix("lse", &mut ident)
                            => return Tk::Keyword(Keyword::False),
                        'n' if self.match_keyword_suffix("", &mut ident)
                            => return Tk::Keyword(Keyword::Fn),
                        'o' if self.match_keyword_suffix("r", &mut ident)
                            => return Tk::Keyword(Keyword::For),
                        
                        _ => (),
                    };
                }
            }
            
            'e' if self.match_keyword_suffix("lse", &mut ident)
                => return Tk::Keyword(Keyword::Else),
            'i' if self.match_keyword_suffix("f", &mut ident)
                => return Tk::Keyword(Keyword::If),
            'l' if self.match_keyword_suffix("et", &mut ident)
                => return Tk::Keyword(Keyword::Let),
            'r' if self.match_keyword_suffix("eturn", &mut ident)
                => return Tk::Keyword(Keyword::Return),
            't' if self.match_keyword_suffix("rue", &mut ident)
                => return Tk::Keyword(Keyword::True),
            'w' if self.match_keyword_suffix("hile", &mut ident)
                => return Tk::Keyword(Keyword::While),
            
            _ => (),
        };

        while let Some(c) = self.peek() {
            if !is_alphanumeric(c) {
                break
            };
            
            self.advance();
            ident.push(c);
        }
        
        Tk::Ident(ident)
    }

    fn match_keyword_suffix(&mut self, suffix_chars: &str, ident: &mut String) -> bool {
        for expected in suffix_chars.chars() {
            if self.peek() == Some(expected) {
                self.advance();
                ident.push(expected);
            } else {
                return false;
            }
        }

        !is_some_alphanumeric(self.peek())
    }
}

impl<'prgm, 'i, I: Interner<String>> Iterator for Lexer<'prgm, 'i, I> {
    type Item = Token<'i>;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance_token()
    }
}

impl<'prgm, 'i, I: Interner<String>> Clone for Lexer<'prgm, 'i, I> {
    fn clone(&self) -> Self {
        Self {
            program: self.program.clone(),
            interner: self.interner,
            line: self.line
        }
    }
}

fn is_alphanumeric(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='A' | '0'..='9' | '_')
}

fn is_some_alphanumeric(c: Option<char>) -> bool {
    c.map_or(false, is_alphanumeric)
}
