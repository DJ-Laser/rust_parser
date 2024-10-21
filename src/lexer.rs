use std::str::Chars;

use self::TokenKind as Tk;

#[derive(Debug)]
pub enum LiteralKind {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug)]
pub enum TokenKind {
    /// A number or string
    Literal(LiteralKind),
    
    /// A variable or name
    Ident(String),

    Else,
    Fn,
    For,
    If,
    Let,
    Return,
    While,

    /// '.'
    Dot,
    
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

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
}

impl Token {
    fn new(kind:TokenKind, line: usize) -> Self {
        Self { kind, line }
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    program: Chars<'a>,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(program: &'a str) -> Self {
        let program = program.chars();
        Self {
            program, line: 1
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

    pub fn advance_token(&mut self) -> Option<Token> {
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
                // 
                self.line += 1;
            }
        }
    }
    
    fn number(&mut self, first_digit: char) -> TokenKind {     
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
    
    fn string(&mut self) -> TokenKind {
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
        
        Tk::Literal(LiteralKind::String(text))
    }

    fn ident_or_keyword(&mut self, first_char: char) -> TokenKind {
        let mut ident = String::from(first_char);

        // Match all keywords
        match first_char {
            'f' => {
                if let Some(c) = self.peek() {
                    self.advance();
                    ident.push(c);
                    match c {
                        'a' if self.match_keyword_suffix("lse", &mut ident)
                            => return Tk::Literal(LiteralKind::Bool(false)),
                        'n' if self.match_keyword_suffix("", &mut ident)
                            => return Tk::Fn,
                        'o' if self.match_keyword_suffix("r", &mut ident)
                            => return Tk::For,
                        
                        _ => (),
                    };
                }
            }
            
            'e' if self.match_keyword_suffix("lse", &mut ident)
                => return Tk::Else,
            'i' if self.match_keyword_suffix("f", &mut ident)
                => return Tk::If,
            'l' if self.match_keyword_suffix("et", &mut ident)
                => return Tk::Let,
            'r' if self.match_keyword_suffix("eturn", &mut ident)
                => return Tk::Return,
            't' if self.match_keyword_suffix("rue", &mut ident)
                => return Tk::Literal(LiteralKind::Bool(true)),
            'w' if self.match_keyword_suffix("hile", &mut ident)
                => return Tk::While,
            
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

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.advance_token()
    }
}

fn is_alphanumeric(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='A' | '0'..='9' | '_')
}

fn is_some_alphanumeric(c: Option<char>) -> bool {
    c.map_or(false, is_alphanumeric)
}
