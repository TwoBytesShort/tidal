use std::fmt;

#[derive(Clone, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Semicolon,
    // Operations
    Minus,
    Plus,
    Star,
    Slash,
    Modulus,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    NotEqual,
    // Keywords.
    And,
    Break,
    Do,
    Else,
    Elseif,
    End,
    False,
    For,
    Function,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,
    Whitespace,
    EOF,
    Comment(String),
    // Literals.
    Identifier(String),
    String(String),
    Number(f64),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Minus => write!(f, "-"),
            Token::Plus => write!(f, "+"),
            Token::Semicolon => write!(f, ";"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Modulus => write!(f, "%"),
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::NotEqual => write!(f, "~="),
            Token::And => write!(f, "and"),
            Token::Break => write!(f, "break"),
            Token::Do => write!(f, "do"),
            Token::Else => write!(f, "else"),
            Token::Elseif => write!(f, "elseif"),
            Token::End => write!(f, "end"),
            Token::False => write!(f, "false"),
            Token::For => write!(f, "for"),
            Token::Function => write!(f, "function"),
            Token::If => write!(f, "if"),
            Token::In => write!(f, "in"),
            Token::Local => write!(f, "local"),
            Token::Nil => write!(f, "nil"),
            Token::Not => write!(f, "not"),
            Token::Or => write!(f, "or"),
            Token::Repeat => write!(f, "repeat"),
            Token::Return => write!(f, "return"),
            Token::Then => write!(f, "then"),
            Token::True => write!(f, "true"),
            Token::Until => write!(f, "until"),
            Token::While => write!(f, "while"),
            Token::EOF => write!(f, "<EOF>"),
            Token::Identifier(i) => write!(f, "{}", i),
            Token::String(s) => write!(f, "{}", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::Whitespace => write!(f, "<WHITESPACE>"),
            Token::Comment(c) => write!(f, "<COMMENT> {}", c),
        }
    }
}

struct SourceIter<'a> {
    iter: std::str::Chars<'a>,
    pub current: Option<char>,
    pub next: Option<char>,
    pub line: u32,
    pub column: u32,
}

impl SourceIter<'_> {
    pub fn new(source: &String) -> SourceIter {
        let mut itr = source.chars();
        let current = itr.next();
        let next = itr.next();

        SourceIter {
            iter: itr,
            current: current,
            next: next,
            line: 1,
            column: 1,
        }
    }

    pub fn advance(&mut self) -> bool {
        if let Some(c) = self.current {
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }

        self.current = self.next;
        self.next = self.iter.next();

        self.current != None
    }

    fn advance_if_next(&mut self, expected: fn(char) -> bool) -> bool {
        match self.next {
            Some(c) => {
                if expected(c) {
                    self.advance();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn advance_if_current_and_next(
        &mut self,
        expected_current: fn(char) -> bool,
        expected_next: fn(char) -> bool,
    ) -> bool {
        match self.current {
            Some(c) => expected_current(c) && self.advance_if_next(expected_next),
            None => false,
        }
    }
}

pub fn scan_tokens(source: String) -> Result<Vec<Token>, &'static str> {
    let mut source_iterator = SourceIter::new(&source);
    let mut tokens: Vec<Token> = Vec::new();

    loop {
        match scan_token(&mut source_iterator) {
            Ok(Token::EOF) => {
                tokens.push(Token::EOF);
                return Ok(tokens);
            }
            Ok(Token::Whitespace) => (),
            Ok(Token::Comment(comment)) => print!("{}", comment),
            Err(e) => return Err(e),
            Ok(t) => tokens.push(t),
        }

        source_iterator.advance();
    }
}

fn scan_token(source_iterator: &mut SourceIter) -> Result<Token, &'static str> {
    let c = match source_iterator.current {
        Some(c) => c,
        None => return Ok(Token::EOF),
    };

    return match c {
        '(' => Ok(Token::LeftParen),
        ')' => Ok(Token::RightParen),
        '{' => Ok(Token::LeftBrace),
        '}' => Ok(Token::RightBrace),
        ',' => Ok(Token::Comma),
        '.' => Ok(Token::Dot),
        '-' => {
            if source_iterator.advance_if_next(|c| c == '-') {
                scan_comment(source_iterator)
            } else {
                Ok(Token::Minus)
            }
        }
        '+' => Ok(Token::Plus),
        ';' => Ok(Token::Semicolon),
        '*' => Ok(Token::Star),
        '/' => Ok(Token::Slash),
        '%' => Ok(Token::Modulus),
        '~' => {
            if source_iterator.advance_if_next(|c| c == '=') {
                Ok(Token::NotEqual)
            } else {
                return Err("Unexptected token");
            }
        }
        '=' => {
            if source_iterator.advance_if_next(|c| c == '=') {
                Ok(Token::EqualEqual)
            } else {
                Ok(Token::Equal)
            }
        }
        '<' => {
            if source_iterator.advance_if_next(|c| c == '=') {
                Ok(Token::LessEqual)
            } else {
                Ok(Token::Less)
            }
        }
        '>' => {
            if source_iterator.advance_if_next(|c| c == '=') {
                Ok(Token::GreaterEqual)
            } else {
                Ok(Token::Greater)
            }
        }
        '"' => scan_string(source_iterator, '"'),
        '\'' => scan_string(source_iterator, '\''),
        '[' => {
            if source_iterator.advance_if_next(|c| c == '[') {
                scan_multiline_string(source_iterator)
            } else {
                Err("Unexpected token")
            }
        }
        c => {
            if c.is_ascii_whitespace() {
                Ok(Token::Whitespace)
            } else if c.is_ascii_digit() {
                scan_number(source_iterator)
            } else if c.is_ascii_alphabetic() || c == '_' {
                scan_identifier(source_iterator)
            } else {
                Err("Unrecognised character")
            }
        }
    };
}

fn scan_comment(source_iterator: &mut SourceIter) -> Result<Token, &'static str> {
    let mut multiline = false;
    let mut comment = String::new();

    // #current=- #next=-

    if let Some(n1) = source_iterator.next {
        if n1 == '\n' {
            return Ok(Token::Comment(comment));
        }

        source_iterator.advance();

        // #current=- #next=comment char or [ or newline

        if let Some(n2) = source_iterator.next {
            if n2 == '\n' {
                return Ok(Token::Comment(comment));
            }

            if n1 == '[' && n2 == '[' {
                multiline = true;
                source_iterator.advance();
                source_iterator.advance();
            }

        // #current=- or [ #next=comment char or newline
        } else {
            return Ok(Token::Comment(comment));
        }
    } else {
        return Ok(Token::Comment(comment));
    }

    loop {
        if multiline {
            if source_iterator.advance_if_current_and_next(|c| c == ']', |c| c == ']') {
                return Ok(Token::Comment(comment));
            }
        }

        if let Some(c) = source_iterator.current {
            comment.push(c);
        } else {
            return Ok(Token::Comment(comment));
        }
        if let Some(n) = source_iterator.next {
            if !multiline && n == '\n' {
                return Ok(Token::Comment(comment));
            }
        } else {
            return Ok(Token::Comment(comment));
        }

        source_iterator.advance();

        // #current=commnet char #next=comment char or newline
    }
}

fn scan_string(source_iterator: &mut SourceIter, terminator: char) -> Result<Token, &'static str> {
    let mut string = String::new();

    //TODO Escaped characters

    loop {
        if let Some(n) = source_iterator.next {
            if n == '\n' {
                // EROROR
                return Err("Reached end of line to soon");
            }

            source_iterator.advance();

            if n == terminator {
                return Ok(Token::String(string));
            } else {
                string.push(n);
            }
        } else {
            // EROROR
            return Err("Reached end of file too soon");
        }
    }
}

fn scan_multiline_string(source_iterator: &mut SourceIter) -> Result<Token, &'static str> {
    let mut string = String::new();
    loop {
        source_iterator.advance();

        if source_iterator.advance_if_current_and_next(|c| c == ']', |c| c == ']') {
            return Ok(Token::String(string));
        }

        if let Some(c) = source_iterator.current {
            string.push(c);
        } else {
            // ERROR
            return Err("Reached end of file too soon");
        }
    }
}

fn scan_number(source_iterator: &mut SourceIter) -> Result<Token, &'static str> {
    let mut number = String::new();

    loop {
        if let Some(c) = source_iterator.current {
            number.push(c);
        }

        if !source_iterator.advance_if_next(|c| c.is_ascii_digit()) {
            break;
        }
    }

    if source_iterator.advance_if_next(|c| c == '.') {
        number.push('.');
    }

    loop {
        if !source_iterator.advance_if_next(|c| c.is_ascii_digit()) {
            break;
        }

        if let Some(c) = source_iterator.current {
            number.push(c);
        }
    }

    match number.parse::<f64>() {
        Ok(n) => Ok(Token::Number(n)),
        Err(_) => Err("Could not pass number"),
    }
}

fn scan_identifier(source_iterator: &mut SourceIter) -> Result<Token, &'static str> {
    let mut identifier = String::new();

    loop {
        if let Some(c) = source_iterator.current {
            identifier.push(c);
        }

        if !source_iterator.advance_if_next(|c| c.is_ascii_alphanumeric() || c == '_') {
            break;
        }
    }

    Ok(match identifier.as_str() {
        "and" => Token::And,
        "break" => Token::Break,
        "do" => Token::Do,
        "else" => Token::Else,
        "elseif" => Token::Elseif,
        "end" => Token::End,
        "false" => Token::False,
        "for" => Token::For,
        "function" => Token::Function,
        "if" => Token::If,
        "in" => Token::In,
        "local" => Token::Local,
        "nil" => Token::Nil,
        "not" => Token::Not,
        "or" => Token::Or,
        "repeat" => Token::Repeat,
        "return" => Token::Return,
        "then" => Token::Then,
        "true" => Token::True,
        "unitl" => Token::Until,
        "while" => Token::While,
        _ => Token::Identifier(identifier),
    })
}
