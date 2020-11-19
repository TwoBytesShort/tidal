use crate::scanner::Token;

trait AsToken {
    fn token(&self) -> Token;
}

pub enum BinaryOperation {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Subtract,
    Multiple,
    Devide,
    Modulus
}

impl AsToken for BinaryOperation {
    fn token(&self) -> Token {
       match *self {
           BinaryOperation::Add => Token::Plus,
           BinaryOperation::Devide => Token::Slash,
           BinaryOperation::Equal => Token::EqualEqual,
           BinaryOperation::Greater => Token::Greater,
           BinaryOperation::GreaterEqual => Token::GreaterEqual,
           BinaryOperation::Less => Token::Less,
           BinaryOperation::LessEqual => Token::LessEqual,
           BinaryOperation::Modulus => Token::Modulus,
           BinaryOperation::Multiple => Token::Star,
           BinaryOperation::NotEqual => Token::NotEqual,
           BinaryOperation::Subtract => Token::Minus
       }
    }
}

pub enum UniaryOperation {
    Negative,
    Not
}

impl AsToken for UniaryOperation {
    fn token(&self) -> Token {
       match *self {
        UniaryOperation::Negative => Token::Minus,
        UniaryOperation::Not => Token::Not
       }
    }
}

pub enum Expression {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
    
    Binary(BinaryOperation, Box<Expression>, Box<Expression>),
    Unary(UniaryOperation, Box<Expression>),
    Grouping(Box<Expression>),
}

struct TokenIter<'b> {
    iter: std::slice::Iter<'b, Token>,
    pub current: Option<&'b Token>,
}

impl TokenIter<'_> {
    pub fn new(tokens: &Vec<Token>) -> TokenIter {
        let mut itr = tokens.iter();
        let current = itr.next();

        TokenIter {
            iter: itr,
            current: current
        }
    }

    pub fn advance(&mut self) -> bool {
        self.current = self.iter.next();
        self.current.is_some()
    }

    pub fn match_from_options<T: AsToken>(&mut self, options: Vec<T>) -> Option<T> {
        match self.current {
            Some(token) => {
                for option in options  {
                    if *token == option.token() {
                        self.advance();
                        return Some(option)
                    }
                }
                None
            },
            None => None
        }
    }
}

fn expression(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    equality(token_iterator)
}

fn equality(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let mut expression = comparison(token_iterator)?;

    while let Some(operator) = token_iterator.match_from_options(vec!(BinaryOperation::NotEqual, BinaryOperation::Equal)) {
        let right = comparison(token_iterator)?;
        expression = Expression::Binary(operator, Box::new(expression), Box::new(right));
    }
    
    Ok(expression)
}

fn comparison(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let mut expression = term(token_iterator)?;

    while let Some(operator) = token_iterator.match_from_options(vec!(BinaryOperation::Greater, BinaryOperation::GreaterEqual, BinaryOperation::Less, BinaryOperation::LessEqual)) {
        let right = term(token_iterator)?;
        expression = Expression::Binary(operator, Box::new(expression), Box::new(right));
    }
    
    Ok(expression)
}

fn term(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let mut expression = factor(token_iterator)?;

    while let Some(operator) = token_iterator.match_from_options(vec!(BinaryOperation::Subtract, BinaryOperation::Add)) {
        let right = factor(token_iterator)?;
        expression = Expression::Binary(operator, Box::new(expression), Box::new(right));
    }
    
    Ok(expression)
}

fn factor(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let mut expression = unary(token_iterator)?;

    while let Some(operator) = token_iterator.match_from_options(vec!(BinaryOperation::Devide, BinaryOperation::Multiple)) {
        let right = unary(token_iterator)?;
        expression = Expression::Binary(operator, Box::new(expression), Box::new(right));
    }
    
    Ok(expression)
}

fn unary(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    if let Some(operator) = token_iterator.match_from_options(vec!(UniaryOperation::Not, UniaryOperation::Negative)) {
        let right = unary(token_iterator)?;
        return Ok(Expression::Unary(operator, Box::new(right)));
    }
    
    return primary(token_iterator);
}

fn primary(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    match token_iterator.current {
        Some(Token::False) => Ok(Expression::Boolean(false)),
        Some(Token::True) => Ok(Expression::Boolean(true)),
        Some(Token::Nil) => Ok(Expression::Nil),
        Some(Token::Number(n)) => Ok(Expression::Number(*n)),
        Some(Token::String(s)) => Ok(Expression::String(s.clone())),
        _ => Err("Missing expression")
    }
}