use crate::scanner::Token;
use std::fmt;

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
    Divide,
    Modulus,
}

impl fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperation::Equal => write!(f, "="),
            BinaryOperation::NotEqual => write!(f, "!="),
            BinaryOperation::Less => write!(f, "<"),
            BinaryOperation::LessEqual => write!(f, "<="),
            BinaryOperation::Greater => write!(f, ">"),
            BinaryOperation::GreaterEqual => write!(f, ">="),
            BinaryOperation::Add => write!(f, "+"),
            BinaryOperation::Subtract => write!(f, "-"),
            BinaryOperation::Multiple => write!(f, "*"),
            BinaryOperation::Divide => write!(f, "/"),
            BinaryOperation::Modulus => write!(f, "%"),
        }
    }
}

impl AsToken for BinaryOperation {
    fn token(&self) -> Token {
        match *self {
            BinaryOperation::Add => Token::Plus,
            BinaryOperation::Divide => Token::Slash,
            BinaryOperation::Equal => Token::EqualEqual,
            BinaryOperation::Greater => Token::Greater,
            BinaryOperation::GreaterEqual => Token::GreaterEqual,
            BinaryOperation::Less => Token::Less,
            BinaryOperation::LessEqual => Token::LessEqual,
            BinaryOperation::Modulus => Token::Modulus,
            BinaryOperation::Multiple => Token::Star,
            BinaryOperation::NotEqual => Token::NotEqual,
            BinaryOperation::Subtract => Token::Minus,
        }
    }
}

pub enum UniaryOperation {
    Negative,
    Not,
}

impl fmt::Display for UniaryOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UniaryOperation::Negative => write!(f, "-"),
            UniaryOperation::Not => write!(f, "!"),
        }
    }
}

impl AsToken for UniaryOperation {
    fn token(&self) -> Token {
        match *self {
            UniaryOperation::Negative => Token::Minus,
            UniaryOperation::Not => Token::Not,
        }
    }
}

pub enum Scope {
    Local,
    Global,
}

impl fmt::Display for Scope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Scope::Local => write!(f, "local"),
            Scope::Global => write!(f, "global"),
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
    Variable(String),
    Grouping(Box<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Number(n) => write!(f, "{}", n),
            Expression::String(s) => write!(f, "{}", s),
            Expression::Boolean(b) => write!(f, "{}", b),
            Expression::Nil => write!(f, "Nil"),
            Expression::Binary(opp, left, write) => write!(f, "( {} {} {} )", opp, left, write),
            Expression::Unary(opp, expr) => write!(f, "( {} {} )", opp, expr),
            Expression::Variable(name) => write!(f, "( variable {} )", name),
            Expression::Grouping(expr) => write!(f, "( group {} )", expr),
        }
    }
}

pub enum Statement {
    Expr(Expression),
    Print(Expression),
    Decl(Scope, String, Option<Expression>),
    Asn(String, Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expr(expr) => write!(f, "{}", expr),
            Statement::Print(expr) => write!(f, "print ( {} )", expr),
            Statement::Decl(scope, name, None) => write!(f, "{} {}", scope, name),
            Statement::Decl(scope, name, Some(expr)) => write!(f, "{} {} = {}", scope, name, expr),
            Statement::Asn(name, expr) => write!(f, "{} = {}", name, expr),
        }
    }
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
            current: current,
        }
    }

    pub fn advance(&mut self) -> bool {
        self.current = self.iter.next();
        self.current.is_some()
    }

    pub fn match_from_options<T: AsToken>(&mut self, options: Vec<T>) -> Option<T> {
        match self.current {
            Some(current_token) => {
                for option in options {
                    if *current_token == option.token() {
                        self.advance();
                        return Some(option);
                    }
                }
                None
            }
            None => None,
        }
    }

    pub fn match_token(&mut self, token: Token) -> bool {
        match self.current {
            Some(current_token) => {
                if *current_token == token {
                    self.advance();
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }
}

pub fn parse(tokens: &Vec<Token>) -> Result<Vec<Statement>, &'static str> {
    let mut token_iterator = TokenIter::new(&tokens);

    let mut statements = Vec::new();

    loop {
        match token_iterator.current {
            Some(Token::EOF) => return Ok(statements),
            Some(_) => {
                statements.push(declaration(&mut token_iterator)?);
            }
            None => return Err("Unexpected end of file"),
        }
    }
}

fn declaration(token_iterator: &mut TokenIter) -> Result<Statement, &'static str> {
    match token_iterator.current {
        Some(token) => match token {
            Token::Local => {
                token_iterator.advance();
                variable_declaration(token_iterator)
            },
            _ => statement(token_iterator),
        },
        None => Err("Nothing to pass"),
    }
}

fn variable_declaration(token_iterator: &mut TokenIter) -> Result<Statement, &'static str> {
    let name = match token_iterator.current {
        Some(Token::Identifier(identifier)) => identifier,
        _ => return Err("Expected an identifier")
    };

    token_iterator.advance();

    match token_iterator.match_token(Token::Equal)
    {
        true => {
            let expr = expression(token_iterator)?;
            Ok(Statement::Decl(Scope::Local, name.clone(), Some(expr)))
        },
        false => Ok(Statement::Decl(Scope::Local, name.clone(), None))
    }
}

fn statement(token_iterator: &mut TokenIter) -> Result<Statement, &'static str> {
    match token_iterator.current {
        Some(token) => match token {
            Token::Identifier(ident) => match ident.as_str() {
                "print" => {
                    token_iterator.advance();
                    print_statement(token_iterator)
                }
                _ => Err("Unrecognised ident"),
            },
            _ => expression_statement(token_iterator),
        },
        None => Err("Nothing to pass"),
    }
}

fn expression_statement(token_iterator: &mut TokenIter) -> Result<Statement, &'static str> {
    let expr = expression(token_iterator)?;

    Ok(Statement::Expr(expr))
}

fn print_statement(token_iterator: &mut TokenIter) -> Result<Statement, &'static str> {
    if !token_iterator.match_token(Token::LeftParen) {
        return Err("Expected '('");
    }

    let expr = equality(token_iterator)?;

    if !token_iterator.match_token(Token::RightParen) {
        return Err("Expected ')'");
    }

    Ok(Statement::Print(expr))
}

fn expression(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    equality(token_iterator)
}

fn equality(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let mut expression = comparison(token_iterator)?;

    while let Some(operator) =
        token_iterator.match_from_options(vec![BinaryOperation::NotEqual, BinaryOperation::Equal])
    {
        let right = comparison(token_iterator)?;
        expression = Expression::Binary(operator, Box::new(expression), Box::new(right));
    }
    Ok(expression)
}

fn comparison(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let mut expression = term(token_iterator)?;

    while let Some(operator) = token_iterator.match_from_options(vec![
        BinaryOperation::Greater,
        BinaryOperation::GreaterEqual,
        BinaryOperation::Less,
        BinaryOperation::LessEqual,
    ]) {
        let right = term(token_iterator)?;
        expression = Expression::Binary(operator, Box::new(expression), Box::new(right));
    }
    Ok(expression)
}

fn term(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let mut expression = factor(token_iterator)?;

    while let Some(operator) =
        token_iterator.match_from_options(vec![BinaryOperation::Subtract, BinaryOperation::Add])
    {
        let right = factor(token_iterator)?;
        expression = Expression::Binary(operator, Box::new(expression), Box::new(right));
    }
    Ok(expression)
}

fn factor(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let mut expression = unary(token_iterator)?;

    while let Some(operator) = token_iterator.match_from_options(vec![
        BinaryOperation::Divide,
        BinaryOperation::Multiple,
        BinaryOperation::Modulus,
    ]) {
        let right = unary(token_iterator)?;
        expression = Expression::Binary(operator, Box::new(expression), Box::new(right));
    }
    Ok(expression)
}

fn unary(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    if let Some(operator) =
        token_iterator.match_from_options(vec![UniaryOperation::Not, UniaryOperation::Negative])
    {
        let right = unary(token_iterator)?;
        return Ok(Expression::Unary(operator, Box::new(right)));
    }
    return primary(token_iterator);
}

fn primary(token_iterator: &mut TokenIter) -> Result<Expression, &'static str> {
    let result = match token_iterator.current {
        Some(Token::False) => Ok(Expression::Boolean(false)),
        Some(Token::True) => Ok(Expression::Boolean(true)),
        Some(Token::Nil) => Ok(Expression::Nil),
        Some(Token::Number(n)) => Ok(Expression::Number(*n)),
        Some(Token::String(s)) => Ok(Expression::String(s.clone())),
        Some(Token::Identifier(i)) => Ok(Expression::Variable(i.clone())),
        Some(Token::LeftParen) => {
            token_iterator.advance();
            let expression = expression(token_iterator)?;

            if token_iterator.current == Some(&Token::RightParen) {
                Ok(Expression::Grouping(Box::new(expression)))
            } else {
                Err("Expected right parenthersis")
            }
        }
        _ => Err("Missing expression"),
    };

    if result.is_ok() {
        token_iterator.advance();
    }

    result
}
