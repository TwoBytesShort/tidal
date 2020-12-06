pub use crate::parser::BinaryOperation;
pub use crate::parser::Expression;
pub use crate::parser::UniaryOperation;
use std::fmt;

pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(number) => write!(f, "{}", number),
            Value::String(string) => write!(f, "{}", string),
            Value::Boolean(boolean) => write!(f, "{}", boolean),
            Value::Nil => write!(f, "Nil"),
        }
    }
}

impl Expression {
    pub fn evaluate(&self) -> Result<Value, &'static str> {
        match &self {
            Expression::Number(number) => Ok(Value::Number(*number)),
            Expression::String(string) => Ok(Value::String(string.clone())),
            Expression::Boolean(boolean) => Ok(Value::Boolean(*boolean)),
            Expression::Nil => Ok(Value::Nil),
            Expression::Binary(opp, left_expr, right_expr) => {
                evaluate_binary(opp, left_expr, right_expr)
            }
            Expression::Unary(opp, expr) => evaluate_unary(opp, expr),
            Expression::Grouping(expr) => expr.evaluate(),
        }
    }
}

fn evaluate_binary(
    opp: &BinaryOperation,
    left_expr: &Expression,
    right_expr: &Expression,
) -> Result<Value, &'static str> {
    let left_value = left_expr.evaluate()?;
    let right_value = right_expr.evaluate()?;
    match opp {
        BinaryOperation::Add => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left + right)),
            (Value::String(left), Value::String(right)) => {
                Ok(Value::String(format!("{}{}", left, right)))
            }
            _ => Err("Unable to add on supplied types"),
        },
        BinaryOperation::Divide => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left / right)),
            _ => Err("Unable to devide on supplied types"),
        },
        BinaryOperation::Equal => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left == right)),
            (Value::String(left), Value::String(right)) => Ok(Value::Boolean(left == right)),
            (Value::Boolean(left), Value::Boolean(right)) => Ok(Value::Boolean(left == right)),
            _ => Err("Unable to test equale on supplied types"),
        },
        BinaryOperation::Greater => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left > right)),
            _ => Err("Unable to test greater than on supplied types"),
        },
        BinaryOperation::GreaterEqual => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left >= right)),
            _ => Err("Unable to test greater than or equal to on supplied types"),
        },
        BinaryOperation::Less => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left < right)),
            _ => Err("Unable to test less than on supplied types"),
        },
        BinaryOperation::LessEqual => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left <= right)),
            _ => Err("Unable to test less than or equal to on supplied types"),
        },
        BinaryOperation::Modulus => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left % right)),
            _ => Err("Unable to modulus on supplied types"),
        },
        BinaryOperation::Multiple => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left * right)),
            _ => Err("Unable to multiply on supplied types"),
        },
        BinaryOperation::NotEqual => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Boolean(left != right)),
            (Value::String(left), Value::String(right)) => Ok(Value::Boolean(left != right)),
            (Value::Boolean(left), Value::Boolean(right)) => Ok(Value::Boolean(left != right)),
            _ => Err("Unable to test not equal on supplied types"),
        },
        BinaryOperation::Subtract => match (left_value, right_value) {
            (Value::Number(left), Value::Number(right)) => Ok(Value::Number(left - right)),
            _ => Err("Unable to subtract on supplied types"),
        },
    }
}

fn evaluate_unary(opp: &UniaryOperation, expr: &Expression) -> Result<Value, &'static str> {
    let value = expr.evaluate()?;

    match &opp {
        UniaryOperation::Negative => match value {
            Value::Number(number) => Ok(Value::Number(-number)),
            _ => Err("Invalid type for minus"),
        },
        UniaryOperation::Not => match value {
            Value::Boolean(boolean) => Ok(Value::Boolean(!boolean)),
            _ => Err("Invalid type for not"),
        },
    }
}
