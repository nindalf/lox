use std::{collections::HashMap, io::Write};

use crate::{
    expression::{BinaryOperator, Expr, Literal, UnaryOperator},
    parser::Parser,
    statement::Stmt,
};
use thiserror::Error;

pub(crate) struct Interpreter<'a, W>
where
    W: Write,
{
    parser: Parser<'a>,
    environment: Environment,
    output: W,
}

struct Environment {
    parent: Box<Option<Environment>>,
    values: HashMap<String, Literal>,
}

#[derive(Debug, Error)]
pub(crate) enum InterpretError<'a> {
    #[error("Operation {0} is unsupported for {1}")]
    UnsupportedUnaryOperation(&'a UnaryOperator<'a>, &'a Expr<'a>),
    #[error("Operation {0} is unsupported between {1} and {2}")]
    UnsupportedBinaryOperation(&'a BinaryOperator<'a>, &'a Expr<'a>, &'a Expr<'a>),
    #[error("Expression {0} and {1} are of different types")]
    ExpressionTypeMismatch(&'a Expr<'a>, &'a Expr<'a>),
    #[error("Cannot perform operation {0} on nil type")]
    NilPointerException(&'a BinaryOperator<'a>),
    #[error("Attempted to divide by zero - {0}")]
    DivideByZero(&'a Expr<'a>),
    #[error("Error writing to output")]
    WriteError,
}

impl<'a, W> Interpreter<'a, W>
where
    W: Write,
{
    #[allow(dead_code)]
    fn new(program: &'a str, output: W) -> Self {
        let parser = Parser::new(program);
        let environment = Environment::new_base_env();
        Self {
            parser,
            environment,
            output,
        }
    }

    #[allow(dead_code)]
    fn interpret(&mut self) -> Result<(), InterpretError<'a>> {
        for stmt in self.parser.by_ref() {
            match stmt.interpret(&mut self.environment, &mut self.output) {
                Ok(_) => {}
                Err(err) => eprintln!("Interpreter encountered error - {err}"),
            }
        }
        Ok(())
    }
}

impl Environment {
    fn new_base_env() -> Self {
        Self {
            parent: Box::new(None),
            values: HashMap::new(),
        }
    }

    fn add_variable(&mut self, identifier: String, literal: Literal) {
        self.values.insert(identifier, literal);
    }
}

impl<'a> Stmt<'a> {
    // #[allow(dead_code)]
    fn interpret<W: Write>(
        &self,
        env: &mut Environment,
        mut output: W,
    ) -> Result<(), InterpretError> {
        match self {
            Stmt::Expression(expr) => {
                expr.interpret(env)?;
            }
            Stmt::Print(expr) => {
                let literal = expr.interpret(env)?;
                output
                    .write_fmt(format_args!("{literal}\n"))
                    .map_err(|_| InterpretError::WriteError)?;
            }
            Stmt::Var(identifier, expr) => {
                let literal = expr.interpret(env)?;
                env.add_variable(identifier.clone(), literal);
            }
        };
        Ok(())
    }
}

impl<'a> Expr<'a> {
    fn interpret(&self, env: &mut Environment) -> Result<Literal, InterpretError> {
        match self {
            Expr::Binary(left, operator, right) => {
                Expr::interpret_binary(env, operator, left, right)
            }
            Expr::Grouping(expr) => expr.interpret(env),
            Expr::Literal(literal) => Ok(literal.clone()),
            Expr::Unary(operator, expr) => Expr::interpret_unary(env, operator, expr),
        }
    }

    fn interpret_unary(
        env: &mut Environment,
        operator: &'a UnaryOperator<'a>,
        expr: &'a Expr<'a>,
    ) -> Result<Literal, InterpretError<'a>> {
        let literal = expr.interpret(env)?;
        match (operator, literal) {
            (UnaryOperator::Bang(_), Literal::Boolean(val)) => Ok(Literal::Boolean(!val)),
            (UnaryOperator::Minus(_), Literal::Float(val)) => Ok(Literal::Float(-val)),
            (UnaryOperator::Minus(_), Literal::Integer(val)) => Ok(Literal::Integer(-val)),
            (operator, _) => Err(InterpretError::UnsupportedUnaryOperation(operator, expr)),
        }
    }

    fn interpret_binary(
        env: &mut Environment,
        operator: &'a BinaryOperator<'a>,
        left: &'a Expr<'a>,
        right: &'a Expr<'a>,
    ) -> Result<Literal, InterpretError<'a>> {
        let left_lit = left.interpret(env)?;
        let right_lit = right.interpret(env)?;

        match (left_lit, right_lit) {
            (Literal::Boolean(left_val), Literal::Boolean(right_val)) => {
                Expr::interpret_binary_bools(operator, left, right, left_val, right_val)
            }
            (Literal::Float(left_val), Literal::Float(right_val)) => {
                Expr::interpret_binary_floats(operator, left, left_val, right_val)
            }
            (Literal::Integer(left_val), Literal::Integer(right_val)) => {
                Expr::interpret_binary_integers(operator, left, left_val, right_val)
            }
            (Literal::Nil, Literal::Nil) => Expr::interpret_binary_nils(operator),
            (Literal::String(left_val), Literal::String(right_val)) => {
                Expr::interpret_binary_strings(operator, left, right, left_val, right_val)
            }
            _ => Err(InterpretError::ExpressionTypeMismatch(left, right)),
        }
    }

    fn interpret_binary_bools(
        operator: &'a BinaryOperator<'a>,
        left: &'a Expr<'a>,
        right: &'a Expr<'a>,
        left_val: bool,
        right_val: bool,
    ) -> Result<Literal, InterpretError<'a>> {
        match operator {
            BinaryOperator::BangEqual(_) => Ok(Literal::Boolean(left_val != right_val)),
            BinaryOperator::Equals(_) => Ok(Literal::Boolean(left_val == right_val)),
            _ => Err(InterpretError::UnsupportedBinaryOperation(
                operator, left, right,
            )),
        }
    }

    fn interpret_binary_floats(
        operator: &'a BinaryOperator<'a>,
        left: &'a Expr<'a>,
        left_val: f64,
        right_val: f64,
    ) -> Result<Literal, InterpretError<'a>> {
        match operator {
            BinaryOperator::BangEqual(_) => Ok(Literal::Boolean(left_val != right_val)),
            BinaryOperator::Equals(_) => Ok(Literal::Boolean(left_val == right_val)),
            BinaryOperator::Greater(_) => Ok(Literal::Boolean(left_val > right_val)),
            BinaryOperator::GreatOrEquals(_) => Ok(Literal::Boolean(left_val >= right_val)),
            BinaryOperator::Lesser(_) => Ok(Literal::Boolean(left_val < right_val)),
            BinaryOperator::LesserOrEquals(_) => Ok(Literal::Boolean(left_val <= right_val)),
            BinaryOperator::Minus(_) => Ok(Literal::Float(left_val - right_val)),
            BinaryOperator::Plus(_) => Ok(Literal::Float(left_val + right_val)),
            BinaryOperator::Slash(_) => {
                if right_val == 0.0 {
                    return Err(InterpretError::DivideByZero(left));
                }
                Ok(Literal::Float(left_val / right_val))
            }
            BinaryOperator::Star(_) => Ok(Literal::Float(left_val * right_val)),
        }
    }

    fn interpret_binary_integers(
        operator: &'a BinaryOperator<'a>,
        left: &'a Expr<'a>,
        left_val: i64,
        right_val: i64,
    ) -> Result<Literal, InterpretError<'a>> {
        match operator {
            BinaryOperator::BangEqual(_) => Ok(Literal::Boolean(left_val != right_val)),
            BinaryOperator::Equals(_) => Ok(Literal::Boolean(left_val == right_val)),
            BinaryOperator::Greater(_) => Ok(Literal::Boolean(left_val > right_val)),
            BinaryOperator::GreatOrEquals(_) => Ok(Literal::Boolean(left_val >= right_val)),
            BinaryOperator::Lesser(_) => Ok(Literal::Boolean(left_val < right_val)),
            BinaryOperator::LesserOrEquals(_) => Ok(Literal::Boolean(left_val <= right_val)),
            BinaryOperator::Minus(_) => Ok(Literal::Integer(left_val - right_val)),
            BinaryOperator::Plus(_) => Ok(Literal::Integer(left_val + right_val)),
            BinaryOperator::Slash(_) => {
                if right_val == 0 {
                    return Err(InterpretError::DivideByZero(left));
                }
                Ok(Literal::Integer(left_val / right_val))
            }
            BinaryOperator::Star(_) => Ok(Literal::Integer(left_val * right_val)),
        }
    }

    fn interpret_binary_nils(
        operator: &'a BinaryOperator<'a>,
    ) -> Result<Literal, InterpretError<'a>> {
        match operator {
            BinaryOperator::Equals(_) => Ok(Literal::Boolean(true)),
            _ => Err(InterpretError::NilPointerException(operator)),
        }
    }

    fn interpret_binary_strings(
        operator: &'a BinaryOperator<'a>,
        left: &'a Expr<'a>,
        right: &'a Expr<'a>,
        left_val: String,
        right_val: String,
    ) -> Result<Literal, InterpretError<'a>> {
        match operator {
            BinaryOperator::BangEqual(_) => Ok(Literal::Boolean(left_val != right_val)),
            BinaryOperator::Equals(_) => Ok(Literal::Boolean(left_val == right_val)),
            BinaryOperator::Greater(_) => Ok(Literal::Boolean(left_val > right_val)),
            BinaryOperator::GreatOrEquals(_) => Ok(Literal::Boolean(left_val >= right_val)),
            BinaryOperator::Lesser(_) => Ok(Literal::Boolean(left_val < right_val)),
            BinaryOperator::LesserOrEquals(_) => Ok(Literal::Boolean(left_val <= right_val)),
            BinaryOperator::Plus(_) => Ok(Literal::String(left_val + &right_val)),
            _ => Err(InterpretError::UnsupportedBinaryOperation(
                operator, left, right,
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{interpreter::Environment, parser::Parser};

    #[test]
    fn test_simple_expression() {
        let mut environment = Environment::new_base_env();
        let program = "print 3 + 2 * 6 - 1 * 4 + 2 * 2;";
        let mut parser = Parser::new(program);
        let mut buf = Vec::new();
        let stmt = parser.next().unwrap();
        stmt.interpret(&mut environment, &mut buf).unwrap();
        assert_eq!(buf, "15\n".as_bytes());

        let program = "print (3 + 2 * 6 - 1) == (4 + 2 * 6 - 2);";
        let mut parser = Parser::new(program);
        let mut buf = Vec::new();
        let stmt = parser.next().unwrap();
        stmt.interpret(&mut environment, &mut buf).unwrap();
        assert_eq!(buf, "true\n".as_bytes());
    }

    #[test]
    fn test_unsupported_unary_operation() {
        let mut environment = Environment::new_base_env();
        let program = "- true;";
        let mut parser = Parser::new(program);
        let stmt = parser.next().unwrap();
        assert!(stmt.interpret(&mut environment, Vec::new()).is_err());

        let program = "!5;";
        let mut parser = Parser::new(program);
        let stmt = parser.next().unwrap();
        assert!(stmt.interpret(&mut environment, Vec::new()).is_err());
    }

    #[test]
    fn test_unsupported_nil_operation() {
        let mut environment = Environment::new_base_env();
        let program = "5 + nil;";
        let mut parser = Parser::new(program);
        let stmt = parser.next().unwrap();
        assert!(stmt.interpret(&mut environment, Vec::new()).is_err());

        let program = "nil == true;";
        let mut parser = Parser::new(program);
        let stmt = parser.next().unwrap();
        assert!(stmt.interpret(&mut environment, Vec::new()).is_err());

        let program = "!nil;";
        let mut parser = Parser::new(program);
        let stmt = parser.next().unwrap();
        assert!(stmt.interpret(&mut environment, Vec::new()).is_err());

        let program = "print nil == nil;";
        let mut parser = Parser::new(program);
        let mut buf = Vec::new();
        let stmt = parser.next().unwrap();
        stmt.interpret(&mut environment, &mut buf).unwrap();
        assert_eq!(buf, "true\n".as_bytes());
    }

    #[test]
    fn divide_by_zero() {
        let mut environment = Environment::new_base_env();
        let program = "5 / (3 * 0);";
        let mut parser = Parser::new(program);
        let buf = Vec::new();
        let stmt = parser.next().unwrap();
        assert!(stmt.interpret(&mut environment, buf).is_err());
    }
}
