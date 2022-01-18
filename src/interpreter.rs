use crate::expression::{BinaryOperator, Expr, Literal, UnaryOperator};
use thiserror::Error;

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
}

impl<'a> Expr<'a> {
    #[allow(dead_code)]
    pub(crate) fn interpret(&self) -> Result<Literal, InterpretError> {
        match self {
            Expr::Binary(left, operator, right) => Expr::interpret_binary(operator, left, right),
            Expr::Grouping(expr) => expr.interpret(),
            Expr::Literal(literal) => Ok(literal.clone()),
            Expr::Unary(operator, expr) => Expr::interpret_unary(operator, expr),
        }
    }

    fn interpret_unary(
        operator: &'a UnaryOperator<'a>,
        expr: &'a Expr<'a>,
    ) -> Result<Literal, InterpretError<'a>> {
        let literal = expr.interpret()?;
        match (operator, literal) {
            (UnaryOperator::Bang(_), Literal::Boolean(val)) => Ok(Literal::Boolean(!val)),
            (UnaryOperator::Minus(_), Literal::Float(val)) => Ok(Literal::Float(-val)),
            (UnaryOperator::Minus(_), Literal::Integer(val)) => Ok(Literal::Integer(-val)),
            (operator, _) => Err(InterpretError::UnsupportedUnaryOperation(operator, expr)),
        }
    }

    fn interpret_binary(
        operator: &'a BinaryOperator<'a>,
        left: &'a Expr<'a>,
        right: &'a Expr<'a>,
    ) -> Result<Literal, InterpretError<'a>> {
        let left_lit = left.interpret()?;
        let right_lit = right.interpret()?;

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
    use crate::{expression::Literal, parser::Parser};

    #[test]
    fn test_simple_expression() {
        let program = "3 + 2 * 6 - 1 * 4 + 2 * 2";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert_eq!(expr.interpret().unwrap(), Literal::Integer(15));

        let program = "(3 + 2 * 6 - 1) == (4 + 2 * 6 - 2)";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert_eq!(expr.interpret().unwrap(), Literal::Boolean(true));
    }

    #[test]
    fn test_unsupported_unary_operation() {
        let program = "- true";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert!(expr.interpret().is_err());

        let program = "!5";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert!(expr.interpret().is_err());
    }

    #[test]
    fn test_unsupported_nil_operation() {
        let program = "5 + nil";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert!(expr.interpret().is_err());

        let program = "nil == true";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert!(expr.interpret().is_err());

        let program = "!nil";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert!(expr.interpret().is_err());

        let program = "nil == nil";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert_eq!(expr.interpret().unwrap(), Literal::Boolean(true));
    }

    #[test]
    fn divide_by_zero() {
        let program = "5 / (3 * 0)";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert!(expr.interpret().is_err());
    }
}
