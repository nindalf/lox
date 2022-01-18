use crate::token::Token;

#[derive(Debug)]
pub(crate) enum Expr<'a> {
    Binary(Box<Expr<'a>>, BinaryOperator<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Literal(Literal),
    Unary(UnaryOperator<'a>, Box<Expr<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
    Boolean(bool),
    Float(f64),
    Integer(i64),
    Nil,
    String(String),
}

#[derive(Debug)]
pub(crate) enum BinaryOperator<'a> {
    BangEqual(Token<'a>),
    Equals(Token<'a>),
    Greater(Token<'a>),
    GreatOrEquals(Token<'a>),
    Lesser(Token<'a>),
    LesserOrEquals(Token<'a>),
    Minus(Token<'a>),
    Plus(Token<'a>),
    Slash(Token<'a>),
    Star(Token<'a>),
}

#[derive(Debug)]
pub(crate) enum UnaryOperator<'a> {
    Bang(Token<'a>),
    Minus(Token<'a>),
}

mod conversion {
    use crate::token::{Token, TokenKind};
    use thiserror::Error;

    use super::{BinaryOperator, Literal, UnaryOperator};

    #[derive(Error, Debug)]
    pub(crate) enum ConversionError {
        #[error("this token cannot be converted to a literal - `{0}`")]
        Literal(String),
        #[error("this token cannot be converted to an operator  - `{0}`")]
        Operator(String),
    }

    impl TryFrom<Token<'_>> for Literal {
        type Error = ConversionError;

        fn try_from(token: Token<'_>) -> Result<Self, Self::Error> {
            match token.token_kind {
                TokenKind::Boolean(val) => Ok(Literal::Boolean(val)),
                TokenKind::Nil => Ok(Literal::Nil),
                TokenKind::NumberFloat(val) => Ok(Literal::Float(val)),
                TokenKind::NumberInteger(val) => Ok(Literal::Integer(val)),
                TokenKind::String => Ok(Literal::String(token.span.to_string())),
                _ => Err(ConversionError::Literal(token.span.to_string())),
            }
        }
    }

    impl<'a> TryFrom<Token<'a>> for BinaryOperator<'a> {
        type Error = ConversionError;

        fn try_from(token: Token<'a>) -> Result<Self, Self::Error> {
            match token.token_kind {
                TokenKind::BangEqual => Ok(BinaryOperator::BangEqual(token)),
                TokenKind::Equals => Ok(BinaryOperator::Equals(token)),
                TokenKind::Greater => Ok(BinaryOperator::Greater(token)),
                TokenKind::GreatOrEquals => Ok(BinaryOperator::GreatOrEquals(token)),
                TokenKind::Lesser => Ok(BinaryOperator::Lesser(token)),
                TokenKind::LesserOrEquals => Ok(BinaryOperator::LesserOrEquals(token)),
                TokenKind::Minus => Ok(BinaryOperator::Minus(token)),
                TokenKind::PlusSign => Ok(BinaryOperator::Plus(token)),
                TokenKind::Slash => Ok(BinaryOperator::Slash(token)),
                TokenKind::Star => Ok(BinaryOperator::Star(token)),
                _ => Err(ConversionError::Operator(token.span.to_string())),
            }
        }
    }

    impl<'a> TryFrom<Token<'a>> for UnaryOperator<'a> {
        type Error = ConversionError;

        fn try_from(token: Token<'a>) -> Result<Self, Self::Error> {
            match token.token_kind {
                TokenKind::Bang => Ok(UnaryOperator::Bang(token)),
                TokenKind::Minus => Ok(UnaryOperator::Minus(token)),
                _ => Err(ConversionError::Operator(token.span.to_string())),
            }
        }
    }
}

mod display {
    use std::fmt::Display;

    use super::{BinaryOperator, Expr, Literal, UnaryOperator};

    impl<'a> Display for Expr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Expr::Binary(expr_left, operator, expr_right) => {
                    f.write_fmt(format_args!("({expr_left} {operator} {expr_right})"))?;
                }
                Expr::Unary(operator, expr) => {
                    f.write_fmt(format_args!("{operator}{expr}"))?;
                }
                Expr::Grouping(expr) => {
                    f.write_fmt(format_args!("({expr})"))?;
                }
                Expr::Literal(literal) => {
                    literal.fmt(f)?;
                }
            }
            Ok(())
        }
    }

    impl<'a> Display for Literal {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Literal::Boolean(val) => f.write_fmt(format_args!("{val}")),
                Literal::Float(val) => f.write_fmt(format_args!("{val}")),
                Literal::Integer(val) => f.write_fmt(format_args!("{val}")),
                Literal::String(val) => f.write_fmt(format_args!("\"{val}\"")),
                Literal::Nil => f.write_fmt(format_args!("nil")),
            }
        }
    }

    impl<'a> Display for BinaryOperator<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                BinaryOperator::BangEqual(token) => token.fmt(f),
                BinaryOperator::Equals(token) => token.fmt(f),
                BinaryOperator::Greater(token) => token.fmt(f),
                BinaryOperator::GreatOrEquals(token) => token.fmt(f),
                BinaryOperator::Lesser(token) => token.fmt(f),
                BinaryOperator::LesserOrEquals(token) => token.fmt(f),
                BinaryOperator::Minus(token) => token.fmt(f),
                BinaryOperator::Plus(token) => token.fmt(f),
                BinaryOperator::Slash(token) => token.fmt(f),
                BinaryOperator::Star(token) => token.fmt(f),
            }
        }
    }

    impl<'a> Display for UnaryOperator<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                UnaryOperator::Bang(token) => token.fmt(f),
                UnaryOperator::Minus(token) => token.fmt(f),
            }
        }
    }
}
