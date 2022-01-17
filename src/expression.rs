use crate::token::Token;

#[allow(dead_code)]
pub(crate) enum Expr<'a> {
    Binary(Box<Expr<'a>>, BinaryOperator<'a>, Box<Expr<'a>>),
    Grouping(Vec<Expr<'a>>),
    Literal(Literal<'a>),
    Unary(UnaryOperator<'a>, Box<Expr<'a>>),
}

pub(crate) enum Literal<'a> {
    Boolean(Token<'a>),
    Float(Token<'a>),
    Integer(Token<'a>),
    Nil(Token<'a>),
    String(Token<'a>),
}

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

    impl<'a> TryFrom<Token<'a>> for Literal<'a> {
        type Error = ConversionError;

        fn try_from(token: Token<'a>) -> Result<Self, Self::Error> {
            match token.token_kind {
                TokenKind::Boolean(_) => Ok(Literal::Boolean(token)),
                TokenKind::Nil => Ok(Literal::Nil(token)),
                TokenKind::NumberFloat(_) => Ok(Literal::Float(token)),
                TokenKind::NumberInteger(_) => Ok(Literal::Integer(token)),
                TokenKind::String => Ok(Literal::String(token)),
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
    use std::fmt::{Display, Write};

    use super::{BinaryOperator, Expr, Literal, UnaryOperator};

    impl<'a> Display for Expr<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Expr::Binary(expr_left, operator, expr_right) => {
                    f.write_fmt(format_args!("({expr_left} {operator} {expr_right})"))?;
                }
                Expr::Unary(operator, expr) => {
                    f.write_fmt(format_args!("({operator}{expr})"))?;
                }
                Expr::Grouping(exprs) => {
                    f.write_char('(')?;
                    let mut items = exprs.iter();
                    if let Some(expr) = items.next() {
                        f.write_fmt(format_args!("{expr}"))?;
                        for expr in items {
                            f.write_fmt(format_args!(" {expr}"))?;
                        }
                    }
                    f.write_char(')')?;
                }
                Expr::Literal(literal) => {
                    literal.fmt(f)?;
                }
            }
            Ok(())
        }
    }

    impl<'a> Display for Literal<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Literal::Boolean(token) => token.fmt(f),
                Literal::Float(token) => token.fmt(f),
                Literal::Integer(token) => token.fmt(f),
                Literal::Nil(token) => token.fmt(f),
                Literal::String(token) => token.fmt(f),
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
