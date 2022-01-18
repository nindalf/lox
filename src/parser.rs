use std::iter::Peekable;

use thiserror::Error;

use crate::{
    expression::{BinaryOperator, Expr, Literal, UnaryOperator},
    lexer::Lexer,
    token::{Token, TokenKind},
};

pub(crate) struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

type PResult<'a, T> = Result<Box<T>, ParseError<'a>>;

#[derive(Debug, Error)]
pub(crate) enum ParseError<'a> {
    #[error("Unexpected end of file")]
    UnexpectedEOF,
    #[error("Expected {expected}, found {found} instead")]
    UnexpectedToken {
        expected: TokenKind,
        found: Token<'a>,
    },
    #[error("Expected expression, found {0} instead")]
    NoExpression(Token<'a>),
}

impl<'a> Parser<'a> {
    #[allow(dead_code)]
    pub(crate) fn new(source: &'a str) -> Self {
        let lexer = Lexer::new(source, true).peekable();
        Self { lexer }
    }

    #[allow(dead_code)]
    pub(crate) fn parse(&mut self) -> PResult<Expr> {
        Parser::expression(&mut self.lexer)
    }

    fn expression(lexer: &mut Peekable<Lexer<'a>>) -> PResult<'a, Expr<'a>> {
        Parser::equality(lexer)
    }

    fn equality(lexer: &mut Peekable<Lexer<'a>>) -> PResult<'a, Expr<'a>> {
        let mut expr = Parser::comparison(lexer)?;
        while let Some(token) = lexer.peek().cloned() {
            match BinaryOperator::try_from(token) {
                Ok(operator @ BinaryOperator::BangEqual(_))
                | Ok(operator @ BinaryOperator::Equals(_)) => {
                    lexer.next();

                    let right = Parser::comparison(lexer)?;
                    expr = Box::new(Expr::Binary(expr, operator, right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn comparison(lexer: &mut Peekable<Lexer<'a>>) -> PResult<'a, Expr<'a>> {
        let mut expr = Parser::term(lexer)?;
        while let Some(token) = lexer.peek().cloned() {
            match BinaryOperator::try_from(token) {
                Ok(operator @ BinaryOperator::Greater(_))
                | Ok(operator @ BinaryOperator::GreatOrEquals(_))
                | Ok(operator @ BinaryOperator::Lesser(_))
                | Ok(operator @ BinaryOperator::LesserOrEquals(_)) => {
                    lexer.next();

                    let right = Parser::term(lexer)?;
                    expr = Box::new(Expr::Binary(expr, operator, right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn term(lexer: &mut Peekable<Lexer<'a>>) -> PResult<'a, Expr<'a>> {
        let mut expr = Parser::factor(lexer)?;
        while let Some(token) = lexer.peek().cloned() {
            match BinaryOperator::try_from(token) {
                Ok(operator @ BinaryOperator::Minus(_))
                | Ok(operator @ BinaryOperator::Plus(_)) => {
                    lexer.next();

                    let right = Parser::factor(lexer)?;
                    expr = Box::new(Expr::Binary(expr, operator, right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn factor(lexer: &mut Peekable<Lexer<'a>>) -> PResult<'a, Expr<'a>> {
        let mut expr = Parser::unary(lexer)?;
        while let Some(token) = lexer.peek().cloned() {
            match BinaryOperator::try_from(token) {
                Ok(operator @ BinaryOperator::Slash(_))
                | Ok(operator @ BinaryOperator::Star(_)) => {
                    lexer.next();

                    let right = Parser::unary(lexer)?;
                    expr = Box::new(Expr::Binary(expr, operator, right));
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn unary(lexer: &mut Peekable<Lexer<'a>>) -> PResult<'a, Expr<'a>> {
        if let Some(token) = lexer.peek().cloned() {
            match UnaryOperator::try_from(token) {
                Ok(operator @ UnaryOperator::Bang(_)) | Ok(operator @ UnaryOperator::Minus(_)) => {
                    lexer.next();

                    let right = Parser::unary(lexer)?;
                    return Ok(Box::new(Expr::Unary(operator, right)));
                }
                _ => {}
            }
        }
        Parser::primary(lexer)
    }

    fn primary(lexer: &mut Peekable<Lexer<'a>>) -> PResult<'a, Expr<'a>> {
        if let Some(token) = lexer.peek().cloned() {
            if let Ok(literal) = Literal::try_from(token) {
                lexer.next();
                return Ok(Box::new(Expr::Literal(literal)));
            }
            if token.token_kind == TokenKind::LParen {
                lexer.next();
                let expr = Parser::expression(lexer)?;
                if let Some(expected_close) = lexer.next() {
                    if expected_close.token_kind == TokenKind::RParen {
                        return Ok(Box::new(Expr::Grouping(expr)));
                    } else {
                        return Err(ParseError::UnexpectedToken {
                            expected: TokenKind::RParen,
                            found: expected_close,
                        });
                    }
                }
                return Err(ParseError::UnexpectedEOF);
            }
            return Err(ParseError::NoExpression(token));
        };
        Err(ParseError::UnexpectedEOF)
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;

    #[test]
    fn test_simple_expression() {
        let program = "3 + 2 * 6 - 1 * 4 + 2 / 2";
        let mut parser = Parser::new(program);
        let expr = parser.parse().unwrap();
        assert_eq!(expr.to_string(), "(((3 + (2 * 6)) - (1 * 4)) + (2 / 2))");
        println!("{expr}")
    }
}
