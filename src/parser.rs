use std::iter::Peekable;

use thiserror::Error;

use crate::{
    expression::{BinaryOperator, Expr, Literal, UnaryOperator},
    lexer::Lexer,
    statement::Stmt,
    token::{Token, TokenKind},
};

pub(crate) struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

type PResult<'a, T> = Result<Box<T>, ParseError<'a>>;

#[derive(Debug, Error, PartialEq)]
pub(crate) enum ParseError<'a> {
    #[error("End of file")]
    Eof,
    #[error("Unexpected end of file")]
    UnexpectedEof,
    #[error("Expected {expected}, found {found} instead")]
    UnexpectedToken {
        expected: TokenKind,
        found: Token<'a>,
    },
    #[error("Expected expression, found {0} instead")]
    NoExpression(Token<'a>),
}

impl<'a> Iterator for Parser<'a> {
    type Item = Stmt<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match Parser::declaration(&mut self.lexer) {
            Ok(statement) => Some(statement),
            Err(err) => {
                if err == ParseError::Eof {
                    return None;
                }
                // Encountered an error but try to parse the next statement
                // TODO Should move the cursor till the next semicolon
                self.next()
            }
        }
    }
}

impl<'a> Parser<'a> {
    pub(crate) fn new(program: &'a str) -> Self {
        let lexer = Lexer::new(program, true).peekable();
        Self { lexer }
    }

    fn declaration(lexer: &mut Peekable<Lexer<'a>>) -> Result<Stmt<'a>, ParseError<'a>> {
        if let Some(token) = lexer.peek().cloned() {
            let stmt = match token.token_kind {
                TokenKind::Var => {
                    lexer.next();
                    let identifier = Parser::expect(lexer, TokenKind::Identifier)?;
                    let _ = Parser::expect(lexer, TokenKind::Equals)?;
                    let expr = Parser::expression(lexer)?;
                    Stmt::Var(identifier.to_string(), expr)
                }
                _ => Parser::statement(lexer)?,
            };
            return Ok(stmt);
        }
        Err(ParseError::Eof)
    }

    fn statement(lexer: &mut Peekable<Lexer<'a>>) -> Result<Stmt<'a>, ParseError<'a>> {
        if let Some(token) = lexer.peek().cloned() {
            let stmt = match token.token_kind {
                TokenKind::Print => {
                    lexer.next();
                    Stmt::Print(Parser::expression(lexer)?)
                }
                _ => Stmt::Expression(Parser::expression(lexer)?),
            };
            Parser::expect(lexer, TokenKind::Semicolon)?;
            return Ok(stmt);
        }
        Err(ParseError::Eof)
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
                Parser::expect(lexer, TokenKind::RParen)?;
                return Ok(Box::new(Expr::Grouping(expr)));
            }
            return Err(ParseError::NoExpression(token));
        };
        Err(ParseError::UnexpectedEof)
    }

    fn expect(lexer: &mut Peekable<Lexer<'a>>, token_kind: TokenKind) -> PResult<'a, Token<'a>> {
        if let Some(token) = lexer.peek().cloned() {
            if token.token_kind == token_kind {
                lexer.next();
                return Ok(Box::new(token));
            }
            return Err(ParseError::UnexpectedToken {
                expected: TokenKind::RParen,
                found: token,
            });
        }
        Err(ParseError::UnexpectedEof)
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;

    #[test]
    fn test_simple_expression() {
        let program = "3 + 2 * 6 - 1 * 4 + 2 / 2;";
        let mut parser = Parser::new(program);
        let stmt = parser.next().unwrap();
        assert_eq!(stmt.to_string(), "(((3 + (2 * 6)) - (1 * 4)) + (2 / 2))");
        println!("{stmt}")
    }
}
