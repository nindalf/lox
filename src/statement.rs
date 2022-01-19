use std::fmt::Display;

use crate::{
    expression::{Expr, Literal},
    interpreter::InterpretError,
};

pub(crate) enum Stmt<'a> {
    Expression(Box<Expr<'a>>),
    Print(Box<Expr<'a>>),
}

impl<'a> Stmt<'a> {
    #[allow(dead_code)]
    pub(crate) fn interpret(&self) -> Result<Literal, InterpretError> {
        match self {
            Stmt::Expression(expr) => expr.interpret(),
            Stmt::Print(expr) => {
                let literal = expr.interpret()?;
                println!("{literal}");
                Ok(Literal::Nil)
            }
        }
    }
}

impl<'a> Display for Stmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression(expr) => expr.fmt(f),
            Stmt::Print(expr) => expr.fmt(f),
        }
    }
}
