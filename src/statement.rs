use std::fmt::Display;

use crate::expression::Expr;

pub(crate) enum Stmt<'a> {
    Expression(Box<Expr<'a>>),
    Print(Box<Expr<'a>>),
    Var(String, Box<Expr<'a>>),
}

impl<'a> Display for Stmt<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression(expr) => expr.fmt(f),
            Stmt::Print(expr) => expr.fmt(f),
            Stmt::Var(identifier, expr) => expr.fmt(f),
        }
    }
}
