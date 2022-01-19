use std::{fmt::Display, ops::Deref};

use nom_locate::LocatedSpan;

pub(crate) type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Token<'a> {
    pub(crate) span: Span<'a>,
    pub(crate) token_kind: TokenKind,
}

impl<'a> PartialEq for Token<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self.token_kind != other.token_kind {
            return false;
        }
        if self.token_kind == TokenKind::String {
            return self.span.deref() == other.span.deref();
        }
        true
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum TokenKind {
    And,
    Assign,
    Bang,
    BangEqual,
    Boolean(bool),
    Class,
    Comma,
    Comment,
    CommentMultiline,
    Dot,
    Else,
    Equals,
    For,
    Function,
    Greater,
    GreatOrEquals,
    Identifier,
    If,
    Illegal,
    LBrace,
    Lesser,
    LesserOrEquals,
    LParen,
    Minus,
    Nil,
    NumberFloat(f64),
    NumberInteger(i64),
    Or,
    PlusSign,
    Print,
    RBrace,
    Return,
    RParen,
    Semicolon,
    Slash,
    Star,
    String,
    Super,
    This,
    Var,
    While,
    Whitespace,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let span = self.span.deref();
        match self.token_kind {
            TokenKind::Boolean(val) => f.write_fmt(format_args!("{val}")),
            TokenKind::Comment => f.write_fmt(format_args!(" //{span}")),
            TokenKind::CommentMultiline => f.write_fmt(format_args!(" /*{span}*/")),
            TokenKind::Identifier => f.write_fmt(format_args!("{span}")),
            TokenKind::NumberFloat(val) => f.write_fmt(format_args!("{val}")),
            TokenKind::NumberInteger(val) => f.write_fmt(format_args!("{val}")),
            TokenKind::String => f.write_fmt(format_args!("\"{span}\"")),
            TokenKind::Whitespace => f.write_str(span),
            _ => self.token_kind.fmt(f),
        }
    }
}

// This doesn't impl the trait because some `TokenKind`s can't be printed
// Print the Token instead.
impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::And => f.write_str("&&"),
            TokenKind::Assign => f.write_str("="),
            TokenKind::Bang => f.write_str("!"),
            TokenKind::BangEqual => f.write_str("!="),
            TokenKind::Boolean(_) => Err(std::fmt::Error),
            TokenKind::Class => f.write_str("class"),
            TokenKind::Comma => f.write_str(","),
            TokenKind::Comment => Err(std::fmt::Error),
            TokenKind::CommentMultiline => Err(std::fmt::Error),
            TokenKind::Dot => f.write_str("."),
            TokenKind::Else => f.write_str("else"),
            TokenKind::Equals => f.write_str("=="),
            TokenKind::For => f.write_str("for"),
            TokenKind::Function => f.write_str("fun"),
            TokenKind::Greater => f.write_str(">"),
            TokenKind::GreatOrEquals => f.write_str(">="),
            TokenKind::Identifier => Err(std::fmt::Error),
            TokenKind::If => f.write_str("if"),
            TokenKind::Illegal => f.write_str(""),
            TokenKind::LBrace => f.write_str("{"),
            TokenKind::Lesser => f.write_str("<"),
            TokenKind::LesserOrEquals => f.write_str("<="),
            TokenKind::LParen => f.write_str("("),
            TokenKind::Minus => f.write_str("-"),
            TokenKind::Nil => f.write_str("nil"),
            TokenKind::NumberFloat(_) => Err(std::fmt::Error),
            TokenKind::NumberInteger(_) => Err(std::fmt::Error),
            TokenKind::Or => f.write_str("or"),
            TokenKind::PlusSign => f.write_str("+"),
            TokenKind::Print => f.write_str("print"),
            TokenKind::RBrace => f.write_str("}"),
            TokenKind::Return => f.write_str("return"),
            TokenKind::RParen => f.write_str(")"),
            TokenKind::Semicolon => f.write_str(";"),
            TokenKind::Slash => f.write_str("/"),
            TokenKind::Star => f.write_str("*"),
            TokenKind::String => Err(std::fmt::Error),
            TokenKind::Super => f.write_str("super"),
            TokenKind::This => f.write_str("this"),
            TokenKind::Var => f.write_str("var"),
            TokenKind::While => f.write_str("while"),
            TokenKind::Whitespace => Err(std::fmt::Error),
        }
    }
}
