use nom_locate::LocatedSpan;

pub(crate) type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub(crate) struct Token<'a> {
    pub(crate) span: Span<'a>,
    pub(crate) token_kind: TokenKind,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum TokenKind {
    And,
    Assign,
    Bang,
    BangEqual,
    Class,
    Comma,
    Comment,
    CommentMultiline,
    Dot,
    Else,
    Eof,
    Equals,
    False,
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
    NumberInteger(u64),
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
    True,
    Var,
    While,
    Whitespace,
}
