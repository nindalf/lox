use once_cell::sync::Lazy;
use regex::Regex;

static IDENTIFIER_REGEX: Lazy<Regex> = Lazy::new(|| {
    // the first character can be any character or underscore
    // followed by any alphanumeric character or underscore
    Regex::new(r"^[\p{L}_][\w_]*$").unwrap()
});
                     
#[derive(Debug, PartialEq)]
pub(crate) enum TokenKind {
    And,
    Assign,
    Class,
    Comma,
    Comment,
    Else,
    Equals,
    False,
    For,
    Function,
    GreatOrEquals,
    Identifier,
    If,
    Illegal,
    LBrace,
    LesserOrEquals,
    LParen,
    Nil,
    Number,
    Or,
    PlusSign,
    Print,
    RBrace,
    Return,
    RParen,
    Semicolon,
    String,
    Super,
    This,
    True,
    Var,
    While,
    Whitespace,
}
#[derive(Debug)]
pub(crate) struct Token<'a> {
    pub(crate) token_kind: TokenKind,
    pub(crate) span: &'a str
}

impl <'a> Token<'a> {
    pub(crate) fn new(token_kind: TokenKind, span: &'a str) -> Token {
        Token{token_kind, span}
    }
}

impl TokenKind {
    pub(crate) fn new(one: Option<char>, two: Option<char>) -> (Option<TokenKind>, usize) {
        let double = match (one, two) {
            (Some('='), Some('=')) => Some(TokenKind::Equals),
            (Some('>'), Some('=')) => Some(TokenKind::GreatOrEquals),
            (Some('<'), Some('=')) => Some(TokenKind::LesserOrEquals),
            (Some('/'), Some('/')) => Some(TokenKind::Comment),
            (_, _) => None,
        };
        if double.is_some() {
            return (double, 2);
        }
        let single = match one.unwrap() {
            '=' => Some(TokenKind::Assign),
            ',' => Some(TokenKind::Comma),
            '{' => Some(TokenKind::LBrace),
            '(' => Some(TokenKind::LParen),
            '0'..='9' => Some(TokenKind::Number),
            '+' => Some(TokenKind::PlusSign),
            '}' => Some(TokenKind::RBrace),
            ')' => Some(TokenKind::RParen),
            ';' => Some(TokenKind::Semicolon),
            '"' => Some(TokenKind::String),
            _ => None,
        };
        (single, 1)
    }
    
    pub(crate) fn match_identifier(s: &str) -> Option<TokenKind> {        
        if IDENTIFIER_REGEX.is_match(s) {
            return Some(TokenKind::Identifier);
        }
        None
    }

    pub(crate) fn match_keyword(s: &str) -> Option<TokenKind> {
        match s {
            "and" => Some(TokenKind::And),
            "class" => Some(TokenKind::Class),
            "else" => Some(TokenKind::Else),
            "false" => Some(TokenKind::False),
            "for" => Some(TokenKind::For),
            "fun" => Some(TokenKind::Function),
            "if" => Some(TokenKind::If),
            "nil" => Some(TokenKind::Nil),
            "or" => Some(TokenKind::Or),
            "print" => Some(TokenKind::Print),
            "return" => Some(TokenKind::Return),
            "super" => Some(TokenKind::Super),
            "this" => Some(TokenKind::This),
            "true" => Some(TokenKind::True),
            "var" => Some(TokenKind::Var),
            "while" => Some(TokenKind::While),
            _ => None,
        }
    }
}