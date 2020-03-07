#[derive(Debug)]
pub(crate) enum Token {
    Assign,
    Comma,
    Eof,
    Function,
    Identifier(String),
    Illegal,
    Integer(u64),
    LBrace,
    Let,
    LParen,
    PlusSign,
    RBrace,
    RParen,
    Semicolon,
}



impl Token {
    pub(crate) fn new(s: Option<char>) -> Option<Token> {
        match s {
            Some('=') => Some(Token::Assign),
            Some(',') => Some(Token::Comma),
            None => Some(Token::Eof),
            // "" => Some(Token::Illegal),
            Some('{') => Some(Token::LBrace),
            Some('(') => Some(Token::LParen),
            Some('+') => Some(Token::PlusSign),
            Some('}') => Some(Token::RBrace),
            Some(')') => Some(Token::RParen),
            Some(';') => Some(Token::Semicolon),
            _ => None,
        }
    }

    pub(crate) fn new_from_str(s: &str) -> Option<Token> {
        
        match s {
            "=" => Some(Token::Assign),
            "," => Some(Token::Comma),
            // '' => Some(Token::Eof),
            "fn " => Some(Token::Function),
            // "" => Some(Token::Identifier(String)),
            // "" => Some(Token::Illegal),
            // "" => Some(Token::Integer(u64)),
            "{" => Some(Token::LBrace),
            "let " => Some(Token::Let),
            "(" => Some(Token::LParen),
            "+" => Some(Token::PlusSign),
            "}" => Some(Token::RBrace),
            ")" => Some(Token::RParen),
            ";" => Some(Token::Semicolon),
            _ => None,
        }
    }
}