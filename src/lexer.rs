struct Lexer<'a> {
    source: &'a str,
}

impl <'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Lexer<'a> {
        Lexer{
            source: source,
        }
    }

    fn lex(&mut self) -> (TokenKind, usize) {
        let (token_kind, length) = self.consume_whitespace();
        if length > 0 {
            return (token_kind, length);
        }
        let mut chars = self.source.chars();
        let first = chars.next();
        let second = chars.next();

        let (token_kind, length) = TokenKind::new(first, second);
        if token_kind.is_some() {
            return (token_kind.unwrap(), length)
        }
        
        return self.get_token();
    }

    // TODO fix the predicate is_alphanumeric()
    // it does not work for identifiers like மூன்று because of the varnam
    // Also, identifiers like 🥧 are not alphabetic and hence rejected
    fn get_token(&mut self) -> (TokenKind, usize) {
        let accepted_chars = vec!['"', '_', '.'];
        let length = self.source
            .char_indices()
            .take_while(|(_, c)| c.is_alphanumeric() || accepted_chars.contains(c))
            .last()
            .map(|(idx, c)| idx + c.len_utf8())
            .unwrap_or_default();
        if let Some(token_kind) = TokenKind::new_from_str(&self.source[..length]) {
            return (token_kind, length);
        }
        (TokenKind::Illegal, 0)
    }    

    // inspired by https://blog.frondeus.pl/parser-1/
    fn consume_whitespace(&mut self) -> (TokenKind, usize) {
        let length = self.source
            .char_indices()
            .take_while(|(_, c)| c.is_whitespace())
            .last()
            .map(|(idx, c)| idx + c.len_utf8())
            .unwrap_or_default();
        (TokenKind::Whitespace, length)
    }
}

use crate::token::{TokenKind, Token};
impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        if self.source.len() == 0 {
            return None;
        }
        let (token_kind, length) = self.lex();
        let span = &self.source[..length];
        self.source = &self.source[length..];
        let token = Token::new(token_kind, span);
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    use crate::token::TokenKind;
    #[test]
    fn test_string_and_number() {
        let input = "var _ignore=\"test\"
        var pi_approximate = 3.14 
        var GOOP_3 = 0.314
        var 三 = 3
        var 0x";
        let expected_token_kinds = vec![
            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::String,
            
            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Number(3.14),

            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Number(0.314),

            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Number(3.0),
            
            TokenKind::Var,
            TokenKind::Illegal
        ];
        let mut expected = expected_token_kinds.iter();
        let mut lexer = Lexer::new(input);
        while let Some(c) = lexer.next() {
            if c.token_kind == TokenKind::Whitespace {
                continue;
            }
            let exp = expected.next().unwrap();
            assert_eq!(c.token_kind, *exp);
        }
        assert_eq!(None, expected.next());
    }

    #[test]
    fn test_simple_function() {
        let input = "fun sum(a,b){
            var result = (a+b);
            return result;
        }";
        let expected_token_kinds = vec![
            TokenKind::Function,
            TokenKind::Identifier,
            TokenKind::LParen,
            TokenKind::Identifier,
            TokenKind::Comma,
            TokenKind::Identifier,
            TokenKind::RParen,
            TokenKind::LBrace,

            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::LParen,
            TokenKind::Identifier,
            TokenKind::PlusSign,
            TokenKind::Identifier,
            TokenKind::RParen,
            TokenKind::Semicolon,

            TokenKind::Return,
            TokenKind::Identifier,
            TokenKind::Semicolon,

            TokenKind::RBrace,
        ];
        let mut expected = expected_token_kinds.iter();
        let mut lexer = Lexer::new(input);
        while let Some(c) = lexer.next() {
            if c.token_kind == TokenKind::Whitespace {
                continue;
            }
            let exp = expected.next().unwrap();
            assert_eq!(c.token_kind, *exp);
        }
        assert_eq!(None, expected.next());

    }
}