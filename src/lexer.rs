struct Lexer<'a> {
    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Lexer<'a> {
        Lexer { source: source }
    }

    pub(crate) fn lex(&mut self) -> (TokenKind, usize) {
        let (token_kind, length) = self.consume_whitespace();
        if length > 0 {
            return (token_kind, length);
        }
        let mut chars = self.source.chars();
        let first = chars.next();
        let second = chars.next();

        let (token_kind, length) = TokenKind::new(first, second);

        return match token_kind {
            Some(TokenKind::Comment) => {
                (TokenKind::Comment, self.get_token_length(|c| *c != '\n'))
            }
            Some(TokenKind::Number) => {
                // A span like 0x should lex to Illegal, not Number or Identifier
                let length = self.get_token_length(|c| c.is_alphanumeric() || *c == '.');
                if self.source[0..length].parse::<f64>().is_err() {
                    return (TokenKind::Illegal, length);
                }
                (TokenKind::Number, length)
            }
            Some(TokenKind::String) => {
                // Skip the first " character
                self.source = &self.source[1..];
                (TokenKind::String, 1 + self.get_token_length(|c| *c != '"'))
            }
            Some(x) => (x, length),
            None => self.get_keyword_or_identifier()
        }

    }

    // inspired by https://blog.frondeus.pl/parser-1/
    fn get_token_length(&mut self, take_while: fn(&char) -> bool) -> usize {
        let length = self
            .source
            .char_indices()
            .take_while(|(_, c)| take_while(c))
            .last()
            .map(|(idx, c)| idx + c.len_utf8())
            .unwrap_or_default();
        length
    }

    // it does not work for identifiers like மூன்று because of the varnam (not alphanumeric)
    // Also, identifiers like 🥧 are not alphabetic and hence rejected
    fn get_keyword_or_identifier(&mut self) -> (TokenKind, usize) {
        let length = self.get_token_length(|c| c.is_alphanumeric() || *c == '_');
        if let Some(token_kind) = TokenKind::match_keyword(&self.source[..length]) {
            return (token_kind, length);
        }        
        if let Some(token_kind) = TokenKind::match_identifier(&self.source[..length]) {
            return (token_kind, length);
        }
        (TokenKind::Illegal, length)
    }

    fn consume_whitespace(&mut self) -> (TokenKind, usize) {
        let length = self.get_token_length(|c| c.is_whitespace());
        (TokenKind::Whitespace, length)
    }
}

use crate::token::{Token, TokenKind};
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
        let input = r#"var _ignore="return pi@ 3.14 //"
        var pi_approximate = 3.14 
        var GOOP_3 = 0.314
        var 三 = 3
        var 0x"#;
        let expected_token_kinds = vec![
            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::String,
            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Number, //(3.14)

            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Number, //(0.314)

            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Number, //(3.0)

            TokenKind::Var,
            TokenKind::Illegal,
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
            var result = (a+b+0); // 0 is redundant;
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
            TokenKind::PlusSign,
            TokenKind::Number,
            TokenKind::RParen,
            TokenKind::Semicolon,
            TokenKind::Comment,

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
