use std::{ops::Deref, sync::Mutex};

use thiserror::Error;

use crate::{
    span::Span,
    token::{Token, TokenKind},
};

use once_cell::sync::OnceCell;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Recoverable error")]
    Error,
    #[error("Unrecoverable error")]
    Failure,
}

struct NewLexer<'a> {
    source: Span<'a>,
}

impl<'a> NewLexer<'a> {
    pub(crate) fn new(source: &'a str) -> NewLexer<'a> {
        NewLexer {
            source: Span::new(source),
        }
    }

    fn lex(&mut self) -> Result<Token, ParseError> {
        if self.source.is_empty() {
            return Ok(Token::new(TokenKind::Eof, &self.source));
        }

        // let matchers = &*matchers().lock().unwrap();
        // for matcher in matchers {
        //     match matcher(self.source) {
        //         Err(err) => match err {
        //             ParseError::Error => continue,
        //             ParseError::Failure => return Err(err),
        //         },
        //         Ok((remaining, tk)) => {
        //             self.source = remaining;
        //             return Ok(tk);
        //         }
        //     }
        // }

        Err(ParseError::Failure)
    }
}

pub type IResult<'a, O> = Result<(Span<'a>, O), ParseError>;

type Matcher = Box<dyn Fn(&str) -> IResult<Token> + Send>;
type CharPredicate = Box<dyn Fn(char) -> bool>;

// fn matchers() -> &'static Mutex<Vec<Matcher>> {
//     static INSTANCE: OnceCell<Mutex<Vec<Matcher>>> = OnceCell::new();

//     INSTANCE.get_or_init(|| {
//         let v: Vec<Matcher> = vec![
//             // Whitespace
//             Box::new(match_char_predicate(
//                 &char::is_whitespace,
//                 TokenKind::Whitespace,
//             )),
//             // Operators
//             Box::new(match_str("!=", TokenKind::BangEqual)),
//             Box::new(match_str("==", TokenKind::Equals)),
//             Box::new(match_str(">=", TokenKind::GreatOrEquals)),
//             Box::new(match_str("<=", TokenKind::LesserOrEquals)),
//             Box::new(match_str("//", TokenKind::Comment)),
//             Box::new(match_char('=', TokenKind::Assign)),
//             Box::new(match_char('!', TokenKind::Bang)),
//             Box::new(match_char(',', TokenKind::Comma)),
//             Box::new(match_char('.', TokenKind::Dot)),
//             Box::new(match_char('>', TokenKind::Greater)),
//             Box::new(match_char('{', TokenKind::LBrace)),
//             Box::new(match_char('<', TokenKind::Lesser)),
//             Box::new(match_char('(', TokenKind::LParen)),
//             Box::new(match_char('-', TokenKind::Minus)),
//             Box::new(match_char('+', TokenKind::PlusSign)),
//             Box::new(match_char('}', TokenKind::RBrace)),
//             Box::new(match_char(')', TokenKind::RParen)),
//             Box::new(match_char(';', TokenKind::Semicolon)),
//             Box::new(match_char('/', TokenKind::Slash)),
//             Box::new(match_char('*', TokenKind::Star)),
//             Box::new(match_char('"', TokenKind::String)),
//             // Keywords
//             Box::new(match_str("and", TokenKind::And)),
//             Box::new(match_str("class", TokenKind::Class)),
//             Box::new(match_str("else", TokenKind::Else)),
//             Box::new(match_str("false", TokenKind::False)),
//             Box::new(match_str("for", TokenKind::For)),
//             Box::new(match_str("fun", TokenKind::Function)),
//             Box::new(match_str("if", TokenKind::If)),
//             Box::new(match_str("nil", TokenKind::Nil)),
//             Box::new(match_str("or", TokenKind::Or)),
//             Box::new(match_str("print", TokenKind::Print)),
//             Box::new(match_str("return", TokenKind::Return)),
//             Box::new(match_str("super", TokenKind::Super)),
//             Box::new(match_str("this", TokenKind::This)),
//             Box::new(match_str("true", TokenKind::True)),
//             Box::new(match_str("var", TokenKind::Var)),
//             Box::new(match_str("while", TokenKind::While)),
//         ];
//         Mutex::new(v)
//     })
// }

fn match_char<'a>(x: char, tk: TokenKind) -> impl Fn(Span) -> IResult<Token> {
    move |input: Span| {
        let input = input.clone();
        input
            .chars()
            .next()
            .map_or(false, |c| c == x)
            .then(move || (input.slice(1..), Token::new(tk, &input.slice(..1))))
            .ok_or(ParseError::Error)
    }
}

fn match_str(x: &'static str, tk: TokenKind) -> impl Fn(Span) -> IResult<Token> {
    let n = x.len();
    move |input: Span| {
        (input.deref().len() >= n && input.slice(..n).deref() == &x)
            .then(move || (input.slice(n..), Token::new(tk, &input.slice(..n))))
            .ok_or(ParseError::Error)
    }
}

fn match_char_predicate<'a>(
    predicate: &(dyn (Fn(char) -> bool) + Send + Sync),
    tk: TokenKind,
) -> impl Fn(Span<'a>) -> IResult<Token> + '_ {
    move |input: &str| {
        input
            .char_indices()
            .take_while(|(_, c)| predicate(*c))
            .last()
            .map(|(n, c)| n + c.len_utf8())
            .map(|n| (&input[n..], Token::new(tk, &input[..n])))
            .ok_or(ParseError::Error)
    }
}

#[cfg(test)]
mod tests {
    use super::NewLexer;
    use crate::token::TokenKind;

    #[test]
    fn test_simple() {
        let input = "/ + = \n==   fun and";
        let expected = vec![
            TokenKind::Slash,
            TokenKind::PlusSign,
            TokenKind::Assign,
            TokenKind::Equals,
            TokenKind::Function,
            TokenKind::And,
        ];

        let mut lexer = NewLexer::new(input);
        match_output_with_expected(&mut lexer, &mut expected.iter());
    }

    fn match_output_with_expected(
        lexer: &mut NewLexer,
        expected: &mut dyn Iterator<Item = &TokenKind>,
    ) {
        for _ in 0..1 {
            dbg!(lexer.lex());
        }
        // loop {
        //     let c = lexer.lex().unwrap();
        //     if c == TokenKind::Whitespace {
        //         continue;
        //     }
        //     if c == TokenKind::Illegal {
        //         break;
        //     }
        //     let exp = expected.next().unwrap();
        //     assert_eq!(c, *exp);
        // }
        // assert_eq!(None, expected.next());
    }
}
