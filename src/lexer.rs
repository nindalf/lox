use std::ops::Deref;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_till, take_until};
use nom::character::complete::{alpha1, alphanumeric1, char, digit1, multispace1};
use nom::combinator::{peek, recognize};
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::sequence::{delimited, pair, tuple};
use nom::{IResult, Slice};

use crate::token::{Span, Token, TokenKind};

type LexResult<'a, O> = IResult<Span<'a>, O>;

pub(crate) struct Lexer<'a> {
    source: Span<'a>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source: Span::new(source),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.source.deref().is_empty() {
            return None;
        }

        match parse(self.source) {
            Ok((remaining, token)) => {
                self.source = remaining;
                Some(token)
            }
            Err(_) => Some(Token {
                span: self.source,
                token_kind: TokenKind::Illegal,
            }),
        }
    }
}

fn parse(s: Span) -> LexResult<Token> {
    alt((
        parse_whitespace,
        parse_double_operators,
        parse_comment,
        parse_single_operators,
        parse_keywords,
        parse_identifier,
        parse_f64,
        parse_u64,
        parse_string,
    ))(s)
}

fn parse_whitespace(s: Span) -> LexResult<Token> {
    let (remaining, whitespace) = multispace1(s)?;

    Ok((
        remaining,
        Token {
            span: whitespace,
            token_kind: TokenKind::Whitespace,
        },
    ))
}

fn parse_double_operators(s: Span) -> LexResult<Token> {
    alt((
        gen_tag_parser("!=", TokenKind::BangEqual),
        gen_tag_parser("==", TokenKind::Equals),
        gen_tag_parser(">=", TokenKind::GreatOrEquals),
        gen_tag_parser("<=", TokenKind::LesserOrEquals),
    ))(s)
}

fn parse_comment(s: Span) -> LexResult<Token> {
    // TODO handle EOF for single line comments
    let single_line = delimited(tag("//"), take_till(|c| c == '\n'), char('\n'));
    let multi_line = delimited(tag("/*"), take_until("*/"), tag("*/"));
    let (remaining, comment) = alt((single_line, multi_line))(s)?;

    Ok((
        remaining,
        Token {
            span: comment,
            token_kind: TokenKind::Comment,
        },
    ))
}

fn parse_single_operators(s: Span) -> LexResult<Token> {
    alt((
        gen_char_parser('=', TokenKind::Assign),
        gen_char_parser('!', TokenKind::Bang),
        gen_char_parser(',', TokenKind::Comma),
        gen_char_parser('.', TokenKind::Dot),
        gen_char_parser('>', TokenKind::Greater),
        gen_char_parser('{', TokenKind::LBrace),
        gen_char_parser('<', TokenKind::Lesser),
        gen_char_parser('(', TokenKind::LParen),
        gen_char_parser('-', TokenKind::Minus),
        gen_char_parser('+', TokenKind::PlusSign),
        gen_char_parser('}', TokenKind::RBrace),
        gen_char_parser(')', TokenKind::RParen),
        gen_char_parser(';', TokenKind::Semicolon),
        gen_char_parser('/', TokenKind::Slash),
        gen_char_parser('*', TokenKind::Star),
    ))(s)
}

fn parse_keywords(s: Span) -> LexResult<Token> {
    alt((
        gen_keyword_parser("and", TokenKind::And),
        gen_keyword_parser("class", TokenKind::Class),
        gen_keyword_parser("else", TokenKind::Else),
        gen_keyword_parser("false", TokenKind::False),
        gen_keyword_parser("for", TokenKind::For),
        gen_keyword_parser("fun", TokenKind::Function),
        gen_keyword_parser("if", TokenKind::If),
        gen_keyword_parser("nil", TokenKind::Nil),
        gen_keyword_parser("or", TokenKind::Or),
        gen_keyword_parser("print", TokenKind::Print),
        gen_keyword_parser("return", TokenKind::Return),
        gen_keyword_parser("super", TokenKind::Super),
        gen_keyword_parser("this", TokenKind::This),
        gen_keyword_parser("true", TokenKind::True),
        gen_keyword_parser("var", TokenKind::Var),
        gen_keyword_parser("while", TokenKind::While),
    ))(s)
}

fn parse_identifier(s: Span) -> LexResult<Token> {
    let mut parser = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ));

    let (remaining, identifier) = parser(s)?;

    Ok((
        remaining,
        Token {
            span: identifier,
            token_kind: TokenKind::Identifier,
        },
    ))
}

fn parse_f64(s: Span) -> LexResult<Token> {
    let mut parser = pair(
        recognize(tuple((digit1, char('.'), digit1))),
        peek(token_ending),
    );

    let (remaining, (number, _)) = parser(s)?;
    let f = number.deref().parse::<f64>().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input: s,
            code: ErrorKind::Alpha,
        })
    })?;
    Ok((
        remaining,
        Token {
            span: number,
            token_kind: TokenKind::NumberFloat(f),
        },
    ))
}

fn parse_u64(s: Span) -> LexResult<Token> {
    let mut parser = pair(digit1, peek(token_ending));
    let (remaining, (number, _)) = parser(s)?;
    let n = number.deref().parse::<u64>().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input: s,
            code: ErrorKind::Alpha,
        })
    })?;
    Ok((
        remaining,
        Token {
            span: number,
            token_kind: TokenKind::NumberInteger(n),
        },
    ))
}

fn parse_string(s: Span) -> LexResult<Token> {
    let mut parser = pair(
        delimited(char('"'), take_till(|c| c == '"'), char('"')),
        peek(token_ending),
    );
    let (remaining, (string_val, _)) = parser(s)?;
    Ok((
        remaining,
        Token {
            span: string_val,
            token_kind: TokenKind::String,
        },
    ))
}

fn gen_char_parser(c: char, token_kind: TokenKind) -> impl FnMut(Span) -> LexResult<Token> {
    move |s| {
        let (remaining, _) = char(c)(s)?;
        Ok((
            remaining,
            Token {
                span: s.slice(0..c.len_utf8()),
                token_kind,
            },
        ))
    }
}

fn gen_tag_parser(s: &'static str, token_kind: TokenKind) -> impl FnMut(Span) -> LexResult<Token> {
    move |i: Span| {
        let (remaining, word) = tag(s)(i)?;
        Ok((
            remaining,
            Token {
                span: word,
                token_kind,
            },
        ))
    }
}

fn gen_keyword_parser(
    s: &'static str,
    token_kind: TokenKind,
) -> impl FnMut(Span) -> LexResult<Token> {
    move |i: Span| {
        let mut parser = pair(tag(s), peek(token_ending));
        let (remaining, (word, _)) = parser(i)?;
        Ok((
            remaining,
            Token {
                span: word,
                token_kind,
            },
        ))
    }
}

// Some tokens like numbers, strings and keywords _must_ be followed with a whitespace or an operator.
fn token_ending(s: Span) -> LexResult<Token> {
    alt((
        parse_whitespace,
        parse_double_operators,
        parse_single_operators,
    ))(s)
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use nom::{branch::alt, Slice};

    use crate::{
        lexer::{parse_keywords, parse_single_operators, parse_whitespace},
        token::TokenKind,
    };

    use super::{parse_comment, parse_f64, parse_identifier, parse_string, parse_u64, Lexer, Span};

    #[test]
    fn test_string_and_number() {
        let input = r#"var _ignore="return pi@ 3.14 //"
        var var_pi = 3.0 
        var GOOP_3=0.314
        var _=0x"#;
        let expected = vec![
            TokenKind::Var,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::String,
            TokenKind::Whitespace,
            TokenKind::Var,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::Assign,
            TokenKind::Whitespace,
            TokenKind::NumberFloat(3.0), //(3.14)
            TokenKind::Whitespace,
            TokenKind::Var,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::NumberFloat(0.314), //(0.314)
            TokenKind::Whitespace,
            TokenKind::Var,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Illegal, //(3.0)
        ];
        let mut lexer = Lexer::new(input);
        match_output_with_expected(&mut lexer, &mut expected.iter());
    }

    #[test]
    fn test_simple_function() {
        let input = "fun sum(a,b){
            var result = (a+b); // comment;
            return result;
        }";
        let expected = vec![
            TokenKind::Function,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::LParen,
            TokenKind::Identifier,
            TokenKind::Comma,
            TokenKind::Identifier,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Whitespace,
            TokenKind::Var,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::Assign,
            TokenKind::Whitespace,
            TokenKind::LParen,
            TokenKind::Identifier,
            TokenKind::PlusSign,
            TokenKind::Identifier,
            TokenKind::RParen,
            TokenKind::Semicolon,
            TokenKind::Whitespace,
            TokenKind::Comment,
            TokenKind::Whitespace,
            TokenKind::Return,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Semicolon,
            TokenKind::Whitespace,
            TokenKind::RBrace,
        ];
        let mut lexer = Lexer::new(input);
        match_output_with_expected(&mut lexer, &mut expected.iter());
    }

    #[test]
    fn test_math() {
        let input = "a.x != _ and 3 >2.0 or 1/2<= 1*2 ";
        let expected = vec![
            TokenKind::Identifier,
            TokenKind::Dot,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::BangEqual,
            TokenKind::Whitespace,
            TokenKind::Identifier,
            TokenKind::Whitespace,
            TokenKind::And,
            TokenKind::Whitespace,
            TokenKind::NumberInteger(3),
            TokenKind::Whitespace,
            TokenKind::Greater,
            TokenKind::NumberFloat(2.0),
            TokenKind::Whitespace,
            TokenKind::Or,
            TokenKind::Whitespace,
            TokenKind::NumberInteger(1),
            TokenKind::Slash,
            TokenKind::NumberInteger(2),
            TokenKind::LesserOrEquals,
            TokenKind::Whitespace,
            TokenKind::NumberInteger(1),
            TokenKind::Star,
            TokenKind::NumberInteger(2),
            TokenKind::Whitespace,
        ];
        let mut lexer = Lexer::new(input);
        match_output_with_expected(&mut lexer, &mut expected.iter());
    }

    fn match_output_with_expected(
        lexer: &mut Lexer,
        expected: &mut dyn Iterator<Item = &TokenKind>,
    ) {
        for (i, token) in lexer.enumerate() {
            let exp = expected.next().unwrap();
            let span = token.span;
            assert_eq!(
                token.token_kind, *exp,
                "{i}th element did not match - {span}"
            );
            if *exp == TokenKind::Illegal {
                return;
            }
        }
        assert_eq!(None, expected.next());
    }

    #[test]
    fn test_comment() {
        let input = "// a comment containing a multiline /*
/* multiline // but the second one is ignored
comment that ends here*// //another comment but no newline\n";
        let span = Span::new(input);
        let mut parser = alt((parse_comment, parse_whitespace, parse_single_operators));

        let (remaining, token) = parser(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Comment);
        assert_eq!(token.span.deref(), &" a comment containing a multiline /*");

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Comment);
        assert_eq!(
            token.span.deref(),
            &" multiline // but the second one is ignored\ncomment that ends here"
        );

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Slash);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Comment);
        assert_eq!(token.span.deref(), &"another comment but no newline");

        assert!(remaining.deref().is_empty());
    }

    #[test]
    fn test_single_operators() {
        let input = "+ -/= !\n{\t\t}";
        let span = Span::new(input);
        let mut parser = alt((parse_single_operators, parse_whitespace));

        let (remaining, token) = parser(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::PlusSign);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Minus);
        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Slash);
        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Assign);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Bang);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::LBrace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::RBrace);

        assert!(remaining.deref().is_empty());
    }

    #[test]
    fn test_keyword() {
        let input = "fun return\nfor\tif ";
        let span = Span::new(input);
        let mut parser = alt((parse_keywords, parse_whitespace));

        let (remaining, token) = parser(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Function);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Return);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::For);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::If);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        assert!(remaining.deref().is_empty());
    }

    #[test]
    fn test_identifier() {
        let input = "a foo_10 _unused x_y_z0 0xyz";
        let span = Span::new(input);
        let mut parser = alt((parse_identifier, parse_whitespace));

        let (remaining, token) = parser(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"a");

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"foo_10");

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"_unused");

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"x_y_z0");

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let illegal_identifier_error = parser(remaining);
        assert!(illegal_identifier_error.is_err());
    }

    #[test]
    fn test_numbers() {
        let input = "3 1.414 12345678901 123.456.3. 1 ";
        let span = Span::new(input);
        let mut parser = alt((parse_f64, parse_u64, parse_whitespace));

        let (remaining, token) = parser(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberInteger(3));

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberFloat(1.414));

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberInteger(12345678901));

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberFloat(123.456));

        let illegal_identifier_error = parser(remaining);
        assert!(illegal_identifier_error.is_err());

        let (remaining, token) = parser(remaining.slice(1..)).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberInteger(3));

        let (remaining, token) = parser(remaining.slice(2..)).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberInteger(1));

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        assert!(remaining.deref().is_empty());
    }

    #[test]
    fn test_string() {
        let input = r#"
"next one is empty" "" "multi
line
string
"
"#;
        let span = Span::new(input);
        let mut parser = alt((parse_string, parse_whitespace));

        let (remaining, token) = parser(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::String);
        assert_eq!(token.span.deref(), &"next one is empty");

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::String);
        assert_eq!(token.span.deref(), &"");

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::String);
        assert_eq!(token.span.deref(), &"multi\nline\nstring\n");

        let (remaining, token) = parser(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        assert!(remaining.deref().is_empty());
    }
}
