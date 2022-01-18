use std::ops::Deref;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_till, take_until};
use nom::character::complete::{char, line_ending, not_line_ending};
use nom::combinator::{peek, recognize};
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::sequence::{delimited, pair, tuple};
use nom::{IResult, InputTake, Slice};
use nom_unicode::complete::{alpha1, alphanumeric1, digit1, space1};

use crate::token::{Span, Token, TokenKind};

type LexResult<'a, O> = IResult<Span<'a>, O>;

pub(crate) struct Lexer<'a> {
    source: Span<'a>,
    ignore_whitespace: bool,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str, ignore_whitespace: bool) -> Self {
        Self {
            source: Span::new(source),
            ignore_whitespace,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.source.deref().is_empty() {
            return None;
        }

        match lex(self.source) {
            Ok((remaining, token)) => {
                self.source = remaining;
                if self.ignore_whitespace && token.token_kind == TokenKind::Whitespace {
                    return self.next();
                }
                Some(token)
            }
            Err(_) => Some(Token {
                span: self.source,
                token_kind: TokenKind::Illegal,
            }),
        }
    }
}

fn lex(s: Span) -> LexResult<Token> {
    alt((
        lex_whitespace,
        lex_double_operators,
        lex_comment,
        lex_single_operators,
        lex_boolean,
        lex_keywords,
        lex_identifier,
        lex_f64,
        lex_i64,
        lex_string,
    ))(s)
}

fn lex_whitespace(s: Span) -> LexResult<Token> {
    let (remaining, whitespace) = space1(s)?;

    Ok((
        remaining,
        Token {
            span: whitespace,
            token_kind: TokenKind::Whitespace,
        },
    ))
}

fn lex_double_operators(s: Span) -> LexResult<Token> {
    alt((
        gen_tag_lexer("!=", TokenKind::BangEqual),
        gen_tag_lexer("==", TokenKind::Equals),
        gen_tag_lexer(">=", TokenKind::GreatOrEquals),
        gen_tag_lexer("<=", TokenKind::LesserOrEquals),
    ))(s)
}

fn lex_comment(s: Span) -> LexResult<Token> {
    alt((lex_comment_single_line, lex_comment_multi_line))(s)
}

fn lex_comment_single_line(s: Span) -> LexResult<Token> {
    let (remaining, comment) = alt((
        delimited(tag("//"), not_line_ending, line_ending),
        delimited(tag("//"), lex_any, lex_eof),
    ))(s)?;

    Ok((
        remaining,
        Token {
            span: comment,
            token_kind: TokenKind::Comment,
        },
    ))
}

fn lex_comment_multi_line(s: Span) -> LexResult<Token> {
    let (remaining, comment) = delimited(tag("/*"), take_until("*/"), tag("*/"))(s)?;

    Ok((
        remaining,
        Token {
            span: comment,
            token_kind: TokenKind::CommentMultiline,
        },
    ))
}

fn lex_single_operators(s: Span) -> LexResult<Token> {
    alt((
        gen_char_lexer('=', TokenKind::Assign),
        gen_char_lexer('!', TokenKind::Bang),
        gen_char_lexer(',', TokenKind::Comma),
        gen_char_lexer('.', TokenKind::Dot),
        gen_char_lexer('>', TokenKind::Greater),
        gen_char_lexer('{', TokenKind::LBrace),
        gen_char_lexer('<', TokenKind::Lesser),
        gen_char_lexer('(', TokenKind::LParen),
        gen_char_lexer('-', TokenKind::Minus),
        gen_char_lexer('+', TokenKind::PlusSign),
        gen_char_lexer('}', TokenKind::RBrace),
        gen_char_lexer(')', TokenKind::RParen),
        gen_char_lexer(';', TokenKind::Semicolon),
        gen_char_lexer('/', TokenKind::Slash),
        gen_char_lexer('*', TokenKind::Star),
    ))(s)
}

fn lex_boolean(s: Span) -> LexResult<Token> {
    let mut lexer = pair(alt((tag("true"), tag("false"))), peek(token_ending));
    let (remaining, (word, _)) = lexer(s)?;
    let val = word.parse::<bool>().map_err(|_| {
        nom::Err::Error(nom::error::Error {
            input: s,
            code: ErrorKind::Alpha,
        })
    })?;
    Ok((
        remaining,
        Token {
            span: word,
            token_kind: TokenKind::Boolean(val),
        },
    ))
}

fn lex_keywords(s: Span) -> LexResult<Token> {
    alt((
        gen_keyword_lexer("and", TokenKind::And),
        gen_keyword_lexer("class", TokenKind::Class),
        gen_keyword_lexer("else", TokenKind::Else),
        gen_keyword_lexer("for", TokenKind::For),
        gen_keyword_lexer("fun", TokenKind::Function),
        gen_keyword_lexer("if", TokenKind::If),
        gen_keyword_lexer("nil", TokenKind::Nil),
        gen_keyword_lexer("or", TokenKind::Or),
        gen_keyword_lexer("print", TokenKind::Print),
        gen_keyword_lexer("return", TokenKind::Return),
        gen_keyword_lexer("super", TokenKind::Super),
        gen_keyword_lexer("this", TokenKind::This),
        gen_keyword_lexer("var", TokenKind::Var),
        gen_keyword_lexer("while", TokenKind::While),
    ))(s)
}

fn lex_identifier(s: Span) -> LexResult<Token> {
    // TODO fix some valid identifiers being rejected - 🧑‍🚀, नमस्ते, வணக்கம்
    let mut lexer = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ));

    let (remaining, identifier) = lexer(s)?;

    Ok((
        remaining,
        Token {
            span: identifier,
            token_kind: TokenKind::Identifier,
        },
    ))
}

fn lex_f64(s: Span) -> LexResult<Token> {
    let mut lexer = pair(
        recognize(tuple((digit1, char('.'), digit1))),
        peek(token_ending),
    );

    let (remaining, (number, _)) = lexer(s)?;
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

fn lex_i64(s: Span) -> LexResult<Token> {
    let mut lexer = pair(digit1, peek(token_ending));
    let (remaining, (number, _)) = lexer(s)?;
    let n = number.deref().parse::<i64>().map_err(|_| {
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

fn lex_string(s: Span) -> LexResult<Token> {
    // TODO handle escaped strings. Possibly use `nom::escaped`
    let mut lexer = pair(
        delimited(char('"'), take_till(|c| c == '"'), char('"')),
        peek(token_ending),
    );
    let (remaining, (string_val, _)) = lexer(s)?;
    Ok((
        remaining,
        Token {
            span: string_val,
            token_kind: TokenKind::String,
        },
    ))
}

fn gen_char_lexer(c: char, token_kind: TokenKind) -> impl FnMut(Span) -> LexResult<Token> {
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

fn gen_tag_lexer(s: &'static str, token_kind: TokenKind) -> impl FnMut(Span) -> LexResult<Token> {
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

fn gen_keyword_lexer(
    s: &'static str,
    token_kind: TokenKind,
) -> impl FnMut(Span) -> LexResult<Token> {
    move |i: Span| {
        let mut lexer = pair(tag(s), peek(token_ending));
        let (remaining, (word, _)) = lexer(i)?;
        Ok((
            remaining,
            Token {
                span: word,
                token_kind,
            },
        ))
    }
}

fn lex_any(s: Span) -> LexResult<Span> {
    Ok(s.take_split(s.len()))
}

fn lex_eof(s: Span) -> LexResult<Span> {
    if s.len() == 0 {
        return Ok((s, s));
    }
    Err(nom::Err::Error(nom::error::Error {
        input: s,
        code: ErrorKind::Eof,
    }))
}

// Some tokens like numbers, strings and keywords _must_ be followed with a whitespace or an operator.
fn token_ending(s: Span) -> LexResult<Span> {
    let double_operators = alt((tag("=="), tag(">="), tag("<="), tag("!=")));
    alt((
        space1,
        lex_eof,
        lex_any_char("=!,.>{<(-+});/*"),
        double_operators,
    ))(s)
}

fn lex_any_char(chars: &str) -> impl FnMut(Span) -> LexResult<Span> + '_ {
    move |s: Span| match s.chars().next() {
        None => Err(nom::Err::Error(nom::error::Error {
            input: s,
            code: ErrorKind::Char,
        })),
        Some(c) => {
            if chars.contains(c) {
                return Ok((s.slice(c.len_utf8()..), s.slice(..c.len_utf8())));
            }
            Err(nom::Err::Error(nom::error::Error {
                input: s,
                code: ErrorKind::Char,
            }))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Deref;

    use nom::{branch::alt, Slice};

    use crate::{
        lexer::{lex_boolean, lex_keywords, lex_single_operators, lex_whitespace},
        token::TokenKind,
    };

    use super::{lex_comment, lex_f64, lex_i64, lex_identifier, lex_string, Lexer, Span};

    #[test]
    fn test_string_and_number() {
        let input = r#"var _ignore="return pi@ 3.14 //"
        var var_pi = 3.0 
        var GOOP_3=0.314
        var _=0x"#;
        let expected = vec![
            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::String,
            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::NumberFloat(3.0), //(3.14)
            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::NumberFloat(0.314), //(0.314)
            TokenKind::Var,
            TokenKind::Identifier,
            TokenKind::Assign,
            TokenKind::Illegal, //(0x)
        ];
        let mut lexer = Lexer::new(input, true);
        match_output_with_expected(&mut lexer, &mut expected.iter());
    }

    #[test]
    fn test_simple_function() {
        let input = "fun sum(a,b){
            var result = (a+b); // true comment;
            return result;
        }";
        let expected = vec![
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
            TokenKind::Comment,
            TokenKind::Return,
            TokenKind::Identifier,
            TokenKind::Semicolon,
            TokenKind::RBrace,
        ];
        let mut lexer = Lexer::new(input, true);
        match_output_with_expected(&mut lexer, &mut expected.iter());
    }

    #[test]
    fn test_math() {
        let input = "a.x != _ and 3 >2.0 or 1/2<= 1*2 ";
        let expected = vec![
            TokenKind::Identifier,
            TokenKind::Dot,
            TokenKind::Identifier,
            TokenKind::BangEqual,
            TokenKind::Identifier,
            TokenKind::And,
            TokenKind::NumberInteger(3),
            TokenKind::Greater,
            TokenKind::NumberFloat(2.0),
            TokenKind::Or,
            TokenKind::NumberInteger(1),
            TokenKind::Slash,
            TokenKind::NumberInteger(2),
            TokenKind::LesserOrEquals,
            TokenKind::NumberInteger(1),
            TokenKind::Star,
            TokenKind::NumberInteger(2),
        ];
        let mut lexer = Lexer::new(input, true);
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
        let input = "// a comment containing a multiline /*\r\n/* multiline // but the second one is ignored
comment that ends here*// //a comment ending with CRLF \r\n
// a comment ending with EOF";
        let span = Span::new(input);
        let mut lexer = alt((lex_comment, lex_whitespace, lex_single_operators));

        let (remaining, token) = lexer(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Comment);
        assert_eq!(token.span.deref(), &" a comment containing a multiline /*");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::CommentMultiline);
        assert_eq!(
            token.span.deref(),
            &" multiline // but the second one is ignored\ncomment that ends here"
        );

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Slash);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Comment);
        assert_eq!(token.span.deref(), &"a comment ending with CRLF ");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Comment);
        assert_eq!(token.span.deref(), &" a comment ending with EOF");

        assert!(remaining.deref().is_empty());
    }

    #[test]
    fn test_single_operators() {
        let input = "+ -/= !\n{\t\t}";
        let span = Span::new(input);
        let mut lexer = alt((lex_single_operators, lex_whitespace));

        let (remaining, token) = lexer(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::PlusSign);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Minus);
        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Slash);
        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Assign);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Bang);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::LBrace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::RBrace);

        assert!(remaining.deref().is_empty());
    }

    #[test]
    fn test_boolean() {
        let input = "true false trueish falseish True False TRUE FALSE xtrue xfalse";
        let span = Span::new(input);
        let mut lexer = alt((lex_boolean, lex_identifier, lex_whitespace));

        let (remaining, token) = lexer(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Boolean(true));

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Boolean(false));

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);

        assert!(remaining.deref().is_empty());
    }

    #[test]
    fn test_keyword() {
        let input = "fun return\nfor\tif ";
        let span = Span::new(input);
        let mut lexer = alt((lex_keywords, lex_whitespace));

        let (remaining, token) = lexer(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Function);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Return);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::For);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::If);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        assert!(remaining.deref().is_empty());
    }

    #[test]
    fn test_identifier() {
        let input = "a foo_10 _unused x_y_z0  世界 0xyz";
        let span = Span::new(input);
        let mut lexer = alt((lex_identifier, lex_whitespace));

        let (remaining, token) = lexer(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"a");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"foo_10");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"_unused");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"x_y_z0");

        // let (remaining, token) = lexer(remaining).unwrap();
        // assert_eq!(token.token_kind, TokenKind::Whitespace);

        // let (remaining, token) = lexer(remaining).unwrap();
        // assert_eq!(token.token_kind, TokenKind::Identifier);
        // assert_eq!(token.span.deref(), &"🚀");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Identifier);
        assert_eq!(token.span.deref(), &"世界");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let illegal_identifier_error = lexer(remaining);
        assert!(illegal_identifier_error.is_err());
    }

    #[test]
    fn test_numbers() {
        let input = "3 1.414 12345678901 123.456.3. 1 ";
        let span = Span::new(input);
        let mut lexer = alt((lex_f64, lex_i64, lex_whitespace));

        let (remaining, token) = lexer(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberInteger(3));

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberFloat(1.414));

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberInteger(12345678901));

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberFloat(123.456));

        let illegal_identifier_error = lexer(remaining);
        assert!(illegal_identifier_error.is_err());

        let (remaining, token) = lexer(remaining.slice(1..)).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberInteger(3));

        let (remaining, token) = lexer(remaining.slice(2..)).unwrap();
        assert_eq!(token.token_kind, TokenKind::NumberInteger(1));

        let (remaining, token) = lexer(remaining).unwrap();
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
        let mut lexer = alt((lex_string, lex_whitespace));

        let (remaining, token) = lexer(span).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::String);
        assert_eq!(token.span.deref(), &"next one is empty");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::String);
        assert_eq!(token.span.deref(), &"");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::String);
        assert_eq!(token.span.deref(), &"multi\nline\nstring\n");

        let (remaining, token) = lexer(remaining).unwrap();
        assert_eq!(token.token_kind, TokenKind::Whitespace);

        assert!(remaining.deref().is_empty());
    }
}
