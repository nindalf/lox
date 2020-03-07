use core::str::Chars;


struct Lexer<'a> {
    source: Chars<'a>,
    position: usize,
    read_position: usize,
}

impl Lexer<'_> {
    pub(crate) fn new<'a>(source: &'a str) -> Lexer<'a> {
        Lexer{
            source: source.chars(),
            position: 0,
            read_position: 0,
        }
    }
}

use crate::token::Token;
impl<'a> Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let c = self.source.next();
        let token = Token::new(c);
        if token.is_some() {
            return token;
        }
        self.source.next_back();
        let rest = self.source.as_str(); 
        for i in 1 .. rest.len() {
            let token = Token::new_from_str(&rest[..i]);
            self.source.next();
            if token.is_some() {
                return token;
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;
    #[test]
    fn test_nothing() {
        let test = "We gon be us💯💯💕😝, we gon get crazy 🌚😎☺🙈😍";
        let mut lexer = Lexer::new(test);
        while let Some(c) = lexer.next() {
            println!("{:?}", c);
        }
    }
 
    #[test]
    fn test_simple() {
        let test = "=,{(+)};letfn ";
        let mut lexer = Lexer::new(test);
        while let Some(c) = lexer.next() {
            println!("{:?}", c);
        }
    }
}