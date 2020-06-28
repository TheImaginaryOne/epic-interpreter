use crate::compiler::error::Error;
use std::iter::Peekable;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token {
    String,
    Integer,
    Identifier,

    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semicolon,

    Equal,
    DoubleEqual, // '=='
    Less,
    Greater,
    Let,
    If,
    Else,

    Comment,
    Whitespace,

    Eof, // a dummy token
}

pub type SpannedToken<T, L, E> = Result<(L, T, L), (L, E, L)>;

fn is_whitespace(s: char) -> bool {
    s == '\n' || s == ' ' || s == '\r' || s == '\t'
}
/// Includes all (Unicode) characters and numbers in other languages
fn is_alphanum_or_underscore(s: char) -> bool {
    s == '_' || s.is_alphanumeric()
}

pub struct Lexer<'a> {
    done: bool,
    len: usize,
    start_byte: usize,
    input: &'a str,
    input_iter: Peekable<std::str::CharIndices<'a>>,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            done: false,
            len: input.len(),
            start_byte: 0,
            input,
            input_iter: input.char_indices().peekable(),
        }
    }
}
impl<'a> Lexer<'a> {
    /// Make a token from the given token type, or an error.
    fn wrap_token_result(
        &mut self,
        result: Result<Token, Error>,
    ) -> Result<(usize, Token, usize), (usize, Error, usize)> {
        match result {
            Ok(t) => Ok((self.start_byte, t, self.next_pos())),
            Err(e) => Err((self.start_byte, e, self.next_pos())),
        }
    }
    fn next_pos(&mut self) -> usize {
        let last_pos = self.len;
        self.input_iter.peek().map_or_else(|| last_pos, |x| x.0)
    }
    fn advance_while(&mut self, check: &dyn Fn(char) -> bool) {
        while let Some((_, c)) = self.input_iter.peek() {
            if check(*c) {
                self.input_iter.next();
            } else {
                break;
            }
        }
    }
    fn advance_if_next_is(&mut self, c: char) -> bool {
        if let Some((_, next_char)) = self.input_iter.peek() {
            if *next_char == c {
                self.input_iter.next();
                return true;
            }
        }
        false
    }
    fn process_equal(&mut self) -> Result<Token, Error> {
        if self.advance_if_next_is('=') {
            self.input_iter.next();
            return Ok(Token::DoubleEqual);
        }
        Ok(Token::Equal)
    }
    fn process_slash(&mut self) -> Result<Token, Error> {
        if self.advance_if_next_is('/') {
            self.advance_while(&|c| c != '\n');
            return Ok(Token::Comment);
        }
        Ok(Token::Slash)
    }
    fn process_alpha(&mut self) -> Result<Token, Error> {
        self.advance_while(&is_alphanum_or_underscore);
        let identifier = &self.input[self.start_byte..self.next_pos()];
        return match identifier {
            "let" => Ok(Token::Let),
            "if" => Ok(Token::If),
            "else" => Ok(Token::Else),
            _ => Ok(Token::Identifier),
        };
    }
    fn process_digit(&mut self) -> Result<Token, Error> {
        // TODO handle float
        self.advance_while(&|c| c.is_ascii_digit());
        Ok(Token::Integer)
    }
    fn process_double_quote(&mut self) -> Result<Token, Error> {
        // TODO handle escape
        self.advance_while(&|c| c != '"');
        if let None = self.input_iter.next() {
            Err(Error::UnterminatedString)
        } else {
            Ok(Token::String)
        }
    }
}
impl<'a> Iterator for Lexer<'a> {
    type Item = SpannedToken<Token, usize, Error>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let (start_byte, next_char) = if let Some(x) = self.input_iter.next() {
                x
            } else {
                if self.done {
                    return None
                } else {
                    self.done = true;
                    return Some(Ok((self.len, Token::Eof, self.len)));
                }
            };
            self.start_byte = start_byte;
            let token_result = match next_char {
                '(' => Ok(Token::LParen),
                ')' => Ok(Token::RParen),
                '{' => Ok(Token::LBrace),
                '}' => Ok(Token::RBrace),
                '+' => Ok(Token::Plus),
                '-' => Ok(Token::Minus),
                '=' => self.process_equal(),
                '*' => Ok(Token::Star),
                '/' => self.process_slash(),
                ';' => Ok(Token::Semicolon),
                '<' => Ok(Token::Less),
                '>' => Ok(Token::Greater),
                '"' => self.process_double_quote(),
                c if is_alphanum_or_underscore(c) && !c.is_ascii_digit() => self.process_alpha(),
                c if c.is_ascii_digit() => self.process_digit(),

                //c if is_alpha_or_underscore(c) => self.process_identifier(),
                c if is_whitespace(c) => Ok(Token::Whitespace),
                _ => Err(Error::UnexpectedToken),
            };
            if token_result == Ok(Token::Whitespace) || token_result == Ok(Token::Comment) {
                continue;
            }
            return Some(self.wrap_token_result(token_result));
        }
    }
}
#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn simple() {
        let r = Lexer::new("let xbi = * if;").collect::<Vec<_>>();
        assert_eq!(
            r,
            vec![
                Ok((0, Token::Let, 3)),
                Ok((4, Token::Identifier, 7)),
                Ok((8, Token::Equal, 9)),
                Ok((10, Token::Star, 11)),
                Ok((12, Token::If, 14)),
                Ok((14, Token::Semicolon, 15)),
                Ok((15, Token::Eof, 15)),
            ]
        );
    }
    #[test]
    fn empty_str() {
        let r = Lexer::new("\"\"").collect::<Vec<_>>();
        assert_eq!(r, vec![Ok((0, Token::String, 2)), Ok((2, Token::Eof, 2))]);
    }
    #[test]
    fn empty() {
        let r = Lexer::new("").collect::<Vec<_>>();
        assert_eq!(r, vec![Ok((0, Token::Eof, 0))]);
    }
    #[test]
    fn arithmetic() {
        let r = Lexer::new("(5 + 35)/7").collect::<Vec<_>>();
        assert_eq!(
            r,
            vec![
                Ok((0, Token::LParen, 1)),
                Ok((1, Token::Integer, 2)),
                Ok((3, Token::Plus, 4)),
                Ok((5, Token::Integer, 7)),
                Ok((7, Token::RParen, 8)),
                Ok((8, Token::Slash, 9)),
                Ok((9, Token::Integer, 10)),
                Ok((10, Token::Eof, 10)),
            ]
        );
    }
    #[test]
    fn comment() {
        let r = Lexer::new("51 //jeff\n\"y&\"").collect::<Vec<_>>();
        assert_eq!(
            r,
            vec![Ok((0, Token::Integer, 2)), Ok((10, Token::String, 14)), Ok((14, Token::Eof, 14))]
        );
    }
    #[test]
    fn unterminated_string() {
        let r = Lexer::new("let xb_j = \"bo").collect::<Vec<_>>();
        assert_eq!(
            r,
            vec![
                Ok((0, Token::Let, 3)),
                Ok((4, Token::Identifier, 8)),
                Ok((9, Token::Equal, 10)),
                Err((11, Error::UnterminatedString, 14)),
                Ok((14, Token::Eof, 14)),
            ]
        );
    }
}
