use crate::compiler::lexer::Token;

pub fn token_to_string(token: Token) -> String {
    match token {
        Token::RParen => ")",
        Token::Semicolon => ";",
        Token::Eof => "end of file",
        _ => "",
    }
    .to_string()
}
