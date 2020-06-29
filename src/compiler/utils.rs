use crate::compiler::lexer::Token;

pub fn token_to_string(token: Token) -> String {
    match token {
        Token::String => "<string>",
        Token::Integer => "<integer>",
        Token::Identifier => "<identifier>",

        Token::Plus => "+",
        Token::Minus => "-",
        Token::Star => "*",
        Token::Slash => "/",
        Token::LParen => "(",
        Token::RParen => ")",
        Token::LBrace => "{",
        Token::RBrace => "}",
        Token::Semicolon => ";",

        Token::Equal => "=",
        Token::DoubleEqual => "==",
        Token::Less => "<",
        Token::Greater => ">",
        Token::Let => "let",
        Token::If => "if",
        Token::Else => "else",
        Token::Eof => "end of file",
        _ => " "
    }
    .to_string()
}
