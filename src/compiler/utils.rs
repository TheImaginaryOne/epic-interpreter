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
        Token::Return => "return",
        Token::While => "while",
        Token::Fun => "fun",
        Token::Comma => "`,`",
        Token::Eof => "end of file",

        // these will not appear in error messages
        Token::Whitespace | Token::Comment => "",
    }
    .to_string()
}
