use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    UnexpectedEOF,
    UnexpectedToken,
    UnterminatedString,

    CannotParseInteger,
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("cannot parse integer, out of range?")]
    CannotParseInteger,
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("lex error")]
    LexError(Error),
    #[error("unexpected end of file")]
    UnexpectedEOF,
}
