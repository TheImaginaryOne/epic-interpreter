use thiserror::Error;

use crate::compiler::lexer::Token;
use crate::compiler::utils::token_to_string;

// TODO rename
#[derive(Debug, PartialEq, Clone, Error)]
pub enum Error {
    #[error("unexpected token")]
    UnexpectedToken,
    #[error("unterminated string")]
    UnterminatedString,
    // TODO remove
    #[error("")]
    CannotParseInteger,
}

#[derive(Debug, PartialEq, Error)]
pub enum ParseError {
    #[error("cannot parse integer, out of range?")]
    CannotParseInteger,
    // note an EOF is also considered a token
    #[error("unexpected {}, expected {1}", token_to_string(*.0))]
    UnexpectedToken(Token, String),
    #[error("lex error: {0}")]
    LexError(#[from] Error),
    #[error("invalid assignment target")]
    InvalidAssignment,
}
