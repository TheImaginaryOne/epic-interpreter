#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedEOF,
    UnexpectedToken,
    UnterminatedString,

    CannotParseInteger,
}
