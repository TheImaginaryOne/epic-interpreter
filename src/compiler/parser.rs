use std::iter::Peekable;
use std::str::FromStr;

use crate::compiler::ast::{BinaryOp, Expression, Literal, Spanned};
use crate::compiler::error::ParseError;
use crate::compiler::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}
pub fn infix_op(token: Token) -> Option<BinaryOp> {
    Some(match token {
        Token::DoubleEqual => BinaryOp::Equal,
        Token::Less => BinaryOp::Less,
        Token::Greater => BinaryOp::Greater,
        Token::Plus => BinaryOp::Add,
        Token::Minus => BinaryOp::Subtract,
        Token::Star => BinaryOp::Multiply,
        Token::Slash => BinaryOp::Divide,
        _ => return None,
    })
}
pub fn infix_binding_power(token: Token) -> Option<(u8, u8)> {
    // (left bp, right bp)
    match token {
        Token::DoubleEqual => Some((0, 1)),
        // todo add || and &&
        Token::Less | Token::Greater => Some((4, 5)),
        Token::Plus | Token::Minus => Some((6, 7)),
        Token::Star | Token::Slash => Some((8, 9)),
        _ => None,
    }
}
#[allow(dead_code)]
impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: Lexer::new(source).peekable(),
        }
    }
    pub fn parse_program(&mut self) -> Result<(), Vec<()>> {
        Ok(())
    }
    /// Only use for testing
    pub fn parse_expr(&mut self) -> Result<Spanned<Expression>, Spanned<ParseError>> {
        self.expr_binding_power(0)
    }

    fn expr_binding_power(
        &mut self,
        min_bp: u8,
    ) -> Result<Spanned<Expression>, Spanned<ParseError>> {
        let unary = match self.lexer.next() {
            None => panic!("nani?!"),
            Some(u) => u.map_err(|e| Spanned::new(e.0, ParseError::LexError(e.1), e.2))?,
        };
        let mut left_expr = match unary.1 {
            Token::Integer(s) => i32::from_str(s)
                .map_err(|_| Spanned::new(unary.0, ParseError::CannotParseInteger, unary.2))
                .and_then(|x| {
                    Ok(Spanned::new(
                        unary.0,
                        Expression::Literal(Literal::Integer(x)),
                        unary.2,
                    ))
                })?,
            _ => return Err(Spanned::new(unary.0, ParseError::UnexpectedToken, unary.2)),
        };

        loop {
            let infix_token = match self.lexer.peek() {
                None => break,
                Some(u) => u
                    .clone()
                    .map_err(|e| Spanned::new(e.0, ParseError::LexError(e.1.clone()), e.2))?,
            };
            let (lbp, rbp) = infix_binding_power(infix_token.1).ok_or_else(|| {
                Spanned::new(infix_token.0, ParseError::UnexpectedToken, infix_token.2)
            })?;
            if lbp < min_bp {
                break;
            }
            // consume
            self.lexer.next();
            let right_expr = self.expr_binding_power(rbp)?;

            let (l, r) = (left_expr.left, right_expr.right);
            left_expr = Spanned::new(
                l,
                Expression::Binary(
                    Box::new(left_expr),
                    Spanned::new(
                        infix_token.0,
                        infix_op(infix_token.1).unwrap(),
                        infix_token.2,
                    ),
                    Box::new(right_expr),
                ),
                r,
            );
        }

        Ok(left_expr)
    }
}
