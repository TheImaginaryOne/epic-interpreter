use std::iter::Peekable;
use std::str::FromStr;

use crate::compiler::ast::{
    assign, binary, unary, BinaryOp, Expression, Identifier, Literal, Spanned, Statement, UnaryOp,
};
use crate::compiler::error::ParseError;
use crate::compiler::lexer::{Lexer, Token};
use crate::compiler::utils::token_to_string;

pub struct Parser<'a> {
    source: &'a str,
    lexer: Peekable<Lexer<'a>>,
}
pub fn prefix_op(token: Token) -> Option<UnaryOp> {
    Some(match token {
        Token::Minus => UnaryOp::Negate,
        _ => return None,
    })
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
pub fn prefix_binding_power(token: Token) -> Option<u8> {
    // (left bp, right bp)
    match token {
        Token::Minus => Some(8),
        _ => None,
    }
}
pub fn infix_binding_power(token: Token) -> Option<(u8, u8)> {
    // (left bp, right bp)
    match token {
        Token::Equal => Some((0, 1)),
        Token::DoubleEqual => Some((2, 3)),
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
            source,
            lexer: Lexer::new(source).peekable(),
        }
    }
    pub fn parse_program(&mut self) -> Result<Vec<Spanned<Statement>>, Vec<Spanned<ParseError>>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        loop {
            if let Ok(token_sp) = self.lexer.peek().unwrap() {
                if token_sp.inner == Token::Eof {
                    break;
                }
            }

            match self.parse_statement() {
                Ok(s) => statements.push(s),
                Err(e) => errors.push(e), // todo
            }
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(statements)
        }
    }
    pub fn parse_statement(&mut self) -> Result<Spanned<Statement>, Spanned<ParseError>> {
        let token_sp = self.peek_token()?;

        let stmt = match token_sp.inner {
            Token::Let => {
                self.next_token()?;
                let identifier = self.expect_token(Token::Identifier)?;
                self.expect_token(Token::Equal)?;

                let expr = self.parse_expr()?;

                let (id_left, id_right) = (identifier.left, identifier.right);
                let expr_right = expr.right;
                Spanned::new(
                    id_left,
                    Statement::LetBinding(
                        Spanned::new(
                            id_left,
                            Identifier {
                                name: self.source[id_left..id_right].into(),
                            },
                            id_right,
                        ),
                        expr,
                    ),
                    expr_right,
                )
            }
            // expression statement
            _ => self.parse_expr().and_then(|e| {
                let r = e.right;
                Ok(Spanned::new(e.left, Statement::Expression(e), r))
            })?,
        };
        self.expect_token(Token::Semicolon)?;
        Ok(stmt)
    }

    /// Only use for testing
    pub fn parse_expr(&mut self) -> Result<Spanned<Expression>, Spanned<ParseError>> {
        self.parse_expr_binding_power(0)
    }

    fn peek_token(&mut self) -> Result<Spanned<Token>, Spanned<ParseError>> {
        Ok(self
            .lexer
            .peek()
            .expect("parser must not pass EOF")
            .clone()
            .map_err(|e| Spanned::new(e.left, ParseError::LexError(e.inner), e.right))?)
    }
    fn next_token(&mut self) -> Result<Spanned<Token>, Spanned<ParseError>> {
        Ok(self
            .lexer
            .next()
            .expect("parser must not pass EOF")
            .map_err(|e| Spanned::new(e.left, ParseError::LexError(e.inner), e.right))?)
    }

    fn expect_token(&mut self, t: Token) -> Result<Spanned<Token>, Spanned<ParseError>> {
        let spanned_token = self.next_token()?;
        if spanned_token.inner != t {
            let message = token_to_string(t);
            return Err(Spanned::new(
                spanned_token.left,
                ParseError::UnexpectedToken(t, message),
                spanned_token.right,
            ));
        }
        Ok(spanned_token)
    }
    fn parse_integer(&self, s: Spanned<Token>) -> Result<Spanned<Expression>, Spanned<ParseError>> {
        i32::from_str(&self.source[s.left..s.right])
            .map_err(|_| Spanned::new(s.left, ParseError::CannotParseInteger, s.right))
            .and_then(|x| {
                Ok(Spanned::new(
                    s.left,
                    Expression::Literal(Literal::Integer(x)),
                    s.right,
                ))
            })
    }
    fn parse_unary(
        &mut self,
        token_sp: Spanned<Token>,
    ) -> Result<Spanned<Expression>, Spanned<ParseError>> {
        let left_bp = prefix_binding_power(token_sp.inner).unwrap();
        let right_expr_sp = self.parse_expr_binding_power(left_bp)?;

        let (l, r) = (token_sp.left, right_expr_sp.right);
        Ok(unary(
            l,
            Spanned::new(
                token_sp.left,
                prefix_op(token_sp.inner).unwrap(),
                token_sp.right,
            ),
            right_expr_sp,
            r,
        ))
    }

    fn parse_expr_binding_power(
        &mut self,
        min_bp: u8,
    ) -> Result<Spanned<Expression>, Spanned<ParseError>> {
        let unary_token_sp = self.next_token()?;

        let mut left_expr_sp = match unary_token_sp.inner {
            Token::Integer => self.parse_integer(unary_token_sp)?,
            Token::Minus => self.parse_unary(unary_token_sp)?,
            Token::LParen => {
                let expr = self.parse_expr_binding_power(0)?;
                self.expect_token(Token::RParen)?;
                expr
            }
            Token::String => {
                let (id_left, id_right) = (unary_token_sp.left, unary_token_sp.right);
                // TODO escape the string
                Spanned::new(
                    id_left,
                    Expression::Literal(Literal::String(
                        self.source[id_left + 1..id_right - 1].into(),
                    )),
                    id_right,
                )
            }
            Token::Identifier => {
                let (id_left, id_right) = (unary_token_sp.left, unary_token_sp.right);
                Spanned::new(
                    id_left,
                    Expression::Identifier(Identifier {
                        name: self.source[id_left..id_right].into(),
                    }),
                    id_right,
                )
            }
            _ => {
                return Err(Spanned::new(
                    unary_token_sp.left,
                    ParseError::UnexpectedToken(unary_token_sp.inner, "expression".into()),
                    unary_token_sp.right,
                ))
            }
        };

        loop {
            let infix_token_sp = self.peek_token()?;
            if infix_token_sp.inner == Token::Eof {
                break;
            }

            if let Some((left_bp, right_bp)) = infix_binding_power(infix_token_sp.inner) {
                // if left binding power too low
                if left_bp < min_bp {
                    break;
                }
                // consume infix operator
                self.lexer.next();
                // recursively call
                let right_expr_sp = self.parse_expr_binding_power(right_bp)?;

                let (l, r) = (left_expr_sp.left, right_expr_sp.right);

                if infix_token_sp.inner == Token::Equal {
                    // case for expressions like x = 889 + 77
                    if let Expression::Identifier(i) = left_expr_sp.inner {
                        let right = right_expr_sp.right;
                        left_expr_sp = assign(
                            left_expr_sp.left,
                            Spanned::new(left_expr_sp.left, i, left_expr_sp.right),
                            right_expr_sp,
                            right,
                        );
                    } else {
                        return Err(Spanned::new(
                            left_expr_sp.left,
                            ParseError::InvalidAssignment,
                            right_expr_sp.right,
                        ));
                    }
                } else {
                    // construct binary expression
                    left_expr_sp = binary(
                        l,
                        left_expr_sp,
                        Spanned::new(
                            infix_token_sp.left,
                            infix_op(infix_token_sp.inner).unwrap(),
                            infix_token_sp.right,
                        ),
                        right_expr_sp,
                        r,
                    );
                }
            } else {
                // not an infix operator - we can stop
                break;
            }
        }

        Ok(left_expr_sp)
    }
}
