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
            if let Ok(s) = self.parse_statement(&mut errors) {
                statements.push(s);
            }
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(statements)
        }
    }
    fn parse_let(&mut self) -> Result<Spanned<Statement>, Spanned<ParseError>> {
        self.lexer.next();
        
        let identifier = self.expect_token(Token::Identifier)?;
        self.expect_token(Token::Equal)?;

        let expr = self.parse_expr()?;

        let (id_left, id_right) = (identifier.left, identifier.right);
        let expr_right = expr.right;
        self.expect_token(Token::Semicolon)?;
        Ok(Spanned::new(
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
        ))
    }

    /// Recover if there is an error
    fn synchronise(&mut self) {
        loop {
            let token_result = self.peek_token().map(|t| t.inner);
            if let Ok(token_sp) = token_result {
                match token_sp {
                    Token::Eof | Token::Let/* | Token::If */ => break,
                    Token::Semicolon => {
                        self.lexer.next();
                        break;
                    }
                    _ => (),
                }
            }
            self.lexer.next();
        }
    }
    pub fn parse_statement(
        &mut self,
        errors: &mut Vec<Spanned<ParseError>>,
    ) -> Result<Spanned<Statement>, ()> {
        let stmt = self.peek_token().and_then(|t| {
            match t.inner {
                Token::Let => self.parse_let(),
                // expression statement
                _ => self.parse_expr().and_then(|expr| {
                    let r = expr.right;
                    self.expect_token(Token::Semicolon)?;
                    Ok(Spanned::new(expr.left, Statement::Expression(expr), r))
                }),
            }
        });
        match stmt {
            Ok(s) => return Ok(s),
            Err(e) => {
                errors.push(e);
                self.synchronise();
                // todo error synchronise
                return Err(());
            }
        }
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

    /// If the token is the required one, it is consumed, otherwise the function returns an error.
    fn expect_token(&mut self, t: Token) -> Result<Spanned<Token>, Spanned<ParseError>> {
        let spanned_token = self.peek_token()?;
        if spanned_token.inner != t {
            let message = token_to_string(t);
            return Err(Spanned::new(
                spanned_token.left,
                ParseError::UnexpectedToken(spanned_token.inner, message),
                spanned_token.right,
            ));
        }
        self.lexer.next();
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
        // we should not advance the lexer if there is
        // actually an error
        let unary_token_sp = self.peek_token()?;

        let mut left_expr_sp = match unary_token_sp.inner {
            Token::Integer => {
                self.lexer.next();
                self.parse_integer(unary_token_sp)?
            }
            Token::Minus => {
                self.lexer.next();
                self.parse_unary(unary_token_sp)?
            }
            Token::LParen => {
                self.lexer.next();
                let expr = self.parse_expr_binding_power(0)?;
                self.expect_token(Token::RParen)?;
                expr
            }
            Token::String => {
                self.lexer.next();
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
                self.lexer.next();
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
