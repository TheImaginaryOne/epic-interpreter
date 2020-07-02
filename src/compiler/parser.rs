use std::iter::Peekable;
use std::str::FromStr;

use crate::compiler::ast::{
    assignment, binary, unary, BinaryOp, Block, Expression, Identifier, Literal, Spanned,
    Statement, UnaryOp,
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
    // right bp
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
        // for functions!
        Token::LParen => Some((10, 11)),
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
    /// Produces an identifier from the token,
    /// assuming the token is Token::Identifier.
    fn identifier_from(&self, token_sp: Spanned<Token>) -> Spanned<Identifier> {
        Spanned::new(
            token_sp.left,
            Identifier {
                name: self.source[token_sp.left..token_sp.right].into(),
            },
            token_sp.right,
        )
    }
    pub fn parse_program(&mut self) -> Result<Vec<Spanned<Statement>>, Vec<Spanned<ParseError>>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while self.peek_token().map_or(true, |t| t.inner != Token::Eof) {
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

    fn parse_while(
        &mut self,
        errors: &mut Vec<Spanned<ParseError>>,
    ) -> Result<Spanned<Statement>, Spanned<ParseError>> {
        let while_sp = self.next_token().unwrap();

        let left = while_sp.left;

        let condition = self.parse_expr()?;
        let body = self.parse_block(errors)?;

        let right = body.right;

        Ok(Spanned::new(
            left,
            Statement::While(condition, Box::new(body)),
            right,
        ))
    }
    fn parse_if(
        &mut self,
        errors: &mut Vec<Spanned<ParseError>>,
    ) -> Result<Spanned<Statement>, Spanned<ParseError>> {
        // Consume the if
        let if_sp = self.next_token().unwrap();

        let left = if_sp.left;

        let mut then_clauses = Vec::new();
        let condition = self.parse_expr()?;
        let then_block = self.parse_block(errors)?;

        let mut right = then_block.right;
        let mut else_clause = None;
        then_clauses.push((condition, Box::new(then_block)));

        if self.peek_token().map_or(false, |t| t.inner == Token::Else) {
            self.lexer.next();
            loop {
                // there may be other "else if" blocks
                if self.peek_token().map_or(false, |t| t.inner == Token::If) {
                    self.lexer.next();

                    let condition = self.parse_expr()?;
                    let then = self.parse_block(errors)?;
                    then_clauses.push((condition, Box::new(then)));
                    self.expect_token(Token::Else)?;
                } else {
                    // else block
                    let block = self.parse_block(errors)?;
                    right = block.right;
                    else_clause = Some(Box::new(block));
                    break;
                }
            }
        }
        Ok(Spanned::new(
            left,
            Statement::IfElse(then_clauses, else_clause),
            right,
        ))
    }

    fn parse_block(
        &mut self,
        errors: &mut Vec<Spanned<ParseError>>,
    ) -> Result<Spanned<Block>, Spanned<ParseError>> {
        // this should be a LBrace
        let lbrace_sp = self.next_token().unwrap();
        let mut statements = Vec::new();

        while self
            .peek_token()
            .map(|t| t.inner)
            .map_or(true, |t| t != Token::Eof && t != Token::RBrace)
        {
            if let Ok(s) = self.parse_statement(errors) {
                statements.push(s);
            }
        }
        let rbrace_sp = self.expect_token(Token::RBrace)?;
        Ok(Spanned::new(
            lbrace_sp.left,
            Block(statements),
            rbrace_sp.right,
        ))
    }
    fn parse_let(&mut self) -> Result<Spanned<Statement>, Spanned<ParseError>> {
        let let_sp = self.next_token().unwrap();

        let identifier = self.expect_token(Token::Identifier)?;
        self.expect_token(Token::Equal)?;

        let expr = self.parse_expr()?;

        let (id_left, id_right) = (identifier.left, identifier.right);
        let expr_right = expr.right;
        self.expect_token(Token::Semicolon)?;
        Ok(Spanned::new(
            let_sp.left,
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
    fn parse_function(
        &mut self,
        errors: &mut Vec<Spanned<ParseError>>,
    ) -> Result<Spanned<Statement>, Spanned<ParseError>> {
        let fun_sp = self.next_token().unwrap();
        let left = fun_sp.left;
        let mut identifiers = Vec::new();

        let name = self.expect_token(Token::Identifier)?;
        self.expect_token(Token::LParen)?;

        // parameters
        loop {
            let parameter_sp = self.expect_token(Token::Identifier)?;
            identifiers.push(parameter_sp);
            if self.peek_token()?.inner == Token::RParen {
                self.lexer.next();
                break;
            }
            self.expect_token(Token::Comma)?;
        }
        dbg!(&self.peek_token());
        let body = self.parse_block(errors)?;

        let right = body.right;

        Ok(Spanned::new(
            left,
            Statement::Function {
                name: self.identifier_from(name),
                arguments: identifiers
                    .into_iter()
                    .map(|i| self.identifier_from(i))
                    .collect(),
                body: Box::new(body),
            },
            right,
        ))
    }
    fn parse_fun_call(
        &mut self,
        left_expr_sp: Spanned<Expression>,
    ) -> Result<Spanned<Expression>, Spanned<ParseError>> {
        if let Expression::Identifier(ident) = left_expr_sp.inner {
            let mut arguments = Vec::new();
            // <ident> "(" already consumed
            let right = loop {
                let expr_sp = self.parse_expr()?;
                arguments.push(Box::new(expr_sp));
                if self.peek_token()?.inner == Token::RParen {
                    break self.next_token()?.left;
                }
                self.expect_token(Token::Comma)?;
            };
            Ok(Spanned::new(
                left_expr_sp.left,
                Expression::FunctionCall(
                    Spanned::new(left_expr_sp.left, ident, left_expr_sp.right),
                    arguments,
                ),
                right,
            ))
        } else {
            Err(Spanned::new(
                left_expr_sp.left,
                ParseError::InvalidFunctionCall,
                left_expr_sp.right,
            ))
        }
    }

    /// Recover if there is an error
    fn synchronise(&mut self) {
        loop {
            let token_result = self.peek_token().map(|t| t.inner);
            if let Ok(token_sp) = token_result {
                match token_sp {
                    Token::Eof | Token::Let | Token::LBrace | Token::Identifier | Token::If => {
                        break
                    }
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
                Token::LBrace => {
                    let block_sp = self.parse_block(errors)?;
                    Ok(Spanned::new(
                        block_sp.left,
                        Statement::Block(block_sp.inner),
                        block_sp.right,
                    ))
                }
                Token::While => self.parse_while(errors),
                Token::Fun => self.parse_function(errors),
                Token::If => self.parse_if(errors),
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
        let token_sp = self.peek_token()?;
        if token_sp.inner != t {
            let message = token_to_string(t);
            return Err(Spanned::new(
                token_sp.left,
                ParseError::UnexpectedToken(token_sp.inner, message),
                token_sp.right,
            ));
        }
        self.lexer.next();
        Ok(token_sp)
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

                if infix_token_sp.inner == Token::LParen {
                    left_expr_sp = self.parse_fun_call(left_expr_sp)?;
                    continue;
                }
                // recursively call
                let right_expr_sp = self.parse_expr_binding_power(right_bp)?;

                let (l, r) = (left_expr_sp.left, right_expr_sp.right);

                if infix_token_sp.inner == Token::Equal {
                    // case for expressions like x = 889 + 77
                    if let Expression::Identifier(i) = left_expr_sp.inner {
                        let right = right_expr_sp.right;
                        left_expr_sp = assignment(
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
