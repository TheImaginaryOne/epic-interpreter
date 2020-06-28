/// Utility functions for running tests
use crate::compiler::ast::*;
use crate::compiler::error::ParseError;
use crate::compiler::lexer::{Lexer, Token};
use crate::compiler::parser::Parser;
use crate::vm::chunk::*;
pub fn clear_expr_span(e: &mut Spanned<Expression>) {
    e.left = 0;
    e.right = 0;
    match &mut e.inner {
        Expression::Binary(e1, o, e2) => {
            clear_expr_span(e1);
            clear_expr_span(e2);
            // TODO!
            o.left = 0;
            o.right = 0;
        }
        Expression::Unary(o, e) => {
            clear_expr_span(e);
            o.left = 0;
            o.right = 0;
        }
        Expression::Assign(i, e) => {
            i.left = 0;
            i.right = 0;
            clear_expr_span(e);
        }
        _ => (),
    }
}
pub fn clear_stmt_span(stmt: &mut Spanned<Statement>) {
    stmt.left = 0;
    stmt.right = 0;
    match &mut stmt.inner {
        Statement::Expression(e) => {
            clear_expr_span(e);
        }
        Statement::LetBinding(i, e) => {
            i.left = 0;
            i.right = 0;
            clear_expr_span(e);
        }
        Statement::Block(block) => {
            for stmt in &mut block.statements {
                clear_stmt_span(stmt);
            }
        }
        Statement::IfElse(i) => {
            let IfElse {
                then_clauses,
                else_clause,
            } = i.as_mut();
            for (cond, block) in then_clauses {
                clear_expr_span(cond);
                block.left = 0;
                block.right = 0;
                for stmt in &mut block.inner.statements {
                    clear_stmt_span(stmt);
                }
            }
            if let Some(b) = else_clause {
                b.left = 0;
                b.right = 0;
                for stmt in &mut b.inner.statements {
                    clear_stmt_span(stmt);
                }
            }
        }
    }
}
pub fn parse_program_error(source: &str) -> Vec<Spanned<ParseError>> {
    Parser::new(source).parse_program().unwrap_err()
}
pub fn parse_program(source: &str) -> Vec<Spanned<Statement>> {
    let mut p = Parser::new(source).parse_program().unwrap();
    for s in &mut p {
        clear_stmt_span(s);
    }
    p
}
pub fn parse_dbg(source: &str) -> Spanned<Expression> {
    let mut e = Parser::new(source).parse_expr().unwrap();
    clear_expr_span(&mut e);
    e
}
pub fn parse_err(s: &str) -> Spanned<ParseError> {
    let tokens = Lexer::new(s);
    Parser::new(s).parse_expr().unwrap_err()
}
pub fn dummy_span<T>(e: T) -> Spanned<T> {
    Spanned {
        left: 0,
        right: 0,
        inner: e,
    }
}

pub fn expr_stmt(expr: Spanned<Expression>) -> Spanned<Statement> {
    dummy_span(Statement::Expression(expr))
}
pub fn let_stmt(id: Spanned<Identifier>, expr: Spanned<Expression>) -> Spanned<Statement> {
    dummy_span(Statement::LetBinding(id, expr))
}
pub fn if_stmt(t: Vec<(Spanned<Expression>, Spanned<Block>)>) -> Spanned<Statement> {
    dummy_span(Statement::IfElse(Box::new(IfElse {
        then_clauses: t,
        else_clause: None,
    })))
}
pub fn if_else_stmt(
    t: Vec<(Spanned<Expression>, Spanned<Block>)>,
    e: Spanned<Block>,
) -> Spanned<Statement> {
    dummy_span(Statement::IfElse(Box::new(IfElse {
        then_clauses: t,
        else_clause: Some(e),
    })))
}

pub fn un(op_str: &str, e1: Spanned<Expression>) -> Spanned<Expression> {
    let op = match op_str {
        "-" => UnaryOp::Negate,
        _ => unimplemented!(),
    };
    Spanned {
        left: 0,
        right: 0,
        inner: Expression::Unary(dummy_span(op), Box::new(e1)),
    }
}
pub fn asgn(e1: Spanned<Identifier>, e2: Spanned<Expression>) -> Spanned<Expression> {
    Spanned {
        left: 0,
        right: 0,
        inner: Expression::Assign(e1, Box::new(e2)),
    }
}
pub fn bin(e1: Spanned<Expression>, op_str: &str, e2: Spanned<Expression>) -> Spanned<Expression> {
    let op = match op_str {
        "+" => BinaryOp::Add,
        "==" => BinaryOp::Equal,
        "-" => BinaryOp::Subtract,
        "*" => BinaryOp::Multiply,
        "/" => BinaryOp::Divide,
        ">" => BinaryOp::Greater,
        "<" => BinaryOp::Less,
        _ => unimplemented!(),
    };
    Spanned {
        left: 0,
        right: 0,
        inner: Expression::Binary(Box::new(e1), dummy_span(op), Box::new(e2)),
    }
}
pub fn int(i: i32) -> Spanned<Expression> {
    dummy_span(Expression::Literal(Literal::Integer(i)))
}
pub fn string(s: &str) -> Spanned<Expression> {
    dummy_span(Expression::Literal(Literal::String(s.into())))
}
pub fn id(s: &str) -> Spanned<Identifier> {
    dummy_span(Identifier { name: s.into() })
}
pub fn expr_id(s: &str) -> Spanned<Expression> {
    dummy_span(Expression::Identifier(Identifier { name: s.into() }))
}
pub fn block_stmt(statements: Vec<Spanned<Statement>>) -> Spanned<Statement> {
    dummy_span(Statement::Block(Block { statements }))
}
pub fn block(statements: Vec<Spanned<Statement>>) -> Spanned<Block> {
    dummy_span(Block { statements })
}

// chunk utility
pub fn chunk(ints: Vec<i32>, instrs: Vec<Instruction>) -> Chunk {
    let mut c = Chunk::new();
    c.values = ints.iter().map(|x| Value::Integer(*x)).collect();
    for i in instrs {
        c.write_instr(i);
    }
    c
}
