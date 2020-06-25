/// Utility functions for running tests
use crate::compiler::ast::*;
use crate::compiler::error::Error;
use crate::compiler::grammar;
use crate::compiler::lexer::{Lexer, Token};
use crate::vm::chunk::*;
use lalrpop_util::*;
pub fn clear_span(e: &mut Spanned<Expression>) {
    e.left = 0;
    e.right = 0;
    match &mut e.inner {
        Expression::Binary(e1, o, e2) => {
            clear_span(e1);
            clear_span(e2);
            // TODO!
            o.left = 0;
            o.right = 0;
        }
        Expression::Unary(o, e) => {
            clear_span(e);
            o.left = 0;
            o.right = 0;
        }
        Expression::Assign(i, e) => {
            i.left = 0;
            i.right = 0;
            clear_span(e);
        }
        _ => (),
    }
}
pub fn parse_program(source: &str) -> Vec<Spanned<Statement>> {
    let tokens = Lexer::new(source);
    let mut p = grammar::ProgramParser::new().parse(source, tokens).unwrap();
    for s in &mut p {
        s.left = 0;
        s.right = 0;
        match &mut s.inner {
            Statement::Expression(e) => {
                clear_span(e);
            }
            Statement::LetBinding(i, e) => {
                i.left = 0;
                i.right = 0;
                clear_span(e);
            }
        }
    }
    p
}
pub fn parse_dbg(s: &str) -> Spanned<Expression> {
    let tokens = Lexer::new(s);
    let t = Lexer::new(s);
    println!("{:?}", t.collect::<Vec<_>>());
    let mut e = grammar::ExpressionParser::new().parse(s, tokens).unwrap();
    clear_span(&mut e);
    e
}
pub fn parse_err(s: &str) -> ParseError<usize, Token<'_>, (usize, Error, usize)> {
    let tokens = Lexer::new(s);
    grammar::ExpressionParser::new()
        .parse(s, tokens)
        .unwrap_err()
}
pub fn dummy_span<T>(e: T) -> Spanned<T> {
    Spanned {
        left: 0,
        right: 0,
        inner: e,
    }
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

// chunk utility
pub fn chunk(ints: Vec<i32>, instrs: Vec<Instruction>) -> Chunk {
    let mut c = Chunk::new();
    c.values = ints.iter().map(|x| Value::Integer(*x)).collect();
    for i in instrs {
        c.write_instr(i)
    }
    c
}
