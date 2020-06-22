/// Utility functions for running tests
use crate::compiler::ast::*;
use crate::compiler::grammar;
use crate::vm::chunk::*;
use lalrpop_util::*;
pub fn clear_span(e: &mut Spanned<Expr>) {
    e.left = 0;
    e.right = 0;
    match &mut e.inner {
        Expr::Binary(e1, o, e2) => {
            clear_span(e1);
            clear_span(e2);
            // TODO!
            o.left = 0;
            o.right = 0;
        }
        Expr::Unary(o, e) => {
            clear_span(e);
            o.left = 0;
            o.right = 0;
        }
        Expr::Assign(i, e) => {
            i.left = 0;
            i.right = 0;
            clear_span(e);
        }
        _ => (),
    }
}
pub fn parse_program(s: &str) -> Vec<Statement> {
    let mut p = grammar::ProgramParser::new().parse(s).unwrap();
    for s in &mut p {
        match s {
            Statement::ExprStmt(e) => {
                clear_span(e);
            }
            Statement::LetDecl(i, e) => {
                i.left = 0;
                i.right = 0;
                clear_span(e);
            }
        }
    }
    p
}
pub fn parse_dbg(s: &str) -> Spanned<Expr> {
    let mut e = grammar::ExprParser::new().parse(s).unwrap();
    clear_span(&mut e);
    e
}
pub fn parse_err(s: &str) -> ParseError<usize, lexer::Token<'_>, &str> {
    grammar::ExprParser::new().parse(s).unwrap_err()
}
pub fn dummy_span<T>(e: T) -> Spanned<T> {
    Spanned {
        left: 0,
        right: 0,
        inner: e,
    }
}
pub fn un(op_str: &str, e1: Spanned<Expr>) -> Spanned<Expr> {
    let op = match op_str {
        "-" => UnaryOp::Neg,
        _ => unimplemented!(),
    };
    Spanned {
        left: 0,
        right: 0,
        inner: Expr::Unary(dummy_span(op), Box::new(e1)),
    }
}
pub fn asgn(e1: Spanned<Identifier>, e2: Spanned<Expr>) -> Spanned<Expr> {
    Spanned {
        left: 0,
        right: 0,
        inner: Expr::Assign(e1, Box::new(e2)),
    }
}
pub fn bin(e1: Spanned<Expr>, op_str: &str, e2: Spanned<Expr>) -> Spanned<Expr> {
    let op = match op_str {
        "+" => BinaryOp::Add,
        "==" => BinaryOp::Eq,
        "-" => BinaryOp::Sub,
        "*" => BinaryOp::Mul,
        "/" => BinaryOp::Div,
        _ => unimplemented!(),
    };
    Spanned {
        left: 0,
        right: 0,
        inner: Expr::Binary(Box::new(e1), dummy_span(op), Box::new(e2)),
    }
}
pub fn int(i: i32) -> Spanned<Expr> {
    dummy_span(Expr::Literal(Literal::Int(i)))
}
pub fn id(s: &str) -> Spanned<Identifier> {
    dummy_span(Identifier { name: s.into() })
}
pub fn expr_id(s: &str) -> Spanned<Expr> {
    dummy_span(Expr::Identifier(Identifier { name: s.into() }))
}

// chunk utility
pub fn chunk(ints: Vec<i32>, instrs: Vec<Instruction>) -> Chunk {
    let mut c = Chunk::new();
    c.values = ints.iter().map(|x| Value::Int(*x)).collect();
    for i in instrs {
        c.write_instr(i)
    }
    c
}
