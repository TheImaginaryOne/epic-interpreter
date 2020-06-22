#[derive(Debug, PartialEq)]
pub struct Spanned<T> {
    pub inner: T,
    pub left: usize,
    pub right: usize,
}
impl<T> Spanned<T> {
    pub fn new(left: usize, inner: T, right: usize) -> Self {
        Self { left, inner, right }
    }
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
    // todo
}
///
#[derive(Debug, PartialEq)]
pub enum Statement {
    LetDecl(Spanned<Identifier>, Spanned<Expr>),
    ExprStmt(Spanned<Expr>),
}
#[derive(Debug, PartialEq)]
pub enum Expr {
    // TODO THIS MIGHT CHANGE LATER
    Identifier(Identifier),
    Assign(Spanned<Identifier>, Box<Spanned<Expr>>),
    Binary(Box<Spanned<Expr>>, Spanned<BinaryOp>, Box<Spanned<Expr>>),
    Unary(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i32),
}
#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Neg,
}
#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Mul,
    Div,
    Add,
    Sub,
    Eq,
    Gt,
    Lt,
}

pub fn unary(left: usize, op: Spanned<UnaryOp>, e1: Spanned<Expr>, right: usize) -> Spanned<Expr> {
    Spanned {
        left,
        right,
        inner: Expr::Unary(op, Box::new(e1)),
    }
}
pub fn binary(
    left: usize,
    e1: Spanned<Expr>,
    op: Spanned<BinaryOp>,
    e2: Spanned<Expr>,
    right: usize,
) -> Spanned<Expr> {
    Spanned {
        left,
        right,
        inner: Expr::Binary(Box::new(e1), op, Box::new(e2)),
    }
}
pub fn literal(left: usize, literal: Literal, right: usize) -> Spanned<Expr> {
    Spanned {
        left,
        right,
        inner: Expr::Literal(literal),
    }
}
pub fn assign(
    left: usize,
    identifier: Spanned<Identifier>,
    e2: Spanned<Expr>,
    right: usize,
) -> Spanned<Expr> {
    Spanned {
        left,
        right,
        inner: Expr::Assign(identifier, Box::new(e2)),
    }
}
