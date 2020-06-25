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

/// a special block for if/else/else if statements
#[derive(Debug, PartialEq)]
pub struct IfElse {
    pub then_clauses: Vec<(Spanned<Expression>, Spanned<Block>)>,
    pub else_clause: Option<Spanned<Block>>,
}
#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub name: String,
    // todo
}
#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Spanned<Statement>>,
}
///
#[derive(Debug, PartialEq)]
pub enum Statement {
    // let declaration
    LetBinding(Spanned<Identifier>, Spanned<Expression>),
    Expression(Spanned<Expression>),
    // TODO modify later!
    Block(Block),
    IfElse(Box<IfElse>),
}
#[derive(Debug, PartialEq)]
pub enum Expression {
    // TODO THIS MIGHT CHANGE LATER
    Identifier(Identifier),
    Assign(Spanned<Identifier>, Box<Spanned<Expression>>),
    Binary(
        Box<Spanned<Expression>>,
        Spanned<BinaryOp>,
        Box<Spanned<Expression>>,
    ),
    Unary(Spanned<UnaryOp>, Box<Spanned<Expression>>),
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i32),
    String(String),
}
#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
}
#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Multiply,
    Divide,
    Add,
    Subtract,
    Equal,
    Greater,
    Less,
}

pub fn unary(
    left: usize,
    op: Spanned<UnaryOp>,
    e1: Spanned<Expression>,
    right: usize,
) -> Spanned<Expression> {
    Spanned {
        left,
        right,
        inner: Expression::Unary(op, Box::new(e1)),
    }
}
pub fn binary(
    left: usize,
    e1: Spanned<Expression>,
    op: Spanned<BinaryOp>,
    e2: Spanned<Expression>,
    right: usize,
) -> Spanned<Expression> {
    Spanned {
        left,
        right,
        inner: Expression::Binary(Box::new(e1), op, Box::new(e2)),
    }
}
pub fn literal(left: usize, literal: Literal, right: usize) -> Spanned<Expression> {
    Spanned {
        left,
        right,
        inner: Expression::Literal(literal),
    }
}
pub fn assign(
    left: usize,
    identifier: Spanned<Identifier>,
    e2: Spanned<Expression>,
    right: usize,
) -> Spanned<Expression> {
    Spanned {
        left,
        right,
        inner: Expression::Assign(identifier, Box::new(e2)),
    }
}
