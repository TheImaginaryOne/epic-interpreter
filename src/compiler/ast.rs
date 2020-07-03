#[derive(Debug, PartialEq, Clone)]
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
#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Spanned<Statement>>);

#[derive(Debug, PartialEq)]
pub enum Statement {
    // let declaration
    LetBinding(Spanned<Identifier>, Spanned<Expression>),
    Expression(Spanned<Expression>),
    Return(Option<Spanned<Expression>>),
    // TODO modify later!
    Block(Block),
    IfElse(
        Vec<(Spanned<Expression>, Box<Spanned<Block>>)>,
        Option<Box<Spanned<Block>>>,
    ),
    While(Spanned<Expression>, Box<Spanned<Block>>),
    Function {
        name: Spanned<Identifier>,
        arguments: Vec<Spanned<Identifier>>,
        body: Box<Spanned<Block>>,
    },
}
#[derive(Debug, PartialEq)]
pub enum Expression {
    // TODO THIS MIGHT CHANGE LATER
    Identifier(Identifier),
    Assignment(Spanned<Identifier>, Box<Spanned<Expression>>),
    FunctionCall(Spanned<Identifier>, Vec<Box<Spanned<Expression>>>),
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
pub fn assignment(
    left: usize,
    identifier: Spanned<Identifier>,
    e2: Spanned<Expression>,
    right: usize,
) -> Spanned<Expression> {
    Spanned {
        left,
        right,
        inner: Expression::Assignment(identifier, Box::new(e2)),
    }
}
