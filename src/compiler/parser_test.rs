/// Tests grammar, including the LALRPOP parser
#[cfg(test)]
mod test {
    use crate::compiler::ast::*;
    use crate::compiler::grammar;
    use lalrpop_util::*;
    fn clear_span(e: &mut Spanned<Expr>) {
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
    fn parse_program(s: &str) -> Vec<Statement> {
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
    fn parse_dbg(s: &str) -> Spanned<Expr> {
        let mut e = grammar::ExprParser::new().parse(s).unwrap();
        clear_span(&mut e);
        e
    }
    fn parse_err(s: &str) -> ParseError<usize, lexer::Token<'_>, &str> {
        grammar::ExprParser::new().parse(s).unwrap_err()
    }
    fn s<T>(e: T) -> Spanned<T> {
        Spanned {
            left: 0,
            right: 0,
            inner: e,
        }
    }
    pub fn un(op: Spanned<UnaryOp>, e1: Spanned<Expr>) -> Spanned<Expr> {
        Spanned {
            left: 0,
            right: 0,
            inner: Expr::Unary(op, Box::new(e1)),
        }
    }
    pub fn asgn(e1: Spanned<Identifier>, e2: Spanned<Expr>) -> Spanned<Expr> {
        Spanned {
            left: 0,
            right: 0,
            inner: Expr::Assign(e1, Box::new(e2)),
        }
    }
    pub fn bin(e1: Spanned<Expr>, op: Spanned<BinaryOp>, e2: Spanned<Expr>) -> Spanned<Expr> {
        Spanned {
            left: 0,
            right: 0,
            inner: Expr::Binary(Box::new(e1), op, Box::new(e2)),
        }
    }
    pub fn lit(literal: Literal) -> Spanned<Expr> {
        Spanned {
            left: 0,
            right: 0,
            inner: Expr::Literal(literal),
        }
    }
    pub fn id(s: &str) -> Identifier {
        Identifier { name: s.into() }
    }
    #[test]
    fn unexpected() {
        let l = parse_err("1 * 3 + /");
        match l {
            ParseError::UnrecognizedToken { token, .. } => {
                // start, end
                assert_eq!(token.0, 8);
                assert_eq!(token.2, 9);
            }
            _ => panic!(),
        }
    }
    #[test]
    fn eof() {
        let l = parse_err("1 * 3 +");
        match l {
            ParseError::UnrecognizedEOF { location, .. } => assert_eq!(location, 7),
            _ => panic!(),
        }
    }
    #[test]
    fn add() {
        assert_eq!(
            parse_dbg("1 * -2"),
            bin(
                lit(Literal::Int(1)),
                s(BinaryOp::Mul),
                un(s(UnaryOp::Neg), lit(Literal::Int(2))),
            )
        );
    }
    #[test]
    fn harder() {
        assert_eq!(
            parse_dbg("5 == 20 - 33 * 2"),
            bin(
                lit(Literal::Int(5)),
                s(BinaryOp::Eq),
                bin(
                    lit(Literal::Int(20)),
                    s(BinaryOp::Sub),
                    bin(
                        lit(Literal::Int(33)),
                        s(BinaryOp::Mul),
                        lit(Literal::Int(2))
                    )
                ),
            )
        );
    }
    #[test]
    fn bracket() {
        assert_eq!(
            parse_dbg("(10 + 3) / 2"),
            bin(
                bin(
                    lit(Literal::Int(10)),
                    s(BinaryOp::Add),
                    lit(Literal::Int(3))
                ),
                s(BinaryOp::Div),
                lit(Literal::Int(2)),
            )
        );
    }
    #[test]
    fn prog_simple() {
        assert_eq!(
            parse_program("let xy = 1 * 2; xy = xy - 11;"),
            vec![
                Statement::LetDecl(
                    s(id("xy")),
                    bin(lit(Literal::Int(1)), s(BinaryOp::Mul), lit(Literal::Int(2)),)
                ),
                Statement::ExprStmt(asgn(
                    s(id("xy")),
                    bin(
                        s(Expr::Identifier(id("xy"))),
                        s(BinaryOp::Sub),
                        lit(Literal::Int(11))
                    )
                ))
            ]
        );
    }
}
