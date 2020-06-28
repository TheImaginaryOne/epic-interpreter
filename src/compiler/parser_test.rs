/// Tests grammar, including the LALRPOP parser
#[cfg(test)]
mod test {
    use crate::compiler::ast::*;
    use crate::compiler::error::{Error, ParseError};
    use crate::compiler::lexer::Token;
    use crate::test_utils::*;
    #[test]
    fn lex_invalid() {
        let l = parse_err("1 * ~");
        assert_eq!(
            l,
            Spanned {
                inner: ParseError::LexError(Error::UnexpectedToken),
                left: 4,
                right: 5
            }
        );
    }
    #[test]
    fn unexpected() {
        let l = parse_err("1 * 3 + /");
        assert_eq!(
            l,
            Spanned {
                inner: ParseError::UnexpectedToken(Token::Slash, "expression".into()),
                left: 8,
                right: 9
            }
        );
    }
    #[test]
    fn out_of_range() {
        let l = parse_err("178998662548746775989486268475244265837474723410000118");
        assert_eq!(
            l,
            Spanned {
                inner: ParseError::CannotParseInteger,
                left: 0,
                right: 54
            }
        );
    }
    #[test]
    fn eof() {
        let l = parse_err("1 * 3 +");
        assert_eq!(
            l,
            Spanned {
                inner: ParseError::UnexpectedToken(Token::Eof, "expression".into()),
                left: 7,
                right: 7
            }
        );
    }
    #[test]
    fn left_assoc_1() {
        assert_eq!(
            parse_dbg("20 - 33 - 2 + 100"),
            bin(bin(bin(int(20), "-", int(33)), "-", int(2)), "+", int(100))
        );
    }
    #[test]
    fn left_assoc_2() {
        assert_eq!(
            parse_dbg("1 == 4 == 6"),
            bin(bin(int(1), "==", int(4)), "==", int(6))
        );
    }
    #[test]
    fn add() {
        assert_eq!(parse_dbg("1 * -2"), bin(int(1), "*", un("-", int(2))));
    }
    #[test]
    fn harder() {
        assert_eq!(
            parse_dbg("5 == 20 - 33 * 2"),
            bin(int(5), "==", bin(int(20), "-", bin(int(33), "*", int(2))))
        );
    }
    #[test]
    fn bracket() {
        assert_eq!(
            parse_dbg("(10 + 3) / 2"),
            bin(bin(int(10), "+", int(3)), "/", int(2))
        );
    }
    #[test]
    fn simple_block() {
        assert_eq!(
            parse_program("let b = 10; { let xx = 78 + 2; b = xx + 99; }"),
            vec![
                let_stmt(id("b"), int(10)),
                block_stmt(vec![
                    let_stmt(id("xx"), bin(int(78), "+", int(2))),
                    expr_stmt(asgn(id("b"), bin(expr_id("xx"), "+", int(99)))),
                ]),
            ]
        );
    }
    #[test]
    fn prog_simple() {
        assert_eq!(
            parse_program("let xy = 1 * 2; xy = xy - 11;"),
            vec![
                let_stmt(id("xy"), bin(int(1), "*", int(2))),
                expr_stmt(asgn(id("xy"), bin(expr_id("xy"), "-", int(11))))
            ]
        );
    }
    #[test]
    fn if_else_simple() {
        assert_eq!(
            parse_program("if y > 1 { x = 1; } else if y < 1 { x = 2; } else { x = 3; }"),
            vec![if_else_stmt(
                vec![
                    (
                        bin(expr_id("y"), ">", int(1)),
                        block(vec![expr_stmt(asgn(id("x"), int(1)))])
                    ),
                    (
                        bin(expr_id("y"), "<", int(1)),
                        block(vec![expr_stmt(asgn(id("x"), int(2)))])
                    )
                ],
                block(vec![expr_stmt(asgn(id("x"), int(3)))])
            ),]
        );
    }
}
