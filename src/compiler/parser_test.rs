/// Tests grammar, including the LALRPOP parser
#[cfg(test)]
mod test {
    use crate::compiler::ast::*;
    use crate::compiler::error::{LexError, ParseError};
    use crate::compiler::lexer::Token;
    use crate::test_utils::*;
    #[test]
    fn lex_invalid() {
        let l = parse_err("1 * ~");
        assert_eq!(
            l,
            Spanned {
                inner: ParseError::LexError(LexError::UnexpectedToken),
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
    fn error_sync_1() {
        assert_eq!(
            parse_program_error("let x = 8 - $; a = 7"),
            vec![
                Spanned::new(12, ParseError::LexError(LexError::UnexpectedToken), 13),
                Spanned::new(20, ParseError::UnexpectedToken(Token::Eof, ";".into()), 20),
            ],
        );
    }
    #[test]
    fn error_sync_2() {
        assert_eq!(
            parse_program_error("let x = 8 - 9 let y = 7"),
            vec![
                Spanned::new(14, ParseError::UnexpectedToken(Token::Let, ";".into()), 17),
                Spanned::new(23, ParseError::UnexpectedToken(Token::Eof, ";".into()), 23),
            ],
        );
    }
    #[test]
    fn error_sync_block() {
        assert_eq!(
            parse_program_error("let x = 8 + (5 { let y = \"hi\""),
            vec![
                Spanned::new(
                    15,
                    ParseError::UnexpectedToken(Token::LBrace, ")".into()),
                    16
                ),
                Spanned::new(29, ParseError::UnexpectedToken(Token::Eof, ";".into()), 29),
                Spanned::new(29, ParseError::UnexpectedToken(Token::Eof, "}".into()), 29),
            ],
        );
    }
    #[test]
    fn error_sync_if_else() {
        assert_eq!(
            parse_program_error("let y = (8 if x = 9 { let y = \"hi\";"),
            vec![
                Spanned::new(11, ParseError::UnexpectedToken(Token::If, ")".into()), 13),
                Spanned::new(35, ParseError::UnexpectedToken(Token::Eof, "}".into()), 35),
            ],
        );
    }
    #[test]
    fn simple_assignment() {
        assert_eq!(
            parse_dbg("x = 78 - 99"),
            asgn(id("x"), bin(int(78), "-", int(99)))
        );
    }
    #[test]
    fn bad_assignment() {
        let l = parse_err("xy - yz = 50 + 8;");
        assert_eq!(
            l,
            Spanned {
                inner: ParseError::InvalidAssignment,
                left: 0,
                right: 16
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
    fn function_decl() {
        assert_eq!(
            parse_program("fun bob(hi, bye) { let x = hi == bye; }"),
            vec![func_stmt(
                "bob",
                vec!["hi", "bye"],
                block(vec![let_stmt(
                    id("x"),
                    bin(expr_id("hi"), "==", expr_id("bye"))
                )]),
            )]
        );
    }
    #[test]
    fn func_decl_empty_params() {
        assert_eq!(
            parse_program("fun bob() {}"),
            vec![func_stmt("bob", vec![], block(vec![])),]
        );
    }
    #[test]
    fn function_empty_args() {
        assert_eq!(parse_dbg("bob()"), call_func("bob", vec![]));
    }
    #[test]
    fn return_nothing() {
        assert_eq!(parse_program("return ;"), vec![return_stmt(None)]);
    }
    #[test]
    fn return_simple() {
        assert_eq!(
            parse_program("return 8 - 9;"),
            vec![return_stmt(Some(bin(int(8), "-", int(9))))]
        );
    }
    #[test]
    fn function_call() {
        assert_eq!(
            parse_dbg("5 + bob(8*6, hi);"),
            bin(
                int(5),
                "+",
                call_func("bob", vec![bin(int(8), "*", int(6)), expr_id("hi")])
            )
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
    fn simple_string() {
        assert_eq!(
            parse_dbg("\"jeff\" == \"bob\""),
            bin(string("jeff"), "==", string("bob"))
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
    fn while_simple() {
        assert_eq!(
            parse_program("while x < 3 { x = x + 1; }"),
            vec![while_stmt(
                bin(expr_id("x"), "<", int(3)),
                block(vec![expr_stmt(asgn(
                    id("x"),
                    bin(expr_id("x"), "+", int(1))
                ))])
            )]
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
    #[test]
    fn if_no_else() {
        assert_eq!(
            parse_program("if y > 1 { x = 55; }"),
            vec![if_stmt(vec![(
                bin(expr_id("y"), ">", int(1)),
                block(vec![expr_stmt(asgn(id("x"), int(55)))])
            ),],),]
        );
    }
    #[test]
    fn if_else_if() {
        assert_eq!(
            parse_program("if y > 1 { x = 1; } else if y < 1 { x = 2; }"),
            vec![if_stmt(vec![
                (
                    bin(expr_id("y"), ">", int(1)),
                    block(vec![expr_stmt(asgn(id("x"), int(1)))])
                ),
                (
                    bin(expr_id("y"), "<", int(1)),
                    block(vec![expr_stmt(asgn(id("x"), int(2)))])
                )
            ],),]
        );
    }
}
