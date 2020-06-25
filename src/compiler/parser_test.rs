/// Tests grammar, including the LALRPOP parser
#[cfg(test)]
mod test {
    use crate::compiler::ast::*;
    use crate::compiler::error::Error;
    use crate::test_utils::*;
    use lalrpop_util::*;
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
    fn out_of_range() {
        let l = parse_err("178998662548746775989486268475244265837474723410000118");
        match l {
            ParseError::User { error, .. } => assert_eq!(error.1, Error::CannotParseInteger),
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
}
