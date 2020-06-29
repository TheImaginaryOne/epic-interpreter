use super::ast::{
    BinaryOp, Block, Expression, Identifier, IfElse, Literal, Spanned, Statement, UnaryOp,
};
use crate::vm::chunk::{Chunk, Instruction, Value};
use crate::vm::heap::{Heap, Object};

#[derive(Debug)]
pub struct CodeGenError {
    // add error info later TODO
    pub ty: CodeGenErrorType,
}
impl CodeGenError {
    fn new(ty: CodeGenErrorType) -> Self {
        Self { ty }
    }
}
#[derive(Debug, PartialEq)]
pub enum CodeGenErrorType {
    UndefinedVariable,
    VariableExists,
}
#[derive(Debug)]
struct Local {
    name: String,
    scope: u8,
}
pub struct CodeGen {
    // TODO for now there must be <= 256 locals
    locals: Vec<Local>,
    current_scope: u8,
}
#[allow(dead_code)]
impl CodeGen {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            current_scope: 0,
        }
    }
    pub fn generate(
        &mut self,
        statements: &Vec<Spanned<Statement>>,
    ) -> Result<(Chunk, Heap), CodeGenError> {
        let mut chunk = Chunk::new();
        let mut heap = Heap::new(); // TODO this might be changed later, editing a Heap directly is a bit strange
        for statement in statements {
            self.gen_statement(&mut chunk, &mut heap, statement)?;
        }
        chunk.write_instr(Instruction::Return);
        Ok((chunk, heap))
    }
    fn gen_statement(
        &mut self,
        chunk: &mut Chunk,
        heap: &mut Heap,
        statement: &Spanned<Statement>,
    ) -> Result<(), CodeGenError> {
        match &statement.inner {
            Statement::LetBinding(ident, expr) => {
                self.gen_expression(chunk, heap, &expr)?;

                let name = &ident.inner.name;
                if let Some(_) = self.resolve_local(&ident.inner) {
                    return Err(CodeGenError::new(CodeGenErrorType::VariableExists));
                }
                let new_local = Local {
                    name: name.clone(),
                    scope: self.current_scope,
                };

                self.locals.push(new_local);
            }
            Statement::Expression(expr) => self.gen_expression(chunk, heap, &expr)?,
            Statement::Block(block) => {
                self.current_scope += 1;
                self.gen_block(chunk, heap, block)?;
                for i in (0..self.locals.len()).rev() {
                    if self.locals[i].scope == self.current_scope {
                        self.locals.pop();
                        chunk.write_instr(Instruction::PopStack);
                    } else {
                        break;
                    }
                }
                self.current_scope -= 1;
            }
            Statement::IfElse(if_else) => {
                let IfElse {
                    then_clauses,
                    else_clause,
                } = if_else.as_ref();

                let mut previous_jump_loc = 0;
                // the final instruction of each "if ... { ... }" block
                let mut block_ends = vec![];
                for (i, (cond, block)) in then_clauses.iter().enumerate() {
                    // generate the "if ..." expression
                    let jump_target = chunk.next_location();
                    if i > 0 {
                        chunk.write_instr(Instruction::PopStack);
                    }
                    self.gen_expression(chunk, heap, cond)?;
                    let jump_loc = chunk.write_instr(Instruction::JumpIfFalse(0));
                    chunk.write_instr(Instruction::PopStack);

                    if i > 0 {
                        // if the previous "if" doesn't exist, jump to start of the next "if"
                        self.patch_jump(chunk, previous_jump_loc, jump_target);
                    }
                    previous_jump_loc = jump_loc;

                    // generate the body
                    self.gen_block(chunk, heap, &block.inner)?;
                    let block_end = chunk.write_instr(Instruction::Jump(0));
                    block_ends.push(block_end);
                }
                let else_loc = chunk.next_location();
                self.patch_jump(chunk, previous_jump_loc, else_loc);
                chunk.write_instr(Instruction::PopStack);
                // generate else statement if any exists
                if let Some(block) = else_clause {
                    self.gen_block(chunk, heap, &block.inner)?;
                }
                // After the if statements are finished, the program must
                // jump to the end.
                let final_loc = chunk.next_location();
                for end in block_ends {
                    self.patch_jump(chunk, end, final_loc);
                }
            }
            _ => todo!(),
        }
        Ok(())
    }
    /// Patches a "jump" instruction so it points to a target.
    fn patch_jump(&self, chunk: &mut Chunk, location: usize, target: usize) {
        let offset = target as i32 - location as i32 - 3; // TODO?!
        chunk.write_i16_at(offset as i16, location + 1);
    }
    fn resolve_local(&self, id: &Identifier) -> Option<u8> {
        for (i, local) in self.locals.iter().rev().enumerate() {
            if id.name == local.name {
                return Some(self.locals.len() as u8 - 1 - i as u8);
            }
        }
        None
    }
    fn gen_block(
        &mut self,
        chunk: &mut Chunk,
        heap: &mut Heap,
        block: &Block,
    ) -> Result<(), CodeGenError> {
        for statement in &block.statements {
            self.gen_statement(chunk, heap, &statement)?;
        }
        Ok(())
    }
    fn gen_expression(
        &mut self,
        chunk: &mut Chunk,
        heap: &mut Heap,
        expr: &Spanned<Expression>,
    ) -> Result<(), CodeGenError> {
        let expr_inner = &expr.inner;
        match expr_inner {
            Expression::Literal(lit) => match lit {
                Literal::Integer(i) => {
                    let index = chunk.write_constant(Value::Integer(*i));
                    chunk.write_instr(Instruction::LoadConstant(index as u8));
                }
                Literal::String(s) => {
                    let handle = heap.push(Object::String(s.clone()));
                    // ummm TODO
                    let index = chunk.write_constant(Value::Object(handle));
                    chunk.write_instr(Instruction::LoadConstant(index as u8));
                }
                _ => (),
            },
            Expression::Binary(e1, op, e2) => {
                self.gen_expression(chunk, heap, e2.as_ref())?;
                self.gen_expression(chunk, heap, e1.as_ref())?;
                match op.inner {
                    BinaryOp::Multiply => chunk.write_instr(Instruction::Multiply),
                    BinaryOp::Divide => chunk.write_instr(Instruction::Divide),
                    BinaryOp::Add => chunk.write_instr(Instruction::Add),
                    BinaryOp::Subtract => chunk.write_instr(Instruction::Subtract),
                    BinaryOp::Equal => chunk.write_instr(Instruction::Equal),
                    BinaryOp::Greater => chunk.write_instr(Instruction::Greater),
                    BinaryOp::Less => chunk.write_instr(Instruction::Less),
                    _ => todo!(),
                };
            }
            Expression::Assign(id, e1) => {
                self.gen_expression(chunk, heap, e1)?;
                let index = self
                    .resolve_local(&id.inner)
                    .ok_or_else(|| CodeGenError::new(CodeGenErrorType::UndefinedVariable))?;
                chunk.write_instr(Instruction::WriteLocal(index));
            }
            Expression::Identifier(id) => {
                let index = self
                    .resolve_local(&id)
                    .ok_or_else(|| CodeGenError::new(CodeGenErrorType::UndefinedVariable))?;
                chunk.write_instr(Instruction::ReadLocal(index));
            }
            Expression::Unary(op, e1) => {
                self.gen_expression(chunk, heap, e1.as_ref())?;
                match op.inner {
                    UnaryOp::Negate => chunk.write_instr(Instruction::Negate),
                };
            }
            _ => todo!(),
        }
        Ok(())
    }
}

// When my tests are longer than the code...
#[cfg(test)]
mod test {
    use super::*;
    use crate::test_utils::*;
    use crate::vm::chunk::Instruction;
    use crate::vm::heap::Object;

    #[test]
    fn many_locals() {
        let ast = vec![
            let_stmt(id("xy"), int(12)),
            let_stmt(id("xz"), bin(int(2), "+", expr_id("xy"))),
            let_stmt(id("b"), bin(expr_id("xy"), "*", expr_id("xz"))),
            let_stmt(id("c"), bin(expr_id("xz"), "/", expr_id("b"))),
        ];

        let mut code_gen = CodeGen::new();
        let bytecode = code_gen.generate(&ast);
        let c = chunk(
            vec![12, 2],
            vec![
                Instruction::LoadConstant(0),
                Instruction::ReadLocal(0),
                Instruction::LoadConstant(1),
                Instruction::Add,
                Instruction::ReadLocal(1),
                Instruction::ReadLocal(0),
                Instruction::Multiply,
                Instruction::ReadLocal(2),
                Instruction::ReadLocal(1),
                Instruction::Divide,
                Instruction::Return,
            ],
        );
        assert_eq!(bytecode.unwrap().0, c);
    }
    #[test]
    fn prog_simple() {
        let ast = vec![
            let_stmt(id("xy"), bin(int(1), "*", int(2))),
            expr_stmt(asgn(id("xy"), bin(expr_id("xy"), "+", int(11)))),
        ];

        let mut code_gen = CodeGen::new();
        let bytecode = code_gen.generate(&ast);
        let c = chunk(
            vec![2, 1, 11],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::Multiply,
                Instruction::LoadConstant(2),
                Instruction::ReadLocal(0),
                Instruction::Add,
                Instruction::WriteLocal(0),
                Instruction::Return,
            ],
        );
        assert_eq!(bytecode.unwrap().0, c);
    }
    #[test]
    fn unary_simple() {
        let ast = vec![let_stmt(id("xyx"), un("-", int(2)))];

        let mut code_gen = CodeGen::new();
        let bytecode = code_gen.generate(&ast);
        let c = chunk(
            vec![2],
            vec![
                Instruction::LoadConstant(0),
                Instruction::Negate,
                Instruction::Return,
            ],
        );
        assert_eq!(bytecode.unwrap().0, c);
    }
    #[test]
    fn undefined_variable() {
        let ast = vec![
            let_stmt(id("xy"), bin(int(1), "*", int(2))),
            expr_stmt(asgn(id("xyz"), bin(int(3), "*", int(6)))),
        ];

        let mut code_gen = CodeGen::new();
        let err = code_gen.generate(&ast).unwrap_err();
        assert_eq!(err.ty, CodeGenErrorType::UndefinedVariable);
    }
    #[test]
    fn duplicate_let() {
        let ast = vec![
            let_stmt(id("xy"), bin(int(1), "*", int(2))),
            let_stmt(id("xy"), bin(int(3), "*", int(6))),
        ];

        let mut code_gen = CodeGen::new();
        let err = code_gen.generate(&ast).unwrap_err();
        assert_eq!(err.ty, CodeGenErrorType::VariableExists);
    }
    #[test]
    fn undefined_variable_block_stmt() {
        let ast = vec![
            block_stmt(vec![let_stmt(id("bc"), int(3))]),
            expr_stmt(asgn(id("bc"), int(6))),
        ];
        let err = CodeGen::new().generate(&ast).unwrap_err();
        assert_eq!(err.ty, CodeGenErrorType::UndefinedVariable);
    }
    #[test]
    fn simple_block() {
        let ast = vec![
            let_stmt(id("aa"), int(10)),
            block_stmt(vec![
                let_stmt(id("bc"), int(3)),
                block_stmt(vec![let_stmt(id("cc"), int(5))]),
                expr_stmt(asgn(id("aa"), bin(expr_id("bc"), "+", int(99)))),
            ]),
        ];
        let (bytecode, _) = CodeGen::new().generate(&ast).unwrap();
        let c = chunk(
            vec![10, 3, 5, 99],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::LoadConstant(2),
                Instruction::PopStack,
                Instruction::LoadConstant(3),
                Instruction::ReadLocal(1),
                Instruction::Add,
                Instruction::WriteLocal(0),
                Instruction::PopStack,
                Instruction::Return,
            ],
        );
        assert_eq!(bytecode, c);
    }
    #[test]
    fn string_literal() {
        let ast = vec![let_stmt(id("xy"), bin(string("ooo"), "+", string("oof")))];

        let mut code_gen = CodeGen::new();
        let bytecode = code_gen.generate(&ast).unwrap();

        let mut c = Chunk::new();
        let instrs = vec![
            Instruction::LoadConstant(0),
            Instruction::LoadConstant(1),
            Instruction::Add,
            Instruction::Return,
        ];
        for i in instrs {
            c.write_instr(i);
        }
        c.values = vec![Value::Object(0), Value::Object(1)];

        assert_eq!(bytecode.0, c);
        assert_eq!(bytecode.1.get(0).unwrap(), &Object::String("oof".into()));
        assert_eq!(bytecode.1.get(1).unwrap(), &Object::String("ooo".into()));
    }
    // WARNING!! Monster code blocks ahead
    #[test]
    fn if_only() {
        let ast = vec![
            let_stmt(id("x"), int(0)),
            let_stmt(id("y"), int(3)),
            if_stmt(vec![(
                bin(expr_id("y"), ">", int(1)),
                block(vec![expr_stmt(asgn(id("x"), int(4)))]),
            )]),
        ];
        let mut code_gen = CodeGen::new();
        let bytecode = code_gen.generate(&ast).unwrap();
        let c = chunk(
            vec![0, 3, 1, 4],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::LoadConstant(2),
                Instruction::ReadLocal(1),
                Instruction::Greater,
                Instruction::JumpIfFalse(8), //
                Instruction::PopStack,
                Instruction::LoadConstant(3),
                Instruction::WriteLocal(0),
                Instruction::Jump(1),
                Instruction::PopStack,
                Instruction::Return,
            ],
        );
        assert_eq!(bytecode.0, c);
    }
    #[test]
    fn if_else_short() {
        let ast = vec![
            let_stmt(id("x"), int(0)),
            let_stmt(id("y"), int(3)),
            if_else_stmt(
                vec![(
                    bin(expr_id("y"), ">", int(1)),
                    block(vec![expr_stmt(asgn(id("x"), int(4)))]),
                )],
                block(vec![expr_stmt(asgn(id("x"), int(6)))]),
            ),
        ];
        let mut code_gen = CodeGen::new();
        let bytecode = code_gen.generate(&ast).unwrap();
        let c = chunk(
            vec![0, 3, 1, 4, 6],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::LoadConstant(2),
                Instruction::ReadLocal(1),
                Instruction::Greater,
                Instruction::JumpIfFalse(8), //
                Instruction::PopStack,
                Instruction::LoadConstant(3),
                Instruction::WriteLocal(0),
                Instruction::Jump(5), //
                Instruction::PopStack,
                Instruction::LoadConstant(4),
                Instruction::WriteLocal(0),
                Instruction::Return,
            ],
        );
        assert_eq!(bytecode.0, c);
    }
    #[test]
    fn if_else_long() {
        let ast = vec![
            let_stmt(id("x"), int(0)),
            let_stmt(id("y"), int(3)),
            if_else_stmt(
                vec![
                    (
                        bin(expr_id("y"), ">", int(1)),
                        block(vec![expr_stmt(asgn(id("x"), int(4)))]),
                    ),
                    (
                        bin(expr_id("y"), "<", int(1)),
                        block(vec![expr_stmt(asgn(id("x"), int(5)))]),
                    ),
                ],
                block(vec![expr_stmt(asgn(id("x"), int(6)))]),
            ),
        ];
        let mut code_gen = CodeGen::new();
        let bytecode = code_gen.generate(&ast).unwrap();
        let c = chunk(
            vec![0, 3, 1, 4, 1, 5, 6],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::LoadConstant(2),
                Instruction::ReadLocal(1),
                Instruction::Greater,
                Instruction::JumpIfFalse(8), //
                Instruction::PopStack,
                Instruction::LoadConstant(3),
                Instruction::WriteLocal(0),
                Instruction::Jump(22), //
                Instruction::PopStack,
                Instruction::LoadConstant(4),
                Instruction::ReadLocal(1),
                Instruction::Less,
                Instruction::JumpIfFalse(8), //
                Instruction::PopStack,
                Instruction::LoadConstant(5),
                Instruction::WriteLocal(0),
                Instruction::Jump(5),
                Instruction::PopStack,
                Instruction::LoadConstant(6),
                Instruction::WriteLocal(0),
                Instruction::Return,
            ],
        );
        assert_eq!(bytecode.0, c);
    }
}
