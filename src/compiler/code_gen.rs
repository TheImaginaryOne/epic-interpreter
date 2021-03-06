use super::ast::{BinaryOp, Block, Expression, Identifier, Literal, Spanned, Statement, UnaryOp};
use crate::vm::chunk::{Chunk, Instruction, Value};
use crate::vm::heap::{Heap, NativeFunction, ObjFunction, Object};

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
    TooManyArguments,
}
#[derive(Debug)]
struct Local {
    name: String,
    scope: u8,
}
pub struct CodeGen<'a> {
    // TODO for now there must be <= 256 locals
    locals: Vec<Local>,
    current_scope: u8,
    is_root: bool,
    parent: Option<&'a CodeGen<'a>>,
    //current_function: ObjFunction,
}
#[allow(dead_code)]
impl<'a> CodeGen<'a> {
    pub fn new() -> Self {
        Self::new_internal(true, None)
    }
    pub fn new_internal(is_root: bool, parent: Option<&'a CodeGen<'a>>) -> Self {
        Self {
            locals: Vec::new(),
            current_scope: 0,
            is_root,
            parent,
            //current_function: ObjFunction::new("bob".into(), 0),
        }
    }
    /// Generate a program
    pub fn generate(
        &mut self,
        statements: &Vec<Spanned<Statement>>,
        natives: Vec<NativeFunction>,
    ) -> Result<(ObjFunction, Heap), CodeGenError> {
        let mut heap = Heap::new();
        let mut chunk = Chunk::new();
        for f in natives {
            let name = f.name.clone();
            let handle = heap.push(Object::NativeFunction(f));
            let value_index = chunk.write_constant(Value::Object(handle));
            chunk.write_instr(Instruction::LoadConstant(value_index as u8));
            self.add_local(&Identifier { name });
        }
        for statement in statements {
            self.gen_statement(&mut chunk, &mut heap, statement)?;
        }
        chunk.write_instr(Instruction::Return);
        Ok((ObjFunction::new("".into(), 0, chunk), heap))
    }
    /// generate one chunk for a function
    fn gen_chunk(
        &mut self,
        statements: &Vec<Spanned<Statement>>,
        heap: &mut Heap,
    ) -> Result<Chunk, CodeGenError> {
        let mut chunk = Chunk::new();
        for statement in statements {
            self.gen_statement(&mut chunk, heap, statement)?;
        }
        Ok(chunk)
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

                if let Ok(_) = self.resolve_local(&ident.inner) {
                    return Err(CodeGenError::new(CodeGenErrorType::VariableExists));
                }
                self.add_local(&ident.inner);
            }
            Statement::Expression(expr) => self.gen_expression(chunk, heap, &expr)?,
            Statement::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.gen_expression(chunk, heap, &expr)?;
                } else {
                    chunk.write_instr(Instruction::LoadNil);
                }
                chunk.write_instr(Instruction::Return);
            }
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
            Statement::IfElse(then_clauses, else_clause) => {
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
            // function declaration
            Statement::Function {
                name,
                arguments,
                body,
            } => {
                if arguments.len() > 256 {
                    return Err(CodeGenError::new(CodeGenErrorType::TooManyArguments));
                }
                // generate function as an inner chunk
                let mut inner_code_gen = CodeGen::new_internal(false, Some(self));
                // refer to own value to add support for recursion!
                inner_code_gen.add_local(&name.inner);
                for arg in arguments {
                    // add locals as arguments
                    inner_code_gen.add_local(&arg.inner);
                }

                let mut inner_chunk = inner_code_gen.gen_chunk(&body.inner.0, heap)?;
                // if the function returns before, these instructions are
                // unneeded, but if the function returns nothing then Nil must
                // be implicitly returned.
                inner_chunk.write_instr(Instruction::LoadNil);
                inner_chunk.write_instr(Instruction::Return);
                // functions are first class objects! add them to the stack
                let handle = heap.push(Object::Function(ObjFunction {
                    arity: arguments.len() as u8,
                    name: name.inner.name.clone(),
                    chunk: inner_chunk,
                }));
                let i = chunk.write_constant(Value::Object(handle));
                chunk.write_instr(Instruction::LoadConstant(i as u8 /* TODO */));
                self.add_local(&name.inner);
            }
            Statement::While(condition, body) => {
                let start = chunk.next_location();
                self.gen_expression(chunk, heap, condition)?;
                // to patch later
                let jump = chunk.write_instr(Instruction::JumpIfFalse(0));
                chunk.write_instr(Instruction::PopStack);
                self.gen_block(chunk, heap, &body.as_ref().inner)?;

                let end = chunk.next_location() + 3;
                chunk.write_instr(Instruction::Jump(start as i16 - end as i16));
                self.patch_jump(chunk, jump, chunk.next_location());
                chunk.write_instr(Instruction::PopStack);
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
    fn add_local(&mut self, name: &Identifier) {
        self.locals.push(Local {
            name: name.name.clone(),
            scope: self.current_scope,
        });
    }
    fn resolve_upper(&self, id: &Identifier) -> Result<u8, CodeGenError> {
        if self.is_root {
            self.resolve_local(id)
        } else {
            self.parent
                .ok_or(CodeGenError::new(CodeGenErrorType::UndefinedVariable))?
                .resolve_local(id)
        }
    }
    fn resolve_local(&self, id: &Identifier) -> Result<u8, CodeGenError> {
        for (i, local) in self.locals.iter().rev().enumerate() {
            if id.name == local.name {
                return Ok(self.locals.len() as u8 - 1 - i as u8);
            }
        }
        Err(CodeGenError::new(CodeGenErrorType::UndefinedVariable))
    }
    fn gen_block(
        &mut self,
        chunk: &mut Chunk,
        heap: &mut Heap,
        block: &Block,
    ) -> Result<(), CodeGenError> {
        for statement in &block.0 {
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
            Expression::Assignment(id, e1) => {
                self.gen_expression(chunk, heap, e1)?;
                if let Ok(index) = self.resolve_local(&id.inner) {
                    chunk.write_instr(Instruction::WriteLocal(index));
                } else {
                    let index = self.resolve_upper(&id.inner)?;
                    chunk.write_instr(Instruction::WriteGlobal(index));
                }
            }
            Expression::Identifier(id) => {
                if let Ok(index) = self.resolve_local(&id) {
                    chunk.write_instr(Instruction::ReadLocal(index));
                } else {
                    let index = self.resolve_upper(&id)?;
                    chunk.write_instr(Instruction::ReadGlobal(index));
                }
            }
            Expression::Unary(op, e1) => {
                self.gen_expression(chunk, heap, e1.as_ref())?;
                match op.inner {
                    UnaryOp::Negate => chunk.write_instr(Instruction::Negate),
                };
            }
            Expression::FunctionCall(id, arguments) => {
                // TODO allow later declaration of functions
                // Load the function into the stack as a reference to the ObjFunction
                if let Ok(index) = self.resolve_local(&id.inner) {
                    chunk.write_instr(Instruction::ReadLocal(index));
                } else {
                    let index = self.resolve_upper(&id.inner)?;
                    chunk.write_instr(Instruction::ReadGlobal(index));
                }
                for arg in arguments {
                    // compile arguments
                    self.gen_expression(chunk, heap, arg)?;
                }
                if arguments.len() > 256 {
                    return Err(CodeGenError::new(CodeGenErrorType::TooManyArguments));
                }
                chunk.write_instr(Instruction::Call(arguments.len() as u8));
            }
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

    fn check(ast: Vec<Spanned<Statement>>, chunk: ObjFunction, heap: Heap) {
        let mut code_gen = CodeGen::new();
        let (bytecode, heap2) = code_gen
            .generate(&ast, vec![])
            .expect("error generating chunk");
        assert_eq!(heap, heap2);
        assert_eq!(bytecode, chunk);
    }
    fn check_error(ast: Vec<Spanned<Statement>>, error: CodeGenErrorType) {
        let mut code_gen = CodeGen::new();
        let mut heap2 = Heap::new();
        assert_eq!(error, code_gen.generate(&ast, vec![]).unwrap_err().ty);
    }
    #[test]
    fn many_locals() {
        check(
            vec![
                let_stmt(id("xy"), int(12)),
                let_stmt(id("xz"), bin(int(2), "+", expr_id("xy"))),
                let_stmt(id("b"), bin(expr_id("xy"), "*", expr_id("xz"))),
                let_stmt(id("c"), bin(expr_id("xz"), "/", expr_id("b"))),
            ],
            main_func_any_val(
                vec![Value::Integer(12), Value::Integer(2)],
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
            ),
            Heap::new(),
        );
    }
    #[test]
    fn prog_simple() {
        check(
            vec![
                let_stmt(id("xy"), bin(int(1), "*", int(2))),
                expr_stmt(asgn(id("xy"), bin(expr_id("xy"), "+", int(11)))),
            ],
            main_func_any_val(
                vec![Value::Integer(2), Value::Integer(1), Value::Integer(11)],
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
            ),
            Heap::new(),
        );
    }
    #[test]
    fn unary_simple() {
        check(
            vec![let_stmt(id("xyx"), un("-", int(2)))],
            main_func_any_val(
                vec![Value::Integer(2)],
                vec![
                    Instruction::LoadConstant(0),
                    Instruction::Negate,
                    Instruction::Return,
                ],
            ),
            Heap::new(),
        );
    }
    #[test]
    fn undefined_variable() {
        check_error(
            vec![
                let_stmt(id("xy"), bin(int(1), "*", int(2))),
                expr_stmt(asgn(id("xyz"), bin(int(3), "*", int(6)))),
            ],
            CodeGenErrorType::UndefinedVariable,
        );
    }
    #[test]
    fn duplicate_let() {
        check_error(
            vec![
                let_stmt(id("xy"), bin(int(1), "*", int(2))),
                let_stmt(id("xy"), bin(int(3), "*", int(6))),
            ],
            CodeGenErrorType::VariableExists,
        );
    }
    #[test]
    fn undefined_variable_block_stmt() {
        check_error(
            vec![
                block_stmt(vec![let_stmt(id("bc"), int(3))]),
                expr_stmt(asgn(id("bc"), int(6))),
            ],
            CodeGenErrorType::UndefinedVariable,
        );
    }
    #[test]
    fn simple_block() {
        check(
            vec![
                let_stmt(id("aa"), int(10)),
                block_stmt(vec![
                    let_stmt(id("bc"), int(3)),
                    block_stmt(vec![let_stmt(id("cc"), int(5))]),
                    expr_stmt(asgn(id("aa"), bin(expr_id("bc"), "+", int(99)))),
                ]),
            ],
            main_func_any_val(
                vec![
                    Value::Integer(10),
                    Value::Integer(3),
                    Value::Integer(5),
                    Value::Integer(99),
                ],
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
            ),
            Heap::new(),
        );
    }
    #[test]
    fn globals() {
        let f = main_func_any_val(
            vec![
                Value::Integer(45),
                Value::Object(0), // the function at heap location 0
            ],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::ReadLocal(1),
                Instruction::Call(0),
                Instruction::Return,
            ],
        );
        let a = function(
            "fa",
            0,
            vec![],
            vec![
                Instruction::ReadGlobal(0),
                Instruction::Return,
                // redundant
                Instruction::LoadNil,
                Instruction::Return,
            ],
        );
        let mut h = Heap::new();
        h.push(Object::Function(a));

        let ast = vec![
            let_stmt(id("aa"), int(45)),
            func_stmt("fa", vec![], block(vec![return_stmt(Some(expr_id("aa")))])),
            let_stmt(id("bb"), call_func("fa", vec![])),
        ];

        check(ast, f, h);
    }
    #[test]
    fn function_call() {
        let f = main_func_any_val(
            vec![
                Value::Object(0), // the function
                Value::Integer(5),
                Value::Integer(15),
                Value::Integer(13),
            ],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::ReadLocal(0),
                Instruction::LoadConstant(2),
                Instruction::LoadConstant(3),
                Instruction::Call(2),
                Instruction::Multiply,
                Instruction::Return,
            ],
        );
        let a = function(
            "sub",
            2,
            vec![],
            vec![
                Instruction::ReadLocal(2),
                Instruction::ReadLocal(1),
                Instruction::Subtract,
                Instruction::Return,
                // redundant
                Instruction::LoadNil,
                Instruction::Return,
            ],
        );
        let mut h = Heap::new();
        h.push(Object::Function(a));

        let ast = vec![
            func_stmt(
                "sub",
                vec!["a", "b"],
                block(vec![return_stmt(Some(bin(
                    expr_id("a"),
                    "-",
                    expr_id("b"),
                )))]),
            ),
            let_stmt(
                id("aa"),
                bin(call_func("sub", vec![int(15), int(13)]), "*", int(5)),
            ),
        ];
        check(ast, f, h);
    }
    #[test]
    fn string_literal() {
        let ast = vec![let_stmt(id("xy"), bin(string("ooo"), "+", string("oof")))];

        let mut heap = Heap::new();
        heap.push(Object::String("oof".into()));
        heap.push(Object::String("ooo".into()));

        let f = main_func_any_val(
            vec![Value::Object(0), Value::Object(1)],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::Add,
                Instruction::Return,
            ],
        );

        check(ast, f, heap);
    }
    // WARNING!! Monster code blocks ahead
    #[test]
    fn while_simple() {
        check(
            vec![
                let_stmt(id("x"), int(0)),
                while_stmt(
                    bin(expr_id("x"), "<", int(6)),
                    block(vec![expr_stmt(asgn(
                        id("x"),
                        bin(int(1), "+", expr_id("x")),
                    ))]),
                ),
            ],
            main_func_any_val(
                vec![Value::Integer(0), Value::Integer(6), Value::Integer(1)],
                vec![
                    Instruction::LoadConstant(0),
                    Instruction::LoadConstant(1),
                    Instruction::ReadLocal(0),
                    Instruction::Less,
                    Instruction::JumpIfFalse(11), //
                    Instruction::PopStack,
                    Instruction::ReadLocal(0),
                    Instruction::LoadConstant(2),
                    Instruction::Add,
                    Instruction::WriteLocal(0),
                    Instruction::Jump(-19),
                    Instruction::PopStack,
                    Instruction::Return,
                ],
            ),
            Heap::new(),
        );
    }
    // WARNING!! Monster code blocks ahead
    #[test]
    fn if_only() {
        check(
            vec![
                let_stmt(id("x"), int(0)),
                let_stmt(id("y"), int(3)),
                if_stmt(vec![(
                    bin(expr_id("y"), ">", int(1)),
                    block(vec![expr_stmt(asgn(id("x"), int(4)))]),
                )]),
            ],
            main_func_any_val(
                vec![
                    Value::Integer(0),
                    Value::Integer(3),
                    Value::Integer(1),
                    Value::Integer(4),
                ],
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
            ),
            Heap::new(),
        );
    }
    #[test]
    fn if_else_short() {
        check(
            vec![
                let_stmt(id("x"), int(0)),
                let_stmt(id("y"), int(3)),
                if_else_stmt(
                    vec![(
                        bin(expr_id("y"), ">", int(1)),
                        block(vec![expr_stmt(asgn(id("x"), int(4)))]),
                    )],
                    block(vec![expr_stmt(asgn(id("x"), int(6)))]),
                ),
            ],
            main_func_any_val(
                vec![
                    Value::Integer(0),
                    Value::Integer(3),
                    Value::Integer(1),
                    Value::Integer(4),
                    Value::Integer(6),
                ],
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
            ),
            Heap::new(),
        );
    }
    #[test]
    fn if_else_long() {
        check(
            vec![
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
            ],
            main_func_any_val(
                vec![
                    Value::Integer(0),
                    Value::Integer(3),
                    Value::Integer(1),
                    Value::Integer(4),
                    Value::Integer(1),
                    Value::Integer(5),
                    Value::Integer(6),
                ],
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
            ),
            Heap::new(),
        );
    }
}
