use super::ast::{BinaryOp, Expr, Identifier, Literal, Spanned, Statement};
use crate::vm::chunk::{Chunk, Instruction, Value};

#[derive(Debug)]
struct CodeGenError {
    // add error info later TODO
    pub ty: CodeGenErrorType,
}
impl CodeGenError {
    fn new(ty: CodeGenErrorType) -> Self {
        Self { ty }
    }
}
#[derive(Debug, PartialEq)]
enum CodeGenErrorType {
    UndefinedVariable,
    VariableExists,
}
struct Local {
    name: String,
    scope: u8,
}
struct CodeGen {
    // TODO for now there must be <= 256 locals
    locals: Vec<Local>,
    current_scope: u8,
}
impl CodeGen {
    pub fn new() -> Self {
        Self {
            locals: Vec::new(),
            current_scope: 0,
        }
    }
    pub fn generate(&mut self, program: &Vec<Statement>) -> Result<Chunk, CodeGenError> {
        let mut chunk = Chunk::new();
        for stmt in program {
            match stmt {
                Statement::LetDecl(ident, expr) => {
                    self.gen_expr(&mut chunk, expr)?;

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
                Statement::ExprStmt(expr) => self.gen_expr(&mut chunk, expr)?,
            }
        }
        chunk.write_instr(Instruction::Return);
        Ok(chunk)
    }
    fn resolve_local(&self, id: &Identifier) -> Option<u8> {
        for (i, local) in self.locals.iter().rev().enumerate() {
            if id.name == local.name {
                return Some(i as u8);
            }
        }
        None
    }
    fn gen_expr(&mut self, chunk: &mut Chunk, expr: &Spanned<Expr>) -> Result<(), CodeGenError> {
        let expr_inner = &expr.inner;
        match expr_inner {
            Expr::Literal(lit) => match lit {
                Literal::Int(i) => {
                    let index = chunk.write_constant(Value::Int(*i));
                    chunk.write_instr(Instruction::Constant(index as u8));
                }
                _ => (),
            },
            Expr::Binary(e1, op, e2) => {
                self.gen_expr(chunk, e2.as_ref())?;
                self.gen_expr(chunk, e1.as_ref())?;
                match op.inner {
                    BinaryOp::Mul => chunk.write_instr(Instruction::Multiply),
                    BinaryOp::Div => chunk.write_instr(Instruction::Divide),
                    BinaryOp::Add => chunk.write_instr(Instruction::Add),
                    _ => todo!(),
                }
            }
            Expr::Assign(id, e1) => {
                self.gen_expr(chunk, e1)?;
                let index = self
                    .resolve_local(&id.inner)
                    .ok_or_else(|| CodeGenError::new(CodeGenErrorType::UndefinedVariable))?;
                chunk.write_instr(Instruction::WriteLocal(index));
            }
            Expr::Identifier(id) => {
                let index = self
                    .resolve_local(&id)
                    .ok_or_else(|| CodeGenError::new(CodeGenErrorType::UndefinedVariable))?;
                chunk.write_instr(Instruction::ReadLocal(index));
            }
            _ => todo!(),
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::vm::chunk::Instruction;
    use crate::test_utils::*;

    // TODO defunct
    #[test]
    fn prog_simple() {
        let ast = vec![
            Statement::LetDecl(id("xy"), bin(int(1), "*", int(2))),
            Statement::ExprStmt(asgn(id("xy"), bin(expr_id("xy"), "+", int(11)))),
        ];

        let mut code_gen = CodeGen::new();
        let bytecode = code_gen.generate(&ast);
        assert_eq!(bytecode.unwrap(), Chunk::new());
    }
    #[test]
    fn undefined_variable() {
        let ast = vec![
            Statement::LetDecl(id("xy"), bin(int(1), "*", int(2))),
            Statement::ExprStmt(asgn(id("xyz"), bin(int(3), "*", int(6)))),
        ];

        let mut code_gen = CodeGen::new();
        let err = code_gen.generate(&ast).unwrap_err();
        assert_eq!(err.ty, CodeGenErrorType::UndefinedVariable);
    }
    #[test]
    fn duplicate_let() {
        let ast = vec![
            Statement::LetDecl(id("xy"), bin(int(1), "*", int(2))),
            Statement::LetDecl(id("xy"), bin(int(3), "*", int(6))),
        ];

        let mut code_gen = CodeGen::new();
        let err = code_gen.generate(&ast).unwrap_err();
        assert_eq!(err.ty, CodeGenErrorType::VariableExists);
    }
}
