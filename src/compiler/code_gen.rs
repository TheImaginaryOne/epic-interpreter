use super::ast::{BinaryOp, Expr, Identifier, Literal, Spanned, Statement};
use crate::vm::chunk::{Chunk, Opcode, Value};

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
#[derive(Debug)]
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
        chunk.write_op(Opcode::Return);
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
                    chunk.write_op(Opcode::Constant);
                    // TODO
                    chunk.write_byte(index as u8);
                }
                _ => (),
            },
            Expr::Binary(e1, op, e2) => {
                self.gen_expr(chunk, e2.as_ref())?;
                self.gen_expr(chunk, e1.as_ref())?;
                match op.inner {
                    BinaryOp::Mul => chunk.write_op(Opcode::Multiply),
                    BinaryOp::Div => chunk.write_op(Opcode::Divide),
                    BinaryOp::Add => chunk.write_op(Opcode::Add),
                    _ => todo!(),
                }
            }
            Expr::Assign(id, e1) => {
                self.gen_expr(chunk, e1)?;
                let index = self
                    .resolve_local(&id.inner)
                    .ok_or_else(|| CodeGenError::new(CodeGenErrorType::UndefinedVariable))?;
                chunk.write_op(Opcode::WriteLocal);
                chunk.write_byte(index);
            }
            Expr::Identifier(id) => {
                let index = self
                    .resolve_local(&id)
                    .ok_or_else(|| CodeGenError::new(CodeGenErrorType::UndefinedVariable))?;
                chunk.write_op(Opcode::ReadLocal);
                chunk.write_byte(index);
            }
            _ => todo!(),
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::vm::Vm;
    use crate::compiler::grammar::ProgramParser;
    #[test]
    fn simple() {
        let code = "let x = 4 * 6; x = x + 3;";
        let mut code_gen = CodeGen::new();
        let prog = ProgramParser::new().parse(code).unwrap();

        let bytecode = code_gen.generate(&prog);
        //assert_eq!(bytecode.unwrap(), Chunk::new());

        let mut vm = Vm::new(bytecode.unwrap());
        vm.interpret().unwrap();
    }
}
