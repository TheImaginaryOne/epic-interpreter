pub mod chunk;

use chunk::{Chunk, Opcode, Value};
use num_traits::FromPrimitive;

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    InvalidOpcode,
    UnexpectedEof,
    InvalidType, // TODO
    StackUnderflow,
    StackOutOfRange,
    CannotFindConstant,
}
pub struct Vm {
    chunk: Chunk,
    pc: usize,
    stack: Vec<Value>,
}
impl Vm {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            pc: 0,
            chunk,
            stack: Vec::new(),
        }
    }
    fn pop_stack(&mut self) -> Result<Value, RuntimeError> {
        let next = self.stack.pop().ok_or_else(|| RuntimeError::StackUnderflow);
        next
    }
    pub fn next_byte(&mut self) -> Result<u8, RuntimeError> {
        let next = self
            .chunk
            .bytes
            .get(self.pc)
            .copied() // deref
            .ok_or_else(|| RuntimeError::UnexpectedEof);
        self.pc += 1;
        next
    }
    pub fn interpret(&mut self) -> Result<(), RuntimeError> {
        loop {
            let next_byte = self.next_byte()?;
            let op = Opcode::from_u8(next_byte).ok_or_else(|| RuntimeError::InvalidOpcode)?;

            match op {
                Opcode::Constant => {
                    let i = self.next_byte()?;
                    let value = self
                        .chunk
                        .values
                        .get(i as usize)
                        .ok_or_else(|| RuntimeError::CannotFindConstant)?;
                    self.stack.push(value.clone());
                }
                Opcode::Add => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::Multiply => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::Divide => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x / y),
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::ReadLocal => {
                    // Copies a local at a position in the index
                    // and copies to the stack top.
                    let index = self.next_byte()?;
                    let val = self
                        .stack
                        .get(index as usize)
                        .ok_or_else(|| RuntimeError::StackOutOfRange)?
                        .clone();
                    self.stack.push(val);
                }
                Opcode::WriteLocal => {
                    // Writes the stack top to a
                    // local somewhere down the stack
                    let index = self.next_byte()?;
                    let new_val = self.pop_stack()?;
                    let val = self
                        .stack
                        .get_mut(index as usize)
                        .ok_or_else(|| RuntimeError::StackOutOfRange)?;
                    std::mem::replace(val, new_val);
                }
                Opcode::Return => {
                    break;
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn basic_vm() {
        let mut chunk = Chunk::new();
        let a = chunk.write_constant(Value::Int(5));
        let b = chunk.write_constant(Value::Int(3));
        let c = chunk.write_constant(Value::Int(7));

        chunk.write_op(Opcode::Constant);
        chunk.write_byte(a as u8);
        chunk.write_op(Opcode::Constant);
        chunk.write_byte(b as u8);
        chunk.write_op(Opcode::Constant);
        chunk.write_byte(c as u8);
        chunk.write_op(Opcode::Add);
        chunk.write_op(Opcode::Multiply);
        chunk.write_op(Opcode::Return);

        let mut vm = Vm::new(chunk);
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(0).unwrap(), &Value::Int(50));
    }
    #[test]
    fn vm_stack_1() {
        let mut chunk = Chunk::new();
        let a = chunk.write_constant(Value::Int(5));
        let b = chunk.write_constant(Value::Int(3));

        chunk.write_op(Opcode::Constant);
        chunk.write_byte(a as u8);
        chunk.write_op(Opcode::Constant);
        chunk.write_byte(b as u8);
        chunk.write_op(Opcode::Add);
        chunk.write_op(Opcode::ReadLocal);
        chunk.write_byte(0);
        chunk.write_op(Opcode::Add);
        chunk.write_op(Opcode::Return);

        let mut vm = Vm::new(chunk);
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(0).unwrap(), &Value::Int(16));
    }
    #[test]
    fn vm_stack_2() {
        let mut chunk = Chunk::new();
        let a = chunk.write_constant(Value::Int(5));
        let b = chunk.write_constant(Value::Int(3));
        let c = chunk.write_constant(Value::Int(7));

        chunk.write_op(Opcode::Constant);
        chunk.write_byte(a as u8);
        chunk.write_op(Opcode::Constant);
        chunk.write_byte(b as u8);
        chunk.write_op(Opcode::Constant);
        chunk.write_byte(c as u8);
        chunk.write_op(Opcode::ReadLocal);
        chunk.write_byte(0);
        chunk.write_op(Opcode::WriteLocal);
        chunk.write_byte(2);
        chunk.write_op(Opcode::Return);

        let mut vm = Vm::new(chunk);
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(2).unwrap(), &Value::Int(5));
    }
}
