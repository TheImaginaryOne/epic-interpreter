pub mod chunk;
pub mod heap;

use chunk::{Chunk, Instruction, Opcode, Value};
use heap::{Heap, Object};
use num_traits::FromPrimitive;

#[derive(Debug, Copy, Clone)]
struct Offset(bool, usize);
impl From<i16> for Offset {
    fn from(i: i16) -> Self {
        if i < 0 {
            Self(true, -i as usize)
        } else {
            Self(false, i as usize)
        }
    }
}
impl Offset {
    fn add_to_usize(&self, u: usize) -> Option<usize> {
        if self.0 {
            u.checked_sub(self.1)
        } else {
            u.checked_add(self.1)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RuntimeError {
    InvalidOpcode,
    UnexpectedEof,
    InvalidType, // TODO
    StackUnderflow,
    StackOutOfRange,
    CannotFindConstant,
    CannotFindObject,
    JumpOutOfRange,
}
pub struct Vm {
    heap: Heap,
    chunk: Chunk,
    pc: usize,
    stack: Vec<Value>,
}
impl Vm {
    pub fn new(chunk: Chunk, heap: Heap) -> Self {
        Self {
            pc: 0,
            chunk,
            heap,
            stack: Vec::new(),
        }
    }
    fn pop_stack(&mut self) -> Result<Value, RuntimeError> {
        let next = self.stack.pop().ok_or_else(|| RuntimeError::StackUnderflow);
        next
    }
    fn peek_stack(&mut self) -> Result<&Value, RuntimeError> {
        let next = self.stack.last().ok_or_else(|| RuntimeError::StackUnderflow);
        next
    }
    pub fn next_i16(&mut self) -> Result<i16, RuntimeError> {
        let next = self
            .chunk
            .read_i16(self.pc)
            .ok_or_else(|| RuntimeError::UnexpectedEof);
        self.pc += 2;
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
                Opcode::LoadConstant => {
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
                        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y),
                        (Value::Object(x), Value::Object(y)) => {
                            let (a, b) = (
                                self.heap
                                    .get(x)
                                    .ok_or_else(|| RuntimeError::CannotFindObject)?,
                                self.heap
                                    .get(y)
                                    .ok_or_else(|| RuntimeError::CannotFindObject)?,
                            );
                            let handle = match (a, b) {
                                (Object::String(sa), Object::String(sb)) => {
                                    let x = [&sa[..], &sb[..]].concat();
                                    self.heap.push(Object::String(x))
                                } //_ => return Err(RuntimeError::InvalidType),
                            };

                            Value::Object(handle)
                        }
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::Multiply => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x * y),
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::Divide => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x / y),
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
                Opcode::PopStack => {
                    self.pop_stack()?;
                }
                Opcode::Jump => {
                    let offset = self.next_i16()?;
                    self.pc = Offset::from(offset)
                        .add_to_usize(self.pc)
                        .ok_or_else(|| RuntimeError::JumpOutOfRange)?;
                }
                Opcode::JumpIfFalse => {
                    let offset = self.next_i16()?;
                    let a = self.peek_stack()?;
                    if let Value::Bool(b) = a {
                        if !b {
                            self.pc = Offset::from(offset)
                                .add_to_usize(self.pc)
                                .ok_or_else(|| RuntimeError::JumpOutOfRange)?;
                        }
                    } else {
                        Err(RuntimeError::InvalidType)?;
                    }
                }
                Opcode::Greater => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Bool(x > y),
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::Less => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Bool(x < y),
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::Equal => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Bool(x == y),
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
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
    use crate::test_utils::*;
    #[test]
    fn basic_vm() {
        let c = chunk(
            vec![5, 3, 7],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::LoadConstant(2),
                Instruction::Add,
                Instruction::Multiply,
                Instruction::Return,
            ],
        );

        let mut vm = Vm::new(c, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(0).unwrap(), &Value::Integer(50));
    }
    #[test]
    fn vm_stack_1() {
        let c = chunk(
            vec![5, 3],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::Add,
                Instruction::ReadLocal(0),
                Instruction::Add,
                Instruction::Return,
            ],
        );
        let mut vm = Vm::new(c, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(0).unwrap(), &Value::Integer(16));
    }
    #[test]
    fn vm_stack_2() {
        let c = chunk(
            vec![5, 3, 7],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::LoadConstant(2),
                Instruction::ReadLocal(0),
                Instruction::WriteLocal(2),
                Instruction::Return,
            ],
        );

        let mut vm = Vm::new(c, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(2).unwrap(), &Value::Integer(5));
    }
    #[test]
    fn vm_stack_pop() {
        let c = chunk(
            vec![11, 13, 15],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::PopStack,
                Instruction::LoadConstant(2),
                Instruction::PopStack,
                Instruction::Return,
            ],
        );

        let mut vm = Vm::new(c, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.len(), 1);
        assert_eq!(vm.stack.get(0).unwrap(), &Value::Integer(11))
    }
    #[test]
    fn string_literal() {
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

        let mut h = Heap::new();
        h.push(Object::String("oof".into()));
        h.push(Object::String("ooo".into()));

        let mut vm = Vm::new(c, h);
        assert_eq!(vm.interpret(), Ok(()));
        let stack_top = vm.stack.get(0).unwrap();
        match stack_top {
            Value::Object(i) => assert_eq!(
                vm.heap.get(*i as usize).unwrap(),
                &Object::String("ooooof".into())
            ),
            _ => panic!("Stack has {:?}", stack_top),
        }
    }
    #[test]
    fn jump_conditional() {
        let c = chunk(
            vec![11, 13, 100, 200],
            vec![
                Instruction::LoadConstant(0),
                Instruction::LoadConstant(1),
                Instruction::Less,
                Instruction::JumpIfFalse(2),  //--|
                Instruction::LoadConstant(2), //  |
                Instruction::LoadConstant(3), //<-|
                Instruction::Return,
            ],
        );
        let mut vm = Vm::new(c, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack, vec![Value::Bool(false), Value::Integer(200)])
    }
    #[test]
    fn jump() {
        let c = chunk(
            vec![11, 13, 15],
            vec![
                Instruction::LoadConstant(0),
                Instruction::Jump(5),         //---|
                Instruction::LoadConstant(1), //<--|--|
                Instruction::Jump(5),         //---|--|--|
                Instruction::LoadConstant(2), //<--|  |  |
                Instruction::Jump(-10),       //------|  |
                Instruction::Return,          //<--------|
            ],
        );
        let mut vm = Vm::new(c, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(
            vm.stack,
            vec![Value::Integer(11), Value::Integer(15), Value::Integer(13)]
        )
    }
}
