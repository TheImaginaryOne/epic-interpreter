pub mod chunk;
pub mod heap;

use chunk::{Chunk, Instruction, Opcode, Value};
use heap::{Handle, Heap, ObjFunction, Object};
use num_traits::FromPrimitive;
//use crate::test_utils::let_stmt;

struct CallFrame {
    function: Handle,
    pc: usize, // cached
    // Stack start on the value stack
    stack_start: usize,
}

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
    NoCallFrame,
    CannotFindConstant,
    CannotFindObject,
    JumpOutOfRange,
    WrongArity,
}
pub struct Vm {
    heap: Heap,
    pc: usize,

    stack: Vec<Value>,
    call_frames: Vec<CallFrame>,
}
impl Vm {
    pub fn new(main_function: ObjFunction, mut heap: Heap) -> Self {
        let mut call_frames = Vec::with_capacity(256);
        let handle = heap.push(Object::Function(main_function));
        call_frames.push(CallFrame {
            pc: 0,
            function: handle,
            stack_start: 0,
        });

        Self {
            pc: 0,
            heap,
            stack: Vec::new(),
            call_frames,
        }
    }
    fn get_object(&self, handle: Handle) -> Result<&Object, RuntimeError> {
        self.heap
            .get(handle)
            .ok_or_else(|| RuntimeError::CannotFindObject)
    }
    fn pop_stack(&mut self) -> Result<Value, RuntimeError> {
        let next = self.stack.pop().ok_or_else(|| RuntimeError::StackUnderflow);
        next
    }
    fn peek_stack(&mut self) -> Result<&Value, RuntimeError> {
        let next = self
            .stack
            .last()
            .ok_or_else(|| RuntimeError::StackUnderflow);
        next
    }
    pub fn next_i16(&mut self) -> Result<i16, RuntimeError> {
        let next = self
            .current_chunk()?
            .read_i16(self.pc)
            .ok_or_else(|| RuntimeError::UnexpectedEof);
        self.pc += 2;
        next
    }
    pub fn next_byte(&mut self) -> Result<u8, RuntimeError> {
        let next = self
            .current_chunk()?
            .read_byte(self.pc)
            .ok_or_else(|| RuntimeError::UnexpectedEof);
        self.pc += 1;
        next
    }
    fn current_chunk(&self) -> Result<&Chunk, RuntimeError> {
        let current_function = self
            .heap
            .get(self.current_frame()?.function)
            .ok_or_else(|| RuntimeError::CannotFindObject)?;

        match current_function {
            Object::Function(f) => Ok(&f.chunk),
            _ => Err(RuntimeError::InvalidType), // TODO
        }
    }
    fn current_frame_mut(&mut self) -> Result<&mut CallFrame, RuntimeError> {
        self.call_frames
            .last_mut()
            .ok_or_else(|| RuntimeError::NoCallFrame)
    }
    fn current_frame(&self) -> Result<&CallFrame, RuntimeError> {
        self.call_frames
            .last()
            .ok_or_else(|| RuntimeError::NoCallFrame)
    }
    pub fn interpret(&mut self) -> Result<(), RuntimeError> {
        loop {
            let next_byte = self.next_byte()?;
            let op = Opcode::from_u8(next_byte).ok_or_else(|| RuntimeError::InvalidOpcode)?;
            match op {
                Opcode::LoadConstant => {
                    let i = self.next_byte()?;
                    let value = self
                        .current_chunk()?
                        .values
                        .get(i as usize)
                        .ok_or_else(|| RuntimeError::CannotFindConstant)?
                        .clone();
                    self.stack.push(value);
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
                                }
                                _ => return Err(RuntimeError::InvalidType),
                            };

                            Value::Object(handle)
                        }
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::Subtract => {
                    let a = self.pop_stack()?;
                    let b = self.pop_stack()?;
                    let result = match (a, b) {
                        (Value::Integer(x), Value::Integer(y)) => Value::Integer(x - y),
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
                Opcode::Negate => {
                    let a = self.pop_stack()?;
                    let result = match a {
                        Value::Integer(x) => Value::Integer(-x),
                        _ => return Err(RuntimeError::InvalidType),
                    };
                    self.stack.push(result);
                }
                Opcode::ReadLocal => {
                    // Copies a local at a position in the index
                    // and copies to the stack top.
                    let index = self.next_byte()?;
                    let offset = self.current_frame()?.stack_start;
                    let val = self
                        .stack
                        .get(index as usize + offset)
                        .ok_or_else(|| RuntimeError::StackOutOfRange)?
                        .clone();
                    self.stack.push(val);
                }
                Opcode::WriteLocal => {
                    // Writes the stack top to a
                    // local somewhere down the stack
                    let index = self.next_byte()?;
                    let new_val = self.pop_stack()?;
                    let offset = self.current_frame()?.stack_start;
                    let val = self
                        .stack
                        .get_mut(index as usize + offset)
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
                Opcode::Call => {
                    // arity is in the operand
                    let call_arity = self.next_byte()? as usize;
                    // Important to archive the pc
                    self.current_frame_mut()?.pc = self.pc;

                    let next_function_value = self
                        .stack
                        .get(
                            self.stack
                                .len()
                                .checked_sub(1 + call_arity)
                                .ok_or_else(|| RuntimeError::WrongArity)?,
                        )
                        .ok_or_else(|| RuntimeError::WrongArity)?;

                    let handle = next_function_value
                        .as_object_handle()
                        .ok_or_else(|| RuntimeError::WrongArity)?;

                    let object = self.get_object(handle)?;

                    match object {
                        Object::Function(f) => {
                            let arity = f.arity as usize;
                            self.call_frames.push(CallFrame {
                                function: handle,
                                pc: 0,
                                stack_start: self.stack.len() - arity - 1 as usize,
                            })
                        }
                        _ => return Err(RuntimeError::InvalidType),
                    }
                    self.pc = 0;
                }
                Opcode::Return => {
                    // root frame
                    if self.call_frames.len() <= 1 {
                        break;
                    }
                    let return_value = self.pop_stack()?;

                    // remove variables of callee function
                    self.stack.truncate(self.current_frame()?.stack_start);
                    // put back return value
                    self.stack.push(return_value);
                    self.call_frames.pop();
                    self.pc = self.current_frame()?.pc;
                }
                Opcode::LoadNil => self.stack.push(Value::Nil),
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
        let f = main_func(
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

        let mut vm = Vm::new(f, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(0).unwrap(), &Value::Integer(50));
    }
    #[test]
    fn unary_simple() {
        let f = main_func(
            vec![2],
            vec![
                Instruction::LoadConstant(0),
                Instruction::Negate,
                Instruction::Return,
            ],
        );
        let mut vm = Vm::new(f, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(0).unwrap(), &Value::Integer(-2));
    }
    #[test]
    fn vm_stack_1() {
        let f = main_func(
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
        let mut vm = Vm::new(f, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(0).unwrap(), &Value::Integer(16));
    }
    #[test]
    fn vm_stack_2() {
        let f = main_func(
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

        let mut vm = Vm::new(f, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack.get(2).unwrap(), &Value::Integer(5));
    }
    #[test]
    fn vm_stack_pop() {
        let f = main_func(
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

        let mut vm = Vm::new(f, Heap::new());
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
        let f = ObjFunction::new("".into(), 0, c);

        let mut vm = Vm::new(f, h);
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
        let f = main_func(
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
        let mut vm = Vm::new(f, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack, vec![Value::Bool(false), Value::Integer(200)])
    }
    #[test]
    fn jump() {
        let f = main_func(
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
        let mut vm = Vm::new(f, Heap::new());
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(
            vm.stack,
            vec![Value::Integer(11), Value::Integer(15), Value::Integer(13)]
        )
    }
    #[test]
    fn function_call() {
        let f = main_func_any_val(
            vec![
                Value::Object(0),
                Value::Integer(5),
                Value::Integer(13),
                Value::Integer(15),
            ],
            vec![
                Instruction::LoadConstant(1),
                Instruction::LoadConstant(0),
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
                Instruction::ReadLocal(1),
                Instruction::ReadLocal(2),
                Instruction::Subtract,
                Instruction::Return,
            ],
        );
        let mut h = Heap::new();
        h.push(Object::Function(a));
        dbg!(&f);
        let mut vm = Vm::new(f, h);
        assert_eq!(vm.interpret(), Ok(()));
        assert_eq!(vm.stack, vec![Value::Integer(10)]);
    }
}
