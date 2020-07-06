use crate::vm::heap::{Handle, Object};
use crate::vm::Offset;
use num_traits::cast::FromPrimitive;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i32),
    Float(f32),
    Bool(bool),
    Object(Handle),
    Nil,
}
impl Value {
    pub fn as_object_handle(&self) -> Option<Handle> {
        if let Self::Object(h) = self {
            Some(*h)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Return,
    LoadConstant(u8),
    Add,
    Subtract,
    Negate,
    Multiply,
    Divide,
    ReadLocal(u8),
    WriteLocal(u8),
    PopStack,
    Jump(i16),
    JumpIfFalse(i16),
    Equal,
    Less,
    Greater,
    Call(u8),
    LoadNil,
    ReadGlobal(u8),
    WriteGlobal(u8),
}
#[derive(Primitive, Debug)]
pub enum Opcode {
    Return = 0,
    LoadConstant = 1,
    Add = 2, // subtract is a special case!
    Multiply = 3,
    Subtract = 4,
    Negate = 5,
    Divide = 6,
    ReadLocal = 7,
    WriteLocal = 8,
    PopStack = 9,
    Jump = 10,
    JumpIfFalse = 11,
    Equal = 12,
    Less = 13,
    Greater = 14,
    Call = 15,
    LoadNil = 16,
    ReadGlobal = 17,
    WriteGlobal = 18,
}
#[derive(PartialEq)]
pub struct Chunk {
    pub bytes: Vec<u8>,
    pub values: Vec<Value>,
}
impl Chunk {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
            bytes: Vec::new(),
        }
    }
    pub fn next_location(&self) -> usize {
        self.bytes.len()
    }
    pub fn read_byte(&self, i: usize) -> Option<u8> {
        self.bytes.get(i).copied()
    }
    pub fn write_byte(&mut self, b: u8) {
        self.bytes.push(b);
    }
    pub fn read_i16(&self, i: usize) -> Option<i16> {
        let b = [*self.bytes.get(i)?, *self.bytes.get(i + 1)?];
        // from big endian bytes
        Some(i16::from_be_bytes(b))
    }
    pub fn write_i16(&mut self, i: i16) {
        let b = i.to_be_bytes();
        self.bytes.push(b[0]);
        self.bytes.push(b[1]);
    }
    pub fn write_i16_at(&mut self, value: i16, i: usize) {
        let b = value.to_be_bytes();
        // unchecked!
        self.bytes[i] = b[0];
        self.bytes[i + 1] = b[1];
    }
    pub fn write_op(&mut self, op: Opcode) -> usize {
        self.bytes.push(op as u8);
        self.bytes.len() - 1
    }
    pub fn write_constant(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }
    pub fn write_instr(&mut self, i: Instruction) -> usize {
        match i {
            Instruction::Return => self.write_op(Opcode::Return),
            Instruction::LoadConstant(b) => {
                let offset = self.write_op(Opcode::LoadConstant);
                self.write_byte(b);
                offset
            }
            Instruction::Add => self.write_op(Opcode::Add),
            Instruction::Subtract => self.write_op(Opcode::Subtract),
            Instruction::Negate => self.write_op(Opcode::Negate),
            Instruction::Multiply => self.write_op(Opcode::Multiply),
            Instruction::Divide => self.write_op(Opcode::Divide),
            Instruction::ReadGlobal(b) => {
                let offset = self.write_op(Opcode::ReadGlobal);
                self.write_byte(b);
                offset
            }
            Instruction::WriteGlobal(b) => {
                let offset = self.write_op(Opcode::WriteGlobal);
                self.write_byte(b);
                offset
            }
            Instruction::ReadLocal(b) => {
                let offset = self.write_op(Opcode::ReadLocal);
                self.write_byte(b);
                offset
            }
            Instruction::WriteLocal(b) => {
                let offset = self.write_op(Opcode::WriteLocal);
                self.write_byte(b);
                offset
            }
            Instruction::PopStack => self.write_op(Opcode::PopStack),
            Instruction::Jump(i) => {
                let offset = self.write_op(Opcode::Jump);
                self.write_i16(i);
                offset
            }
            Instruction::JumpIfFalse(i) => {
                let offset = self.write_op(Opcode::JumpIfFalse);
                self.write_i16(i);
                offset
            }
            Instruction::Greater => self.write_op(Opcode::Greater),
            Instruction::Less => self.write_op(Opcode::Less),
            Instruction::Equal => self.write_op(Opcode::Equal),
            Instruction::Call(b) => {
                let offset = self.write_op(Opcode::Call);
                self.write_byte(b);
                offset
            }
            Instruction::LoadNil => self.write_op(Opcode::LoadNil),
        }
    }
}
impl std::fmt::Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        let mut pc = 0;
        write!(f, "[")?;
        while let Some(next) = self.bytes.get(pc) {
            let op = match Opcode::from_u8(*next) {
                Some(x) => x,
                None => {
                    continue;
                }
            };

            let start = pc + 1; // add one to go to first operand
            let (instr, offset) = match op {
                Opcode::LoadConstant => (
                    Instruction::LoadConstant(self.read_byte(start).unwrap_or(0)),
                    2,
                ),
                Opcode::Add => (Instruction::Add, 1),
                Opcode::Subtract => (Instruction::Subtract, 1),
                Opcode::Negate => (Instruction::Negate, 1),
                Opcode::Multiply => (Instruction::Multiply, 1),
                Opcode::Divide => (Instruction::Divide, 1),
                Opcode::ReadGlobal => (
                    Instruction::ReadGlobal(self.read_byte(start).unwrap_or(0)),
                    2,
                ),
                Opcode::WriteGlobal => (
                    Instruction::WriteGlobal(self.read_byte(start).unwrap_or(0)),
                    2,
                ),
                Opcode::ReadLocal => (
                    Instruction::ReadLocal(self.read_byte(start).unwrap_or(0)),
                    2,
                ),
                Opcode::WriteLocal => (
                    Instruction::WriteLocal(self.read_byte(start).unwrap_or(0)),
                    2,
                ),
                Opcode::PopStack => (Instruction::PopStack, 1),
                Opcode::Jump => (Instruction::Jump(self.read_i16(start).unwrap_or(0)), 3),
                Opcode::JumpIfFalse => (
                    Instruction::JumpIfFalse(self.read_i16(start).unwrap_or(0)),
                    3,
                ),
                Opcode::Greater => (Instruction::Greater, 1),
                Opcode::Less => (Instruction::Less, 1),
                Opcode::Equal => (Instruction::Equal, 1),
                Opcode::Return => (Instruction::Return, 1),
                Opcode::Call => (Instruction::Call(self.read_byte(start).unwrap_or(0)), 2),
                Opcode::LoadNil => (Instruction::LoadNil, 1),
            };
            write!(f, "{}: {:?}, ", pc, instr)?;
            pc += offset;
        }
        write!(f, "] ")?;
        writeln!(f, "values: {:?}", self.values)?;
        Ok(())
    }
}
#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn check_offsets() {
        let mut c = Chunk::new();
        let instrs = vec![
            (0, Instruction::Return),
            (1, Instruction::LoadConstant(0)),
            (3, Instruction::Add),
            (4, Instruction::Multiply),
            (5, Instruction::Divide),
            (6, Instruction::ReadLocal(2)),
            (8, Instruction::WriteLocal(4)),
            (10, Instruction::PopStack),
            (11, Instruction::Jump(-10)),
            (14, Instruction::JumpIfFalse(3)),
            (17, Instruction::Greater),
            (18, Instruction::Less),
            (19, Instruction::Equal),
            (20, Instruction::Subtract),
            (21, Instruction::Negate),
            (22, Instruction::Call(4)),
            (24, Instruction::LoadNil),
            (25, Instruction::ReadLocal(2)),
            (27, Instruction::WriteLocal(4)),
        ];
        for (offset, i) in instrs {
            assert_eq!(offset, c.write_instr(i.clone()));
        }
    }
    #[test]
    fn write_integers() {
        let mut c = Chunk::new();
        c.write_instr(Instruction::Jump(0x1ba8));
        c.write_instr(Instruction::JumpIfFalse(0x0a12));
        assert_eq!(c.read_i16(1).unwrap(), 0x1ba8);
        assert_eq!(c.read_i16(4).unwrap(), 0x0a12);
    }
    #[test]
    fn patch_bytes() {
        let mut c = Chunk::new();
        c.write_instr(Instruction::Jump(0x1ba8));
        c.write_i16_at(0x0101, 1);
        assert_eq!(c.read_i16(1).unwrap(), 0x0101);
    }
}
