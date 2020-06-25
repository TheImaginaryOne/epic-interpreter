use crate::vm::heap::{Handle, Object};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i32),
    Float(f32),
    Object(Handle),
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Return,
    LoadConstant(u8),
    Add,
    Multiply,
    Divide,
    ReadLocal(u8),
    WriteLocal(u8),
    PopStack,
}
#[derive(Primitive)]
pub enum Opcode {
    Return = 0x00,
    LoadConstant = 0x01,
    Add = 0x02, // subtract is a special case!
    Multiply = 0x03,
    Divide = 0x04,
    ReadLocal = 0x05,
    WriteLocal = 0x06,
    PopStack = 0x07,
}
#[derive(Debug, PartialEq)]
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
    pub fn write_byte(&mut self, b: u8) {
        self.bytes.push(b);
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
            Instruction::Add => self.write_op(Opcode::Add), // subtract is a special case!
            Instruction::Multiply => self.write_op(Opcode::Multiply),
            Instruction::Divide => self.write_op(Opcode::Multiply),
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
        }
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
        ];
        for (offset, i) in instrs {
            assert_eq!(offset, c.write_instr(i.clone()));
        }
    }
}
