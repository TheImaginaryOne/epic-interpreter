#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f32),
}

#[derive(Primitive)]
pub enum Opcode {
    Return = 0x00,
    Constant = 0x01,
    Add = 0x02, // subtract is a special case!
    Multiply = 0x03,
    Divide = 0x04,
    ReadLocal = 0x05,
    WriteLocal = 0x06,
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
    pub fn write_op(&mut self, op: Opcode) {
        self.bytes.push(op as u8);
    }
    pub fn write_constant(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }
}
