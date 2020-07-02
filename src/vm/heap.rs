use crate::vm::chunk::Chunk;

pub type Handle = usize;

#[derive(PartialEq, Debug)]
pub enum Object {
    String(String),
    Function(ObjFunction),
}

#[derive(PartialEq, Debug)]
pub struct ObjFunction {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: String, // is this useful??
}
impl ObjFunction {
    pub fn new(name: String, arity: u8, chunk: Chunk) -> Self {
        Self { arity, chunk, name }
    }
}

#[derive(Debug, PartialEq)]
pub struct Heap {
    objects: Vec<Object>,
}
impl Heap {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }
    pub fn push(&mut self, obj: Object) -> Handle {
        self.objects.push(obj);
        self.objects.len() - 1
    }
    pub fn get(&self, handle: Handle) -> Option<&Object> {
        self.objects.get(handle)
    }
}
