use crate::vm::chunk::Chunk;
use crate::vm::Value;

pub type Handle = usize;

#[derive(PartialEq, Debug)]
pub enum Object {
    String(String),
    Function(ObjFunction),
    NativeFunction(NativeFunction),
}

pub struct RustFunction(pub Box<dyn Fn(&Heap, &[Value]) -> Value>);
impl std::fmt::Debug for RustFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "<native function>")?;
        Ok(())
    }
}
/// dummy
impl std::cmp::PartialEq for RustFunction {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

#[derive(PartialEq, Debug)]
pub struct NativeFunction {
    pub arity: u8,
    pub name: String,
    pub function: RustFunction,
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
