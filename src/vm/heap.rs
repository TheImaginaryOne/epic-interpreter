pub type Handle = usize;

#[derive(PartialEq, Debug)]
pub enum Object {
    String(String),
}

#[derive(Debug)]
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
