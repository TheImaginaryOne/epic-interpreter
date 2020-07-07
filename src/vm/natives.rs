use super::heap::{Heap, NativeFunction, Object, RustFunction};
use super::Value;

pub fn load_native_functions() -> Vec<NativeFunction> {
    vec![NativeFunction {
        arity: 1,
        name: "print".into(),
        function: RustFunction(Box::new(print)),
    }]
}

pub fn print(heap: &Heap, values: &[Value]) -> Value {
    match values[0] {
        Value::Integer(i) => print!("{}", i),
        Value::Object(handle) => {
            if let Some(obj) = heap.get(handle) {
                match obj {
                    Object::String(s) => print!("{}", s),
                    _ => print!("<object>"),
                }
            }
        }
        _ => print!("<value>"),
    };
    Value::Nil
}
