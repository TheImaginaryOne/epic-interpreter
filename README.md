# What is this?

A basic C-like language interpreter, which compiles to bytecode and is run on a VM.

Note that there is nothing special about this language, because it is just a project to learn basic parsing and execution of programming languages.

## Features

- variable assignment, integer arithmetic, strings
- if, while statement
- Partial implementation of (first class) functions, no support for closures
- native function calls (eg printing)

## Sample
```
fun sum(a, n) {
    // sum integers from 1 .. 50
    let i = 0;
    let x = a - 1;
    while x < n {
        x = x + 1;
        i = i + x;
    }
    return i;
}
let s = "Sum of ";
let t = "ints from 1..50: ";
fun tfunc() {
    return t;
}
print(s + tfunc());
print(sum(1, 50));
```
