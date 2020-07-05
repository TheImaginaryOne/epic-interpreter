# What is this?

A basic C-like language interpreter, which compiles to bytecode and is run on a VM.

Note that there is nothing special about this language, because it is just a project to learn basic parsing and execution of programming languages.

## Features

- variable assignment, integer arithmetic
- if, while statement
- Partial implementation of (first class) functions, no support for closures

## Sample
```
fun sum(n) {
    // sum integers from 1 .. n
    let i = 0;                     
    let x = 0;        
    while x < n {
        x = x + 1;
        i = i + x;
    }
    return i;
}
let p = sum(50);
```
