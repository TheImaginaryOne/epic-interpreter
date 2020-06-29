#[macro_use]
extern crate enum_primitive_derive;

#[cfg(test)]
pub mod test_utils;

mod compiler;
mod vm;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use compiler::ast::Spanned;
use compiler::code_gen::CodeGen;
use compiler::error::ParseError;
use compiler::parser::Parser;
use vm::Vm;

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
    }
}

fn run() -> anyhow::Result<()> {
    let src = r#"
    let x = 0;
    if 53 == 51 - 2 {
        x = -600
    } else if 44 == 43 + 1 {
        if -x * (-1) == x {
            x = 40000;
        }
    } else {
        x = -98480984;
    }
    let u = 122;
"#;
    let mut reporter = SimpleFiles::new();
    let file = reporter.add("source", src);

    let mut diagnostics = Vec::new();
    let (bytecode, heap) = match Parser::new(src).parse_program() {
        Ok(p) => match CodeGen::new().generate(&p) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("{:?}", e);
                return Ok(())
            }
        },
        Err(errors) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();
            
            for err in errors {
                diagnostics.push(diagnostic_from(file, err));
            }
            for diagnostic in diagnostics {
                codespan_reporting::term::emit(&mut writer.lock(), &config, &reporter, &diagnostic)?;
            }
            return Ok(());
        }
    };
    let mut vm = Vm::new(bytecode, heap);
    match vm.interpret() {
        Ok(_) => (),
        Err(e) => eprintln!("{:?}", e),
    }
    println!("{:#?}", vm);
    Ok(())
}

fn diagnostic_from(
    file: usize,
    err: Spanned<ParseError>,
) -> Diagnostic<usize> {

    Diagnostic::error()
        .with_message(format!("{}", err.inner))
        .with_labels(vec![Label::primary(file, err.left..err.right)])
}
