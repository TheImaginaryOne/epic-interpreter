#[macro_use]
extern crate enum_primitive_derive;

#[cfg(test)]
pub mod test_utils;

mod compiler;
mod vm;

use clap::Clap;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use compiler::ast::Spanned;
use compiler::code_gen::CodeGen;
use compiler::error::ParseError;
use compiler::parser::Parser;
use vm::{heap::Heap, natives, Vm};

fn main() {
    if let Err(e) = run() {
        eprintln!("{}", e);
    }
}

#[derive(Clap)]
struct Options {
    #[clap(short, long)]
    filename: String,
    #[clap(short, long)]
    debug: bool,
}

fn run() -> anyhow::Result<()> {
    let options = Options::parse();
    // TODO
    let src = std::fs::read_to_string(std::path::Path::new(&options.filename))?;

    let mut reporter = SimpleFiles::new();
    let file = reporter.add("source", &src);

    let mut diagnostics = Vec::new();
    let (bytecode, heap) = match Parser::new(&src).parse_program() {
        Ok(p) => match CodeGen::new().generate(&p, natives::load_native_functions()) {
            Ok(b) => b,
            Err(e) => {
                eprintln!("{:?}", e);
                return Ok(());
            }
        },
        Err(errors) => {
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = codespan_reporting::term::Config::default();

            for err in errors {
                diagnostics.push(diagnostic_from(file, err));
            }
            for diagnostic in diagnostics {
                codespan_reporting::term::emit(
                    &mut writer.lock(),
                    &config,
                    &reporter,
                    &diagnostic,
                )?;
            }
            return Ok(());
        }
    };
    let mut vm = Vm::new(bytecode, heap);
    match vm.interpret() {
        Ok(_) => (),
        Err(e) => eprintln!("{:?}", e),
    }
    if options.debug {
        println!("-- DEBUG INFO --");
        println!("{:#?}", vm);
    }
    Ok(())
}

fn diagnostic_from(file: usize, err: Spanned<ParseError>) -> Diagnostic<usize> {
    Diagnostic::error()
        .with_message(format!("{}", err.inner))
        .with_labels(vec![Label::primary(file, err.left..err.right)])
}
