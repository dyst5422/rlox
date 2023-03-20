mod ast;
mod interpreter;
mod parser;
mod scanner;
mod token;

use crate::interpreter::Runtime;
use interpreter::Environment;
use parser::Parser;
use rustyline::DefaultEditor;
use scanner::Scanner;
use std::{cell::RefCell, fs::read_to_string, rc::Rc};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut args: Vec<String> = std::env::args().collect();

	args.remove(0);

	if args.len() > 1 {
		println!("Usage: rlox [script]");
		return Ok(());
	} else if args.len() == 1 {
		run_file(args[0].clone())?;
	} else {
		run_prompt()?;
	}
	Ok(())
}

fn run_file(path: String) -> Result<(), Box<dyn std::error::Error>> {
	let source_code = read_to_string(path)?;
	run_source(source_code, Rc::new(RefCell::new(Environment::new(None))))?;
	Ok(())
}

fn run_prompt() -> Result<(), Box<dyn std::error::Error>> {
	let environment = Rc::new(RefCell::new(Environment::new(None)));

	let mut rl = DefaultEditor::new()?;
	loop {
		let readline = rl.readline("> ");
		match readline {
			Ok(line) => {
				rl.add_history_entry(line.as_str())?;
				run_source(line, environment.clone())?;
			}
			Err(rustyline::error::ReadlineError::Interrupted) => {
				println!("CTRL-C");
				break;
			}
			Err(rustyline::error::ReadlineError::Eof) => {
				println!("CTRL-D");
				break;
			}
			Err(err) => {
				println!("Error: {:?}", err);
				break;
			}
		}
	}
	Ok(())
}

fn run_source(
	source_code: String,
	environment: Rc<RefCell<Environment>>,
) -> Result<(), Box<dyn std::error::Error>> {
	let mut scanner = Scanner::new();
	let (tokens, scan_errors) = scanner.scan_tokens(source_code);

	// println!("\nTokens: ");
	// for token in &tokens {
	// 	println!("{token}");
	// }

	if scan_errors.len() > 0 {
		println!("\nScan Errors: ");
		for error in &scan_errors {
			println!("{error}");
		}
	}

	let mut parser = Parser::new();
	let (statements, parse_errors) = parser.parse(tokens);

	// println!("\nAST: {}", ExprVecDisplay(&ast));

	if parse_errors.len() > 0 {
		println!("\nParse Errors: ");
		for error in &parse_errors {
			println!("{error}");
		}
	}

	let runtime = Runtime::new();

	match runtime.run(statements, environment) {
		Ok(_) => {}
		Err(err) => {
			println!("Runtime Error: {}", err);
		}
	}

	Ok(())
}
