use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
	ast,
	token::{Token, TokenType},
};

#[derive(Debug, Clone)]
pub enum RuntimeError {
	TypeError(TypeError),
	UndefinedVariableError(UndefinedVariableError),
	RedefineVariableError(RedefineVariableError),
	ImmmutableAssignmentError(ImmmutableAssignmentError),
}

impl std::fmt::Display for RuntimeError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			RuntimeError::TypeError(type_error) => write!(f, "{}", type_error),
			RuntimeError::UndefinedVariableError(undefined_error) => {
				write!(f, "{}", undefined_error)
			}
			RuntimeError::RedefineVariableError(already_defined_error) => {
				write!(f, "{}", already_defined_error)
			}
			RuntimeError::ImmmutableAssignmentError(immutable_assignment_error) => {
				write!(f, "{}", immutable_assignment_error)
			}
		}
	}
}

impl std::error::Error for RuntimeError {}

#[derive(Debug, Clone)]
pub struct TypeError {
	message: &'static str,
	token: Token,
	values: Vec<Value>,
}

impl TypeError {
	fn new(token: Token, values: Vec<Value>, message: &'static str) -> Self {
		Self {
			token,
			values,
			message,
		}
	}
}

impl std::fmt::Display for TypeError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let values_string = self
			.values
			.iter()
			.map(|value| format!("{value}"))
			.collect::<Vec<String>>()
			.join(", ");
		write!(
			f,
			"[{}, {}] TypeError: {}\nValues: [{}]",
			self.token.line, self.token.column, self.message, values_string
		)
	}
}

impl std::error::Error for TypeError {}

#[derive(Debug, Clone)]
pub struct UndefinedVariableError {
	token: Token,
}

impl UndefinedVariableError {
	fn new(token: Token) -> Self {
		Self { token }
	}
}

impl std::fmt::Display for UndefinedVariableError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"[{}, {}] UndefinedError: Undefined variable '{}'",
			self.token.line, self.token.column, self.token.lexeme
		)
	}
}

impl std::error::Error for UndefinedVariableError {}

#[derive(Debug, Clone)]
pub struct RedefineVariableError {
	token: Token,
}

impl RedefineVariableError {
	fn new(token: Token) -> Self {
		Self { token }
	}
}

impl std::fmt::Display for RedefineVariableError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"[{}, {}] RedefineVariableError: Variable '{}' already defined",
			self.token.line, self.token.column, self.token.lexeme
		)
	}
}

impl std::error::Error for RedefineVariableError {}

#[derive(Debug, Clone)]
pub struct ImmmutableAssignmentError {
	token: Token,
}

impl ImmmutableAssignmentError {
	fn new(token: Token) -> Self {
		Self { token }
	}
}

impl std::fmt::Display for ImmmutableAssignmentError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"[{}, {}] ImmmutableAssignmentError: Variable '{}' is immutable",
			self.token.line, self.token.column, self.token.lexeme
		)
	}
}

impl std::error::Error for ImmmutableAssignmentError {}

#[derive(Debug, Clone)]
pub enum Value {
	Number(f64),
	String(String),
	Bool(bool),
	Nil,
}

impl std::fmt::Display for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Value::Number(number) => write!(f, "{}", number),
			Value::String(string) => write!(f, "{}", string),
			Value::Bool(boolean) => write!(f, "{}", boolean),
			Value::Nil => write!(f, "nil"),
		}
	}
}

pub struct Environment {
	parent: Option<Rc<RefCell<Environment>>>,
	variables: HashMap<String, (Value, bool)>,
}

impl Environment {
	pub fn new(parent: Option<Rc<RefCell<Environment>>>) -> Self {
		Self {
			parent,
			variables: HashMap::new(),
		}
	}

	pub fn define(
		&mut self,
		name: &Token,
		value: Value,
		mutability: bool,
	) -> Result<(), RuntimeError> {
		match self.variables.get(&name.lexeme) {
			Some(_) => Err(RuntimeError::RedefineVariableError(
				RedefineVariableError::new(name.clone()),
			)),
			None => {
				self.variables
					.insert(name.lexeme.clone(), (value, mutability));
				Ok(())
			}
		}
	}

	pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
		match self.variables.get(&name.lexeme) {
			Some((value, _mutability)) => Ok(value.clone()),
			None => match &self.parent {
				Some(parent) => parent.borrow_mut().get(name),
				None => Err(RuntimeError::UndefinedVariableError(
					UndefinedVariableError::new(name.clone()),
				)),
			},
		}
	}

	pub fn assign(&mut self, name: &Token, value: Value) -> Result<(), RuntimeError> {
		match self.variables.get(&name.lexeme) {
			Some((_value, mutability)) => match mutability {
				true => {
					self.variables
						.insert(name.lexeme.clone(), (value.clone(), true));
					Ok(())
				}
				false => {
					return Err(RuntimeError::ImmmutableAssignmentError(
						ImmmutableAssignmentError::new(name.clone()),
					))
				}
			},
			None => match &mut self.parent {
				Some(parent) => parent.borrow_mut().assign(name, value),
				None => Err(RuntimeError::UndefinedVariableError(
					UndefinedVariableError::new(name.clone()),
				)),
			},
		}
	}
}

#[derive(Debug, Clone)]
pub struct Runtime {}

impl Runtime {
	pub fn new() -> Self {
		Self {}
	}

	pub fn run(
		&self,
		statements: Vec<ast::Stmt>,
		environment: Rc<RefCell<Environment>>,
	) -> Result<(), RuntimeError> {
		for statement in statements {
			match statement.execute(environment.clone()) {
				Ok(_) => continue,
				Err(err) => return Err(err),
			}
		}
		Ok(())
	}
}

pub trait Executable {
	fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError>;
}

impl Executable for ast::Stmt {
	fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
		match self {
			ast::Stmt::Expression(expr) => {
				expr.evaluate(environment)?;
			}
			ast::Stmt::Print(expr) => {
				let value = expr.evaluate(environment)?;
				println!("{}", value);
			}
			ast::Stmt::Declaration(declaration) => {
				declaration.execute(environment)?;
			}
			ast::Stmt::Block(block) => {
				let environment = Rc::new(RefCell::new(Environment::new(Some(environment))));
				for statement in block {
					statement.execute(environment.clone())?;
				}
			}
			ast::Stmt::If(r#if) => {
				let condition_result = r#if.condition.evaluate(environment.clone())?;
				match condition_result {
					Value::Bool(true) => {
						r#if.then_block.execute(environment.clone())?;
					}
					Value::Bool(false) => {
						if let Some(else_block) = &r#if.else_block {
							else_block.execute(environment.clone())?;
						}
					}
					_ => {
						return Err(RuntimeError::TypeError(TypeError::new(
							r#if.token.clone(),
							vec![condition_result],
							"if condition must be a boolean",
						)))
					}
				}
			}
		}
		Ok(())
	}
}

impl Executable for ast::Declaration {
	fn execute(&self, environment: Rc<RefCell<Environment>>) -> Result<(), RuntimeError> {
		let value = self.initializer.evaluate(environment.clone())?;
		environment
			.borrow_mut()
			.define(&self.name, value, self.mutability)?;
		Ok(())
	}
}

pub trait Evaluateable {
	fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError>;
}

impl Evaluateable for ast::Expr {
	fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
		match self {
			ast::Expr::Literal(literal) => literal.evaluate(environment),
			ast::Expr::Unary(unary) => unary.evaluate(environment),
			ast::Expr::Binary(binary) => binary.evaluate(environment),
			ast::Expr::Grouping(grouping) => grouping.evaluate(environment),
			ast::Expr::Variable(variable) => environment.borrow().get(&variable),
			ast::Expr::Assign(assign) => {
				let value = assign.right.evaluate(environment.clone())?;
				environment
					.borrow_mut()
					.assign(&assign.left, value.clone())?;
				Ok(value)
			}
		}
	}
}

impl Evaluateable for ast::Literal {
	fn evaluate(&self, _environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
		match self {
			ast::Literal::Number(token) => match token.r#type {
				TokenType::Number(number) => Ok(Value::Number(number)),
				_ => unreachable!("Invalid token type for number literal"),
			},
			ast::Literal::String(token) => match &token.r#type {
				TokenType::String(string) => Ok(Value::String(string.clone())),
				_ => unreachable!("Invalid token type for string literal"),
			},
			ast::Literal::False => Ok(Value::Bool(false)),
			ast::Literal::True => Ok(Value::Bool(true)),
			ast::Literal::Nil => Ok(Value::Nil),
		}
	}
}

impl Evaluateable for ast::Grouping {
	fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
		self.expr.evaluate(environment)
	}
}

impl Evaluateable for ast::Unary {
	fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
		let right = self.right.evaluate(environment)?;
		match self.operator.r#type {
			TokenType::Minus => match right {
				Value::Number(number) => Ok(Value::Number(-number)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![right],
					"Invalid operand for unary minus",
				))),
			},
			TokenType::Bang => match right {
				Value::Bool(boolean) => Ok(Value::Bool(!boolean)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![right],
					"Invalid operand for unary bang",
				))),
			},
			_ => unreachable!("Invalid token type for unary operator"),
		}
	}
}

impl Evaluateable for ast::Binary {
	fn evaluate(&self, environment: Rc<RefCell<Environment>>) -> Result<Value, RuntimeError> {
		let left = self.left.evaluate(environment.clone())?;
		let right = self.right.evaluate(environment)?;

		match self.operator.r#type {
			TokenType::Minus => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Number(left - right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary minus",
				))),
			},
			TokenType::Slash => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Number(left / right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary slash",
				))),
			},
			TokenType::Star => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Number(left * right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary star",
				))),
			},
			TokenType::Plus => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Number(left + right)),
				(Value::String(left), Value::String(right)) => {
					Ok(Value::String(left.to_owned() + right))
				}
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary plus",
				))),
			},
			TokenType::Greater => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left > right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary greater",
				))),
			},
			TokenType::GreaterEqual => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left >= right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary greater equal",
				))),
			},
			TokenType::Less => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left < right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary less",
				))),
			},
			TokenType::LessEqual => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left <= right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary less equal",
				))),
			},
			TokenType::BangEqual => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left != right)),
				(Value::String(left), Value::String(right)) => Ok(Value::Bool(left != right)),
				(Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left != right)),
				(Value::Nil, Value::Nil) => Ok(Value::Bool(false)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary bang equal",
				))),
			},
			TokenType::EqualEqual => match (&left, &right) {
				(Value::Number(left), Value::Number(right)) => Ok(Value::Bool(left == right)),
				(Value::String(left), Value::String(right)) => Ok(Value::Bool(left == right)),
				(Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(left == right)),
				(Value::Nil, Value::Nil) => Ok(Value::Bool(true)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary equal equal",
				))),
			},
			TokenType::And => match (&left, &right) {
				(Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(*left && *right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary and",
				))),
			},
			TokenType::Or => match (&left, &right) {
				(Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(*left || *right)),
				_ => Err(RuntimeError::TypeError(TypeError::new(
					self.operator.clone(),
					vec![left, right],
					"Invalid operands for binary or",
				))),
			},
			_ => unreachable!("Invalid token type for binary operator"),
		}
	}
}
