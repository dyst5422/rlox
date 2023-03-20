use std::fmt::Display;

use crate::token::Token;

pub enum Stmt {
	Expression(Expr),
	Print(Expr),
	Declaration(Declaration),
	Block(Vec<Stmt>),
	If(Box<If>),
	// While(Expr, Box<Stmt>),
	// Function(Token, Vec<Token>, Vec<Stmt>),
	// Return(Token, Option<Expr>),
	// Class(Token, Option<Token>, Vec<Stmt>),
}

impl Display for Stmt {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Stmt::Expression(expr) => write!(f, "{}", expr),
			Stmt::Print(expr) => write!(f, "{}", expr),
			Stmt::Declaration(decl) => write!(f, "{}", decl),
			Stmt::Block(stmts) => {
				let mut string = String::new();
				string.push_str("{");
				for stmt in stmts {
					string.push_str(format!("{}", stmt).as_str());
				}
				string.push_str("}");
				write!(f, "{}", string)
			}
			Stmt::If(r#if) => write!(f, "{}", r#if),
			// Self::Assignment(assignment) => write!(f, "{}", assignment),
		}
	}
}

pub struct If {
	pub token: Token,
	pub condition: Expr,
	pub then_block: Stmt,
	pub else_block: Option<Stmt>,
}

impl If {
	pub fn new(token: Token, condition: Expr, then_block: Stmt, else_block: Option<Stmt>) -> Self {
		Self {
			token,
			condition,
			then_block,
			else_block,
		}
	}
}

impl Display for If {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut string = String::new();
		string.push_str("if (");
		string.push_str(format!("{}", self.condition).as_str());
		string.push_str(") ");
		string.push_str(format!("{}", self.then_block).as_str());
		if let Some(r#else) = &self.else_block {
			string.push_str(" else ");
			string.push_str(format!("{}", r#else).as_str());
		}
		write!(f, "{}", string)
	}
}

#[derive(Debug, Clone)]
pub struct Assignment {
	pub left: Token,
	pub right: Expr,
}

impl Assignment {
	pub fn new(left: Token, right: Expr) -> Self {
		Self { left, right }
	}
}

impl Display for Assignment {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} = {};", self.left.lexeme, self.right)
	}
}

#[derive(Debug, Clone)]
pub struct Declaration {
	pub name: Token,
	pub initializer: Expr,
	pub mutability: bool,
}

impl Declaration {
	pub fn new(name: Token, initializer: Expr, mutability: bool) -> Self {
		Self {
			name,
			initializer,
			mutability,
		}
	}
}

impl Display for Declaration {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mutability_string = match self.mutability {
			true => " mut",
			false => "",
		};
		write!(
			f,
			"let{} {} = {};",
			mutability_string, self.name.lexeme, self.initializer
		)
	}
}

#[derive(Debug, Clone)]
pub enum Expr {
	Binary(Box<Binary>),
	Grouping(Box<Grouping>),
	Literal(Box<Literal>),
	Unary(Box<Unary>),
	Variable(Token),
	Assign(Box<Assignment>),
}

impl Display for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Expr::Binary(expr) => write!(f, "{}", expr),
			Expr::Grouping(expr) => write!(f, "{}", expr),
			Expr::Literal(expr) => write!(f, "{}", expr),
			Expr::Unary(expr) => write!(f, "{}", expr),
			Expr::Variable(token) => write!(f, "{}", token.lexeme),
			Expr::Assign(expr) => write!(f, "{}", expr),
		}
	}
}

#[derive(Debug, Clone)]
pub enum Literal {
	String(Token),
	Number(Token),
	True,
	False,
	Nil,
}

impl std::fmt::Display for Literal {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Literal::String(token) => write!(f, "{}", token.lexeme),
			Literal::Number(token) => write!(f, "{}", token.lexeme),
			Literal::True => write!(f, "true"),
			Literal::False => write!(f, "false"),
			Literal::Nil => write!(f, "nil"),
		}
	}
}

#[derive(Debug, Clone)]
pub struct Grouping {
	pub expr: Expr,
}

impl std::fmt::Display for Grouping {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "(group {})", self.expr)
	}
}

#[derive(Debug, Clone)]
pub struct Unary {
	pub operator: Token,
	pub right: Expr,
}

impl std::fmt::Display for Unary {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} {})", self.operator.lexeme, self.right)
	}
}

#[derive(Debug, Clone)]
pub struct Binary {
	pub left: Expr,
	pub right: Expr,
	pub operator: Token,
}

impl std::fmt::Display for Binary {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "({} {} {})", self.operator.lexeme, self.left, self.right)
	}
}

// Tests
#[cfg(test)]
mod tests {

	use crate::token::TokenType;

	use super::*;

	#[test]
	fn test_print() {
		let expr = Expr::Binary(Box::new(Binary {
			left: Expr::Unary(Box::new(Unary {
				operator: Token::new(TokenType::Minus, "-".into(), 1, 1),
				right: Expr::Literal(Box::new(Literal::Number(Token::new(
					TokenType::Number(123.0),
					"123".into(),
					1,
					1,
				)))),
			})),
			right: Expr::Grouping(Box::new(Grouping {
				expr: Expr::Literal(Box::new(Literal::Number(Token::new(
					TokenType::Number(45.67),
					"45.67".into(),
					1,
					1,
				)))),
			})),
			operator: Token::new(TokenType::Star, "*".into(), 1, 1),
		}));

		let printed: String = format!("{expr}");

		assert_eq!(printed, "(* (- 123) (group 45.67))".to_owned())
	}
}
