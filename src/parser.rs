use crate::ast::{Assignment, Binary, Declaration, If, Stmt};
use crate::{
	ast::{Expr, Grouping, Literal, Unary},
	token::{Token, TokenType},
};

#[derive(Debug, Clone)]
pub struct ParseError {
	pub message: &'static str,
	pub token: Token,
}

impl ParseError {
	pub fn new(token: Token, message: &'static str) -> Self {
		Self { message, token }
	}
}

impl std::fmt::Display for ParseError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let location = match self.token.r#type {
			TokenType::Eof => " at end".into(),
			_ => format!(" at '{}'", self.token.lexeme),
		};
		write!(
			f,
			"[{}, {}] Error{location}: {}",
			self.token.line, self.token.column, self.message
		)
	}
}

impl std::error::Error for ParseError {}

pub struct Parser {
	tokens: Vec<Token>,
	current: usize,
	declarations: Vec<String>,
}

impl Parser {
	pub fn new() -> Self {
		Self {
			tokens: vec![],
			current: 0,
			declarations: vec![],
		}
	}

	pub fn parse(&mut self, tokens: Vec<Token>) -> (Vec<Stmt>, Vec<ParseError>) {
		self.tokens = tokens;

		let mut statements = vec![];
		let mut errors = vec![];

		while !self.is_at_end() {
			match self.declaration() {
				Ok(expr) => {
					statements.push(expr);
					continue;
				}
				Err(err) => {
					errors.push(err);
					Expr::Literal(Box::new(Literal::Nil))
				}
			};
		}

		self.current = 0;
		self.tokens = vec![];
		self.declarations = vec![];

		(statements, errors)
	}

	fn declaration(&mut self) -> Result<Stmt, ParseError> {
		if self.match_tokens(vec![TokenType::Let]) {
			return self.let_declaration();
		}

		match self.statement() {
			Ok(stmt) => Ok(stmt),
			Err(err) => {
				self.synchronize();
				Err(err)
			}
		}
	}

	fn let_declaration(&mut self) -> Result<Stmt, ParseError> {
		println!("{:?}", self.tokens[self.current]);

		println!("is mut token: {}", self.check(TokenType::Mut));

		let mutability = self.match_tokens(vec![TokenType::Mut]);

		let name = self.consume(
			TokenType::Identifier("".to_string()),
			"Expect variable name.",
		)?;

		if self.declarations.iter().any(|decl| decl.eq(&name.lexeme)) {
			return Err(ParseError::new(
				name,
				"Already a variable with this name in this scope.",
			));
		}

		self.consume(TokenType::Equal, "Expect '=' after variable name.")?;

		let initializer = self.expression()?;
		self.consume(
			TokenType::Semicolon,
			"Expect ';' after variable declaration.",
		)?;
		self.declarations.push(name.lexeme.clone());

		println!("mutable: {:?}", mutability);
		Ok(Stmt::Declaration(Declaration::new(
			name,
			initializer,
			mutability,
		)))
	}

	fn statement(&mut self) -> Result<Stmt, ParseError> {
		if self.match_tokens(vec![TokenType::Print]) {
			return Ok(self.print_statement()?);
		}
		if self.match_tokens(vec![TokenType::LeftBrace]) {
			return Ok(Stmt::Block(self.block()?));
		}
		if self.match_tokens(vec![TokenType::If]) {
			return Ok(self.if_statement()?);
		}

		Ok(self.expression_statement()?)
	}

	fn if_statement(&mut self) -> Result<Stmt, ParseError> {
		self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
		let condition = self.expression()?;
		self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

		let then_branch = self.statement()?;
		let else_branch = if self.match_tokens(vec![TokenType::Else]) {
			Some(self.statement()?)
		} else {
			None
		};

		Ok(Stmt::If(Box::new(If::new(
			self.previous(),
			condition,
			then_branch,
			else_branch,
		))))
	}

	fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
		let mut statements = vec![];

		while !self.check(TokenType::RightBrace) && !self.is_at_end() {
			statements.push(self.declaration()?);
		}

		self.consume(TokenType::RightBrace, "Expect '}' after block.")?;

		Ok(statements)
	}

	fn print_statement(&mut self) -> Result<Stmt, ParseError> {
		let expr = self.expression()?;
		self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
		Ok(Stmt::Print(expr))
	}

	fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
		let expr = self.expression()?;
		self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
		Ok(Stmt::Expression(expr))
	}

	fn expression(&mut self) -> Result<Expr, ParseError> {
		Ok(self.assignment()?)
	}

	fn assignment(&mut self) -> Result<Expr, ParseError> {
		let expr = self.logic_or()?;

		if self.match_tokens(vec![TokenType::Equal]) {
			let equals = self.previous();
			let value = self.assignment()?;

			match expr {
				Expr::Variable(token) => {
					return Ok(Expr::Assign(Box::new(Assignment::new(token, value))));
				}
				_ => return Err(ParseError::new(equals, "Invalid assignment target.")),
			}
		}

		Ok(expr)
	}

	fn logic_or(&mut self) -> Result<Expr, ParseError> {
		let mut expr = self.logic_and()?;

		while self.match_tokens(vec![TokenType::Or]) {
			let operator = self.previous();
			let right = self.logic_and()?;
			expr = Expr::Binary(Box::new(Binary {
				left: expr,
				operator: operator,
				right: right,
			}));
		}

		Ok(expr)
	}

	fn logic_and(&mut self) -> Result<Expr, ParseError> {
		let mut expr = self.equality()?;

		while self.match_tokens(vec![TokenType::And]) {
			let operator = self.previous();
			let right = self.equality()?;
			expr = Expr::Binary(Box::new(Binary {
				left: expr,
				operator: operator,
				right: right,
			}));
		}

		Ok(expr)
	}

	fn equality(&mut self) -> Result<Expr, ParseError> {
		let mut expr = self.comparison()?;

		while self.match_tokens(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
			let operator = self.previous();
			let right = self.comparison()?;
			expr = Expr::Binary(Box::new(Binary {
				left: expr,
				operator: operator,
				right: right,
			}));
		}

		Ok(expr)
	}

	fn match_tokens(&mut self, types: Vec<TokenType>) -> bool {
		for token_type in types {
			if self.check(token_type) {
				self.advance();
				return true;
			}
		}

		false
	}

	fn check(&self, token_type: TokenType) -> bool {
		if self.is_at_end() {
			return false;
		}

		std::mem::discriminant(&self.peek().r#type) == std::mem::discriminant(&token_type)
	}

	fn advance(&mut self) -> () {
		if !self.is_at_end() {
			self.current += 1;
		}
	}

	fn is_at_end(&self) -> bool {
		self.peek().r#type == TokenType::Eof
	}

	fn peek(&self) -> &Token {
		&self.tokens[self.current]
	}

	fn previous(&mut self) -> Token {
		let current = self.current;
		self.current -= 1;
		self.tokens.remove(current - 1)
	}

	fn comparison(&mut self) -> Result<Expr, ParseError> {
		let mut expr = self.term()?;

		while self.match_tokens(vec![
			TokenType::Greater,
			TokenType::GreaterEqual,
			TokenType::Less,
			TokenType::LessEqual,
		]) {
			let operator = self.previous();
			let right = self.term()?;
			expr = Expr::Binary(Box::new(Binary {
				left: expr,
				operator: operator,
				right: right,
			}));
		}

		Ok(expr)
	}

	fn term(&mut self) -> Result<Expr, ParseError> {
		let mut expr = self.factor()?;

		while self.match_tokens(vec![TokenType::Minus, TokenType::Plus]) {
			let operator = self.previous();
			let right = self.factor()?;
			expr = Expr::Binary(Box::new(Binary {
				left: expr,
				operator: operator,
				right: right,
			}));
		}

		Ok(expr)
	}

	fn factor(&mut self) -> Result<Expr, ParseError> {
		let mut expr = self.unary()?;

		while self.match_tokens(vec![TokenType::Slash, TokenType::Star]) {
			let operator = self.previous();
			let right = self.unary()?;
			expr = Expr::Binary(Box::new(Binary {
				left: expr,
				operator: operator,
				right: right,
			}));
		}

		Ok(expr)
	}

	fn unary(&mut self) -> Result<Expr, ParseError> {
		if self.match_tokens(vec![TokenType::Bang, TokenType::Minus]) {
			let operator = self.previous();
			let right = self.unary()?;
			return Ok(Expr::Unary(Box::new(Unary {
				operator: operator,
				right: right,
			})));
		}

		Ok(self.primary()?)
	}

	fn primary(&mut self) -> Result<Expr, ParseError> {
		if self.match_tokens(vec![TokenType::False]) {
			return Ok(Expr::Literal(Box::new(Literal::False)));
		}

		if self.match_tokens(vec![TokenType::True]) {
			return Ok(Expr::Literal(Box::new(Literal::True)));
		}

		if self.match_tokens(vec![TokenType::Nil]) {
			return Ok(Expr::Literal(Box::new(Literal::Nil)));
		}

		if self.match_tokens(vec![TokenType::Number(0.0)]) {
			return Ok(Expr::Literal(Box::new(Literal::Number(self.previous()))));
		}

		if self.match_tokens(vec![TokenType::String("".into())]) {
			return Ok(Expr::Literal(Box::new(Literal::String(self.previous()))));
		}

		if self.match_tokens(vec![TokenType::Identifier("".into())]) {
			return Ok(Expr::Variable(self.previous()));
		}

		if self.match_tokens(vec![TokenType::LeftParen]) {
			let expr = self.expression()?;
			self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
			return Ok(Expr::Grouping(Box::new(Grouping { expr: expr })));
		}

		Err(ParseError::new(self.peek().clone(), "Expect expression."))
	}

	/** Consume a token and return it, errors if the token type does not match */
	fn consume(
		&mut self,
		token_type: TokenType,
		message: &'static str,
	) -> Result<Token, ParseError> {
		if self.check(token_type) {
			self.advance();
			Ok(self.previous())
		} else {
			Err(ParseError::new(self.peek().clone(), message))
		}
	}

	fn synchronize(&mut self) {
		self.advance();

		while !self.is_at_end() {
			if self.previous().r#type == TokenType::Semicolon {
				return;
			}

			match self.peek().r#type {
				TokenType::Class
				| TokenType::Fun
				| TokenType::Let
				| TokenType::For
				| TokenType::If
				| TokenType::While
				| TokenType::Print
				| TokenType::Return => {
					return;
				}
				_ => {
					self.advance();
				}
			}
		}
	}
}
