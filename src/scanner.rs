use crate::token::{Token, TokenType};

#[derive(Debug, Clone)]
pub struct ScanError {
	pub line: usize,
	pub column: usize,
	pub message: String,
}

impl ScanError {
	pub fn new(line: usize, column: usize, message: String) -> Self {
		Self {
			line,
			column,
			message,
		}
	}
}

impl std::fmt::Display for ScanError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(
			f,
			"[{}, {}] Error: {}",
			self.line, self.column, self.message
		)
	}
}

impl std::error::Error for ScanError {}

pub struct Scanner {
	source_code: String,
	tokens: Vec<Token>,
	line: usize,
	start: usize,
	current: usize,
	errors: Vec<ScanError>,
}

impl Scanner {
	pub fn new() -> Self {
		Self {
			source_code: "".into(),
			tokens: vec![],
			line: 1,
			start: 0,
			current: 0,
			errors: vec![],
		}
	}

	pub fn scan_tokens(&mut self, source_code: String) -> (Vec<Token>, Vec<ScanError>) {
		self.source_code = source_code;

		while !self.is_at_end() {
			self.start = self.current;
			self.scan_token();
		}

		self.tokens
			.push(Token::new(TokenType::Eof, "".into(), self.line, 1));

		// Reset the scanner for a next run
		self.source_code = "".into();
		self.line = 1;
		self.start = 0;
		self.current = 0;

		let tokens = std::mem::take(&mut self.tokens);
		let errors = std::mem::take(&mut self.errors);

		(tokens, errors)
	}

	fn is_at_end(&self) -> bool {
		self.current >= self.source_code.len() as usize
	}

	fn scan_token(&mut self) {
		let c = self.advance();
		match c {
			'(' => self.add_token(TokenType::LeftParen),
			')' => self.add_token(TokenType::RightParen),
			'{' => self.add_token(TokenType::LeftBrace),
			'}' => self.add_token(TokenType::RightBrace),
			',' => self.add_token(TokenType::Comma),
			'.' => self.add_token(TokenType::Dot),
			'-' => self.add_token(TokenType::Minus),
			'+' => self.add_token(TokenType::Plus),
			';' => self.add_token(TokenType::Semicolon),
			'*' => self.add_token(TokenType::Star),
			'!' => {
				let token_type = if self.match_char('=') {
					TokenType::BangEqual
				} else {
					TokenType::Bang
				};
				self.add_token(token_type);
			}
			'=' => {
				let token_type = if self.match_char('=') {
					TokenType::EqualEqual
				} else {
					TokenType::Equal
				};
				self.add_token(token_type);
			}
			'<' => {
				let token_type = if self.match_char('=') {
					TokenType::LessEqual
				} else {
					TokenType::Less
				};
				self.add_token(token_type);
			}
			'>' => {
				let token_type = if self.match_char('=') {
					TokenType::GreaterEqual
				} else {
					TokenType::Greater
				};
				self.add_token(token_type);
			}
			'/' => {
				if self.match_char('/') {
					while self.peek() != '\n' && !self.is_at_end() {
						self.advance();
					}
					self.add_token(TokenType::Comment(
						self.source_code[self.start..self.current].to_string(),
					));
				} else {
					self.add_token(TokenType::Slash);
				}
			}
			'|' => {
				if self.match_char('|') {
					self.add_token(TokenType::Or);
				} else {
					self.errors.push(ScanError::new(
						self.line,
						self.start + 1,
						"Unexpected character: |".into(),
					));
				}
			}
			'&' => {
				if self.match_char('&') {
					self.add_token(TokenType::And);
				} else {
					self.errors.push(ScanError::new(
						self.line,
						self.start + 1,
						"Unexpected character: &".into(),
					));
				}
			}
			' ' | '\r' | '\t' => (),
			'\n' => self.line += 1,
			'"' => self.string(),
			'0'..='9' => self.number(),
			'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
			_ => self.errors.push(ScanError::new(
				self.line,
				self.start + 1,
				format!("Unexpected character: {}", c),
			)),
		}
	}

	fn identifier(&mut self) {
		while self.peek().is_alphanumeric() || self.peek() == '_' {
			self.advance();
		}

		let text = self.source_code[self.start..self.current].to_string();
		let token_type = match text.as_str() {
			"class" => TokenType::Class,
			"else" => TokenType::Else,
			"false" => TokenType::False,
			"for" => TokenType::For,
			"fun" => TokenType::Fun,
			"if" => TokenType::If,
			"nil" => TokenType::Nil,
			"print" => TokenType::Print,
			"return" => TokenType::Return,
			"super" => TokenType::Super,
			"this" => TokenType::This,
			"true" => TokenType::True,
			"let" => TokenType::Let,
			"while" => TokenType::While,
			"mut" => TokenType::Mut,
			_ => TokenType::Identifier(text),
		};
		self.add_token(token_type);
	}

	fn number(&mut self) {
		while self.peek().is_digit(10) {
			self.advance();
		}

		if self.peek() == '.' && self.peek_next().is_digit(10) {
			self.advance();
			while self.peek().is_digit(10) {
				self.advance();
			}
		}

		let value = self.source_code[self.start..self.current]
			.parse::<f64>()
			.unwrap();
		self.add_token(TokenType::Number(value));
	}

	fn string(&mut self) {
		while self.peek() != '"' && !self.is_at_end() {
			if self.peek() == '\n' {
				self.line += 1;
			}
			self.advance();
		}

		if self.is_at_end() {
			self.errors.push(ScanError::new(
				self.line,
				self.start + 1,
				"Unterminated string.".into(),
			));
			return;
		}

		self.advance();

		let value = self.source_code[self.start + 1..self.current - 1].to_string();
		self.add_token(TokenType::String(value));
	}

	fn advance(&mut self) -> char {
		self.current += 1;
		self.source_code.chars().nth(self.current - 1).unwrap()
	}

	fn add_token(&mut self, token_type: TokenType) {
		let text = self.source_code[self.start..self.current].to_string();
		self.tokens
			.push(Token::new(token_type, text, self.line, self.start + 1));
	}

	fn match_char(&mut self, expected: char) -> bool {
		if self.is_at_end() {
			return false;
		}
		if self.source_code.chars().nth(self.current).unwrap() != expected {
			return false;
		}
		self.current += 1;
		true
	}

	fn peek(&self) -> char {
		if self.is_at_end() {
			return '\0';
		}
		self.source_code.chars().nth(self.current).unwrap()
	}

	fn peek_next(&self) -> char {
		if self.current + 1 >= self.source_code.len() {
			return '\0';
		}
		self.source_code.chars().nth(self.current + 1).unwrap()
	}
}
