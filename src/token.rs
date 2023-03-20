#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
	// Single Character Tokens
	LeftParen,
	RightParen,
	LeftBrace,
	RightBrace,
	Comma,
	Dot,
	Minus,
	Plus,
	Semicolon,
	Slash,
	Star,

	// One or two character tokens
	Bang,
	BangEqual,
	Equal,
	EqualEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,

	// Literals
	Identifier(String),
	String(String),
	Number(f64),

	Comment(String),

	// Keywords
	And,
	Class,
	Else,
	False,
	Fun,
	For,
	If,
	Nil,
	Or,
	Print,
	Return,
	Super,
	This,
	True,
	Let,
	While,
	Mut,

	Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
	pub r#type: TokenType,
	pub lexeme: String,
	pub line: usize,
	pub column: usize,
}

impl Token {
	pub fn new(r#type: TokenType, lexeme: String, line: usize, column: usize) -> Self {
		Self {
			r#type,
			lexeme,
			line,
			column,
		}
	}
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?} {}", self.r#type, self.lexeme)
	}
}
