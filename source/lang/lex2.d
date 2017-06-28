/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.lex2;

/*
import std.stdio;
void main()
{
	string input = "  ( ) { } 	[]:; hi while iff if return do else func FUNC __super123 \0 1231320  //dads
	second line
	// uni тест
	//eoi comment";
	auto lexer = Lexer2(input);
	while (true)
	{
		auto tok = lexer.nextToken();
		writefln("tok %s \"%s\"", tok, input[tok.start..tok.start+tok.size]);
		if (tok.type == TT.EOI) break;
	}
}
*/

enum TokenType : ubyte
{
	EOI,
	BANG,
	DOUBLEQUOTE,
	HASH,
	DOLLAR,
	PERCENT,
	QUOTE,
	LPAREN,
	RPAREN,
	STAR,
	PLUS,
	COMMA,
	MINUS,
	DOT,
	SLASH,
	COLON,
	SEMICOLON,
	LT,
	EQ,
	GT,
	QUESTION,
	AT,
	LBRACKET,
	BACKSLASH,
	RBRACKET,
	CIRCUMFLEX,
	UNDERSCORE,
	BACKTICK,
	LCURLY,
	PIPE,
	RCURLY,
	TILDE,
	INVALID,
	AND,

	DO_SYM,
	ELSE_SYM,
	IF_SYM,
	WHILE_SYM,
	FUNC_SYM,
	RET_SYM,

	ID,
	DECIMAL_NUM,
	COMMENT,
/*
	SHL,   // <<
	SHR,   // >>
	ASHR,  // >>>

	AND,   // &
	ANDAND,// &&
	OR,    // |
	OROR,  // ||

	ASSIGN,// =
	LT,    // <
	GT,    // >
	LE,    // <=
	GE,    // >=
	EQUAL, // ==
	NOTEQUAL, // !=

	HEX_NUM,
	BINARY_NUM,
	*/
}

struct Token
{
	TokenType type;
	ushort size;
	uint start;
	uint line;
	uint col;
	string getTokenString(string input) { return input[start..start+size]; }
}

alias TT = TokenType;

struct Lexer2
{
	string input;

	char c; // current symbol

	uint position;
	uint line;
	uint column;

	uint startPos;
	uint startLine;
	uint startCol;

	string getTokenString(Token tok) { return input[tok.start..tok.start+tok.size]; }

	void nextChar()
	{
		++position;
		++column;
		if (position >= input.length) c = '\3';
		else c = input[position];
	}

	ushort tokSize() { return cast(ushort)(position - startPos); }

	Token nextToken()
	{
		if (position >= input.length) c = '\3';
		else c = input[position];

		while (true)
		{
			startPos = position;
			startLine = line;
			startCol = column;

			switch(c)
			{
				case '\3': return Token(TT.EOI, 0, position, line, column);
				case '\t': nextChar(); continue;
				case '\n': lex_EOL_N(); continue;
				case '\r': lex_EOL_RN(); continue;
				case ' ': nextChar(); continue;
				case '!': nextChar(); return Token(TT.BANG, 1, position-1, line, column);
				case '"': nextChar(); return Token(TT.DOUBLEQUOTE, 1, position-1, line, column);
				case '#': nextChar(); return Token(TT.HASH, 1, position-1, line, column);
				case '$': nextChar(); return Token(TT.DOLLAR, 1, position-1, line, column);
				case '%': nextChar(); return Token(TT.PERCENT, 1, position-1, line, column);
				case '&': nextChar(); return Token(TT.AND, 1, position-1, line, column);
				case '\'': nextChar(); return Token(TT.QUOTE, 1, position-1, line, column);
				case '(': nextChar(); return Token(TT.LPAREN, 1, position-1, line, column);
				case ')': nextChar(); return Token(TT.RPAREN, 1, position-1, line, column);
				case '*': nextChar(); return Token(TT.STAR, 1, position-1, line, column);
				case '+': nextChar(); return Token(TT.PLUS, 1, position-1, line, column);
				case ',': nextChar(); return Token(TT.COMMA, 1, position-1, line, column);
				case '-': nextChar(); return Token(TT.MINUS, 1, position-1, line, column);
				case '.': nextChar(); return Token(TT.DOT, 1, position-1, line, column);
				case '/': return lex_SLASH();
				case '0': .. case '9': return lex_DIGIT();
				case ':': nextChar(); return Token(TT.COLON, 1, position-1, line, column);
				case ';': nextChar(); return Token(TT.SEMICOLON, 1, position-1, line, column);
				case '<': nextChar(); return Token(TT.LT, 1, position-1, line, column);
				case '=': nextChar(); return Token(TT.EQ, 1, position-1, line, column);
				case '>': nextChar(); return Token(TT.GT, 1, position-1, line, column);
				case '?': nextChar(); return Token(TT.QUESTION, 1, position-1, line, column);
				case '@': nextChar(); return Token(TT.AT, 1, position-1, line, column);
				case 'A': .. case 'Z': return lex_LETTER();
				case '[': nextChar(); return Token(TT.LBRACKET, 1, position-1, line, column);
				case '\\': nextChar(); return Token(TT.BACKSLASH, 1, position-1, line, column);
				case ']': nextChar(); return Token(TT.RBRACKET, 1, position-1, line, column);
				case '^': nextChar(); return Token(TT.CIRCUMFLEX, 1, position-1, line, column);
				case '_': nextChar(); return lex_LETTER();
				case '`': nextChar(); return Token(TT.BACKTICK, 1, position-1, line, column);
				case 'a': .. case 'z': return lex_LETTER();
				case '{': nextChar(); return Token(TT.LCURLY, 1, position-1, line, column);
				case '|': nextChar(); return Token(TT.PIPE, 1, position-1, line, column);
				case '}': nextChar(); return Token(TT.RCURLY, 1, position-1, line, column);
				case '~': nextChar(); return Token(TT.TILDE, 1, position-1, line, column);
				default:
					nextChar(); return Token(TT.INVALID, 1, position-1, line, column);
			}
		}
	}

	void lex_EOL_RN() // \r[\n]
	{
		nextChar();
		if (c == '\n') nextChar();
		++line;
		column = 0;
	}

	void lex_EOL_N() // \n
	{
		nextChar();
		++line;
		column = 0;
	}

	Token lex_SLASH() // /
	{
		nextChar();
		if (c == '/')
		{
			consumeLine();
			return Token(TT.COMMENT, tokSize(), startPos, startLine, startCol);
		}
		return Token(TT.SLASH, 1, startPos, startLine, startCol);
	}

	Token lex_DIGIT() // 0-9
	{
		consumeDecimal();
		return Token(TT.DECIMAL_NUM, tokSize(), startPos, startLine, startCol);
	}

	Token lex_LETTER() // a-zA-Z_
	{
		switch (c)
		{
			case 'd': if (match("do")) return Token(TT.DO_SYM, tokSize(), startPos, startLine, startCol); break;
			case 'e': if (match("else")) return Token(TT.ELSE_SYM, tokSize(), startPos, startLine, startCol); break;
			case 'i': if (match("if")) return Token(TT.IF_SYM, tokSize(), startPos, startLine, startCol); break;
			case 'w': if (match("while")) return Token(TT.WHILE_SYM, tokSize(), startPos, startLine, startCol); break;
			case 'f': if (match("func")) return Token(TT.FUNC_SYM, tokSize(), startPos, startLine, startCol); break;
			case 'r': if (match("return")) return Token(TT.RET_SYM, tokSize(), startPos, startLine, startCol); break;
			default: break;
		}
		consumeId();
		return Token(TT.ID, tokSize(), startPos, startLine, startCol);
	}

	bool match(string identifier)
	{
		uint index = 0;
		while (identifier[index] == c)
		{
			nextChar();
			++index;
			if (index == identifier.length)
			{
				// check that no valid symbol follow this id. ifff for if id.
				if (isIdSecond(c)) return false;
				return true;
			}
		}
		return false;
	}

	void consumeId()
	{
		while (isIdSecond(c))
		{
			nextChar();
		}
	}

	void consumeDecimal()
	{
		while (isDigit(c))
		{
			nextChar();
		}
	}

	void consumeLine()
	{
		while (true)
		{
			switch(c)
			{
				case '\3': return;
				case '\n': return;
				case '\r': return;
				default: break;
			}
			nextChar();
		}
	}
}

bool isDigit(char chr)
{
	return '0' <= chr && chr <= '9';
}

bool isIdSecond(char chr)
{
	return
		'0' <= chr && chr <= '9' ||
		'a' <= chr && chr <= 'z' ||
		'A' <= chr && chr <= 'Z' ||
		chr == '_';
}
