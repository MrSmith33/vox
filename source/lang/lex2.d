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
	BREAK_SYM,
	CONTINUE_SYM,

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

	private char c; // current symbol

	private uint position;
	private uint line;
	private uint column;

	private uint startPos;
	private uint startLine;
	private uint startCol;

	private void nextChar()
	{
		++position;
		++column;
		if (position >= input.length) c = '\3';
		else c = input[position];
	}

	private Token new_tok(TokenType type)
	{
		ushort tokSize = cast(ushort)(position - startPos);
		return Token(type, tokSize, startPos, startLine, startCol);
	}

	string getTokenString(Token tok) { return input[tok.start..tok.start+tok.size]; }

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
				case '\3':             return new_tok(TT.EOI);
				case '\t': nextChar(); continue;
				case '\n': lex_EOLN(); continue;
				case '\r': lex_EOLR(); continue;
				case ' ' : nextChar(); continue;
				case '!' : nextChar(); return new_tok(TT.BANG);
				case '"' : nextChar(); return new_tok(TT.DOUBLEQUOTE);
				case '#' : nextChar(); return new_tok(TT.HASH);
				case '$' : nextChar(); return new_tok(TT.DOLLAR);
				case '%' : nextChar(); return new_tok(TT.PERCENT);
				case '&' : nextChar(); return new_tok(TT.AND);
				case '\'': nextChar(); return new_tok(TT.QUOTE);
				case '(' : nextChar(); return new_tok(TT.LPAREN);
				case ')' : nextChar(); return new_tok(TT.RPAREN);
				case '*' : nextChar(); return new_tok(TT.STAR);
				case '+' : nextChar(); return new_tok(TT.PLUS);
				case ',' : nextChar(); return new_tok(TT.COMMA);
				case '-' : nextChar(); return new_tok(TT.MINUS);
				case '.' : nextChar(); return new_tok(TT.DOT);
				case '/' :             return lex_SLASH();
				case '0' : ..case '9': return lex_DIGIT();
				case ':' : nextChar(); return new_tok(TT.COLON);
				case ';' : nextChar(); return new_tok(TT.SEMICOLON);
				case '<' : nextChar(); return new_tok(TT.LT);
				case '=' : nextChar(); return new_tok(TT.EQ);
				case '>' : nextChar(); return new_tok(TT.GT);
				case '?' : nextChar(); return new_tok(TT.QUESTION);
				case '@' : nextChar(); return new_tok(TT.AT);
				case 'A' : ..case 'Z': return lex_LETTER();
				case '[' : nextChar(); return new_tok(TT.LBRACKET);
				case '\\': nextChar(); return new_tok(TT.BACKSLASH);
				case ']' : nextChar(); return new_tok(TT.RBRACKET);
				case '^' : nextChar(); return new_tok(TT.CIRCUMFLEX);
				case '_' : nextChar(); return lex_LETTER();
				case '`' : nextChar(); return new_tok(TT.BACKTICK);
				case 'a' : ..case 'z': return lex_LETTER();
				case '{' : nextChar(); return new_tok(TT.LCURLY);
				case '|' : nextChar(); return new_tok(TT.PIPE);
				case '}' : nextChar(); return new_tok(TT.RCURLY);
				case '~' : nextChar(); return new_tok(TT.TILDE);
				default  : nextChar(); return new_tok(TT.INVALID);
			}
		}
	}

	void lex_EOLR() // \r[\n]
	{
		nextChar();
		if (c == '\n') nextChar();
		++line;
		column = 0;
	}

	void lex_EOLN() // \n
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
			return new_tok(TT.COMMENT);
		}
		return new_tok(TT.SLASH);
	}

	Token lex_DIGIT() // 0-9
	{
		consumeDecimal();
		return new_tok(TT.DECIMAL_NUM);
	}

	Token lex_LETTER() // a-zA-Z_
	{
		switch (c)
		{
			case 'b': if (match("break")) return new_tok(TT.BREAK_SYM); break;
			case 'c': if (match("continue")) return new_tok(TT.CONTINUE_SYM); break;
			case 'd': if (match("do")) return new_tok(TT.DO_SYM); break;
			case 'e': if (match("else")) return new_tok(TT.ELSE_SYM); break;
			case 'i': if (match("if")) return new_tok(TT.IF_SYM); break;
			case 'w': if (match("while")) return new_tok(TT.WHILE_SYM); break;
			case 'f': if (match("func")) return new_tok(TT.FUNC_SYM); break;
			case 'r': if (match("return")) return new_tok(TT.RET_SYM); break;
			default: break;
		}
		consumeId();
		return new_tok(TT.ID);
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
