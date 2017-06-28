/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lang.lex;

import std.range;
import std.uni;
import std.utf : byDchar, decodeFront;
import std.stdio;

/*
void main()
{
	//string input = "0x1234567890ABCDEFabcdef 0xFF";
	string input = "{ while (i<100) { do i=i+10; while (i<50); if (i<5) n=1; } }//do while";
	//string input = "while";

	auto stream = CharStream!string(input);
	StringLexer lexer = StringLexer(stream);

	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'{', TokenType.LCURLY);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'}', TokenType.RCURLY);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'(', TokenType.LPAREN);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!')', TokenType.RPAREN);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'+', TokenType.PLUS);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'-', TokenType.MINUS);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'<', TokenType.LT);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!';', TokenType.SEMICOLON);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!'=', TokenType.ASSIGN);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchSymbol!',', TokenType.COMMA);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"func", TokenType.FUNC_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"return", TokenType.RET_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"while", TokenType.WHILE_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"if", TokenType.IF_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"else", TokenType.ELSE_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchId!"do", TokenType.DO_SYM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchIdent, TokenType.ID);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchDecimalNumber, TokenType.DECIMAL_NUM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchHexNumber, TokenType.HEX_NUM);
	lexer.matchers ~= TokenMatcher(&lexer.stream.matchComment, TokenType.COMMENT);
	foreach (token; lexer)
	{
		stream.skipSpace();
		string text;
		if (token.start.pos >= 0) text = input[token.start.pos..token.end.pos];
		writefln("> %s %s", token, text);
		if (token.type == TokenType.INVALID) break;
	}

	//splitLines();
}
*/

void splitLines()
{
	string input = "тест\r\n4567\n\r890AB\nCDEFab\ncdef";
	//string input = "";
	//string input = "123\n456";

	auto stream = CharStream!string(input);

	size_t off;
	StreamPos pos = stream.currentPos;

	while (!stream.empty)
	{
		if (stream.matchEol)
		{
			writefln("> line off %s pos %s cur_off %s bytes %s", off, pos, stream.currentOffset, stream.currentOffset - off);
			writeln(stream.originalInput[off..stream.currentOffset]);
			off = stream.currentOffset;
			pos = stream.currentPos;
		}
		else
		{
			stream.next;
		}
	}

	writefln("> line off %s pos %s cur_off %s", off, pos, stream.currentOffset);
	writeln(stream.originalInput[off..stream.currentOffset]);
}

struct Stack(T)
{
	import std.array;
	T[100] data;
	size_t index;
	@property bool empty(){ return index == 0; }
	@property size_t length(){ return index; }
	void push(T val){
		data[index] = val;
		++index;
	}
	T top() { return data[index-1]; }
	T pop()
	{
		auto val = data[index-1];
		--index;
		return val;
	}
}

enum TokenType : ubyte
{
	SOI, // start of input
	EOI, // end of input
	INVALID,
	COMMENT, // //

	LPAREN, // (
    RPAREN, // )
    LBRACKET, // [
    RBRACKET, // ]
    LCURLY, // {
    RCURLY, // }

    COLON, // :
	SEMICOLON, // ;

	PLUS,  // +
	MINUS, // -
	MUL,   // *
	DIV,   // /
	MOD,   // %
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

	COMMA, // ,
	DOT,   // .

	HEX_NUM,
	DECIMAL_NUM,
	BINARY_NUM,
	ID,

	// key words
	DO_SYM,
	ELSE_SYM,
	IF_SYM,
	WHILE_SYM,
	FUNC_SYM,
	RET_SYM,
}

struct Token
{
	TokenType type;
	StreamPos start;
	StreamPos end;
	//string text;
	void toString()(scope void delegate(const(char)[]) sink) const {
		import std.format : formattedWrite;
		sink.formattedWrite("Tok(%s, %s, %s)", type, start.pos, end.pos);
	}
}

struct StreamPos
{
	int pos = -1;
}

struct CharStream(R)
	if (isForwardRange!R && is(ElementType!R : dchar))
{
	R originalInput;

	struct StreamState
	{
		dchar current = '\2'; // start of text
		bool empty;
		StreamPos currentPos;
		R input;
		size_t currentOffset;
	}

	StreamState _state;
	alias _state this;

	Stack!StreamState _checkpointStack;

	this(R inp)
	{
		originalInput = input = inp;
		next();
	}

	/// Returns false if input is empty
	/// Updates current and returns true otherwise
	bool next()
	{
		if (input.empty)
		{
			if (!this.empty) // advance past last char
			{
				++currentPos.pos;
				currentOffset = originalInput.length - this.input.length;
				current = '\3'; // end of text
			}

			this.empty = true;
			return false;
		}

		currentOffset = originalInput.length - this.input.length;
		current = decodeFront!(Yes.useReplacementDchar)(input);
		++currentPos.pos;

		return true;
	}

	/// Skips zero or more whitespace chars consuming input
	void skipSpace()
	{
		while (isWhite(current) && next()) {}
	}

	/// Matches all chars from str consuming input and returns true
	/// If fails consumes no input and returns false
	bool match(R)(R str)
		if (isInputRange!R && is(ElementType!R : dchar))
	{
		foreach (dchar item; str.byDchar)
		{
			if (this.empty) return false;
			if (toLower(item) != toLower(current)) return false;
			next();
		}
		return true;
	}

	bool matchId(string id)()
	{
		return matchCase(id);
	}

	bool match(dchar chr)
	{
		if (current == chr)
		{
			next();
			return true;
		}

		return false;
	}

	bool matchCase(R)(R str)
		if (isInputRange!R && is(ElementType!R : dchar))
	{
		foreach (dchar item; str.byDchar)
		{
			if (this.empty) { return false; }
			if (item != current) { return false; }
			next();
		}

		return true;
	}

	/// Matches single char
	bool match(alias pred)()
	{
		if (pred(current))
		{
			next();
			return true;
		}

		return false;
	}

	bool matchAnyOf(dchar[] options...)
	{
		foreach (option; options)
		{
			if (option == current)
			{
				next();
				return true;
			}
		}

		return false;
	}

	bool matchOpt(dchar optional)
	{
		match(optional);
		return true;
	}

	bool matchEol()
	{
		return matchAnyOf('\n', '\v', '\f') || (match('\r') && matchOpt('\n'));
	}

	bool matchHexNumber()
	{
		if (!match("0x")) return false;
		if (!match!isHexDigit) return false;
		while (match!isHexDigit) {}
		return true;
	}

	bool matchDecimalNumber()
	{
		if (!match!isDigit) return false;
		while (match!isDigit) {}
		return true;
	}

	bool matchSymbol(dchar sym)()
	{
		if (!match(sym)) return false;
		return true;
	}

	bool matchIdent()
	{
		if (!match!isIdFirst) return false;
		while (match!isIdSecond) {}
		return true;
	}

	bool matchComment()
	{
		if (!match("//")) return false;
		while (!matchEol() && next()) {}
		return true;
	}

	/// save current stream position
	void pushCheckpoint() {
		_checkpointStack.push(_state);
	}

	/// restore saved position
	void discardCheckpoint() {
		_checkpointStack.pop;
	}

	/// restore saved position
	void popCheckpoint() {
		_state = _checkpointStack.pop;
	}
}

bool isDigit(dchar chr)
{
	return '0' <= chr && chr <= '9';
}

bool isIdFirst(dchar chr)
{
	return
		'a' <= chr && chr <= 'z' ||
		'A' <= chr && chr <= 'Z' ||
		chr == '_';
}

bool isIdSecond(dchar chr)
{
	return
		'0' <= chr && chr <= '9' ||
		'a' <= chr && chr <= 'z' ||
		'A' <= chr && chr <= 'Z' ||
		chr == '_';
}

bool isHexDigit(dchar chr)
{
	return
		'0' <= chr && chr <= '9' ||
		'a' <= chr && chr <= 'f' ||
		'A' <= chr && chr <= 'F';
}

struct TokenMatcher
{
	bool delegate() matcher;
	TokenType type;
}

alias StringLexer = Lexer!string;
struct Lexer(R)
{
	CharStream!R stream;
	TokenMatcher[] matchers;
	Token current;
	bool empty;

	int opApply(scope int delegate(in Token) del)
	{
		do
		{
			if (auto ret = del(current))
				return ret;
			next();
		}
		while (!empty);
		return 0;
	}

	string getTokenString(Token token)
	{
		string text;
		if (token.start.pos >= 0) text = stream.originalInput[token.start.pos..token.end.pos];
		return text;
	}

	void next()
	{
		stream.skipSpace();

		if (checkStreamState) return;

		foreach (matcher; matchers)
		{
			stream.pushCheckpoint;
			StreamPos startPos = stream.currentPos;

			bool matchSuccess = matcher.matcher();

			if (matchSuccess)
			{
				current = Token(matcher.type, startPos, stream.currentPos);
				//writefln("success on %s, state %s", matcher.type, stream._state);
				stream.discardCheckpoint;
				return;
			}

			stream.popCheckpoint;
			//writefln("fail %s", matcher.type);
		}

		current = Token(TokenType.INVALID, stream.currentPos, stream.currentPos);
	}

	// returns true if no matching should be done
	private bool checkStreamState()
	{
		if (stream.empty)
		{
			if (current.type == TokenType.EOI) // on second try mark as empty and return
			{
				empty = true;
			}
			else // when stream just became empty emit EOI token, but do not mark us as empty
			{
				current = Token(TokenType.EOI, stream.currentPos, stream.currentPos);
			}

			return true; // exit matching
		}

		return false; // continue matching
	}
}
