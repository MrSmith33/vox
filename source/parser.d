/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module parser;

import std.format : formattedWrite;
import std.string : format;
import std.range : repeat;
import std.stdio;

import all;


// Grammar
/**
	<module> = <declaration>* EOF
	<declaration> = <func_decl> / <var_decl> / <struct_decl>

	<func_decl> = <type> <identifier> "(" <param_list> ")" (<block_statement> / ';')
	<param_list> = <parameter> "," <parameter_list> / <parameter>?
	<parameter> = <type> <identifier>?

	<var_decl> = <type> <identifier> ("=" <expression>)? ";"
	<struct_decl> = "struct" <identifier> "{" <declaration>* "}"

	<statement> = "if" <paren_expression> <statement> ("else" <statement>)?
				  "while" <paren_expression> <statement> /
				  "do" <statement> "while" <paren_expression> ";" /
				  "return" <expression>? ";" /
				  "continue" ";" /
				  "break" ";" /
				  <block_statement> /
				  <expression> ("=" <expression>)? ";" /
				  <declaration_statement>

	<declaration_statement> = <declaration>
	<block_statement> = "{" <statement>* "}"

	<expression> = <test>
	<test> = <sum> | <sum> ("=="|"!="|"<"|">"|"<="|">=") <sum>
	<sum> = <term> / <sum> ("+"|"-") <term>
	<term> = <identifier> "(" <expression_list> ")" / <identifier> "[" <expression> "]" / <identifier> / <int_literal> / <string_literal> / <paren_expression>
	<paren_expression> = "(" <expression> ")"

	<expression_list> = (<expression> ",")*
	<identifier> = [_a-zA-Z] [_a-zA-Z0-9]*

	<type> = (<type_basic> / <type_user>) <type_specializer>*
	<type_specializer> = '*' / '[' <expression> ']'
	<type_basic> = ("i8" | "i16" | "i32" | "i64" | "isize" |
		"u8" | "u16" | "u32" | "u64" | "usize" | "void" | "f32" | "f64")

	<type_user> = <identifier>

	<int_literal> = <literal_dec_int> / <literal_hex_int>
	<literal_dec_int> = 0|[1-9][0-9_]*
	<literal_hex_int> = ("0x"|"0X")[0-9A-Fa-f_]+
	<literal_bin_int> = ("0b"|"0B")[01_]+
	<literal_oct_int> = 0[01_]*
*/

//                 #######    ###    #    #   #######  #     #
//                    #      #   #   #   #    #        ##    #
//                    #     #     #  #  #     #        # #   #
//                    #     #     #  ###      #####    #  #  #
//                    #     #     #  #  #     #        #   # #
//                    #      #   #   #   #    #        #    ##
//                    #       ###    #    #   #######  #     #
// -----------------------------------------------------------------------------
alias TT = TokenType;
enum TokenType : ubyte {
	EOI,

	AND,                     // &
	AND_AND,                 // &&
	AND_EQUAL,               // &=
	AT,                      // @
	BACKSLASH,               // \
	COLON,                   // :
	COMMA,                   // ,
	DOLLAR,                  // $
	DOT,                     // .
	DOT_DOT,                 // ..
	DOT_DOT_DOT,             // ...
	EQUAL,                   // =
	EQUAL_EQUAL,             // ==
	GREATER,                 // >
	GREATER_EQUAL,           // >=
	GREATER_GREATER,         // >>
	GREATER_GREATER_EQUAL,   // >>=
	GREATER_GREATER_GREATER, // >>>
	GREATER_GREATER_GREATER_EQUAL, // >>>=
	HASH,                    // #
	LESS,                    // <
	LESS_EQUAL,              // <=
	LESS_LESS,               // <<
	LESS_LESS_EQUAL,         // <<=
	MINUS,                   // -
	MINUS_EQUAL,             // -=
	MINUS_MINUS,             // --
	NOT,                     // !
	NOT_EQUAL,               // !=
	OR,                      // |
	OR_EQUAL,                // |=
	OR_OR,                   // ||
	PERCENT,                 // %
	PERCENT_EQUAL,           // %=
	PLUS,                    // +
	PLUS_EQUAL,              // +=
	PLUS_PLUS,               // ++
	QUESTION,                // ?
	SEMICOLON,               // ;
	SLASH,                   // /
	SLASH_EQUAL,             // /=
	STAR,                    // *
	STAR_EQUAL,              // *=
	TILDE,                   // ~
	TILDE_EQUAL,             // ~=
	XOR,                     // ^
	XOR_EQUAL,               // ^=

	LPAREN,                  // (
	RPAREN,                  // )
	LBRACKET,                // [
	RBRACKET,                // ]
	LCURLY,                  // {
	RCURLY,                  // }

	INVALID,

	BREAK_SYM,               // break
	CONTINUE_SYM,            // continue
	DO_SYM,                  // do
	ELSE_SYM,                // else
	IF_SYM,                  // if
	RETURN_SYM,              // return
	STRUCT_SYM,              // struct
	WHILE_SYM,               // while

	IDENTIFIER,              // [a-zA-Z_] [a-zA-Z_0-9]*

	// ----------------------------------------
	// list of basic types. The order is the same as in `enum BasicType`

	TYPE_VOID,               // void
	TYPE_BOOL,               // bool
	TYPE_I8,                 // i8
	TYPE_I16,                // i16
	TYPE_I32,                // i32
	TYPE_I64,                // i64

	TYPE_U8,                 // u8
	TYPE_U16,                // u16
	TYPE_U32,                // u32
	TYPE_U64,                // u64

	TYPE_F32,                // f32
	TYPE_F64,                // f64
	// ----------------------------------------

	TYPE_ISIZE,              // isize
	TYPE_USIZE,              // usize

	INT_LITERAL,
	STRING_LITERAL,
	//DECIMAL_LITERAL,         // 0|[1-9][0-9_]*
	//BINARY_LITERAL,          // ("0b"|"0B")[01_]+
	//HEX_LITERAL,             // ("0x"|"0X")[0-9A-Fa-f_]+

	COMMENT,                 // // /*
}

enum TokenType TYPE_TOKEN_FIRST = TokenType.TYPE_VOID;
enum TokenType TYPE_TOKEN_LAST = TokenType.TYPE_F64;

struct Token {
	TokenType type;
	SourceLocation loc;
	Identifier id; // when type is IDENTIFIER
}

struct SourceLocation {
	uint start;
	uint line;
	uint col;
	uint size;
	string getTokenString(string input) const { return input[start..start+size]; }
	void toString(scope void delegate(const(char)[]) sink) const {
		sink.formattedWrite("line %s col %s", line+1, col+1);
	}
}

enum char EOI_CHAR = '\3';

immutable string[] keyword_strings = ["bool","break","continue","do","else","f32","f64",
	"i16","i32","i64","i8","if","isize","return","struct","u16","u32","u64",
	"u8","usize","void","while"];
enum NUM_KEYWORDS = keyword_strings.length;
immutable TokenType[] keyword_tokens = [TT.TYPE_BOOL,TT.BREAK_SYM,TT.CONTINUE_SYM,TT.DO_SYM,
	TT.ELSE_SYM,TT.TYPE_F32,TT.TYPE_F64,TT.TYPE_I16,TT.TYPE_I32,TT.TYPE_I64,
	TT.TYPE_I8,TT.IF_SYM,TT.TYPE_ISIZE,TT.RETURN_SYM,TT.STRUCT_SYM,
	TT.TYPE_U16,TT.TYPE_U32,TT.TYPE_U64,TT.TYPE_U8,TT.TYPE_USIZE,
	TT.TYPE_VOID,TT.WHILE_SYM];

//                          #        #######  #     #
//                          #        #         #   #
//                          #        #          # #
//                          #        #####       #
//                          #        #          # #
//                          #        #         #   #
//                          ######   #######  #     #
// -----------------------------------------------------------------------------
struct Lexer {
	string input;
	CompilationContext* context;

	private dchar c; // current symbol

	private uint position; // offset of 'c' in input
	private uint line; // line of 'c'
	private uint column; // column of 'c'

	private uint startPos; // offset of first token byte in input
	private uint startLine; // line of first token byte
	private uint startCol; // column of first token byte

	long numberRep; // output of integer literal parsing

	this(string input, CompilationContext* context)
	{
		this.input = input;
		this.context = context;
	}

	private void restartToken()
	{
		position = startPos;
		line = startLine;
		column = startCol;
		if (position >= input.length) c = EOI_CHAR;
		else c = input[position];
	}

	private void nextChar()
	{
		++position;
		++column;
		if (position >= input.length) c = EOI_CHAR;
		else c = input[position];
	}

	private SourceLocation new_loc() pure
	{
		uint tokSize = position - startPos;
		return SourceLocation(startPos, startLine, startCol, tokSize);
	}

	private Token new_tok(TokenType type) pure
	{
		return Token(type, new_loc());
	}

	string getTokenString(Token tok) pure { return input[tok.loc.start..tok.loc.start+tok.loc.size]; }
	string getTokenString(SourceLocation loc) pure { return input[loc.start..loc.start+loc.size]; }
	long getTokenNumber() { return numberRep; }

	int opApply(scope int delegate(Token) dg)
	{
		Token tok;
		while ((tok = nextToken()).type != TokenType.EOI)
			if (int res = dg(tok))
				return res;
		return 0;
	}

	Token nextToken()
	{
		if (position >= input.length) c = EOI_CHAR;
		else c = input[position];

		while (true)
		{
			startPos = position;
			startLine = line;
			startCol = column;

			switch(c)
			{
				case EOI_CHAR:         return new_tok(TT.EOI);
				case '\t': nextChar;   continue;
				case '\n': lex_EOLN(); continue;
				case '\r': lex_EOLR(); continue;
				case ' ' : nextChar;   continue;
				case '!' : nextChar; return lex_multi_equal2(TT.NOT, TT.NOT_EQUAL);
				case '#' : nextChar; return new_tok(TT.HASH);
				case '$' : nextChar; return new_tok(TT.DOLLAR);
				case '%' : nextChar; return lex_multi_equal2(TT.PERCENT, TT.PERCENT_EQUAL);
				case '&' : nextChar; return lex_multi_equal2_3('&', TT.AND, TT.AND_EQUAL, TT.AND_AND);
				case '(' : nextChar; return new_tok(TT.LPAREN);
				case ')' : nextChar; return new_tok(TT.RPAREN);
				case '*' : nextChar; return lex_multi_equal2(TT.STAR, TT.STAR_EQUAL);
				case '+' : nextChar; return lex_multi_equal2_3('+', TT.PLUS, TT.PLUS_EQUAL, TT.PLUS_PLUS);
				case ',' : nextChar; return new_tok(TT.COMMA);
				case '-' : nextChar; return lex_multi_equal2_3('-', TT.MINUS, TT.MINUS_EQUAL, TT.MINUS_MINUS);
				case '.' : nextChar;
					if (c == '.') { nextChar;
						if (c == '.') { nextChar;
							return new_tok(TT.DOT_DOT_DOT);
						}
						return new_tok(TT.DOT_DOT);
					}
					return new_tok(TT.DOT);
				case '\"': nextChar; return lex_QUOTE_QUOTE();
				case '/' :           return lex_SLASH();
				case '0' :           return lex_ZERO();
				case '1' : ..case '9': return lex_DIGIT();
				case ':' : nextChar; return new_tok(TT.COLON);
				case ';' : nextChar; return new_tok(TT.SEMICOLON);
				case '<' : nextChar;
					if (c == '<') { nextChar;
						if (c == '=') { nextChar;
							return new_tok(TT.LESS_LESS_EQUAL);
						}
						return new_tok(TT.LESS_LESS);
					}
					if (c == '=') { nextChar;
						return new_tok(TT.LESS_EQUAL);
					}
					return new_tok(TT.LESS);
				case '=' : nextChar; return lex_multi_equal2(TT.EQUAL, TT.EQUAL_EQUAL);
				case '>' : nextChar;
					if (c == '=') { nextChar;
						return new_tok(TT.GREATER_EQUAL);
					}
					if (c == '>') { nextChar;
						if (c == '>') { nextChar;
							if (c == '=') { nextChar;
								return new_tok(TT.GREATER_GREATER_GREATER_EQUAL);
							}
							return new_tok(TT.GREATER_GREATER_GREATER);
						}
						if (c == '=') { nextChar;
							return new_tok(TT.GREATER_GREATER_EQUAL);
						}
						return new_tok(TT.GREATER_GREATER);
					}
					return new_tok(TT.GREATER);
				case '?' : nextChar; return new_tok(TT.QUESTION);
				case '@' : nextChar; return new_tok(TT.AT);
				case 'A' : ..case 'Z': return lex_LETTER();
				case '[' : nextChar; return new_tok(TT.LBRACKET);
				case '\\': nextChar; return new_tok(TT.BACKSLASH);
				case ']' : nextChar; return new_tok(TT.RBRACKET);
				case '^' : nextChar; return lex_multi_equal2(TT.XOR, TT.XOR_EQUAL);
				case '_' : nextChar; return lex_LETTER();
				case 'a' : ..case 'z': return lex_LETTER();
				case '{' : nextChar; return new_tok(TT.LCURLY);
				case '|' : nextChar; return lex_multi_equal2_3('|', TT.OR, TT.OR_EQUAL, TT.OR_OR);
				case '}' : nextChar; return new_tok(TT.RCURLY);
				case '~' : nextChar; return lex_multi_equal2(TT.TILDE, TT.TILDE_EQUAL);
				default  : nextChar; return new_tok(TT.INVALID);
			}
		}
	}

	private void lex_EOLR() // \r[\n]
	{
		nextChar;
		if (c == '\n') nextChar;
		++line;
		column = 0;
	}

	private void lex_EOLN() // \n
	{
		nextChar;
		++line;
		column = 0;
	}

	// Lex X= tokens
	private Token lex_multi_equal2(TokenType single_tok, TokenType eq_tok)
	{
		if (c == '=') {
			nextChar;
			return new_tok(eq_tok);
		}
		return new_tok(single_tok);
	}

	private Token lex_multi_equal2_3(dchar chr, TokenType single_tok, TokenType eq_tok, TokenType double_tok)
	{
		if (c == chr) { nextChar;
			return new_tok(double_tok);
		}
		if (c == '=') { nextChar;
			return new_tok(eq_tok);
		}
		return new_tok(single_tok);
	}

	private Token lex_SLASH() // /
	{
		nextChar;
		if (c == '/')
		{
			consumeLine();
			return new_tok(TT.COMMENT);
		}
		if (c == '*')
		{
			while (true)
			{
				switch(c)
				{
					case EOI_CHAR:
						auto loc = new_loc();
						context.unrecoverable_error(loc, "Unterminated comment");
						return new_tok(TT.INVALID);

					case '\n': lex_EOLN(); continue;
					case '\r': lex_EOLR(); continue;
					case '*':
						nextChar;
						if (c == '/') {
							nextChar;
							return new_tok(TT.COMMENT);
						}
						break;
					default: break;
				}
				nextChar;
			}
			return new_tok(TT.COMMENT);
		}
		if (c == '=') { nextChar;
			return new_tok(TT.SLASH_EQUAL);
		}
		return new_tok(TT.SLASH);
	}

	private Token lex_QUOTE_QUOTE() // "
	{
		while (true)
		{
			switch(c)
			{
				case EOI_CHAR:
					auto loc = new_loc();
					context.unrecoverable_error(loc, "Unterminated string literal");
					return new_tok(TT.INVALID);
				case '\n': lex_EOLN(); continue;
				case '\r': lex_EOLR(); continue;
				case '\"':
					nextChar; // skip "
					return new_tok(TT.STRING_LITERAL);
				default: break;
			}
			nextChar;
		}
	}

	private Token lex_ZERO() // 0
	{
		numberRep = 0;
		nextChar;

		if (c == 'x' || c == 'X')
		{
			nextChar;
			consumeHexadecimal();
			return new_tok(TT.INT_LITERAL);
		}
		else if (c == 'b' || c == 'B')
		{
			nextChar;
			consumeBinary();
			return new_tok(TT.INT_LITERAL);
		}
		else
		{
			consumeDecimal();
			return new_tok(TT.INT_LITERAL);
		}
	}

	private Token lex_DIGIT() // 1-9
	{
		numberRep = c - '0';
		nextChar;
		consumeDecimal();
		return new_tok(TT.INT_LITERAL);
	}

	private Token lex_LETTER() // a-zA-Z_
	{
		switch (c)
		{
			case 'b':
				nextChar;
				if (c == 'o' && match("ool")) return new_tok(TT.TYPE_BOOL);
				else if (c == 'r' && match("reak")) return new_tok(TT.BREAK_SYM);
				break;
			case 'c': if (match("continue")) return new_tok(TT.CONTINUE_SYM); break;
			case 'd': if (match("do")) return new_tok(TT.DO_SYM); break;
			case 'e': if (match("else")) return new_tok(TT.ELSE_SYM); break;
			case 'f':
				nextChar;
				if (c == '3' && match("32")) return new_tok(TT.TYPE_F32);
				if (c == '6' && match("64")) return new_tok(TT.TYPE_F64);
				break;
			case 'i':
				nextChar;
				switch(c) {
					case '1': if (match("16")) return new_tok(TT.TYPE_I16); break;
					case '3': if (match("32")) return new_tok(TT.TYPE_I32); break;
					case '6': if (match("64")) return new_tok(TT.TYPE_I64); break;
					case '8': if (match("8"))  return new_tok(TT.TYPE_I8);  break;
					case 's': if (match("size")) return new_tok(TT.TYPE_ISIZE); break;
					case 'f': if (match("f")) return new_tok(TT.IF_SYM); break;
					default: break;
				}
				break;
			case 'r': if (match("return")) return new_tok(TT.RETURN_SYM); break;
			case 's': if (match("struct")) return new_tok(TT.STRUCT_SYM); break;
			case 'u':
				nextChar;
				switch(c) {
					case '1': if (match("16")) return new_tok(TT.TYPE_U16); break;
					case '3': if (match("32")) return new_tok(TT.TYPE_U32); break;
					case '6': if (match("64")) return new_tok(TT.TYPE_U64); break;
					case '8': if (match("8"))  return new_tok(TT.TYPE_U8);  break;
					case 's': if (match("size")) return new_tok(TT.TYPE_USIZE); break;
					default: break;
				}
				break;
			case 'v': if (match("void")) return new_tok(TT.TYPE_VOID); break;
			case 'w': if (match("while")) return new_tok(TT.WHILE_SYM); break;
			default: break;
		}

		consumeId();
		Token t = new_tok(TT.IDENTIFIER);
		string str = getTokenString(t);
		t.id = context.idMap.getOrRegNoDup(str);
		return t;
	}

	private bool match(string identifier)
	{
		uint index = 0;
		while (identifier[index] == c)
		{
			nextChar;
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

	private void consumeId()
	{
		while (isIdSecond(c)) nextChar;
	}

	private void consumeDecimal()
	{
		while (true)
		{
			if ('0' <= c && c <= '9') {
				numberRep = numberRep * 10 + c - '0';
			} else if (c != '_') return;
			nextChar;
		}
	}

	private void consumeHexadecimal()
	{
		while (true)
		{
			if ('0' <= c && c <= '9') {
				numberRep = numberRep * 16 + c - '0';
			} else if ('a' <= c && c <= 'f') {
				numberRep = numberRep * 16 + c - 'a' + 10;
			} else if ('A' <= c && c <= 'F') {
				numberRep = numberRep * 16 + c - 'A' + 10;
			} else if (c != '_') return;
			nextChar;
		}
	}

	private void consumeBinary()
	{
		while (true)
		{
			if (c == '0' || c == '1') {
				numberRep = numberRep * 2 + c - '0';
			} else if (c != '_') return;
			nextChar;
		}
	}

	private void consumeLine()
	{
		while (true)
		{
			switch(c)
			{
				case EOI_CHAR: return;
				case '\n': lex_EOLN(); return;
				case '\r': lex_EOLR(); return;
				default: break;
			}
			nextChar;
		}
	}
}

private bool isIdSecond(dchar chr) pure nothrow {
	return
		'0' <= chr && chr <= '9' ||
		'a' <= chr && chr <= 'z' ||
		'A' <= chr && chr <= 'Z' ||
		chr == '_';
}


unittest
{
	CompilationContext ctx;

	foreach(i, keyword; keyword_strings)
	{
		Lexer lexer = Lexer(keyword, &ctx);
		Token token = lexer.nextToken;
		assert(token.type == keyword_tokens[i],
			format("For %s expected %s got %s", keyword, keyword_tokens[i], token.type));
	}

	foreach(i, keyword; keyword_strings)
	{
		Lexer lexer = Lexer(keyword~"A", &ctx);
		Token token = lexer.nextToken;
		assert(token.type == TT.IDENTIFIER);
	}

	{
		string[] ops = ["&","&&","&=","@","\\",":",",","$",".","..","...",
			"=","==",">",">=",">>",">>=",">>>",">>>=","#","<","<=","<<","<<=","-",
			"-=","--","!","!=","|","|=","||","%","%=","+","+=","++","?",";","/",
			"/=","*","*=","~","~=","^","^=","(",")","[","]","{","}",];
		TokenType[] tokens_ops = [TT.AND,TT.AND_AND,TT.AND_EQUAL,TT.AT,TT.BACKSLASH,
			TT.COLON,TT.COMMA,TT.DOLLAR,TT.DOT,TT.DOT_DOT,TT.DOT_DOT_DOT,TT.EQUAL,
			TT.EQUAL_EQUAL,TT.GREATER,TT.GREATER_EQUAL,TT.GREATER_GREATER,
			TT.GREATER_GREATER_EQUAL,TT.GREATER_GREATER_GREATER,
			TT.GREATER_GREATER_GREATER_EQUAL,TT.HASH,
			TT.LESS,TT.LESS_EQUAL,TT.LESS_LESS,TT.LESS_LESS_EQUAL,TT.MINUS,
			TT.MINUS_EQUAL,TT.MINUS_MINUS,TT.NOT,TT.NOT_EQUAL,TT.OR,TT.OR_EQUAL,
			TT.OR_OR,TT.PERCENT,TT.PERCENT_EQUAL,TT.PLUS,TT.PLUS_EQUAL,TT.PLUS_PLUS,
			TT.QUESTION,TT.SEMICOLON,TT.SLASH,TT.SLASH_EQUAL,TT.STAR,TT.STAR_EQUAL,
			TT.TILDE,TT.TILDE_EQUAL,TT.XOR,TT.XOR_EQUAL,TT.LPAREN,TT.RPAREN,
			TT.LBRACKET,TT.RBRACKET, TT.LCURLY,TT.RCURLY,];
		foreach(i, op; ops)
		{
			Lexer lexer = Lexer(op, &ctx);
			Token token = lexer.nextToken;
			assert(token.type == tokens_ops[i],
				format("For %s expected %s got %s", op, tokens_ops[i], token.type));
		}
	}

	void testNumeric(string input, TokenType tokType, long expectedValue)
	{
		Lexer lexer = Lexer(input, &ctx);
		assert(lexer.nextToken.type == tokType);
		assert(lexer.numberRep == expectedValue);
	}

	assert(Lexer("_10", &ctx).nextToken.type == TT.IDENTIFIER);
	testNumeric("10", TT.INT_LITERAL, 10);
	testNumeric("1_0", TT.INT_LITERAL, 1_0);
	testNumeric("10_", TT.INT_LITERAL, 10_);
	testNumeric("0xFF", TT.INT_LITERAL, 0xFF);
	testNumeric("0XABCDEF0123456789", TT.INT_LITERAL, 0XABCDEF0123456789);
	testNumeric("0x1_0", TT.INT_LITERAL, 0x1_0);
	testNumeric("0b10", TT.INT_LITERAL, 0b10);
	testNumeric("0B10", TT.INT_LITERAL, 0B10);
	testNumeric("0b1_0", TT.INT_LITERAL, 0b1_0);

	{
		string source = "/*\n*/test";
		Lexer lexer = Lexer(source, &ctx);
		Token tok = lexer.nextToken;
		assert(tok.type == TT.COMMENT);
		assert(tok.loc.getTokenString(source) == "/*\n*/");
		tok = lexer.nextToken;
		assert(tok.type == TT.IDENTIFIER);
		assert(tok.loc.getTokenString(source) == "test");
	}
	{
		string source = "//test\nhello";
		Lexer lexer = Lexer(source, &ctx);
		Token tok = lexer.nextToken;
		assert(tok.type == TT.COMMENT);
		assert(tok.loc.getTokenString(source) == "//test\n");
		tok = lexer.nextToken;
		assert(tok.type == TT.IDENTIFIER);
		assert(tok.loc.getTokenString(source) == "hello");
	}
	{
		string source = `"literal"`;
		Lexer lexer = Lexer(source, &ctx);
		Token tok = lexer.nextToken;
		assert(tok.type == TT.STRING_LITERAL);
		assert(tok.loc.getTokenString(source) == `"literal"`, format("%s", tok));
	}
}

void pass_parser(ref CompilationContext ctx) {
	Lexer lexer = Lexer(ctx.input, &ctx);
	Parser parser = Parser(&lexer, &ctx);
	ctx.mod = parser.parseModule();
}

//version = print_parse;
struct Parser {
	Lexer* lexer;
	CompilationContext* context;

	Token tok;

	int nesting;
	auto indent() { return ' '.repeat(nesting*2); }
	struct Scope { Parser* p; ~this(){--p.nesting;}}
	Scope scop(Args...)(string name, Args args) { write(indent); writefln(name, args); ++nesting; return Scope(&this); }

	enum tokenStashSize = 4;
	enum STASH_MASK = tokenStashSize - 1;
	Token[tokenStashSize] stashedTokens;
	size_t stashedIndex;
	size_t numStashedTokens;

	void printChange(Token start) {
		writef("tok %s -> %s stash ", start, tok);
		foreach(i; 0..numStashedTokens)
		{
			writef("%s ", stashedTokens[(stashedIndex+i) & STASH_MASK]);
		}
		writeln;
	}

	void lexToken()
	{
		Token start = tok;
		do {
			tok = lexer.nextToken();
		}
		while (tok.type == TokenType.COMMENT);
		//printChange(start);
	}

	void nextToken()
	{
		if (numStashedTokens == 0)
		{
			lexToken();
		}
		else
		{
			Token start = tok;
			tok = stashedTokens[stashedIndex & STASH_MASK];
			--numStashedTokens;
			++stashedIndex;
			//printChange(start);
		}
	}

	void stashToken()
	{
		Token start = tok;
		stashedTokens[(stashedIndex+numStashedTokens) & STASH_MASK] = tok;
		++numStashedTokens;
		//lexToken();
		//printChange(start);
	}

	void setup() {}
	T* make(T, Args...)(SourceLocation start, Args args) { return new T(start, args); }
	ExpressionNode* makeExpr(T, Args...)(SourceLocation start, Args args) { return cast(ExpressionNode*)new T(start, null, IrIndex(), args); }

	T* enforceNode(T)(T* t)
	{
		if (t is null)
		{
			string tokenString = lexer.getTokenString(tok);
			context.unrecoverable_error(tok.loc, "Expected `%s` while got `%s` token '%s'",
				T.stringof, tok.type, tokenString);
		}
		return t;
	}

	void expect(TokenType type) {
		if (tok.type != type) {
			string tokenString = lexer.getTokenString(tok);
			context.unrecoverable_error(tok.loc, "Expected `%s` token, while got `%s` token '%s'",
				type, tok.type, tokenString);
		}
	}

	void expectAndConsume(TokenType type) {
		expect(type);
		nextToken();
	}

	Identifier expectIdentifier()
	{
		Identifier id = tok.id;
		expectAndConsume(TokenType.IDENTIFIER);
		return id;
	}

	// ------------------------------ PARSING ----------------------------------

	ModuleDeclNode* parseModule() { // <module> ::= <declaration>*
		version(print_parse) auto s1 = scop("parseModule");
		SourceLocation start = tok.loc;
		nextToken();
		return make!ModuleDeclNode(start, parse_declarations(TokenType.EOI));
	}

	AstNode*[] parse_declarations(TokenType until) { // <declaration>*
		AstNode*[] declarations;
		while (tok.type != until)
		{
			declarations ~= enforceNode(parse_declaration);
		}
		return declarations;
	}

	/// Can return null
	AstNode* parse_declaration() // <declaration> ::= <func_declaration> / <var_declaration> / <struct_declaration>
	{
		version(print_parse) auto s1 = scop("parse_declaration %s", tok.loc);
		SourceLocation start = tok.loc;
		if (tok.type == TokenType.STRUCT_SYM) // <struct_declaration> ::= "struct" <id> "{" <declaration>* "}"
		{
			version(print_parse) auto s2 = scop("struct %s", start);
			nextToken(); // skip "struct"
			Identifier structId = expectIdentifier();
			expectAndConsume(TokenType.LCURLY);
			AstNode*[] declarations = parse_declarations(TokenType.RCURLY);
			expectAndConsume(TokenType.RCURLY);
			return cast(AstNode*)make!StructDeclNode(start, declarations, SymbolRef(structId));
		}
		else // <func_declaration> / <var_declaration>
		{
			version(print_parse) auto s2 = scop("<func_declaration> / <var_declaration> %s", start);
			TypeNode* type = parse_type();
			if (type is null)
			{
				version(print_parse) auto s3 = scop("<type> is null %s", tok.loc);
				return null;
			}
			Identifier declarationId = expectIdentifier();

			ExpressionNode* initializer;
			if (tok.type == TokenType.EQUAL) // "=" <expression>
			{
				// <var_decl> = <type> <identifier> ("=" <expression>)? ";"
				nextToken(); // skip "="
				initializer = expr();
				if (!initializer.isExpression) {
					string tokenString = lexer.getTokenString(initializer.loc);
					context.unrecoverable_error(initializer.loc,
						"Variable declaration can be only initialized with expressions, not with %s, '%s'",
						initializer.astType, tokenString);
				}
				expect(TokenType.SEMICOLON);
			}

			if (tok.type == TokenType.SEMICOLON) // <var_declaration> ::= <type> <id> ";"
			{
				version(print_parse) auto s3 = scop("<var_declaration> %s", start);
				nextToken(); // skip ";"
				return cast(AstNode*)make!VariableDeclNode(start, SymbolRef(declarationId), type, initializer);
			}
			else if (tok.type == TokenType.LPAREN) // <func_declaration> ::= <type> <id> "(" <param_list> ")" (<block_statement> / ';')
			{
				version(print_parse) auto s3 = scop("<func_declaration> %s", start);
				expectAndConsume(TokenType.LPAREN);
				VariableDeclNode*[] params;
				while (tok.type != TokenType.RPAREN)
				{
					// <param> ::= <type> <identifier>?
					TypeNode* paramType = parse_type_expected();
					Identifier paramId;
					size_t paramIndex = params.length;

					if (tok.type == TokenType.IDENTIFIER) // named parameter
						paramId = expectIdentifier();
					else // anon parameter
					{
						paramId = context.idMap.getOrRegWithSuffix("__param_", paramIndex);
					}

					VariableDeclNode* param = make!VariableDeclNode(start, SymbolRef(paramId), paramType);
					param.varFlags |= VariableFlags.isParameter;
					param.paramIndex = cast(typeof(param.paramIndex))paramIndex;
					params ~= param;
					if (tok.type == TokenType.COMMA) nextToken(); // skip ","
					else break;
				}
				expectAndConsume(TokenType.RPAREN);

				BlockStmtNode* block;
				if (tok.type != TokenType.SEMICOLON)
				{
					block = block_stmt();
				}
				else expectAndConsume(TokenType.SEMICOLON); // external function

				return cast(AstNode*)make!FunctionDeclNode(start, SymbolRef(declarationId), type, params, block);
			}
			else
			{
				context.unrecoverable_error(tok.loc, "Expected '(' or ';', while got '%s'", lexer.getTokenString(tok));
				assert(false);
			}
		}
	}

	/// Can return null
	TypeNode* parse_type_expected()
	{
		version(print_parse) auto s1 = scop("parse_type_expected %s", tok.loc);
		auto type = parse_type();
		if (type is null) context.unrecoverable_error(tok.loc, "Expected basic type, while got '%s'", lexer.getTokenString(tok));
		return type;
	}

	TypeNode* parse_type() // <type> = (<type_basic> / <type_user>) <type_specializer>*
	{
		version(print_parse) auto s1 = scop("parse_type %s", tok.loc);
		SourceLocation start = tok.loc;
		TypeNode* base;
		if (tok.type == TokenType.IDENTIFIER) {
			Identifier id = expectIdentifier();
			base = cast(TypeNode*)make!UserTypeNode(start, SymbolRef(id));
		} else if (isBasicTypeToken(tok.type)) {
			base = context.basicTypeNodes(parse_type_basic());
		}

		if (base) // <type_specializer> = '*' / '[' <expression> ']'
		{
			while (true)
			{
				if (tok.type == TokenType.STAR) { // '*' pointer
					nextToken();
					base = cast(TypeNode*)make!PtrTypeNode(start, base);
				} else if (tok.type == TokenType.LBRACKET) { // '[' <expression> ']' static array
					nextToken();
					ExpressionNode* e = expr();
					if (e.astType != AstType.literal_int)
						context.unrecoverable_error(e.loc, "Expected int constant, while got '%s'",
							lexer.getTokenString(e.loc));
					expectAndConsume(TokenType.RBRACKET);
					ulong length = (cast(IntLiteralExprNode*)e).value;
					base = cast(TypeNode*)make!StaticArrayTypeNode(start, base, length);
				}
				else break;
			}
		}

		return base;
	}

	BasicType parse_type_basic()
	{
		version(print_parse) auto s1 = scop("parse_type_basic %s", tok.loc);
		if (isBasicTypeToken(tok.type))
		{
			auto res = tokenTypeToBasicType(tok.type);
			nextToken();
			return res;
		}
		context.unrecoverable_error(tok.loc, lexer.input, "Expected basic type, while got '%s'", lexer.getTokenString(tok));
		assert(false);
	}

	BlockStmtNode* block_stmt() // <block_statement> ::= "{" <statement>* "}"
	{
		version(print_parse) auto s1 = scop("block_stmt %s", tok.loc);
		AstNode*[] statements;
		SourceLocation start = tok.loc;
		expectAndConsume(TokenType.LCURLY);
		while (tok.type != TokenType.RCURLY)
		{
			statements ~= statement();
		}
		expectAndConsume(TokenType.RCURLY);
		return make!BlockStmtNode(start, statements);
	}

	AstNode* statement()
	{
		version(print_parse) auto s1 = scop("statement %s", tok.loc);
		SourceLocation start = tok.loc;
		switch (tok.type)
		{
			case TokenType.IF_SYM: /* "if" <paren_expr> <statement> */
				nextToken();
				ExpressionNode* condition = paren_expr();
				AstNode* thenStatement = statement();
				AstNode* elseStatement;
				if (tok.type == TokenType.ELSE_SYM) { /* ... "else" <statement> */
					nextToken();
					elseStatement = statement();
				}
				return cast(AstNode*)make!IfStmtNode(start, condition, thenStatement, elseStatement);
			case TokenType.WHILE_SYM:  /* "while" <paren_expr> <statement> */
				nextToken();
				ExpressionNode* condition = paren_expr();
				AstNode* statement = statement();
				return cast(AstNode*)make!WhileStmtNode(start, condition, statement);
			case TokenType.DO_SYM:  /* "do" <statement> "while" <paren_expr> ";" */
				nextToken();
				AstNode* statement = statement();
				expectAndConsume(TokenType.WHILE_SYM);
				ExpressionNode* condition = paren_expr();
				expectAndConsume(TokenType.SEMICOLON);
				return cast(AstNode*)make!DoWhileStmtNode(start, condition, statement);
			case TokenType.RETURN_SYM:  /* return <expr> */
				nextToken();
				ExpressionNode* expression = tok.type != TokenType.SEMICOLON ? expr() : null;
				expectAndConsume(TokenType.SEMICOLON);
				return cast(AstNode*)make!ReturnStmtNode(start, expression);
			case TokenType.BREAK_SYM:  /* break; */
				nextToken();
				expectAndConsume(TokenType.SEMICOLON);
				return cast(AstNode*)make!BreakStmtNode(start);
			case TokenType.CONTINUE_SYM:  /* continue; */
				nextToken();
				expectAndConsume(TokenType.SEMICOLON);
				return cast(AstNode*)make!ContinueStmtNode(start);
			case TokenType.SEMICOLON:  /* ";" */
				nextToken();
				return cast(AstNode*)make!BlockStmtNode(start, null); // TODO: make this an error
			case TokenType.LCURLY:  /* "{" { <statement> } "}" */
				return cast(AstNode*)block_stmt();
			default:
			{
				version(print_parse) auto s2 = scop("default %s", tok.loc);
				if (isBasicTypeToken(tok.type) || tok.type == TokenType.STRUCT_SYM) // declaration
				{
					AstNode* decl = parse_declaration;
					return decl;
				}
				else if (tok.type == TokenType.IDENTIFIER)
				{
					version(print_parse) auto s3 = scop("<id> %s", tok.loc);
					stashToken(); // copy to stash
					lexToken(); // lex into tok

					if (tok.type == TokenType.IDENTIFIER) // declaration
					{
						version(print_parse) auto s4 = scop("<id> declaration %s", tok.loc);
						stashToken();
						nextToken(); // move from stash to tok
						AstNode* decl = parse_declaration;
						return decl;
					}
					else // expression
					{
						version(print_parse) auto s4 = scop("<id> expression %s", tok.loc);
						stashToken();
						nextToken(); // move from stash to tok
					}
				}

				// <expr> ";"
				ExpressionNode* expression = expr();
				expectAndConsume(TokenType.SEMICOLON);
				return cast(AstNode*)expression;
			}
		}
	}

	ExpressionNode* paren_expr() { /* <paren_expr> ::= "(" <expr> ")" */
		version(print_parse) auto s1 = scop("paren_expr %s", tok.loc);
		expectAndConsume(TokenType.LPAREN);
		auto res = expr();
		expectAndConsume(TokenType.RPAREN);
		return res;
	}

	ExpressionNode* expr() { /* <expr> ::= <test> | <id> "=" <expr> */
		version(print_parse) auto s1 = scop("expr %s", tok.loc);
		SourceLocation start = tok.loc;
		ExpressionNode* node = test(); // left hand side
		if (tok.type == TokenType.EQUAL)
		{
			AssignOp op;
			if (node.astType == AstType.expr_var)
				op = AssignOp.opAssign;
			else if (node.astType == AstType.expr_index)
				op = AssignOp.opIndexAssign;
			else
				context.error(node.loc, "cannot assign to %s", node.astType);

			nextToken(); // skip '='
			ExpressionNode* lhs = node;
			ExpressionNode* rhs = expr();
			lhs.flags |= AstFlags.isLvalue;
			node = cast(ExpressionNode*)make!AssignStmtNode(start, op, lhs, rhs);
		}
		return node;
	}

	ExpressionNode* test() { /* <test> ::= <sum> | <sum> "<" <sum> */
		version(print_parse) auto s1 = scop("test %s", tok.loc);
		SourceLocation start = tok.loc;
		ExpressionNode* t, n = sum();

		BinOp op;
		switch(tok.type)
		{
			case TokenType.EQUAL_EQUAL: op = BinOp.EQUAL_EQUAL; break;
			case TokenType.NOT_EQUAL: op = BinOp.NOT_EQUAL; break;
			case TokenType.LESS: op = BinOp.LESS; break;
			case TokenType.LESS_EQUAL: op = BinOp.LESS_EQUAL; break;
			case TokenType.GREATER: op = BinOp.GREATER; break;
			case TokenType.GREATER_EQUAL: op = BinOp.GREATER_EQUAL; break;
			default: return n;
		}

		nextToken(); // skip op
		t = n;
		n = makeExpr!BinaryExprNode(start, op, t, sum());

		return n;
	}

	ExpressionNode* sum() { /* <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term> */
		version(print_parse) auto s1 = scop("sum %s", tok.loc);
		SourceLocation start = tok.loc;
		ExpressionNode* n = term();
		ExpressionNode* t;
		loop: while (true)
		{
			BinOp op;
			switch(tok.type) {
				case TokenType.PLUS : op = BinOp.PLUS; break;
				case TokenType.MINUS: op = BinOp.MINUS; break;
				default: break loop;
			}
			nextToken();
			t = n;
			n = makeExpr!BinaryExprNode(start, op, t, term());
		}
		return n;
	}

	ExpressionNode* term() /* <term> ::= <id> / <id> "(" <expr_list> ")" / <id> "[" <expr> "]" / <int_literal> / <string_literal> / <paren_expr> */
	{
		version(print_parse) auto s1 = scop("term %s", tok.loc);
		SourceLocation start = tok.loc;
		if (tok.type == TokenType.IDENTIFIER) // <identifier>
		{
			Identifier id = expectIdentifier();
			if (tok.type == TokenType.LPAREN) // <term> ::= <id> "(" <expr_list> ")"
			{
				expectAndConsume(TokenType.LPAREN);
				ExpressionNode*[] args = expr_list();
				expectAndConsume(TokenType.RPAREN);
				return makeExpr!CallExprNode(start, SymbolRef(id), args);
			}
			if (tok.type == TokenType.LBRACKET) // <term> ::= <id> "[" <expr> "]"
			{
				expectAndConsume(TokenType.LBRACKET);
				ExpressionNode* index = expr();
				expectAndConsume(TokenType.RBRACKET);
				ExpressionNode* array = makeExpr!VariableExprNode(start, SymbolRef(id));
				return makeExpr!IndexExprNode(start, array, index);
			}
			return makeExpr!VariableExprNode(start, SymbolRef(id));
		}
		else if (tok.type == TokenType.INT_LITERAL) // <int_literal>
		{
			long value = lexer.getTokenNumber();
			nextToken();
			TypeNode* type = context.basicTypeNodes(BasicType.t_i32);
			return cast(ExpressionNode*)make!IntLiteralExprNode(start, type, IrIndex(), value);
		}
		else if (tok.type == TokenType.STRING_LITERAL) // <string_literal>
		{
			string value = lexer.getTokenString(tok.loc)[1..$-1]; // omit " at the start and end of token
			nextToken();
			TypeNode* type = context.basicTypeNodes(BasicType.t_i32);
			return cast(ExpressionNode*)make!StringLiteralExprNode(start, type, IrIndex(), value);
		}
		else return paren_expr();
	}

	ExpressionNode*[] expr_list() // <expr_list> ::= (<expr> ",")*
	{
		version(print_parse) auto s1 = scop("expr_list %s", tok.loc);
		ExpressionNode*[] expressions;
		while (tok.type != TokenType.RPAREN)
		{
			expressions ~= expr();
			if (tok.type == TokenType.COMMA) nextToken();
			else break;
		}
		return expressions;
	}
}
