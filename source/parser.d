/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// Grammar
/// Lexer
/// Recursive descent parser
/// For expressions pratt parser is used
///   Copyright (c) 2017, Jean-Marc Bourguet
///   https://github.com/bourguet/operator_precedence_parsing/blob/86c11baa737673da521c9cb488fdc3b25d73f0b6/pratt_tdop_parser.py
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

	<type> = (<type_basic> / <type_struct>) <type_specializer>*
	<type_specializer> = '*' / '[' <expression> ']'
	<type_basic> = ("i8" | "i16" | "i32" | "i64" | "isize" |
		"u8" | "u16" | "u32" | "u64" | "usize" | "void" | "f32" | "f64")

	<type_struct> = <identifier>

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
	@("#eoi")  EOI,
	@(null)   INVALID,

	@("&")    AND,
	@("&&")   AND_AND,
	@("&=")   AND_EQUAL,
	@("@")    AT,
	@("\\")   BACKSLASH,
	@(":")    COLON,
	@(",")    COMMA,
	@("$")    DOLLAR,
	@(".")    DOT,
	@("..")   DOT_DOT,
	@("...")  DOT_DOT_DOT,
	@("=")    EQUAL,
	@("==")   EQUAL_EQUAL,
	@(">")    MORE,
	@(">=")   MORE_EQUAL,
	@(">>")   MORE_MORE,
	@(">>=")  MORE_MORE_EQUAL,
	@(">>>")  MORE_MORE_MORE,
	@(">>>=") MORE_MORE_MORE_EQUAL,
	//@("#")  HASH,
	@("<")    LESS,
	@("<=")   LESS_EQUAL,
	@("<<")   LESS_LESS,
	@("<<=")  LESS_LESS_EQUAL,
	@("-")    MINUS,
	@("-=")   MINUS_EQUAL,
	@("--")   MINUS_MINUS,
	@("!")    NOT,
	@("!=")   NOT_EQUAL,
	@("|")    OR,
	@("|=")   OR_EQUAL,
	@("||")   OR_OR,
	@("%")    PERCENT,
	@("%=")   PERCENT_EQUAL,
	@("+")    PLUS,
	@("+=")   PLUS_EQUAL,
	@("++")   PLUS_PLUS,
	//@("?")  QUESTION,
	@(";")    SEMICOLON,
	@("/")    SLASH,
	@("/=")   SLASH_EQUAL,
	@("*")    STAR,
	@("*=")   STAR_EQUAL,
	@("~")    TILDE,
	@("~=")   TILDE_EQUAL,
	@("^")    XOR,
	@("^=")   XOR_EQUAL,

	@("(") LPAREN,
	@(")") RPAREN,
	@("[") LBRACKET,
	@("]") RBRACKET,
	@("{") LCURLY,
	@("}") RCURLY,


	@("break")    BREAK_SYM,
	@("continue") CONTINUE_SYM,
	@("do")       DO_SYM,
	@("else")     ELSE_SYM,
	@("if")       IF_SYM,
	@("return")   RETURN_SYM,
	@("struct")   STRUCT_SYM,
	@("while")    WHILE_SYM,

	@("#id") IDENTIFIER,              // [a-zA-Z_] [a-zA-Z_0-9]*

	// ----------------------------------------
	// list of basic types. The order is the same as in `enum BasicType`

	@("void") TYPE_VOID,               // void
	@("bool") TYPE_BOOL,               // bool
	@("i8") TYPE_I8,                 // i8
	@("i16") TYPE_I16,                // i16
	@("i32") TYPE_I32,                // i32
	@("i64") TYPE_I64,                // i64

	@("u8") TYPE_U8,                 // u8
	@("u16") TYPE_U16,                // u16
	@("u32") TYPE_U32,                // u32
	@("u64") TYPE_U64,                // u64

	@("f32") TYPE_F32,                // f32
	@("f64") TYPE_F64,                // f64
	// ----------------------------------------

	@("isize") TYPE_ISIZE,              // isize
	@("usize") TYPE_USIZE,              // usize

	@("#num_lit") INT_LITERAL,
	@("#str_lit") STRING_LITERAL,
	//@(null) DECIMAL_LITERAL,         // 0|[1-9][0-9_]*
	//@(null) BINARY_LITERAL,          // ("0b"|"0B")[01_]+
	//@(null) HEX_LITERAL,             // ("0x"|"0X")[0-9A-Fa-f_]+

	@("#comm") COMMENT,                 // // /*
}

immutable string[] tokStrings = gatherInfos();

private string[] gatherInfos()
{
	string[] res = new string[__traits(allMembers, TokenType).length];
	foreach (i, m; __traits(allMembers, TokenType))
	{
		res[i] = __traits(getAttributes, mixin("TokenType."~m))[0];
	}
	return res;
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
				//case '#' : nextChar; return new_tok(TT.HASH);
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
						return new_tok(TT.MORE_EQUAL);
					}
					if (c == '>') { nextChar;
						if (c == '>') { nextChar;
							if (c == '=') { nextChar;
								return new_tok(TT.MORE_MORE_MORE_EQUAL);
							}
							return new_tok(TT.MORE_MORE_MORE);
						}
						if (c == '=') { nextChar;
							return new_tok(TT.MORE_MORE_EQUAL);
						}
						return new_tok(TT.MORE_MORE);
					}
					return new_tok(TT.MORE);
				//case '?' : nextChar; return new_tok(TT.QUESTION);
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
			TT.EQUAL_EQUAL,TT.MORE,TT.MORE_EQUAL,TT.MORE_MORE,
			TT.MORE_MORE_EQUAL,TT.MORE_MORE_MORE,
			TT.MORE_MORE_MORE_EQUAL,TT.HASH,
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
		ushort varIndex = 0;
		while (tok.type != until)
		{
			AstNode* decl = enforceNode(parse_declaration);
			if (decl.astType == AstType.decl_var) {
				(cast(VariableDeclNode*)decl).scopeIndex = varIndex++;
			}
			declarations ~= decl;
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
					param.scopeIndex = cast(typeof(param.scopeIndex))paramIndex;
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

	TypeNode* parse_type() // <type> = (<type_basic> / <type_struct>) <type_specializer>*
	{
		version(print_parse) auto s1 = scop("parse_type %s", tok.loc);
		SourceLocation start = tok.loc;
		TypeNode* base;
		if (tok.type == TokenType.IDENTIFIER) {
			Identifier id = expectIdentifier();
			base = cast(TypeNode*)make!StructTypeNode(start, SymbolRef(id));
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
					uint length = cast(uint)(cast(IntLiteralExprNode*)e).value; // TODO check overflow
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
				expression.flags |= AstFlags.isStatement;
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

	ExpressionNode* expr(int rbp = 0)
	{
		Token t = tok;
		nextToken;

		NullInfo null_info = g_tokenLookups.null_lookup[t.type];
		ExpressionNode* node = null_info.parser_null(this, t, null_info.rbp);
		int nbp = null_info.nbp; // next bp
		int lbp = g_tokenLookups.left_lookup[tok.type].lbp;

		while (rbp < lbp && lbp < nbp)
		{
			t = tok;
			nextToken;
			LeftInfo left_info = g_tokenLookups.left_lookup[t.type];
			node = left_info.parser_left(this, t, left_info.rbp, node);
			nbp = left_info.nbp; // next bp
			lbp = g_tokenLookups.left_lookup[tok.type].lbp;
		}

		return node;
	}
}

/// min and max binding powers
enum MIN_BP = 0;
enum MAX_BP = 10000;
enum COMMA_PREC = 10;

alias LeftParser = ExpressionNode* function(ref Parser p, Token token, int rbp, ExpressionNode* left);
alias NullParser = ExpressionNode* function(ref Parser p, Token token, int rbp);

ExpressionNode* left_error_parser(ref Parser p, Token token, int rbp, ExpressionNode* left)
{
	throw new Exception(format("%s can't be used in infix position", token));
}

ExpressionNode* null_error_parser(ref Parser p, Token token, int rbp)
{
	throw new Exception(format("%s can't be used in prefix position", token));
}

struct LeftInfo
{
	LeftParser parser_left = &left_error_parser;
	int lbp = MIN_BP;
	int rbp = MIN_BP;
	int nbp = MIN_BP;
}

struct NullInfo
{
	NullParser parser_null = &null_error_parser;
	int lbp = MIN_BP;
	int rbp = MIN_BP;
	int nbp = MIN_BP;
}

struct TokenLookups
{
	LeftInfo[TokenType.max+1] left_lookup;
	NullInfo[TokenType.max+1] null_lookup;
}

__gshared immutable TokenLookups g_tokenLookups = cexp_parser();

private TokenLookups cexp_parser()
{
	TokenLookups res;

	TokenType strToTok(string str)
	{
		import std.algorithm.searching : countUntil;
		ptrdiff_t pos = countUntil(tokStrings, str);
		assert(pos != -1, str);
		return cast(TokenType)pos;
	}

	void _RegisterNull(int lbp, int rbp, int nbp, NullParser p, string[] tokens...) {
		foreach (string token; tokens) res.null_lookup[strToTok(token)] = NullInfo(p, lbp, rbp, nbp);
	}

	void _RegisterLeft(int lbp, int rbp, int nbp, LeftParser p, string[] tokens...) {
		foreach (string token; tokens) res.left_lookup[strToTok(token)] = LeftInfo(p, lbp, rbp, nbp);
	}

	void nilfix(int bp, NullParser nud, string[] tokens...) {
		_RegisterNull(MIN_BP, MIN_BP, MAX_BP, nud, tokens);
	}

	void prefix(int bp, NullParser nud, string[] tokens...) {
		_RegisterNull(MIN_BP, bp, MAX_BP, nud, tokens);
	}

	void suffix(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, MIN_BP, MAX_BP, led, tokens);
	}

	void infixL(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp, bp + 1, led, tokens);
	}

	void infixR(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp - 1, bp + 1, led, tokens);
	}

	void infixN(int bp, LeftParser led, string[] tokens...) {
		_RegisterLeft(bp, bp, bp, led, tokens);
	}

	// Compare the code below with this table of C operator precedence:
	// http://en.cppreference.com/w/c/language/operator_precedence

	suffix(310, &leftIncDec, ["++", "--"]);
	infixL(310, &leftFuncCall, "(");
	infixL(310, &leftIndex, "[");
	infixL(310, &leftBinaryOp, ".");
	//infixL(310, &leftBinaryOp, "->");

	// 29 -- binds to everything except function call, indexing, postfix ops
	prefix(290, &nullPrefixOp, ["+", "-", "!", "~", "*", "&", "++", "--"]);

	infixL(250, &leftBinaryOp, ["*", "/", "%"]);

	infixL(230, &leftBinaryOp, ["+", "-"]);
	infixL(210, &leftBinaryOp, ["<<", ">>", ">>>"]);
	infixL(190, &leftBinaryOp, ["<", ">", "<=", ">="]);
	infixL(170, &leftBinaryOp, ["!=", "=="]);

	infixL(150, &leftBinaryOp, "&");
	infixL(130, &leftBinaryOp, "^");
	infixL(110, &leftBinaryOp, "|");
	infixL(90, &leftBinaryOp, "&&");
	infixL(70, &leftBinaryOp, "||");

	// Right associative: a = b = 2 is a = (b = 2)
	infixR(30, &leftAssignOp, ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="]);

	// 0 precedence -- doesn"t bind until )
	prefix(0, &nullParen, "("); // for grouping

	// 0 precedence -- never used
	nilfix(0, &nullLiteral, ["#id", "#num_lit", "#str_lit"]);
	nilfix(0, &null_error_parser, [")", "]", ":", "#eoi", ";"]);
	return res;
}

// Null Denotations -- tokens that take nothing on the left

// id, int_literal, string_literal
ExpressionNode* nullLiteral(ref Parser p, Token token, int rbp) {
	switch(token.type) with(TokenType)
	{
		case IDENTIFIER:
			return p.makeExpr!NameUseExprNode(token.loc, SymbolRef(token.id));
		case STRING_LITERAL:
			// omit " at the start and end of token
			string value = p.lexer.getTokenString(token.loc)[1..$-1];
			return p.makeExpr!StringLiteralExprNode(token.loc, value);
		case INT_LITERAL:
			long value = p.lexer.getTokenNumber();
			return p.makeExpr!IntLiteralExprNode(token.loc, value);
		default:
			p.context.unreachable(); assert(false);
	}
}

// Arithmetic grouping
ExpressionNode* nullParen(ref Parser p, Token token, int rbp) {
	ExpressionNode* r = p.expr(rbp);
	p.expectAndConsume(TokenType.RPAREN);
	//r.flags |= NFLG.parenthesis; // NOTE: needed if ternary operator is needed
	return r;
}

// Prefix operator
// ["+", "-", "!", "~", "*", "&", "++", "--"] <expr>
ExpressionNode* nullPrefixOp(ref Parser p, Token token, int rbp) {
	ExpressionNode* right = p.expr(rbp);
	UnOp op;
	switch(token.type) with(TokenType)
	{
		case PLUS: op = UnOp.plus; break;
		case MINUS: op = UnOp.minus; break;
		case NOT: op = UnOp.logicalNot; break;
		case TILDE: op = UnOp.bitwiseNot; break;
		case STAR: op = UnOp.deref; break;
		case AND: op = UnOp.addrOf; break;
		case PLUS_PLUS: op = UnOp.preIncrement; break;
		case MINUS_MINUS: op = UnOp.preDecrement; break;
		default:
			p.context.unreachable(); assert(false);
	}
	return p.makeExpr!UnaryExprNode(token.loc, op, right);
}

// Left Denotations -- tokens that take an expression on the left

// <expr> "++" / "--"
ExpressionNode* leftIncDec(ref Parser p, Token token, int rbp, ExpressionNode* left) {
	UnOp op;
	switch(token.type) with(TokenType)
	{
		case PLUS_PLUS: op = UnOp.postIncrement; break;
		case MINUS_MINUS: op = UnOp.postDecrement; break;
		default:
			p.context.unreachable(); assert(false);
	}
	return p.makeExpr!UnaryExprNode(token.loc, op, left);
}

// <expr> "[" <expr> "]"
ExpressionNode* leftIndex(ref Parser p, Token token, int rbp, ExpressionNode* array) {
	ExpressionNode* index = p.expr(0);
	p.expectAndConsume(TokenType.RBRACKET);
	return p.makeExpr!IndexExprNode(token.loc, array, index);
}

// Normal binary operator <expr> op <expr>
ExpressionNode* leftBinaryOp(ref Parser p, Token token, int rbp, ExpressionNode* left) {
	ExpressionNode* right = p.expr(rbp);
	BinOp op;
	switch(token.type) with(TokenType)
	{
		// logic ops
		case AND_AND: op = BinOp.LOGIC_AND; break;                // &&
		case OR_OR: op = BinOp.LOGIC_OR; break;                   // ||
		case EQUAL_EQUAL: op = BinOp.EQUAL; break;                // ==
		case NOT_EQUAL: op = BinOp.NOT_EQUAL; break;              // !=
		case MORE: op = BinOp.GREATER; break;                     // >
		case MORE_EQUAL: op = BinOp.GREATER_EQUAL; break;         // >=
		case LESS: op = BinOp.LESS; break;                        // <
		case LESS_EQUAL: op = BinOp.LESS_EQUAL; break;            // <=

		// arithmetic ops
		case AND: op = BinOp.BITWISE_AND; break;                  // &
		case OR: op = BinOp.BITWISE_OR; break;                    // |
		case PERCENT: op = BinOp.REMAINDER; break;                // %
		case LESS_LESS: op = BinOp.SHL; break;                    // <<
		case MORE_MORE: op = BinOp.SHR; break;                    // >>
		case MORE_MORE_MORE: op = BinOp.ASHR; break;              // >>>
		case MINUS: op = BinOp.MINUS; break;                      // -
		case PLUS: op = BinOp.PLUS; break;                        // +
		case SLASH: op = BinOp.DIV; break;                        // /
		case STAR: op = BinOp.MULT; break;                        // *
		case XOR: op = BinOp.XOR; break;                          // ^

		// member access
		case DOT:                                                 // .
			NameUseExprNode* name;
			if (right.astType != AstType.expr_name_use) {
				p.context.error(token.loc,
					"Expected identifier after '.', while got '%s'",
					p.lexer.getTokenString(token));
			}
			else
				name = cast(NameUseExprNode*)right;
			return p.makeExpr!MemberExprNode(token.loc, left, name);

		default:
			p.context.internal_error(token.loc, "parse leftBinaryOp %s", token.type);
			assert(false);
	}
	return p.makeExpr!BinaryExprNode(token.loc, op, left, right);
}

// Binary assignment operator <expr> op= <expr>
ExpressionNode* leftAssignOp(ref Parser p, Token token, int rbp, ExpressionNode* left) {
	ExpressionNode* right = p.expr(rbp);
	BinOp op;
	switch(token.type) with(TokenType)
	{
		// arithmetic opEquals
		case EQUAL: op = BinOp.ASSIGN; break;                     // =
		case AND_EQUAL: op = BinOp.BITWISE_AND_ASSIGN; break;     // &=
		case OR_EQUAL: op = BinOp.BITWISE_OR_ASSIGN; break;       // |=
		case PERCENT_EQUAL: op = BinOp.REMAINDER_ASSIGN; break;   // %=
		case LESS_LESS_EQUAL: op = BinOp.SHL_ASSIGN; break;       // <<=
		case MORE_MORE_EQUAL: op = BinOp.SHR_ASSIGN; break;       // >>=
		case MORE_MORE_MORE_EQUAL: op = BinOp.ASHR_ASSIGN; break; // >>>=
		case MINUS_EQUAL: op = BinOp.MINUS_ASSIGN; break;         // -=
		case PLUS_EQUAL: op = BinOp.PLUS_ASSIGN; break;           // +=
		case SLASH_EQUAL: op = BinOp.DIV_ASSIGN; break;           // /=
		case STAR_EQUAL: op = BinOp.MULT_ASSIGN; break;           // *=
		case XOR_EQUAL: op = BinOp.XOR_ASSIGN; break;             // ^=
		default:
			p.context.internal_error(token.loc, "parse leftAssignOp %s", token.type);
			assert(false);
	}
	left.flags |= AstFlags.isLvalue;

	auto e = p.makeExpr!BinaryExprNode(token.loc, op, left, right);
	e.flags |= AstFlags.isAssignment;

	return e;
}

// <id> "(" <expr_list> ")"
ExpressionNode* leftFuncCall(ref Parser p, Token token, int unused_rbp, ExpressionNode* callee) {
	ExpressionNode*[] args;
	while (p.tok.type != TokenType.RPAREN) {
		// We don't want to grab the comma, e.g. it is NOT a sequence operator.
		args ~= p.expr(COMMA_PREC);
		// allows trailing comma too
		if (p.tok.type == TokenType.COMMA)
			p.nextToken;
	}
	p.expectAndConsume(TokenType.RPAREN);
	return p.makeExpr!CallExprNode(token.loc, callee, args);
}
