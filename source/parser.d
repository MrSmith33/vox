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
import std.conv : to;

import all;


// Grammar
/**
	<module> = <declaration>* EOF
	<declaration> = <func_decl> / <var_decl> / <struct_decl> / <enum_decl>

	<func_decl> = <type> <identifier> "(" <param_list> ")" (<block_statement> / ';')
	<param_list> = <parameter> "," <parameter_list> / <parameter>?
	<parameter> = <type> <identifier>?

	<var_decl> = <type> <identifier> ("=" <expression>)? ";"
	<struct_decl> = "struct" <identifier> "{" <declaration>* "}"
	<enum_decl> = <enum_decl_single> / <enum_decl_multi>
	<enum_decl_multi> = "enum" [<identifier>] [":" <type>] {" <identifier> ["=" <expr>] ,* "}"
	<enum_decl_single> = "enum" <identifier> [ "=" <expr> ] ";"

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
	<type_specializer> = '*' / '[' <expression> ']' / '[' ']'
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
	@("#soi")  SOI,
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
	@("#")    HASH,
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
	@("?")    QUESTION,
	@(";")    SEMICOLON,
	@("/")    SLASH,
	@("/=")   SLASH_EQUAL,
	@("*")    STAR,
	@("*=")   STAR_EQUAL,
	@("~")    TILDE,
	@("~=")   TILDE_EQUAL,
	@("^")    XOR,
	@("^=")   XOR_EQUAL,

	@("(")    LPAREN,
	@(")")    RPAREN,
	@("[")    LBRACKET,
	@("]")    RBRACKET,
	@("{")    LCURLY,
	@("}")    RCURLY,


	@("break")    BREAK_SYM,
	@("continue") CONTINUE_SYM,
	@("do")       DO_SYM,
	@("else")     ELSE_SYM,
	@("if")       IF_SYM,
	@("return")   RETURN_SYM,
	@("struct")   STRUCT_SYM,
	@("while")    WHILE_SYM,
	@("cast")     CAST,                 // cast(T)
	@("enum")     ENUM,

	@("#id")      IDENTIFIER,           // [a-zA-Z_] [a-zA-Z_0-9]*

	// ----------------------------------------
	// list of basic types. The order is the same as in `enum BasicType`

	@("void") TYPE_VOID,                // void
	@("bool") TYPE_BOOL,                // bool
	@("i8")   TYPE_I8,                  // i8
	@("i16")  TYPE_I16,                 // i16
	@("i32")  TYPE_I32,                 // i32
	@("i64")  TYPE_I64,                 // i64

	@("u8")   TYPE_U8,                  // u8
	@("u16")  TYPE_U16,                 // u16
	@("u32")  TYPE_U32,                 // u32
	@("u64")  TYPE_U64,                 // u64

	@("f32")  TYPE_F32,                 // f32
	@("f64")  TYPE_F64,                 // f64
	// ----------------------------------------

	@("isize") TYPE_ISIZE,              // isize
	@("usize") TYPE_USIZE,              // usize

	@("#num_dec_lit") INT_DEC_LITERAL,
	@("#num_hex_lit") INT_HEX_LITERAL,
	@("#num_bin_lit") INT_BIN_LITERAL,
	@("#str_lit") STRING_LITERAL,
	//@(null) DECIMAL_LITERAL,          // 0|[1-9][0-9_]*
	//@(null) BINARY_LITERAL,           // ("0b"|"0B")[01_]+
	//@(null) HEX_LITERAL,              // ("0x"|"0X")[0-9A-Fa-f_]+

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
	TokenIndex index;
}

struct TokenIndex
{
	uint index;
	alias index this;
}

struct SourceFileInfo
{
	/// Start of file source code in CompilationContext.sourceBuffer
	uint start;
	/// Length of source code
	uint length;
}

struct SourceLocation {
	uint start;
	uint end;
	uint line;
	uint col;
	const(char)[] getTokenString(const(char)[] input) pure const { return input[start..end]; }
	void toString(scope void delegate(const(char)[]) sink) const {
		sink.formattedWrite("line %s col %s start %s end %s", line+1, col+1, start, end);
	}
}

/// Start of input
enum char SOI_CHAR = '\2';
/// End of input
enum char EOI_CHAR = '\3';

immutable string[] keyword_strings = ["bool","break","continue","do","else","f32","f64",
	"i16","i32","i64","i8","if","isize","return","struct","u16","u32","u64",
	"u8","usize","void","while","cast","enum"];
enum NUM_KEYWORDS = keyword_strings.length;
immutable TokenType[NUM_KEYWORDS] keyword_tokens = [TT.TYPE_BOOL,TT.BREAK_SYM,TT.CONTINUE_SYM,TT.DO_SYM,
	TT.ELSE_SYM,TT.TYPE_F32,TT.TYPE_F64,TT.TYPE_I16,TT.TYPE_I32,TT.TYPE_I64,
	TT.TYPE_I8,TT.IF_SYM,TT.TYPE_ISIZE,TT.RETURN_SYM,TT.STRUCT_SYM,
	TT.TYPE_U16,TT.TYPE_U32,TT.TYPE_U64,TT.TYPE_U8,TT.TYPE_USIZE,
	TT.TYPE_VOID,TT.WHILE_SYM,TT.CAST,TT.ENUM];

//                          #        #######  #     #
//                          #        #         #   #
//                          #        #          # #
//                          #        #####       #
//                          #        #          # #
//                          #        #         #   #
//                          ######   #######  #     #
// -----------------------------------------------------------------------------

void pass_lexer(ref CompilationContext ctx)
{
	Lexer lexer = Lexer(&ctx, ctx.sourceBuffer, ctx.tokenBuffer, ctx.tokenLocationBuffer);
	// TODO: when compiling multiple modules, continue buffers instead of overwriting them

	lexer.lex();

	if (ctx.printLexemes) {
		writeln("// Lexemes");
		Token tok;
		do
		{
			tok.type = ctx.tokenBuffer[tok.index];
			auto loc = ctx.tokenLocationBuffer[tok.index];
			writefln("%s %s, `%s`", tok, loc, loc.getTokenString(ctx.sourceBuffer));
			++tok.index;
		}
		while(tok.type != TokenType.EOI);
	}
}

struct Lexer
{
	CompilationContext* context;
	const(char)[] inputChars;
	TokenType[] outputTokens;
	SourceLocation[] outputTokenLocations;

	TokenIndex tokenIndex;

	private dchar c; // current symbol

	private uint position; // offset of 'c' in input
	private uint line; // line of 'c'
	private uint column; // column of 'c'

	private uint startPos; // offset of first token byte in input
	private uint startLine; // line of first token byte
	private uint startCol; // column of first token byte

	void lex()
	{
		while (true)
		{
			TokenType tokType = nextToken();

			outputTokens[tokenIndex] = tokType;
			set_loc();
			++tokenIndex;

			if (tokType == TokenType.EOI) return;
		}
	}

	private void nextChar()
	{
		++position;
		++column;
		c = inputChars[position];
	}

	private void set_loc()
	{
		outputTokenLocations[tokenIndex] = SourceLocation(startPos, position, startLine, startCol);
	}

	int opApply(scope int delegate(TokenType) dg)
	{
		TokenType tok;
		while ((tok = nextToken()) != TokenType.EOI)
			if (int res = dg(tok))
				return res;
		return 0;
	}

	TokenType nextToken()
	{
		c = inputChars[position];

		while (true)
		{
			startPos = position;
			startLine = line;
			startCol = column;

			switch(c)
			{
				case SOI_CHAR:         nextChar; return TT.SOI;
				case EOI_CHAR:         return TT.EOI;
				case '\t': nextChar;   continue;
				case '\n': lex_EOLN(); continue;
				case '\r': lex_EOLR(); continue;
				case ' ' : nextChar;   continue;
				case '!' : nextChar; return lex_multi_equal2(TT.NOT, TT.NOT_EQUAL);
				//case '#' : nextChar; return TT.HASH;
				case '$' : nextChar; return TT.DOLLAR;
				case '%' : nextChar; return lex_multi_equal2(TT.PERCENT, TT.PERCENT_EQUAL);
				case '&' : nextChar; return lex_multi_equal2_3('&', TT.AND, TT.AND_EQUAL, TT.AND_AND);
				case '(' : nextChar; return TT.LPAREN;
				case ')' : nextChar; return TT.RPAREN;
				case '*' : nextChar; return lex_multi_equal2(TT.STAR, TT.STAR_EQUAL);
				case '+' : nextChar; return lex_multi_equal2_3('+', TT.PLUS, TT.PLUS_EQUAL, TT.PLUS_PLUS);
				case ',' : nextChar; return TT.COMMA;
				case '-' : nextChar; return lex_multi_equal2_3('-', TT.MINUS, TT.MINUS_EQUAL, TT.MINUS_MINUS);
				case '.' : nextChar;
					if (c == '.') { nextChar;
						if (c == '.') { nextChar;
							return TT.DOT_DOT_DOT;
						}
						return TT.DOT_DOT;
					}
					return TT.DOT;
				case '\"': nextChar; return lex_QUOTE_QUOTE();
				case '/' :           return lex_SLASH();
				case '0' :           return lex_ZERO();
				case '1' : ..case '9': return lex_DIGIT();
				case ':' : nextChar; return TT.COLON;
				case ';' : nextChar; return TT.SEMICOLON;
				case '<' : nextChar;
					if (c == '<') { nextChar;
						if (c == '=') { nextChar;
							return TT.LESS_LESS_EQUAL;
						}
						return TT.LESS_LESS;
					}
					if (c == '=') { nextChar;
						return TT.LESS_EQUAL;
					}
					return TT.LESS;
				case '=' : nextChar; return lex_multi_equal2(TT.EQUAL, TT.EQUAL_EQUAL);
				case '#' : nextChar; return TT.HASH;
				case '?' : nextChar; return TT.QUESTION;
				case '>' : nextChar;
					if (c == '=') { nextChar;
						return TT.MORE_EQUAL;
					}
					if (c == '>') { nextChar;
						if (c == '>') { nextChar;
							if (c == '=') { nextChar;
								return TT.MORE_MORE_MORE_EQUAL;
							}
							return TT.MORE_MORE_MORE;
						}
						if (c == '=') { nextChar;
							return TT.MORE_MORE_EQUAL;
						}
						return TT.MORE_MORE;
					}
					return TT.MORE;
				//case '?' : nextChar; return TT.QUESTION;
				case '@' : nextChar; return TT.AT;
				case 'A' : ..case 'Z': return lex_LETTER();
				case '[' : nextChar; return TT.LBRACKET;
				case '\\': nextChar; return TT.BACKSLASH;
				case ']' : nextChar; return TT.RBRACKET;
				case '^' : nextChar; return lex_multi_equal2(TT.XOR, TT.XOR_EQUAL);
				case '_' : nextChar; return lex_LETTER();
				case 'a' : ..case 'z': return lex_LETTER();
				case '{' : nextChar; return TT.LCURLY;
				case '|' : nextChar; return lex_multi_equal2_3('|', TT.OR, TT.OR_EQUAL, TT.OR_OR);
				case '}' : nextChar; return TT.RCURLY;
				case '~' : nextChar; return lex_multi_equal2(TT.TILDE, TT.TILDE_EQUAL);
				default  : nextChar; return TT.INVALID;
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
	private TokenType lex_multi_equal2(TokenType single_tok, TokenType eq_tok)
	{
		if (c == '=') {
			nextChar;
			return eq_tok;
		}
		return single_tok;
	}

	private TokenType lex_multi_equal2_3(dchar chr, TokenType single_tok, TokenType eq_tok, TokenType double_tok)
	{
		if (c == chr) { nextChar;
			return double_tok;
		}
		if (c == '=') { nextChar;
			return eq_tok;
		}
		return single_tok;
	}

	private TokenType lex_SLASH() // /
	{
		nextChar;
		if (c == '/')
		{
			consumeLine();
			return TT.COMMENT;
		}
		if (c == '*')
		{
			while (true)
			{
				switch(c)
				{
					case EOI_CHAR:
						set_loc();
						context.unrecoverable_error(tokenIndex, "Unterminated comment");
						return TT.INVALID;

					case '\n': lex_EOLN(); continue;
					case '\r': lex_EOLR(); continue;
					case '*':
						nextChar;
						if (c == '/') {
							nextChar;
							return TT.COMMENT;
						}
						break;
					default: break;
				}
				nextChar;
			}
			return TT.COMMENT;
		}
		if (c == '=') { nextChar;
			return TT.SLASH_EQUAL;
		}
		return TT.SLASH;
	}

	private TokenType lex_QUOTE_QUOTE() // "
	{
		while (true)
		{
			switch(c)
			{
				case EOI_CHAR:
					set_loc();
					context.unrecoverable_error(tokenIndex, "Unterminated string literal");
					return TT.INVALID;
				case '\n': lex_EOLN(); continue;
				case '\r': lex_EOLR(); continue;
				case '\"':
					nextChar; // skip "
					return TT.STRING_LITERAL;
				default: break;
			}
			nextChar;
		}
	}

	private TokenType lex_ZERO() // 0
	{
		nextChar;

		if (c == 'x' || c == 'X')
		{
			nextChar;
			consumeHexadecimal();
			return TT.INT_HEX_LITERAL;
		}
		else if (c == 'b' || c == 'B')
		{
			nextChar;
			consumeBinary();
			return TT.INT_BIN_LITERAL;
		}
		else
		{
			consumeDecimal();
			return TT.INT_DEC_LITERAL;
		}
	}

	private TokenType lex_DIGIT() // 1-9
	{
		nextChar;
		consumeDecimal();
		return TT.INT_DEC_LITERAL;
	}

	private TokenType lex_LETTER() // a-zA-Z_
	{
		switch (c)
		{
			case 'b':
				nextChar;
				if (c == 'o' && match("ool")) return TT.TYPE_BOOL;
				else if (c == 'r' && match("reak")) return TT.BREAK_SYM;
				break;
			case 'c':
				nextChar;
				if (c == 'o' && match("ontinue")) return TT.CONTINUE_SYM;
				else if (c == 'a' && match("ast")) return TT.CAST;
				break;
			case 'd': if (match("do")) return TT.DO_SYM; break;
			case 'e':
				nextChar;
				if (c == 'l' && match("lse")) return TT.ELSE_SYM;
				else if (c == 'n' && match("num")) return TT.ENUM;
				break;
			case 'f':
				nextChar;
				if (c == '3' && match("32")) return TT.TYPE_F32;
				if (c == '6' && match("64")) return TT.TYPE_F64;
				break;
			case 'i':
				nextChar;
				switch(c) {
					case '1': if (match("16")) return TT.TYPE_I16; break;
					case '3': if (match("32")) return TT.TYPE_I32; break;
					case '6': if (match("64")) return TT.TYPE_I64; break;
					case '8': if (match("8"))  return TT.TYPE_I8;  break;
					case 's': if (match("size")) return TT.TYPE_ISIZE; break;
					case 'f': if (match("f")) return TT.IF_SYM; break;
					default: break;
				}
				break;
			case 'r': if (match("return")) return TT.RETURN_SYM; break;
			case 's': if (match("struct")) return TT.STRUCT_SYM; break;
			case 'u':
				nextChar;
				switch(c) {
					case '1': if (match("16")) return TT.TYPE_U16; break;
					case '3': if (match("32")) return TT.TYPE_U32; break;
					case '6': if (match("64")) return TT.TYPE_U64; break;
					case '8': if (match("8"))  return TT.TYPE_U8;  break;
					case 's': if (match("size")) return TT.TYPE_USIZE; break;
					default: break;
				}
				break;
			case 'v': if (match("void")) return TT.TYPE_VOID; break;
			case 'w': if (match("while")) return TT.WHILE_SYM; break;
			default: break;
		}

		consumeId();
		return TT.IDENTIFIER;
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
			} else if (c != '_') return;
			nextChar;
		}
	}

	private void consumeHexadecimal()
	{
		while (true)
		{
			if ('0' <= c && c <= '9') {
			} else if ('a' <= c && c <= 'f') {
			} else if ('A' <= c && c <= 'F') {
			} else if (c != '_') return;
			nextChar;
		}
	}

	private void consumeBinary()
	{
		while (true)
		{
			if (c == '0' || c == '1') {
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
	TokenType[4] tokenBuffer;
	SourceLocation[4] locs;

	Lexer makeLexer(string input) {
		return Lexer(&ctx, input~EOI_CHAR, tokenBuffer, locs);
	}

	foreach(i, string keyword; keyword_strings)
	{
		Lexer lexer = makeLexer(keyword);
		TokenType token = lexer.nextToken;
		assert(token == keyword_tokens[i],
			format("For %s expected %s got %s", keyword, keyword_tokens[i], token));
	}

	foreach(i, string keyword; keyword_strings)
	{
		Lexer lexer = makeLexer(keyword~"A");
		TokenType token = lexer.nextToken;
		assert(token == TT.IDENTIFIER);
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
		foreach(i, string op; ops)
		{
			Lexer lexer = makeLexer(op);
			TokenType token = lexer.nextToken;
			assert(token == tokens_ops[i],
				format("For %s expected %s got %s", op, tokens_ops[i], token));
		}
	}

	void testNumeric(string input, TokenType tokType)
	{
		Lexer lexer = makeLexer(input);
		assert(lexer.nextToken == tokType);
	}

	assert(makeLexer("_10").nextToken == TT.IDENTIFIER);
	testNumeric("10", TT.INT_DEC_LITERAL);
	testNumeric("1_0", TT.INT_DEC_LITERAL);
	testNumeric("10_", TT.INT_DEC_LITERAL);
	testNumeric("0xFF", TT.INT_HEX_LITERAL);
	testNumeric("0XABCDEF0123456789", TT.INT_HEX_LITERAL);
	testNumeric("0x1_0", TT.INT_HEX_LITERAL);
	testNumeric("0b10", TT.INT_BIN_LITERAL);
	testNumeric("0B10", TT.INT_BIN_LITERAL);
	testNumeric("0b1_0", TT.INT_BIN_LITERAL);

	{
		string source = "/*\n*/test";
		Lexer lexer = makeLexer(source);
		lexer.lex;
		assert(tokenBuffer[0] == TT.COMMENT);
		assert(locs[0].getTokenString(source) == "/*\n*/", format("%s", locs[0]));
		assert(tokenBuffer[1] == TT.IDENTIFIER);
		assert(locs[1].getTokenString(source) == "test");
	}
	{
		string source = "//test\nhello";
		Lexer lexer = makeLexer(source);
		lexer.lex;
		assert(tokenBuffer[0] == TT.COMMENT);
		assert(locs[0].getTokenString(source) == "//test\n");
		assert(tokenBuffer[1] == TT.IDENTIFIER);
		assert(locs[1].getTokenString(source) == "hello");
	}
	{
		string source = `"literal"`;
		Lexer lexer = makeLexer(source);
		lexer.lex;
		assert(tokenBuffer[0] == TT.STRING_LITERAL);
		assert(locs[0].getTokenString(source) == `"literal"`, format("%s", tokenBuffer[0]));
	}
}

void pass_parser(ref CompilationContext ctx) {
	Parser parser = Parser(&ctx);

	ctx.mod = parser.parseModule();

	if (ctx.printAstFresh && ctx.mod !is null) {
		auto astPrinter = AstPrinter(&ctx, 2);
		writeln("// AST fresh");
		astPrinter.printAst(cast(AstNode*)ctx.mod);
	}
}

//version = print_parse;
struct Parser
{
	CompilationContext* context;
	Token tok;
	SourceLocation loc() {
		return context.tokenLocationBuffer[tok.index];
	}

	int nesting;
	auto indent() { return ' '.repeat(nesting*2); }
	struct Scope { Parser* p; ~this(){--p.nesting;}}
	Scope scop(Args...)(string name, Args args) { write(indent); writefln(name, args); ++nesting; return Scope(&this); }

	void nextToken()
	{
		do {
			++tok.index;
			tok.type = context.tokenBuffer[tok.index];
		}
		while (tok.type == TokenType.COMMENT);
	}

	T* make(T, Args...)(TokenIndex start, Args args) { return new T(start, args); }
	ExpressionNode* makeExpr(T, Args...)(TokenIndex start, Args args) { return cast(ExpressionNode*)new T(start, null, IrIndex(), args); }

	T* enforceNode(T)(T* t)
	{
		if (t is null)
		{
			const(char)[] tokenString = context.getTokenString(tok.index);
			context.unrecoverable_error(tok.index, "Expected `%s` while got `%s` tok '%s'",
				T.stringof, tok, tokenString);
		}
		return t;
	}

	void expect(TokenType type) {
		if (tok.type != type) {
			const(char)[] tokenString = context.getTokenString(tok.index);
			context.unrecoverable_error(tok.index, "Expected `%s` token, while got `%s` token '%s'",
				type, tok, tokenString);
		}
	}

	void expectAndConsume(TokenType type) {
		expect(type);
		nextToken();
	}

	Identifier makeIdentifier(TokenIndex index)
	{
		const(char)[] str = context.getTokenString(index);
		return context.idMap.getOrRegNoDup(str);
	}

	Identifier expectIdentifier()
	{
		Identifier id = makeIdentifier(tok.index);
		expectAndConsume(TokenType.IDENTIFIER);
		return id;
	}

	// ------------------------------ PARSING ----------------------------------

	ModuleDeclNode* parseModule() { // <module> ::= <declaration>*
		version(print_parse) auto s1 = scop("parseModule");
		tok.index = TokenIndex(0);
		tok.type = context.tokenBuffer[tok.index];
		expectAndConsume(TokenType.SOI);
		return make!ModuleDeclNode(tok.index, parse_declarations(TokenType.EOI));
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
		version(print_parse) auto s1 = scop("parse_declaration %s", loc);
		TokenIndex start = tok.index;
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
		// <enum_decl> = <enum_decl_single> / <enum_decl_multi>
		// <enum_decl_multi> = "enum" [<identifier>] [":" <type>] {" <identifier> ["=" <expr>] ,* "}"
		// <enum_decl_single> = "enum" <identifier> [ "=" <expr> ] ";"
		//else if (tok == TokenType.ENUM)
		//{

		//}
		else // <func_declaration> / <var_declaration>
		{
			version(print_parse) auto s2 = scop("<func_declaration> / <var_declaration> %s", start);
			TypeNode* type = parse_type();
			if (type is null)
			{
				version(print_parse) auto s3 = scop("<type> is null %s", loc);
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
					const(char)[] tokenString = context.getTokenString(initializer.loc);
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
				context.unrecoverable_error(tok.index, "Expected '(' or ';', while got '%s'", context.getTokenString(tok.index));
				assert(false);
			}
		}
	}

	/// Can return null
	TypeNode* parse_type_expected()
	{
		version(print_parse) auto s1 = scop("parse_type_expected %s", tok.index);
		auto type = parse_type();
		if (type is null) context.unrecoverable_error(tok.index, "Expected basic type, while got '%s'", context.getTokenString(tok.index));
		return type;
	}

	TypeNode* parse_type() // <type> = (<type_basic> / <type_struct>) <type_specializer>*
	{
		version(print_parse) auto s1 = scop("parse_type %s", loc);
		TokenIndex start = tok.index;
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
				} else if (tok.type == TokenType.LBRACKET) {
					nextToken();
					if (tok.type == TokenType.RBRACKET) // '[' ']' slice
					{
						nextToken(); // skip ']'
						base = cast(TypeNode*)make!SliceTypeNode(start, base);
					}
					else // '[' <expression> ']' static array
					{
						ExpressionNode* e = expr();
						if (e.astType != AstType.literal_int)
							context.unrecoverable_error(e.loc, "Expected int constant, while got '%s'",
								context.getTokenString(e.loc));
						expectAndConsume(TokenType.RBRACKET);
						uint length = cast(uint)(cast(IntLiteralExprNode*)e).value; // TODO check overflow
						base = cast(TypeNode*)make!StaticArrayTypeNode(start, base, length);
					}
				}
				else break;
			}
		}

		return base;
	}

	BasicType parse_type_basic()
	{
		version(print_parse) auto s1 = scop("parse_type_basic %s", loc);
		if (isBasicTypeToken(tok.type))
		{
			auto res = tokenTypeToBasicType(tok.type);
			nextToken();
			return res;
		}
		context.unrecoverable_error(tok.index, "Expected basic type, while got '%s'", context.getTokenString(tok.index));
		assert(false);
	}

	BlockStmtNode* block_stmt() // <block_statement> ::= "{" <statement>* "}"
	{
		version(print_parse) auto s1 = scop("block_stmt %s", loc);
		AstNode*[] statements;
		TokenIndex start = tok.index;
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
		version(print_parse) auto s1 = scop("statement %s", loc);
		TokenIndex start = tok.index;
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
				version(print_parse) auto s2 = scop("default %s", loc);
				if (isBasicTypeToken(tok.type) ||
					tok.type == TokenType.STRUCT_SYM ||
					tok.type == TokenType.ENUM) // declaration
				{
					AstNode* decl = parse_declaration;
					return decl;
				}
				else if (tok.type == TokenType.IDENTIFIER)
				{
					Token copy = tok; // save
					version(print_parse) auto s3 = scop("<id> %s", loc);
					nextToken();

					if (tok.type == TokenType.IDENTIFIER) // declaration
					{
						version(print_parse) auto s4 = scop("<id> declaration %s", loc);
						tok = copy; // restore
						AstNode* decl = parse_declaration;
						return decl;
					}
					else // expression
					{
						version(print_parse) auto s4 = scop("<id> expression %s", loc);
						tok = copy; // restore
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
		version(print_parse) auto s1 = scop("paren_expr %s", loc);
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
		assert(pos != -1, str ~ " not found");
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
	prefix(290, &nullCast, "cast");

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
	nilfix(0, &nullLiteral, ["#id", "#num_dec_lit", "#num_bin_lit", "#num_hex_lit", "#str_lit"]);
	nilfix(0, &null_error_parser, [")", "]", ":", "#eoi", ";"]);
	return res;
}

// Null Denotations -- tokens that take nothing on the left

// id, int_literal, string_literal
ExpressionNode* nullLiteral(ref Parser p, Token token, int rbp) {
	switch(token.type) with(TokenType)
	{
		case IDENTIFIER:
			Identifier id = p.makeIdentifier(token.index);
			return p.makeExpr!NameUseExprNode(token.index, SymbolRef(id));
		case STRING_LITERAL:
			// omit " at the start and end of token
			string value = cast(string)p.context.getTokenString(token.index)[1..$-1];
			return p.makeExpr!StringLiteralExprNode(token.index, value);
		case INT_DEC_LITERAL:
			string value = cast(string)p.context.getTokenString(token.index);
			long intValue = to!ulong(value);
			return p.makeExpr!IntLiteralExprNode(token.index, intValue);
		case INT_HEX_LITERAL:
			string value = cast(string)p.context.getTokenString(token.index);
			long intValue = to!ulong(value[2..$], 16); // skip 0x, 0X
			return p.makeExpr!IntLiteralExprNode(token.index, intValue);
		case INT_BIN_LITERAL:
			string value = cast(string)p.context.getTokenString(token.index);
			long intValue = to!ulong(value[2..$], 2); // skip 0b, 0B
			return p.makeExpr!IntLiteralExprNode(token.index, intValue);
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
	return p.makeExpr!UnaryExprNode(token.index, op, right);
}

// "cast" "(" <expr> ")" <expr>
ExpressionNode* nullCast(ref Parser p, Token token, int rbp) {
	p.expectAndConsume(TokenType.LPAREN);
	TypeNode* type = p.parse_type_expected();
	p.expectAndConsume(TokenType.RPAREN);
	ExpressionNode* right = p.expr(rbp);
	return cast(ExpressionNode*) p.make!TypeConvExprNode(token.index, type, IrIndex(), right);
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
	return p.makeExpr!UnaryExprNode(token.index, op, left);
}

// <expr> "[" <expr> "]"
ExpressionNode* leftIndex(ref Parser p, Token token, int rbp, ExpressionNode* array) {
	ExpressionNode* index = p.expr(0);
	p.expectAndConsume(TokenType.RBRACKET);
	return p.makeExpr!IndexExprNode(token.index, array, index);
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
				p.context.error(token.index,
					"Expected identifier after '.', while got '%s'",
					p.context.getTokenString(token.index));
			}
			else
				name = cast(NameUseExprNode*)right;
			return p.makeExpr!MemberExprNode(token.index, left, name);

		default:
			p.context.internal_error(token.index, "parse leftBinaryOp %s", token.type);
			assert(false);
	}
	return p.makeExpr!BinaryExprNode(token.index, op, left, right);
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
			p.context.internal_error(token.index, "parse leftAssignOp %s", token.type);
			assert(false);
	}
	left.flags |= AstFlags.isLvalue;

	auto e = p.makeExpr!BinaryExprNode(token.index, op, left, right);
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
	return p.makeExpr!CallExprNode(token.index, callee, args);
}
