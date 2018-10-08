/**
Copyright: Copyright (c) 2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// This is the simplest complete (not yet) compiler for C-like lang
module compiler1;

import std.array : empty;
import std.string : format;
import std.typecons : Flag, Yes, No;
import std.stdio : writeln, write, writef, writefln, stdout;
import std.format : formattedWrite, FormatSpec;
import std.range : repeat;
import std.range : chain;
import std.bitmanip : bitfields;
import std.algorithm : min, max, sort, swap;

import old_ir;
import utils;
import ir_to_amd64;

// Grammar
/**
	<module> = <declaration>* EOF
	<declaration> = <func_decl> / <var_decl> / <struct_decl>

	<func_decl> = <type> <identifier> "(" <param_list> ")" (<block_statement> / ';')
	<param_list> = <parameter> "," <parameter_list> / <parameter>?
	<parameter> = <type> <identifier>?

	<var_decl> = <type> <identifier> ";"
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
	<term> = <identifier> "(" <expression_list> ")" / <identifier> "[" <expression> "]" / <identifier> / <int_literal> / <paren_expression>
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


struct ExternalSymbol
{
	string name;
	void* ptr;
}

enum IceBehavior : ubyte {
	error,
	breakpoint
}

struct CompilationContext
{
	string input;
	ubyte[] codeBuffer;
	ExternalSymbol[Identifier] externalSymbols;
	ModuleDeclNode* mod;
	MachineInfo* machineInfo = &mach_info_x86_64;
	ScopeStack scopeStack;

	IdentifierMap idMap;
	bool hasErrors;
	TextSink sink;
	IceBehavior iceBehavior = IceBehavior.error;
	bool buildDebug = false;

	//alias idString = idMap.get;
	string idString(const Identifier id) { return idMap.get(id); }

	static __gshared BasicTypeNode[] basicTypes = [
		basicTypeNode(0, BasicType.t_error),
		basicTypeNode(0, BasicType.t_void),
		basicTypeNode(1, BasicType.t_bool , BasicTypeFlag.isBoolean),

		basicTypeNode(1, BasicType.t_i8   , BasicTypeFlag.isInteger),
		basicTypeNode(2, BasicType.t_i16  , BasicTypeFlag.isInteger),
		basicTypeNode(4, BasicType.t_i32  , BasicTypeFlag.isInteger),
		basicTypeNode(8, BasicType.t_i64  , BasicTypeFlag.isInteger),
		//basicTypeNode(1, BasicType.t_isize, BasicTypeFlag.isInteger), // this is alias

		basicTypeNode(1, BasicType.t_u8   , BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned),
		basicTypeNode(2, BasicType.t_u16  , BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned),
		basicTypeNode(4, BasicType.t_u32  , BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned),
		basicTypeNode(8, BasicType.t_u64  , BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned),
		//basicTypeNode(1, BasicType.t_usize, BasicTypeFlag.isInteger | BasicTypeFlag.isUnsigned), // this is alias

		basicTypeNode(4, BasicType.t_f32  , BasicTypeFlag.isFloat),
		basicTypeNode(8, BasicType.t_f64  , BasicTypeFlag.isFloat),
	];

	TypeNode* basicTypeNodes(BasicType basicType) { return cast(TypeNode*)&basicTypes[basicType]; }

	void error(Args...)(SourceLocation loc, string format, Args args)
	{
		sink.putf("file(%s, %s): Error: ", loc.line+1, loc.col+1);
		sink.putfln(format, args);
		hasErrors = true;
	}

	void unrecoverable_error(Args...)(SourceLocation loc, string format, Args args)
	{
		sink.putf("file(%s, %s): Error: ", loc.line+1, loc.col+1);
		sink.putfln(format, args);
		hasErrors = true;
		throw new CompilationException();
	}

	void assertf(Args...)(bool cond, string fmt, lazy Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;
		sink.putf("%s(%s): ICE: Assertion failure: ", file, line);
		sink.putfln(fmt, args);
		hasErrors = true;
		handleICE;
		throw new CompilationException(true);
	}

	void assertf(Args...)(bool cond, SourceLocation loc, string fmt, lazy Args args, string file = __MODULE__, int line = __LINE__)
	{
		if (cond) return;
		sink.putf("%s(%s): file(%s, %s): ICE: ", file, line, loc.line+1, loc.col+1);
		sink.putfln(fmt, args);
		hasErrors = true;
		handleICE;
		throw new CompilationException(true);
	}

	void unreachable(string file = __MODULE__, int line = __LINE__) {
		internal_error_impl("Unreachable", file, line);
	}

	void internal_error(Args...)(SourceLocation loc, string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		sink.putf("source(%s, %s): ", loc.line+1, loc.col+1);
		internal_error_impl(format, file, line, args);
	}

	void internal_error(Args...)(string format, Args args, string file = __MODULE__, int line = __LINE__)
	{
		internal_error_impl(format, file, line, args);
	}

	private void internal_error_impl(Args...)(string format, string file, int line, Args args)
	{
		sink.putf("ICE(%s:%s): ", file, line);
		sink.putfln(format, args);
		hasErrors = true;
		handleICE;
		throw new CompilationException(true);
	}

	private void handleICE() {
		final switch(iceBehavior)
		{
			case IceBehavior.error: assert(false);
			case IceBehavior.breakpoint:
				writeln(sink.text);
				stdout.flush;
				version(DMD) asm { db 0xCC; } // breakpoint
				version(LDC) assert(false); // LDC has no data in assembler
				break;
		}
	}

	void throwOnErrors() {
		if (hasErrors) throw new CompilationException();
	}
}

class CompilationException : Exception { this(bool isICE=false){ super(null); this.isICE = isICE; } bool isICE; }


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

	private dchar c; // current symbol

	private uint position;
	private uint line;
	private uint column;

	private uint startPos;
	private uint startLine;
	private uint startCol;

	private long numberRep;

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

	private Token new_tok(TokenType type) pure
	{
		uint tokSize = position - startPos;
		return Token(type, SourceLocation(startPos, startLine, startCol, tokSize));
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
					case EOI_CHAR: return new_tok(TT.INVALID);
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
		return new_tok(TT.IDENTIFIER);
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

unittest {
	{
		string[] keywords = ["bool","break","continue","do","else","f32","f64",
			"i16","i32","i64","i8","if","isize","return","struct","u16","u32","u64",
			"u8","usize","void","while"];
		TokenType[] tokens = [TT.TYPE_BOOL,TT.BREAK_SYM,TT.CONTINUE_SYM,TT.DO_SYM,
			TT.ELSE_SYM,TT.TYPE_F32,TT.TYPE_F64,TT.TYPE_I16,TT.TYPE_I32,TT.TYPE_I64,
			TT.TYPE_I8,TT.IF_SYM,TT.TYPE_ISIZE,TT.RETURN_SYM,TT.STRUCT_SYM,
			TT.TYPE_U16,TT.TYPE_U32,TT.TYPE_U64,TT.TYPE_U8,TT.TYPE_USIZE,
			TT.TYPE_VOID,TT.WHILE_SYM];
		Lexer lexer;
		foreach(i, keyword; keywords)
		{
			lexer = Lexer(keyword);
			Token token = lexer.nextToken;
			assert(token.type == tokens[i],
				format("For %s expected %s got %s", keyword, tokens[i], token.type));
			lexer = Lexer(keyword~"A");
			assert(lexer.nextToken.type == TT.IDENTIFIER);
		}
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
		Lexer lexer;
		foreach(i, op; ops)
		{
			lexer = Lexer(op);
			Token token = lexer.nextToken;
			assert(token.type == tokens_ops[i],
				format("For %s expected %s got %s", op, tokens_ops[i], token.type));
		}
	}

	void testNumeric(string input, TokenType tokType, long expectedValue)
	{
		Lexer lexer = Lexer(input);
		assert(lexer.nextToken.type == tokType);
		assert(lexer.numberRep == expectedValue);
	}

	assert(Lexer("_10").nextToken.type == TT.IDENTIFIER);
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
		Lexer lexer = Lexer(source);
		Token tok = lexer.nextToken;
		assert(tok.type == TT.COMMENT);
		assert(tok.loc.getTokenString(source) == "/*\n*/");
		tok = lexer.nextToken;
		assert(tok.type == TT.IDENTIFIER);
		assert(tok.loc.getTokenString(source) == "test");
	}
	{
		string source = "//test\nhello";
		Lexer lexer = Lexer(source);
		Token tok = lexer.nextToken;
		assert(tok.type == TT.COMMENT);
		assert(tok.loc.getTokenString(source) == "//test\n");
		tok = lexer.nextToken;
		assert(tok.type == TT.IDENTIFIER);
		assert(tok.loc.getTokenString(source) == "hello");
	}
}

alias Identifier = uint;

struct IdentifierMap {
	string[] strings;
	uint[string] map;

	Buffer!char tempBuf;

	string get(Identifier id) {
		return strings[id];
	}

	Identifier find(string str) {
		return map.get(str, uint.max);
	}

	Identifier getOrRegWithSuffix(string str, size_t suffix) {
		import std.format : formattedWrite;
		tempBuf.clear;
		tempBuf.put(str);
		formattedWrite(tempBuf, "%s%s", str, suffix);
		const(char)[] idString = tempBuf.data;
		return getOrReg(idString);
	}

	Identifier getOrReg(const(char)[] str) {
		uint id = map.get(cast(string)str, uint.max);
		if (id == uint.max) {
			string duppedKey = str.idup;
			id = cast(uint)strings.length;
			map[duppedKey] = id;
			strings ~= duppedKey;
		}
		return id;
	}

	Identifier getOrRegNoDup(string str) {
		uint id = map.get(str, uint.max);
		if (id == uint.max) {
			id = cast(uint)strings.length;
			map[str] = id;
			strings ~= str;
		}
		return id;
	}
}

// The order is the same as in TokenType enum
enum BasicType : ubyte {
	t_error,
	t_void,
	t_bool,

	t_i8,
	t_i16,
	t_i32,
	t_i64,
	//t_isize,

	t_u8,
	t_u16,
	t_u32,
	t_u64,
	//t_usize,

	t_f32,
	t_f64,
}

bool isTypeImplemented(BasicType t) {
	final switch(t) {
		case BasicType.t_error: return false;
		case BasicType.t_void: return false;
		case BasicType.t_bool: return false;
		case BasicType.t_i8: return false;
		case BasicType.t_i16: return false;
		case BasicType.t_i32: return false;
		case BasicType.t_i64: return false;
		case BasicType.t_u8: return false;
		case BasicType.t_u16: return false;
		case BasicType.t_u32: return false;
		case BasicType.t_u64: return false;
		case BasicType.t_f32: return false;
		case BasicType.t_f64: return false;
	}
}

// usage isAutoConvertibleFromToBasic[from][to]
immutable bool[13][13] isAutoConvertibleFromToBasic = [
	//err  void bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64  // to
	[   0,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0], // from error
	[   0,   0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0], // from void
	[   0,   0,  0,  1,  1,  1,  1,  1,  1,  1,  1, 1,  1], // from bool
	[   0,   0,  1,  0,  1,  1,  1,  0,  0,  0,  0, 1,  1], // from i8
	[   0,   0,  1,  0,  0,  1,  1,  0,  0,  0,  0, 1,  1], // from i16
	[   0,   0,  1,  0,  0,  0,  1,  0,  0,  0,  0, 1,  1], // from i32
	[   0,   0,  1,  0,  0,  0,  0,  0,  0,  0,  0, 1,  1], // from i64
	[   0,   0,  1,  0,  1,  1,  1,  0,  1,  1,  1, 1,  1], // from u8
	[   0,   0,  1,  0,  0,  1,  1,  0,  0,  1,  1, 1,  1], // from u16
	[   0,   0,  1,  0,  0,  0,  1,  0,  0,  0,  1, 1,  1], // from u32
	[   0,   0,  1,  0,  0,  0,  0,  0,  0,  0,  0, 1,  1], // from u64
	[   0,   0,  1,  0,  0,  0,  0,  0,  0,  0,  0, 0,  1], // from f32
	[   0,   0,  1,  0,  0,  0,  0,  0,  0,  0,  0, 0,  0], // from f64
];

immutable BasicType[13][13] commonBasicType = (){ with(BasicType){ return [
	// error     void     bool       i8      i16      i32      i64       u8      u16      u32      u64      f32      f64
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error], // error
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error], // void
	[t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error, t_error], // bool
	[t_error, t_error, t_error, t_i8,    t_i16,   t_i32,   t_i64,   t_i8,    t_i16,   t_i32,   t_i64,   t_f32,   t_f64  ], // i8
	[t_error, t_error, t_error, t_i16,   t_i16,   t_i32,   t_i64,   t_i16,   t_i16,   t_i32,   t_i64,   t_f32,   t_f64  ], // i16
	[t_error, t_error, t_error, t_i32,   t_i32,   t_i32,   t_i64,   t_i32,   t_i32,   t_i32,   t_i64,   t_f32,   t_f64  ], // i32
	[t_error, t_error, t_error, t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_i64,   t_f32,   t_f64  ], // i64
	[t_error, t_error, t_error, t_i8,    t_i16,   t_i32,   t_i64,   t_u8,    t_u16,   t_u32,   t_u64,   t_f32,   t_f64  ], // u8
	[t_error, t_error, t_error, t_i16,   t_i16,   t_i32,   t_i64,   t_u16,   t_u16,   t_u32,   t_u64,   t_f32,   t_f64  ], // u16
	[t_error, t_error, t_error, t_i32,   t_i32,   t_i32,   t_i64,   t_u32,   t_u32,   t_u32,   t_u64,   t_f32,   t_f64  ], // u32
	[t_error, t_error, t_error, t_i64,   t_i64,   t_i64,   t_i64,   t_u64,   t_u64,   t_u64,   t_u64,   t_f32,   t_f64  ], // u64
	[t_error, t_error, t_error, t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f32,   t_f64  ], // f32
	[t_error, t_error, t_error, t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64,   t_f64  ], // f64
]; }
}();

string[13] basicTypeNames = ["error", "void", "bool", "i8", "i16", "i32",
"i64", "u8", "u16", "u32", "u64", "f32", "f64"];

bool isBasicTypeToken(TokenType tt) {
	return tt >= TYPE_TOKEN_FIRST && tt <= TYPE_TOKEN_LAST;
}

BasicType tokenTypeToBasicType(TokenType tt) {
	return cast(BasicType)(tt - TYPE_TOKEN_FIRST + BasicType.t_void);
}


//                              #      #####   #######
//                              #     #     #     #
//                             ###    #           #
//                             # #     #####      #
//                            #####         #     #
//                            #   #   #     #     #
//                           ##   ##   #####      #
// -----------------------------------------------------------------------------
enum AstType : ubyte {
	error,
	abstract_node,

	decl_module,
	decl_function,
	decl_var,
	decl_struct,

	stmt_block,
	stmt_if,
	stmt_while,
	stmt_do_while,
	stmt_return,
	stmt_break,
	stmt_continue,
	stmt_assign,

	expr_var,
	expr_literal,
	expr_bin_op,
	expr_call,
	expr_index,
	expr_type_conv,

	type_basic,
	type_ptr,
	type_static_array,
	type_user,
}

enum AstFlags {
	isDeclaration = 1 << 0,
	isScope       = 1 << 1,
	isExpression  = 1 << 2,
	isStatement   = 1 << 3,
	isType        = 1 << 4,
	isSymResolved = 1 << 5,
}

mixin template AstNodeData(AstType _astType = AstType.abstract_node, int default_flags = 0) {
	this(Args...)(SourceLocation loc, Args args) {
		this(loc);
		enum len = this.tupleof.length - 3;
		enum numDefault = len - args.length;
		static assert(args.length <= len, "Too many args");
		this.tupleof[3..$-numDefault] = args;
	}

	this(SourceLocation loc) {
		this.loc = loc; this.astType = _astType; this.flags = cast(ushort)default_flags; }
	/*this(SourceLocation loc, int flags) {
		this.loc = loc; this.astType = _astType; this.flags = cast(ushort)(default_flags|flags); }
	this(SourceLocation loc, AstType astType) {
		this.loc = loc; this.astType = astType; this.flags = cast(ushort)default_flags; }
	this(SourceLocation loc, AstType astType, int flags) {
		this.loc = loc; this.astType = astType; this.flags = cast(ushort)(default_flags|flags); }*/
	SourceLocation loc;
	AstType astType = _astType;
	ushort flags;

	bool isDeclaration() { return cast(bool)(flags & AstFlags.isDeclaration); }
	bool isScope() { return cast(bool)(flags & AstFlags.isScope); }
	bool isExpression() { return cast(bool)(flags & AstFlags.isExpression); }
	bool isStatement() { return cast(bool)(flags & AstFlags.isStatement); }
	bool isType() { return cast(bool)(flags & AstFlags.isType); }
	bool isSymResolved() { return cast(bool)(flags & AstFlags.isSymResolved); }
}

mixin template SymRefNodeData() {
	SymbolRef symRef;
	string strId(CompilationContext* context) { return context.idString(symRef.id(isSymResolved)); }
	Identifier id() { return symRef.id(isSymResolved); }
	void resolveSymbol(Symbol* symbol) {
		symRef._symbol = symbol;
		flags |= AstFlags.isSymResolved;
	}
	Symbol* getSym() { assert(isSymResolved, "Unresolved symbol"); return symRef._symbol; }
}

struct AstNode {
	mixin AstNodeData;
}

// ----------------------------------- Types -----------------------------------
// -----------------------------------------------------------------------------

mixin template TypeNodeData(AstType _astType) {
	mixin AstNodeData!(_astType, AstFlags.isType);
}

struct TypePrinter
{
	TypeNode* node;
	CompilationContext* ctx;

	void toString(scope void delegate(const(char)[]) sink) {
		node.toString(sink, ctx);
	}
}

struct TypeNode {
	mixin AstNodeData!(AstType.abstract_node, AstFlags.isType);

	BasicTypeNode* basicTypeNode() { return cast(BasicTypeNode*)&this; }
	PtrTypeNode* ptrTypeNode() { return cast(PtrTypeNode*)&this; }
	StaticArrayTypeNode* staticArrayTypeNode() { return cast(StaticArrayTypeNode*)&this; }
	UserTypeNode* userTypeNode() { return cast(UserTypeNode*)&this; }

	ulong alignment()
	{
		switch(astType)
		{
			case AstType.type_basic: return basicTypeNode.alignment;
			case AstType.type_ptr: return ptrTypeNode.alignment;
			case AstType.type_static_array: return staticArrayTypeNode.alignment;
			case AstType.type_user: return userTypeNode.alignment;
			default: assert(false, format("got %s", astType));
		}
	}

	ulong size()
	{
		switch(astType)
		{
			case AstType.type_basic: return basicTypeNode.size;
			case AstType.type_ptr: return ptrTypeNode.size;
			case AstType.type_static_array: return staticArrayTypeNode.size;
			case AstType.type_user: return userTypeNode.size;
			default: assert(false, format("got %s", astType));
		}
	}

	string typeName(CompilationContext* context) {
		if (&this == null) return null;
		assert(isType);
		switch(astType)
		{
			case AstType.type_basic:
				return basicTypeNode.strId;
			case AstType.type_ptr:
				return "ptr";
			case AstType.type_static_array: return "[num]";
			case AstType.type_user:
				return userTypeNode.strId(context);
			default: assert(false, format("got %s", astType));
		}
	}

	TypePrinter printer(CompilationContext* context) {
		return TypePrinter(&this, context);
	}

	bool sameType(TypeNode* t2) {
		assert(isType, format("this is %s, not type", astType));
		assert(t2.isType, format("t2 is %s, not type", t2.astType));
		if (astType != t2.astType) return false;

		switch(astType)
		{
			case AstType.type_basic:
				return basicTypeNode.basicType == t2.basicTypeNode.basicType;
			case AstType.type_ptr:
				return ptrTypeNode.base == t2.ptrTypeNode.base;
			case AstType.type_static_array:
				return staticArrayTypeNode.base == t2.staticArrayTypeNode.base &&
					staticArrayTypeNode.length == t2.staticArrayTypeNode.length;
			case AstType.type_user:
				return cast(void*)(&this) == cast(void*)(t2);
			default:
				assert(false, format("got %s %s", astType, t2.astType));
		}
	}

	bool isVoid() {
		return astType == AstType.type_basic &&
			basicTypeNode.basicType == BasicType.t_void;
	}
	bool isError() {
		return astType == AstType.type_basic &&
			basicTypeNode.basicType == BasicType.t_error;
	}

	void assertImplemented(SourceLocation loc, CompilationContext* context) {
		if (!isImplemented)
			context.error(loc, "Type is not implemented `%s`",
				typeName(context));
	}

	bool isImplemented() {
		switch (astType)
		{
			case AstType.type_basic:
			switch (basicTypeNode.basicType)
			{
				case BasicType.t_bool: return true;
				case BasicType.t_i32: return true;
				case BasicType.t_i64: return true;
				default: return false;
			}

			case AstType.type_ptr:
				return ptrTypeNode.base.isImplemented;

			default: return false;
		}
	}

	TypeNode* getElementType(CompilationContext* context) {
		switch(astType)
		{
			case AstType.type_ptr: return ptrTypeNode.base;
			case AstType.type_static_array: return staticArrayTypeNode.base;
			default: context.internal_error(loc, "%s is not indexable", astType); assert(false);
		}
	}

	IrValueType irType(CompilationContext* context) {
		switch (astType)
		{
			case AstType.type_basic:
			switch(basicTypeNode.basicType)
			{
				case BasicType.t_void: return IrValueType.i32;
				case BasicType.t_bool: return IrValueType.i32;
				case BasicType.t_i32: return IrValueType.i32;
				case BasicType.t_i64: return IrValueType.i64;
				default: break;
			}
			break;

			case AstType.type_ptr: return IrValueType.ptr;

			default: break;
		}
		context.internal_error(loc, "Cannot convert `%s` to IrValueType", astType);
		assert(false);
	}

	void toString(scope void delegate(const(char)[]) sink, CompilationContext* ctx) {
		switch(astType)
		{
			case AstType.type_basic:
				sink(basicTypeNames[basicTypeNode.basicType]);
				break;
			case AstType.type_ptr:
				ptrTypeNode.base.toString(sink, ctx);
				sink("*");
				break;
			case AstType.type_static_array:
				staticArrayTypeNode.base.toString(sink, ctx);
				formattedWrite(sink, "[%s]", staticArrayTypeNode.length);
				break;
			case AstType.type_user:
				sink(userTypeNode.strId(ctx));
				sink("*");
				break;
			default: assert(false, format("%s is not type", astType));
		}
	}
}

enum BasicTypeFlag : ubyte {
	isFloat    = 1 << 0,
	isInteger  = 1 << 1,
	isUnsigned = 1 << 2,
	isBoolean  = 1 << 3,
}

enum POINTER_SIZE = 8;
BasicTypeNode basicTypeNode(ulong size, BasicType basicType, int typeFlags = 0)
{
	return BasicTypeNode(SourceLocation(), size, basicType, cast(ubyte)typeFlags);
}

struct BasicTypeNode {
	mixin TypeNodeData!(AstType.type_basic);
	ulong size; // -1 arch dependent
	ulong alignment() { return size; }
	BasicType basicType;
	ubyte typeFlags;
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	string strId() { return basicTypeNames[basicType]; }

	bool isFloat() { return cast(bool)(typeFlags & BasicTypeFlag.isFloat); }
	bool isInteger() { return cast(bool)(typeFlags & BasicTypeFlag.isInteger); }
	bool isUnsigned() { return cast(bool)(typeFlags & BasicTypeFlag.isUnsigned); }
	bool isBoolean() { return cast(bool)(typeFlags & BasicTypeFlag.isBoolean); }
}

struct PtrTypeNode {
	mixin TypeNodeData!(AstType.type_ptr);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	ulong size() { return POINTER_SIZE; }
	ulong alignment() { return POINTER_SIZE; }
}

struct StaticArrayTypeNode {
	mixin TypeNodeData!(AstType.type_static_array);
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	TypeNode* base;
	ulong length;
	ulong size() { return base.size * length; }
	ulong alignment() { return base.alignment; }
}

struct UserTypeNode {
	mixin TypeNodeData!(AstType.type_user);
	mixin SymRefNodeData;
	TypeNode* typeNode() { return cast(TypeNode*)&this; }
	ulong size = 1; // TODO, set in semantic
	ulong alignment = 1; // TODO, set as max alignment of members
}

// ------------------------------- Declarations --------------------------------
// -----------------------------------------------------------------------------
mixin template ScopeDeclNodeData(AstType _astType, int default_flags = 0) {
	mixin AstNodeData!(_astType, default_flags | AstFlags.isScope | AstFlags.isDeclaration);
	/// Each node can be struct, function or variable
	AstNode*[] declarations;
}

struct ModuleDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_module);
	Scope* _scope;
	FunctionDeclNode*[] functions;
	IrModule irModule;

	void addFunction(FunctionDeclNode* func) {
		functions ~= func;
	}

	FunctionDeclNode* findFunction(string idStr, CompilationContext* ctx) {
		Identifier id = ctx.idMap.find(idStr);
		if (id == uint.max) return null;
		return findFunction(id);
	}
	FunctionDeclNode* findFunction(Identifier id) {
		Symbol* sym = _scope.symbols.get(id, null);
		if (sym.symClass != SymbolClass.c_function) return null;
		return sym.funcDecl;
	}
}

struct StructDeclNode {
	mixin ScopeDeclNodeData!(AstType.decl_struct);
	mixin SymRefNodeData;
	Scope* _scope;
}

struct FunctionDeclNode {
	mixin AstNodeData!(AstType.decl_function, AstFlags.isDeclaration);
	mixin SymRefNodeData;
	TypeNode* returnType;
	VariableDeclNode*[] parameters;
	BlockStmtNode* block_stmt; // null if external
	Scope* _scope;
	IrFunction* irData;
	/// Position in buffer or in memory
	void* funcPtr;
}

enum VariableFlags : ubyte {
	isParameter        = 1 << 1,
	forceMemoryStorage = 1 << 0,
}

struct VariableDeclNode {
	mixin AstNodeData!(AstType.decl_var, AstFlags.isDeclaration);
	mixin SymRefNodeData;
	TypeNode* type;
	ubyte varFlags;
	ushort paramIndex; // 0 for non-params
	IrRef irRef;
	IrVar irVar; // unique id of variable within a function
	IrRef stackSlotId;
	bool isParameter() { return cast(bool)(varFlags & VariableFlags.isParameter); }
	bool forceMemoryStorage() { return cast(bool)(varFlags & VariableFlags.forceMemoryStorage); }
}


// -------------------------------- Statements ---------------------------------
// -----------------------------------------------------------------------------
struct IfStmtNode {
	mixin AstNodeData!(AstType.stmt_if, AstFlags.isStatement);
	ExpressionNode* condition;
	AstNode* thenStatement;
	AstNode* elseStatement; // Nullable
	Scope* then_scope;
	Scope* else_scope;
}

struct WhileStmtNode {
	mixin AstNodeData!(AstType.stmt_while, AstFlags.isStatement);
	ExpressionNode* condition;
	AstNode* statement;
	Scope* _scope;
}

struct DoWhileStmtNode {
	mixin AstNodeData!(AstType.stmt_do_while, AstFlags.isStatement);
	ExpressionNode* condition;
	AstNode* statement;
	Scope* _scope;
}

struct ReturnStmtNode {
	mixin AstNodeData!(AstType.stmt_return, AstFlags.isStatement);
	ExpressionNode* expression; // Nullable
}

struct BreakStmtNode {
	mixin AstNodeData!(AstType.stmt_break, AstFlags.isStatement);
}

struct ContinueStmtNode {
	mixin AstNodeData!(AstType.stmt_continue, AstFlags.isStatement);
}

struct BlockStmtNode {
	mixin AstNodeData!(AstType.stmt_block, AstFlags.isStatement);
	/// Each node can be expression, declaration or expression
	AstNode*[] statements;
	Scope* _scope;
}

enum AssignOp : ubyte {
	opAssign,
	opIndexAssign
}

struct AssignStmtNode {
	mixin AstNodeData!(AstType.stmt_assign, AstFlags.isStatement);
	AssignOp op;
	ExpressionNode* left;
	ExpressionNode* right;
}

// ------------------------------- Expressions ---------------------------------
// -----------------------------------------------------------------------------
mixin template ExpressionNodeData(AstType _astType) {
	mixin AstNodeData!(_astType, AstFlags.isExpression);
	TypeNode* type;
	IrRef irRef;
}

// Abstract node, must not be instantiated
struct ExpressionNode {
	mixin ExpressionNodeData!(AstType.abstract_node);
}

struct VariableExprNode {
	mixin ExpressionNodeData!(AstType.expr_var);
	mixin SymRefNodeData;
}

struct LiteralExprNode {
	mixin ExpressionNodeData!(AstType.expr_literal);
	long value;
}

enum BinOp : ubyte {
	// logic ops
	//AND_AND,
	//OR_OR,

	EQUAL_EQUAL,
	NOT_EQUAL,
	GREATER,
	GREATER_EQUAL,
	LESS,
	LESS_EQUAL,

	// arithmetic ops
	//AND,
	//ASHR,
	MINUS,
	//OR,
	//PERCENT,
	PLUS,
	//SHL,
	//SHR,
	SLASH,
	STAR,
	//XOR,
/*
	// arithmetic opEquals
	AND_EQUAL,
	ASHR_EQUAL,
	MINUS_EQUAL,
	OR_EQUAL,
	PERCENT_EQUAL,
	PLUS_EQUAL,
	SHL_EQUAL,
	SHR_EQUAL,
	SLASH_EQUAL,
	STAR_EQUAL,
	XOR_EQUAL,*/
}

enum BinOp BIN_OP_LOGIC_FIRST = BinOp.EQUAL_EQUAL;
enum BinOp BIN_OP_LOGIC_LAST = BinOp.LESS_EQUAL;
enum BinOp BIN_OP_ARITH_FIRST = BinOp.MINUS;
enum BinOp BIN_OP_ARITH_LAST = BinOp.STAR;
//enum BinOp BIN_OP_ARITH_EQUALS_FIRST = BinOp.AND_EQUAL;
//enum BinOp BIN_OP_ARITH_EQUALS_LAST = BinOp.XOR_EQUAL;

struct BinaryExprNode {
	mixin ExpressionNodeData!(AstType.expr_bin_op);
	BinOp op;
	ExpressionNode* left;
	ExpressionNode* right;
}

struct TypeConvExprNode {
	mixin ExpressionNodeData!(AstType.expr_type_conv);
	ExpressionNode* expr;
}

struct CallExprNode {
	mixin ExpressionNodeData!(AstType.expr_call);
	mixin SymRefNodeData; /// Callee
	ExpressionNode*[] args;
}

struct IndexExprNode {
	mixin ExpressionNodeData!(AstType.expr_index);
	ExpressionNode* array;
	ExpressionNode* index;
}


//         ##   ##   #####    #####    #####   #######    ###    ######
//          #   #      #     #     #     #        #      #   #   #     #
//          #   #      #     #           #        #     #     #  #     #
//           # #       #      #####      #        #     #     #  ######
//           # #       #           #     #        #     #     #  #   #
//            #        #     #     #     #        #      #   #   #    #
//            #      #####    #####    #####      #       ###    #     #
// -----------------------------------------------------------------------------
enum VisitOrder { pre, post }
mixin template AstVisitorMixin() {
	void _visit(TypeNode* n) { _visit(cast(AstNode*)n); }
	void _visit(ExpressionNode* n) { _visit(cast(AstNode*)n); }
	void _visit(AstNode* n)
	{
		final switch(n.astType) with(AstType)
		{
			case error: context.internal_error(n.loc, "Visiting error node"); break;
			case abstract_node: context.internal_error(n.loc, "Visiting abstract node"); break;
			case decl_module: auto m = cast(ModuleDeclNode*)n; visit(m); break;
			case decl_function: auto f = cast(FunctionDeclNode*)n; visit(f); break;
			case decl_var: auto v = cast(VariableDeclNode*)n; visit(v); break;
			case decl_struct: auto s = cast(StructDeclNode*)n; visit(s); break;
			case stmt_block: auto b = cast(BlockStmtNode*)n; visit(b); break;
			case stmt_if: auto i = cast(IfStmtNode*)n; visit(i); break;
			case stmt_while: auto w = cast(WhileStmtNode*)n; visit(w); break;
			case stmt_do_while: auto d = cast(DoWhileStmtNode*)n; visit(d); break;
			case stmt_return: auto r = cast(ReturnStmtNode*)n; visit(r); break;
			case stmt_break: auto b = cast(BreakStmtNode*)n; visit(b); break;
			case stmt_continue: auto c = cast(ContinueStmtNode*)n; visit(c); break;
			case stmt_assign: auto a = cast(AssignStmtNode*)n; visit(a); break;
			case expr_var: auto v = cast(VariableExprNode*)n; visit(v); break;
			case expr_literal: auto l = cast(LiteralExprNode*)n; visit(l); break;
			case expr_bin_op: auto b = cast(BinaryExprNode*)n; visit(b); break;
			case expr_call: auto c = cast(CallExprNode*)n; visit(c); break;
			case expr_index: auto i = cast(IndexExprNode*)n; visit(i); break;
			case expr_type_conv: auto t = cast(TypeConvExprNode*)n; visit(t); break;
			case type_basic: auto t = cast(BasicTypeNode*)n; visit(t); break;
			case type_ptr: auto t = cast(PtrTypeNode*)n; visit(t); break;
			case type_static_array: auto t = cast(StaticArrayTypeNode*)n; visit(t); break;
			case type_user: auto t = cast(UserTypeNode*)n; visit(t); break;
		}
	}
}

/*	// Visitor code
	void visit(ModuleDeclNode* m) {
		foreach (decl; m.declarations) _visit(decl); }
	void visit(FunctionDeclNode* f) {
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt) visit(f.block_stmt); }
	void visit(VariableDeclNode* v) {}
	void visit(StructDeclNode* s) {
		foreach (decl; s.declarations) _visit(decl); }
	void visit(BlockStmtNode* b) {
		foreach (stmt; b.statements) _visit(stmt); }
	void visit(IfStmtNode* i) {
		_visit(cast(AstNode*)i.condition);
		_visit(cast(AstNode*)i.thenStatement);
		if (i.elseStatement) _visit(i.elseStatement); }
	void visit(WhileStmtNode* w) {
		_visit(cast(AstNode*)w.condition);
		_visit(cast(AstNode*)w.statement); }
	void visit(DoWhileStmtNode* d) {
		_visit(cast(AstNode*)d.condition);
		_visit(cast(AstNode*)d.statement); }
	void visit(ReturnStmtNode* r) {
		if (r.expression) _visit(r.expression); }
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(AssignStmtNode* a) {
		_visit(a.left); _visit(a.right); }
	void visit(VariableExprNode* v) {}
	void visit(LiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {
		_visit(cast(AstNode*)b.left);
		_visit(cast(AstNode*)b.right); }
	void visit(CallExprNode* c) {
		foreach (arg; c.args) _visit(arg); }
	void visit(IndexExprNode* i) {
		_visit(i.array); _visit(i.index); }
	void visit(TypeConvExprNode* t) {
		_visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(UserTypeNode* t) {}
*/

struct AstPrinter {
	mixin AstVisitorMixin;

	CompilationContext* context;
	int indentSize = 1;

	private int indent;

	void print(Args...)(Args args) {
		auto i = ' '.repeat(indent);
		writeln(i, args);
	}

	void pr_node(AstNode* node) { // print node
		indent += indentSize; _visit(node); indent -= indentSize;
	}

	void visit(ModuleDeclNode* m) {
		print("MODULE");
		foreach (decl; m.declarations) pr_node(decl);
	}
	void visit(FunctionDeclNode* f) {
		print("FUNC ", f.returnType.printer(context), " ", f.strId(context));
		foreach (param; f.parameters) pr_node(cast(AstNode*)param);
		if (f.block_stmt) pr_node(cast(AstNode*)f.block_stmt);
	}
	void visit(VariableDeclNode* v) {
		print(v.isParameter ? "PARAM " : "VAR ", v.type.printer(context), " ", v.strId(context));
	}
	void visit(StructDeclNode* s) {
		print("STRUCT ", s.strId(context));
		foreach (decl; s.declarations) pr_node(decl); }
	void visit(BlockStmtNode* b) {
		print("BLOCK");
		foreach(stmt; b.statements) pr_node(stmt); }
	void visit(IfStmtNode* i) {
		print("IF"); pr_node(cast(AstNode*)i.condition);
		print("THEN"); pr_node(i.thenStatement);
		if (i.elseStatement) { print("ELSE"); pr_node(i.elseStatement); }
	}
	void visit(WhileStmtNode* w) {
		print("WHILE");
		pr_node(cast(AstNode*)w.condition);
		pr_node(cast(AstNode*)w.statement); }
	void visit(DoWhileStmtNode* d) {
		print("DO");
		pr_node(cast(AstNode*)d.condition);
		print("WHILE");
		pr_node(cast(AstNode*)d.statement); }
	void visit(ReturnStmtNode* r) {
		print("RETURN");
		if (r.expression) pr_node(cast(AstNode*)r.expression); }
	void visit(BreakStmtNode* r) { print("BREAK"); }
	void visit(ContinueStmtNode* r) { print("CONTINUE"); }
	void visit(AssignStmtNode* a) { print("ASSIGN"); pr_node(cast(AstNode*)a.left); pr_node(cast(AstNode*)a.right); }
	void visit(VariableExprNode* v) {
		if (v.isSymResolved)
			print("VAR_USE ", v.getSym.getType.printer(context), " ", v.strId(context));
		else
			print("VAR_USE ", v.strId(context));
	}
	void visit(LiteralExprNode* c) { print("LITERAL ", c.type.printer(context), " ", c.value); }
	void visit(BinaryExprNode* b) {
		if (b.type) print("BINOP ", b.type.printer(context), " ", b.op);
		else print("BINOP ", b.op);
		pr_node(cast(AstNode*)b.left);
		pr_node(cast(AstNode*)b.right); }
	void visit(CallExprNode* c) {
		if (c.isSymResolved)
			print("CALL ", c.strId(context), " ", c.getSym.getType.printer(context));
		else print("CALL ", c.strId(context));
		foreach (arg; c.args) pr_node(cast(AstNode*)arg); }
	void visit(IndexExprNode* i) {
		print("INDEX"); pr_node(cast(AstNode*)i.array); pr_node(cast(AstNode*)i.index); }
	void visit(TypeConvExprNode* t) {
		print("CAST to ", t.type.printer(context));
		pr_node(cast(AstNode*)t.expr); }
	void visit(BasicTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(PtrTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(StaticArrayTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }
	void visit(UserTypeNode* t) { print("TYPE ", t.typeNode.printer(context)); }

	void printAst(AstNode* n)
	{
		indent = -indentSize;
		if (!n) return;
		pr_node(n);
	}
}


//               ######      #     ######    #####   #######
//               #     #     #     #     #  #     #  #
//               #     #    ###    #     #  #        #
//               ######     # #    ######    #####   #####
//               #         #####   #   #          #  #
//               #         #   #   #    #   #     #  #
//               #        ##   ##  #     #   #####   #######
// -----------------------------------------------------------------------------

void pass_parser(ref CompilationContext ctx) {
	Lexer lexer = Lexer(ctx.input);
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
	ExpressionNode* makeExpr(T, Args...)(SourceLocation start, Args args) { return cast(ExpressionNode*)new T(start, null, IrRef(), args); }

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

	void expectAndConsume(TokenType type) {
		if (tok.type != type) {
			string tokenString = lexer.getTokenString(tok);
			context.unrecoverable_error(tok.loc, "Expected `%s` token, while got `%s` token '%s'",
				type, tok.type, tokenString);
		}
		nextToken();
	}

	Identifier expectIdentifier()
	{
		auto id = tokenToId();
		expectAndConsume(TokenType.IDENTIFIER);
		return id;
	}

	Identifier tokenToId()
	{
		string name = lexer.getTokenString(tok);
		return context.idMap.getOrRegNoDup(name);
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
			nextToken();
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

			if (tok.type == TokenType.SEMICOLON) // <var_declaration> ::= <type> <id> ";"
			{
				version(print_parse) auto s3 = scop("<var_declaration> %s", start);
				nextToken();
				return cast(AstNode*)make!VariableDeclNode(start, SymbolRef(declarationId), type);
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
					if (tok.type == TokenType.COMMA) nextToken();
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
				if (tok.type == TokenType.STAR) {
					nextToken();
					base = cast(TypeNode*)make!PtrTypeNode(start, base);
				} else if (tok.type == TokenType.LBRACKET) {
					nextToken();
					ExpressionNode* e = expr();
					if (e.astType != AstType.expr_literal)
						context.unrecoverable_error(e.loc, "Expected constant, while got '%s'",
							lexer.getTokenString(e.loc));
					expectAndConsume(TokenType.RBRACKET);
					ulong length = (cast(LiteralExprNode*)e).value;
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

	ExpressionNode* term() /* <term> ::= <id> | <id> "(" <expr_list> ")" | <id> "[" <expr> "]" | <int> | <paren_expr> */
	{
		version(print_parse) auto s1 = scop("term %s", tok.loc);
		SourceLocation start = tok.loc;
		if (tok.type == TokenType.IDENTIFIER)
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
		else if (tok.type == TokenType.INT_LITERAL)
		{
			long value = lexer.getTokenNumber();
			nextToken();
			TypeNode* type = context.basicTypeNodes(BasicType.t_i32);
			return cast(ExpressionNode*)make!LiteralExprNode(start, type, IrRef(), value);
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


//     #####   #######  #     #     #     #     #  #######   #####     ####
//    #     #  #        ##   ##     #     ##    #     #        #      #    #
//    #        #        # # # #    ###    # #   #     #        #     #
//     #####   #####    #  #  #    # #    #  #  #     #        #     #
//          #  #        #     #   #####   #   # #     #        #     #
//    #     #  #        #     #   #   #   #    ##     #        #      #    #
//     #####   #######  #     #  ##   ##  #     #     #      #####     ####
// -----------------------------------------------------------------------------

/// Algorithm of name to Symbol resolution
///

enum SymbolClass : ubyte {
	c_function,
	c_variable,
	c_struct
}

enum SymbolFlags : ubyte {
	isInOrderedScope = 1 << 0,
}

struct SymbolRef
{
	this(Identifier identifier)
	{
		this._id = identifier;
	}

	union
	{
		Symbol* _symbol; // used when resolved, Symbol contains Identifier internally
		Identifier _id; // used when not yet resolved
	}
	/// Resolved is stored in isSymResolved flag of AstNode
	Identifier id(bool resolved) { return resolved ? _symbol.id : _id; }
}

struct Symbol {
	Identifier id;
	SourceLocation loc;
	SymbolClass symClass;
	ubyte flags;
	AstNode* node;
	/// Symbol in outer scope with the same id. Can be null
	Symbol* outerSymbol;

	bool isInOrderedScope() { return cast(bool)(flags & SymbolFlags.isInOrderedScope); }

	VariableDeclNode* varDecl() {
		assert(node.astType == AstType.decl_var, format("varDecl used on %s", node.astType));
		return cast(VariableDeclNode*)node;
	}

	FunctionDeclNode* funcDecl() {
		assert(node.astType == AstType.decl_function, format("funcDecl used on %s", node.astType));
		return cast(FunctionDeclNode*)node;
	}

	TypeNode* getType() {
		switch(node.astType) with(AstType)
		{
			case decl_function: return (cast(FunctionDeclNode*)node).returnType;
			case decl_var: return (cast(VariableDeclNode*)node).type;
			case expr_var, expr_literal, expr_bin_op, expr_call, expr_index, expr_type_conv:
				return (cast(ExpressionNode*)node).type;
			case type_basic: return cast(TypeNode*)node;
			case type_user: return cast(TypeNode*)node;
			default: assert(false, format("getType used on %s", node.astType));
		}
	}
}

struct Scope
{
	Symbol*[Identifier] symbols;
	Scope* parentScope;
	string debugName;
	bool isOrdered;
}

/// Used for semantic analysis
struct ScopeStack
{
	CompilationContext* context;
	// TODO: do not maintain all visible symbols for current scope
	// We will only use a small portion of visible symbols in each scope,
	// so maintaining this is most probably wasted effort, and
	// it is faster to walk up the scope stack. Need to benchmark.
	Symbol*[Identifier] symbols;
	Scope* currentScope;

	/// Used in 1 semantic pass
	Scope* pushScope(string name, Flag!"ordered" isOrdered)
	{
		//print("push scope %s", name); // debug
		//indent += indentSize;
		Scope* newScope = new Scope;
		newScope.isOrdered = isOrdered;
		newScope.debugName = name;
		if (currentScope)
			newScope.parentScope = currentScope;
		return currentScope = newScope;
	}

	/// Used in 2 semantic pass
	void pushCompleteScope(Scope* newScope)
	{
		//print("push scope %s", newScope.debugName); // debug
		//indent += indentSize;
		currentScope = newScope;
		foreach (id, sym; newScope.symbols)
		{
			if (auto outerSymbol = symbols.get(sym.id, null))
				sym.outerSymbol = outerSymbol;
			symbols[id] = sym;
		}
	}

	/// Used in 1 semantic pass
	void popScope1()
	{
		if (currentScope.parentScope) currentScope = currentScope.parentScope;
		else currentScope = null;
	}

	/// Used in 2 semantic pass
	void popScope2()
	{
		assert(currentScope);
		//indent -= indentSize; // debug
		//if (currentScope.debugName) print("pop scope %s", currentScope.debugName);
		//else print("pop scope");

		// Pop all symbols of the scope we are leaving from symbols
		foreach(id, sym; currentScope.symbols)
		{
			if (sym.outerSymbol) // replace by symbol from outer scope
				symbols[id] = sym.outerSymbol;
			else // or simply remove it if no such symbol
				symbols.remove(id);
		}

		if (currentScope.parentScope)
			currentScope = currentScope.parentScope;
		else
			currentScope = null;
	}

	/// Used in 2 semantic pass
	/// Look up symbol by Identifier. Searches the whole stack of scopes.
	Symbol* lookup(const Identifier id, SourceLocation from)
	{
		auto sym = symbols.get(id, null);
		while (sym)
		{
			// print("try lookup %s @ %s", context.idString(sym.id), sym.loc);
			// forward reference allowed for unordered scope
			if (!sym.isInOrderedScope) break;
			// not a forward reference
			else if (from.start > sym.loc.start) break;

			sym = sym.outerSymbol;
		}

		if (sym) {
			//print("lookup %s @ %s", context.idString(sym.id), sym.loc);
		}
		else
		{
			context.error(from, "undefined identifier `%s`", context.idString(id));
		}
		return sym;
	}

	/// Used in 1 semantic pass
	/// Constructs and inserts symbol with id
	Symbol* insert(Identifier id, SourceLocation loc, SymbolClass symClass, AstNode* node)
	{
		typeof(Symbol.flags) flags = currentScope.isOrdered ? SymbolFlags.isInOrderedScope : 0;
		auto sym = new Symbol(id, loc, symClass, flags, node);
		insert(sym);
		return sym;
	}

	/// Used in 1 semantic pass
	/// Inserts symbol `sym`
	void insert(Symbol* sym)
	{
		//print("insert %s @ %s", context.idString(sym.id), sym.loc);
		symbols[sym.id] = sym;
		if (auto s = currentScope.symbols.get(sym.id, null))
		{
			context.error(sym.loc,
				"declaration `%s` is already defined at %s", context.idString(sym.id), s.loc);
		}
		currentScope.symbols[sym.id] = sym;
	}

	int indentSize = 2;
	int indent;
	void print(Args...)(Args args) {
		write(' '.repeat(indent));
		writefln(args);
	}
}

void pass_semantic_decl(ref CompilationContext ctx) {
	ctx.scopeStack = ScopeStack(&ctx);
	auto sem1 = SemanticDeclarations(&ctx, &ctx.scopeStack);
	sem1.visit(ctx.mod);
}

/// Register
struct SemanticDeclarations {
	mixin AstVisitorMixin;

	CompilationContext* context;
	ScopeStack* scopeStack;

	void visit(ModuleDeclNode* m) {
		context.mod._scope = scopeStack.pushScope("Module", No.ordered);
		foreach (decl; context.mod.declarations) _visit(decl);
		scopeStack.popScope1;
	}
	void visit(FunctionDeclNode* f) {
		context.mod.addFunction(f);
		f.resolveSymbol = scopeStack.insert(f.id, f.loc, SymbolClass.c_function, cast(AstNode*)f);
		f._scope = scopeStack.pushScope(context.idString(f.id), Yes.ordered);
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt) visit(f.block_stmt);
		scopeStack.popScope1;
	}
	void visit(VariableDeclNode* v) {
		v.resolveSymbol = scopeStack.insert(v.id, v.loc, SymbolClass.c_variable, cast(AstNode*)v);
	}
	void visit(StructDeclNode* s) {
		s.resolveSymbol = scopeStack.insert(s.id, s.loc, SymbolClass.c_struct, cast(AstNode*)s);
		s._scope = scopeStack.pushScope(context.idString(s.id), No.ordered);
		foreach (decl; s.declarations) _visit(decl);
		scopeStack.popScope1;
	}
	void visit(BlockStmtNode* b) {
		b._scope = scopeStack.pushScope("Block", Yes.ordered);
		foreach(stmt; b.statements) _visit(stmt);
		scopeStack.popScope1;
	}
	void visit(IfStmtNode* i) {
		_visit(i.condition);
		i.then_scope = scopeStack.pushScope("Then", Yes.ordered);
		_visit(i.thenStatement);
		scopeStack.popScope1;
		if (i.elseStatement) {
			i.else_scope = scopeStack.pushScope("Else", Yes.ordered);
			_visit(i.elseStatement);
			scopeStack.popScope1;
		}
	}
	void visit(WhileStmtNode* w) {
		_visit(w.condition);
		w._scope = scopeStack.pushScope("While", Yes.ordered);
		_visit(w.statement);
		scopeStack.popScope1;
	}
	void visit(DoWhileStmtNode* d) {
		d._scope = scopeStack.pushScope("While", Yes.ordered);
		_visit(d.statement);
		scopeStack.popScope1;
		_visit(d.condition);
	}
	void visit(ReturnStmtNode* r) {}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(AssignStmtNode* a) {}
	void visit(VariableExprNode* v) {}
	void visit(LiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {}
	void visit(CallExprNode* c) {}
	void visit(IndexExprNode* i) {}
	void visit(TypeConvExprNode* c) {}
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(UserTypeNode* t) {}
}

void pass_semantic_lookup(ref CompilationContext ctx) {
	auto sem2 = SemanticLookup(&ctx, &ctx.scopeStack);
	sem2.visit(ctx.mod);
}

/// Resolves all symbol references (variable/type uses)
struct SemanticLookup {
	mixin AstVisitorMixin;

	CompilationContext* context;
	ScopeStack* scopeStack;

	void visit(ModuleDeclNode* m) {
		scopeStack.pushCompleteScope(m._scope);
		foreach (decl; m.declarations) _visit(decl);
		scopeStack.popScope2;
	}
	void visit(FunctionDeclNode* f) {
		scopeStack.pushCompleteScope(f._scope);
		_visit(f.returnType);
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt) visit(f.block_stmt);
		scopeStack.popScope2;
	}
	void visit(VariableDeclNode* v) { _visit(v.type); }
	void visit(StructDeclNode* s) {
		scopeStack.pushCompleteScope(s._scope);
		foreach (decl; s.declarations) _visit(decl);
		scopeStack.popScope2;
	}
	void visit(BlockStmtNode* b) {
		scopeStack.pushCompleteScope(b._scope);
		foreach(stmt; b.statements) _visit(stmt);
		scopeStack.popScope2;
	}
	void visit(IfStmtNode* i) {
		_visit(i.condition);
		scopeStack.pushCompleteScope(i.then_scope);
		_visit(i.thenStatement);
		scopeStack.popScope2;
		if (i.elseStatement) {
			scopeStack.pushCompleteScope(i.else_scope);
			_visit(i.elseStatement);
			scopeStack.popScope2;
		}
	}
	void visit(WhileStmtNode* w) {
		_visit(w.condition);
		scopeStack.pushCompleteScope(w._scope);
		_visit(w.statement);
		scopeStack.popScope2;
	}
	void visit(DoWhileStmtNode* d) {
		scopeStack.pushCompleteScope(d._scope);
		_visit(d.statement);
		scopeStack.popScope2;
		_visit(d.condition);
	}
	void visit(ReturnStmtNode* r) {
		if (r.expression) _visit(r.expression);
	}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(AssignStmtNode* a) { _visit(a.left); _visit(a.right); }
	void visit(VariableExprNode* v) { v.resolveSymbol = scopeStack.lookup(v.id, v.loc); }
	void visit(LiteralExprNode* c) {}
	void visit(BinaryExprNode* b) { _visit(b.left); _visit(b.right); }
	void visit(CallExprNode* c) {
		c.resolveSymbol = scopeStack.lookup(c.id, c.loc);
		foreach (arg; c.args) _visit(arg); }
	void visit(IndexExprNode* i) {
		_visit(i.array);
		_visit(i.index);
	}
	void visit(TypeConvExprNode* t) { _visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) { _visit(t.base); }
	void visit(StaticArrayTypeNode* t) { _visit(t.base); }
	void visit(UserTypeNode* t) { t.resolveSymbol = scopeStack.lookup(t.id, t.loc); }
}

void pass_semantic_type(ref CompilationContext ctx) {
	auto sem3 = SemanticStaticTypes(&ctx);
	sem3.visit(ctx.mod);
}

/// Annotates all expression nodes with their type
struct SemanticStaticTypes {
	mixin AstVisitorMixin;

	CompilationContext* context;
	FunctionDeclNode* curFunc;

	bool isBool(TypeNode* type)
	{
		return
			type.astType == AstType.type_basic &&
			type.basicTypeNode.basicType == BasicType.t_bool;
	}

	/// Returns true if types are equal or were converted to common type. False otherwise
	bool autoconvToCommonType(ref ExpressionNode* left, ref ExpressionNode* right)
	{
		if (left.type.astType == AstType.type_basic && right.type.astType == AstType.type_basic)
		{
			BasicTypeNode* leftType = left.type.basicTypeNode;
			BasicTypeNode* rightType = right.type.basicTypeNode;

			BasicType commonType = commonBasicType[leftType.basicType][rightType.basicType];
			bool successLeft = autoconvTo(left, commonType, Yes.force);
			bool successRight = autoconvTo(right, commonType, Yes.force);
			if(successLeft && successRight)
				return true;
		}
		else
		{
			// error for user-defined types

		}

		context.error(left.loc, "No common type between `%s` and `%s`",
			left.type.typeName(context),
			right.type.typeName(context));

		return false;
	}

	bool autoconvToBool(ref ExpressionNode* expr)
	{
		return autoconvTo(expr, BasicType.t_bool, No.force);
	}

	/// Returns true if conversion was successful. False otherwise
	bool autoconvTo(ref ExpressionNode* expr, BasicType toType, Flag!"force" force)
	{
		auto type = context.basicTypeNodes(toType);
		// Skip if already the same type
		if (expr.type is type) return true;

		if (expr.type.astType == AstType.type_basic)
		{
			BasicType fromType = expr.type.basicTypeNode.basicType;
			bool canConvert = isAutoConvertibleFromToBasic[fromType][toType];
			if (canConvert || force)
			{
				expr = cast(ExpressionNode*) new TypeConvExprNode(expr.loc, type, IrRef(), expr);
				return true;
			}
		}

		context.error(expr.loc, "Cannot auto-convert expression of type `%s` to `%s`",
			expr.type.typeName(context),
			basicTypeNames[toType]);
		return false;
	}

	bool autoconvTo(ref ExpressionNode* expr, TypeNode* type)
	{
		if (expr.type is type) return true;

		string extraError;

		if (expr.type.astType == AstType.type_basic && type.astType == AstType.type_basic)
		{
			BasicType fromType = expr.type.basicTypeNode.basicType;
			BasicType toType = type.basicTypeNode.basicType;
			bool canConvert = isAutoConvertibleFromToBasic[fromType][toType];
			if (canConvert)
			{
				expr = cast(ExpressionNode*) new TypeConvExprNode(expr.loc, type, IrRef(), expr);
				return true;
			}
		}
		else
		{
			extraError = ". Cannot convert from/to user-defined type";
		}

		context.error(expr.loc, "Cannot auto-convert expression of type `%s` to `%s`%s",
			expr.type.typeName(context),
			type.typeName(context),
			extraError);
		return false;
	}

	void setResultType(BinaryExprNode* b)
	{
		TypeNode* resRype = context.basicTypeNodes(BasicType.t_error);
		final switch(b.op) with(BinOp)
		{
			/*
			// logic ops. Requires both operands to be bool
			case AND_AND: goto case;
			case OR_OR:
				bool successLeft = autoconvToBool(b.left);
				bool successRight = autoconvToBool(b.right);
				if (successLeft && successRight)
				{
					resRype = context.basicTypeNodes(BasicType.t_bool);
				}
				else
				{
					if (!successLeft) context.error(b.left.loc, "Cannot implicitly convert `%s` of type `%s` to bool",
						b.left.type.typeName(context),
						b.right.type.typeName(context));
					if (!successRight) context.error(b.right.loc, "Cannot implicitly convert `%s` of type `%s` to bool",
						b.left.type.typeName(context),
						b.right.type.typeName(context));
				}
				break;
		*/
			// logic ops. Requires both operands to be of the same type
			case EQUAL_EQUAL, NOT_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL:
				if (b.left.type is b.right.type)
					resRype = context.basicTypeNodes(BasicType.t_bool);
				else
					context.error(b.left.loc, "Cannot compare `%s` and `%s`",
						b.left.type.typeName(context),
						b.right.type.typeName(context));
				break;

			// arithmetic op int float
			case MINUS, PLUS, SLASH, STAR:
				if (autoconvToCommonType(b.left, b.right))
					resRype = b.left.type;
				else
				{
					context.error(b.left.loc, "Cannot perform `%s` %s `%s` operation",
						b.left.type.typeName(context), b.op,
						b.right.type.typeName(context));
				}
				break;
		/*
			// arithmetic op int
			case AND: goto case;
			case ASHR: goto case;
			case OR: goto case;
			case PERCENT: goto case;
			case SHL: goto case;
			case SHR: goto case;
			case XOR:
				resRype = context.basicTypeNodes(BasicType.t_i32);
				break;

			// arithmetic opEqual
			case AND_EQUAL: goto case;
			case ASHR_EQUAL: goto case;
			case MINUS_EQUAL: goto case;
			case OR_EQUAL: goto case;
			case PERCENT_EQUAL: goto case;
			case PLUS_EQUAL: goto case;
			case SHL_EQUAL: goto case;
			case SHR_EQUAL: goto case;
			case SLASH_EQUAL: goto case;
			case STAR_EQUAL: goto case;
			case XOR_EQUAL:
				resRype = context.basicTypeNodes(BasicType.t_i32);
				break;*/
		}
		b.type = resRype;
		b.type.assertImplemented(b.loc, context);
	}

	void calcType(BinaryExprNode* b)
	{
		assert(b.left.type, format("left(%s).type: is null", b.left.astType));
		assert(b.right.type, format("right(%s).type: is null", b.right.astType));

		setResultType(b);
	}

	void checkBodyForReturnType(FunctionDeclNode* f) {
		if (f.returnType.isVoid) return; // void functions don't need return at the end

		if (f.block_stmt.statements.length > 0)
		{
			AstNode* lastStmt = f.block_stmt.statements[$-1];
			if (lastStmt.astType == AstType.stmt_return)
				return; // return type is already checked
		}

		context.error(f.loc,
			"function `%s` has no return statement, but is expected to return a value of type %s",
			context.idString(f.id), f.returnType.typeName(context));
	}

	void visit(ModuleDeclNode* m) {
		foreach (decl; m.declarations) _visit(decl);
	}
	void visit(FunctionDeclNode* f) {
		auto prevFunc = curFunc;
		curFunc = f;
		foreach (param; f.parameters) visit(param);
		if (f.block_stmt)
		{
			visit(f.block_stmt);
			checkBodyForReturnType(f);
		}
		curFunc = prevFunc;
	}
	void visit(VariableDeclNode* v) {
		_visit(v.type);
		switch (v.astType)
		{
			case AstType.type_static_array:
				v.varFlags |= VariableFlags.forceMemoryStorage;
				break;

			default: break;
		}
	}
	void visit(StructDeclNode* s) {
		foreach (decl; s.declarations) _visit(decl);
	}
	void visit(BlockStmtNode* b) {
		foreach(stmt; b.statements) _visit(stmt);
	}
	void visit(IfStmtNode* i) {
		_visit(i.condition);
		autoconvToBool(i.condition);
		_visit(i.thenStatement);
		if (i.elseStatement) {
			_visit(i.elseStatement);
		}
	}
	void visit(WhileStmtNode* w) {
		_visit(w.condition);
		autoconvToBool(w.condition);
		_visit(w.statement);
	}
	void visit(DoWhileStmtNode* d) {
		_visit(d.statement);
		_visit(d.condition);
		autoconvToBool(d.condition);
	}
	// Check return type and function return type
	void visit(ReturnStmtNode* r) {
		if (!curFunc)
		{
			context.error(r.loc,
				"Return statement is not inside function");
			return;
		}

		if (r.expression)
		{
			_visit(r.expression);
			if (curFunc.returnType.isVoid)
			{
				context.error(r.expression.loc,
					"Cannot return expression of type `%s` from void function",
					r.expression.type.typeName(context));
			}
			else
			{
				autoconvTo(r.expression, curFunc.returnType);
			}
		}
		else
		{
			if (!curFunc.returnType.isVoid)
				context.error(r.loc,
					"Cannot return void from non-void function",
					r.expression.type.typeName(context));
		}
	}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(AssignStmtNode* a) {
		_visit(a.left);
		_visit(a.right);
		assert(a.left.type, format("left(%s).type: is null", a.left.astType));
		assert(a.right.type, format("right(%s).type: is null", a.right.astType));

		if (a.left.astType == AstType.expr_var || a.left.astType == AstType.expr_index)
		{
			autoconvTo(a.right, a.left.type);
		}
		else
			context.error(a.left.loc, "Cannot perform assignment into %s", a.left.astType);
	}

	// Get type from variable declaration
	void visit(VariableExprNode* v) {
		v.type = v.getSym.getType;
		v.type.assertImplemented(v.loc, context);
	}
	void visit(LiteralExprNode* c) {
		//v.type =
	}
	void visit(BinaryExprNode* b) {
		_visit(b.left);
		_visit(b.right);
		calcType(b);
		b.type.assertImplemented(b.loc, context);
	}
	// Get type from function declaration
	void visit(CallExprNode* c) {
		auto params = c.getSym.funcDecl.parameters;
		auto numParams = params.length;
		auto numArgs = c.args.length;

		if (numArgs < numParams)
			context.error(c.loc, "Insufficient parameters to '%s', got %s, expected %s",
				c.strId(context), numArgs, numParams);
		else if (numArgs > numParams)
			context.error(c.loc, "Too much parameters to '%s', got %s, expected %s",
				c.strId(context), numArgs, numParams);

		foreach (i, ExpressionNode* arg; c.args)
		{
			_visit(arg);
			if (arg.type != params[i].type)
				context.error(arg.loc,
					"Parameter %s, must have type %s, not %s", i+1,
						params[i].type.printer(context),
						arg.type.printer(context));
		}
		c.type = c.getSym.getType;
	}
	void visit(IndexExprNode* i) {
		_visit(i.array);
		_visit(i.index);
		autoconvTo(i.index, BasicType.t_i64, No.force);
		i.type = i.array.type.getElementType(context);
	}
	void visit(TypeConvExprNode* t) {
		_visit(t.expr);
		t.type.assertImplemented(t.loc, context);
	}
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(UserTypeNode* t) {}
}

//                  #####   ######     ####   #######  #     #
//                    #     #     #   #    #  #        ##    #
//                    #     #     #  #        #        # #   #
//                    #     ######   #   ###  #####    #  #  #
//                    #     #   #    #     #  #        #   # #
//                    #     #    #    #    #  #        #    ##
//                  #####   #     #    #####  #######  #     #
// -----------------------------------------------------------------------------

/// Specifies where the expression is located relative to assign symbol.
/// This is set by assign expression. For ex, before visiting expression on the left it is set to leftSide
enum CurrentAssignSide
{
	rightSide,
	leftSide
}

void pass_ir_gen(ref CompilationContext ctx) {
	auto bin_irgen = BinIrGenerationVisitor(&ctx);
	bin_irgen.visit(ctx.mod);
	ctx.assertf(bin_irgen.irModule !is null, "Module IR is null");
}

/// Converts AST to in-memory linear IR
struct BinIrGenerationVisitor {
	mixin AstVisitorMixin;
	CompilationContext* context;

	IrModule* irModule;
	IrBuilder builder;
	IrFunction* ir;
	CurrentAssignSide currentAssignSide;

	// here are blocks that need their exit to be set to jump to outer block
	// for example body of then and else statements need to jump after the else body.
	// When generating IR inside if-else stmt we don't know where to jump.
	// Creating new basic block after if-else stmt can lead to redundant basic block,
	// that will in turn jump to outer block. Instead we save those block here, and
	// outer blocks are responsible for setting correct jumps (or returns).
	//Buffer!(IrBasicBlock*) jumpToOuterBlockList;

	Identifier tempId;
	Identifier startId;
	Identifier thenId;
	Identifier elseId;
	Identifier blkId;

	int varSuffixCounter;
	int bbCounter;
	int thenCounter;
	int elseCounter;
	int uniqueSuffix() { return ++varSuffixCounter; }

	void visit(ModuleDeclNode* m)
	{
		irModule = &m.irModule;
		tempId = context.idMap.getOrRegNoDup("__tmp");
		startId = context.idMap.getOrRegNoDup("start");
		thenId = context.idMap.getOrRegNoDup("then");
		elseId = context.idMap.getOrRegNoDup("else");
		blkId = context.idMap.getOrRegNoDup("blk");
		foreach (decl; m.functions) _visit(cast(AstNode*)decl);
	}
	void visit(FunctionDeclNode* f)
	{
		if (f.block_stmt is null) // external function
		{
			ExternalSymbol* sym = f.id in context.externalSymbols;
			if (sym is null)
			{
				context.error(f.loc,
					"Unresolved external function %s", f.strId(context));
				return;
			}
			f.funcPtr = sym.ptr;
			// TODO: check that parameters match
			return;
		}

		// create new function
		f.irData = new IrFunction(f, &win64_call_conv, f.returnType.irType(context));

		// TODO: use list of all functions that can be processed linearly, without recursion
		builder.begin(f.irData);
		ir = builder.ir;

		irModule.addFunction(f.irData);

		// create Basic Block for function body
		// other code will use `last` Basic Block
		builder.addBasicBlock(IrName(startId));
		builder.incBlockRefcount(builder.currentBB);
		builder.sealBlock(builder.currentBB);

		foreach (i, param; f.parameters)
		{
			visit(param);
		}

		visit(f.block_stmt);

		if (!builder.last.isFinished)
		{
			// Return must present in case of non-void function
			builder.last.exit = IrJump(IrJump.Type.ret0);
		}
		builder.sealBlock(builder.currentBB);
		builder.finishBlock();
		addUsers();
	}

	// values can be used from exit instruction of basic block. Add them here
	void addUsers()
	{
		foreach (ref IrBasicBlock block; ir.basicBlocks.range)
		{
			switch(block.exit.type) with(IrJump.Type) {
				case ret1, branch: builder.addUser(block.lastInstrRef, block.exit.value); break;
				default: break;
			}
		}
	}

	void store(VariableDeclNode* v, IrRef value)
	{
		if (v.forceMemoryStorage)
		{
			builder.emitInstr2(IrOpcode.o_store, v.type.irType(context), v.stackSlotId, value);
		}
		else
		{
			builder.writeVariable(v.irVar, value);
		}
	}

	IrRef load(VariableDeclNode* v)
	{
		if (v.forceMemoryStorage)
		{
			return builder.emitInstr1(IrOpcode.o_load, v.type.irType(context), v.stackSlotId);
		}
		else
		{
			return builder.readVariable(v.irVar);
		}
	}

	void visit(VariableDeclNode* v)
	{
		IrRef irRef;
		v.irVar = IrVar(v.strId(context), builder.newIrVarId(), v.type.irType(context));

		if (context.buildDebug)
			v.varFlags |= VariableFlags.forceMemoryStorage;

		// Allocate stack slot for parameter that is passed via stack
		bool isParamWithSlot = v.isParameter && ir.callingConvention.isParamOnStack(v.paramIndex);
		bool needsStackSlot = v.forceMemoryStorage || isParamWithSlot;

		if (needsStackSlot)
		{
			v.stackSlotId = ir.stackLayout.addStackItem(v.type.size, v.type.alignment, v.isParameter, v.paramIndex);
		}

		if (v.isParameter)
		{
			++ir.numParameters;
			auto instr = IrInstruction(IrOpcode.o_param, v.type.irType(context));
			instr.paramIndex = v.paramIndex;
			instr.stackSlot = v.stackSlotId;
			irRef = builder.put(instr);
			builder.writeVariable(v.irVar, irRef);
		}
		else
		{
			irRef = builder.put(0);
			store(v, irRef);
		}
	}
	void visit(StructDeclNode* s)
	{
		foreach (decl; s.declarations) _visit(decl);
	}
	void visit(BlockStmtNode* b)
	{
		foreach (stmt; b.statements)
		{
			_visit(stmt);
			if (ir.basicBlocks[builder.currentBB].isFinished) break;
		}
	}

	void tryInvertCondition(ExpressionNode* condition)
	{
		IrRef valueRef = condition.irRef;
		// invert condition, so that we jump to else on success
		if (valueRef.kind == IrValueKind.instr)
		{
			auto instr = &ir.instructions[valueRef.index];
			instr.condition = inverseIrCond[instr.condition];
		}
		else if (valueRef.isLiteral)
		{
			bool arg0 = ir.constants[valueRef.constIndex].i1;
			condition.irRef = builder.put(!arg0);
		}
		else context.internal_error(condition.loc, "Cannot invert condition");
	}

	IrCond getCondition(ExpressionNode* condition)
	{
		IrRef valueRef = condition.irRef;
		context.assertf(valueRef.kind == IrValueKind.instr, condition.loc, "Condition must be instruction");
		return ir.instructions[valueRef.index].condition;
	}

	void visit(IfStmtNode* i)
	{
		_visit(cast(AstNode*)i.condition);
		IrRef condRef = i.condition.irRef;

		// Compile single branch if condition is constant
		if (condRef.isLiteral)
		{
			bool condValue = ir.constants[condRef.constIndex].i1;
			if (condValue)
			{
				_visit(i.thenStatement);
			}
			else if (i.elseStatement)
			{
				_visit(i.elseStatement);
			}
			return;
		}

		//tryInvertCondition(i.condition);
		IrCond cond = getCondition(i.condition);

		// prevBB links to elseBB and (afterBB or thenBB)
		BasicBlockIndex prevBB = builder.currentBB;
		ir.basicBlocks[prevBB].exit = IrJump(IrJump.Type.branch, condRef, cond);

		// create Basic Block for then statement. It links to afterBB
		BasicBlockIndex then_StartBB = builder.addBasicBlock(IrName(thenId, ++thenCounter));
		builder.addBlockTarget(prevBB, then_StartBB);
		builder.sealBlock(then_StartBB);

		_visit(i.thenStatement);
		BasicBlockIndex then_EndBB = builder.currentBB;
		builder.sealBlock(then_EndBB);

		BasicBlockIndex else_StartBB;
		BasicBlockIndex else_EndBB;
		if (i.elseStatement)
		{
			else_StartBB = builder.addBasicBlock(IrName(elseId, ++elseCounter));
			builder.sealBlock(else_StartBB);
			builder.addBlockTarget(prevBB, else_StartBB);
			_visit(i.elseStatement);
			else_EndBB = builder.currentBB;
			builder.sealBlock(else_EndBB);
		}

		BasicBlockIndex afterBB = builder.addBasicBlock(IrName(blkId, ++bbCounter));

		if (!ir.basicBlocks[then_EndBB].isFinished)
		{
			ir.basicBlocks[then_EndBB].exit = IrJump(IrJump.Type.jmp);
			builder.addBlockTarget(then_EndBB, afterBB);
		}

		if (i.elseStatement)
		{
			if (!ir.basicBlocks[else_EndBB].isFinished)
			{
				ir.basicBlocks[else_EndBB].exit = IrJump(IrJump.Type.jmp);
				builder.addBlockTarget(else_EndBB, afterBB);
			}
		}
		else
		{
			builder.addBlockTarget(prevBB, afterBB);
		}
		builder.sealBlock(afterBB);
	}
	void visit(WhileStmtNode* w) {
		//_visit(cast(AstNode*)w.condition);
		//_visit(cast(AstNode*)w.statement);
	}
	void visit(DoWhileStmtNode* d) {
		//_visit(cast(AstNode*)d.condition);
		//_visit(cast(AstNode*)d.statement);
	}
	void visit(ReturnStmtNode* r)
	{
		if (r.expression)
		{
			_visit(r.expression);
			ir.basicBlocks[builder.currentBB].exit = IrJump(IrJump.Type.ret1, r.expression.irRef);
		}
		else ir.basicBlocks[builder.currentBB].exit = IrJump(IrJump.Type.ret0);
	}
	void visit(BreakStmtNode* b) {}
	void visit(ContinueStmtNode* c) {}
	void visit(AssignStmtNode* a) {
		currentAssignSide = CurrentAssignSide.leftSide;
		_visit(a.left);
		currentAssignSide = CurrentAssignSide.rightSide;
		_visit(a.right);

		final switch (a.op)
		{
			case AssignOp.opAssign:
				auto varExpr = cast(VariableExprNode*)a.left;
				store(varExpr.getSym.varDecl, a.right.irRef);
				break;
			case AssignOp.opIndexAssign:
				auto indexExpr = cast(IndexExprNode*)a.left;
				builder.emitInstr2(IrOpcode.o_store, indexExpr.type.irType(context), indexExpr.irRef, a.right.irRef);
				break;
		}
		currentAssignSide = CurrentAssignSide.rightSide;
	}
	void visit(VariableExprNode* v) {
		if (currentAssignSide == CurrentAssignSide.rightSide) {
			v.irRef = load(v.getSym.varDecl);
		}
	}
	void visit(LiteralExprNode* c) {
		c.irRef = builder.put(c.value);
	}
	void visit(BinaryExprNode* b) {
		auto parentAssignSide = currentAssignSide; // save
		scope(exit) currentAssignSide = parentAssignSide; // restore

		currentAssignSide = CurrentAssignSide.rightSide;
		_visit(cast(AstNode*)b.left);
		_visit(cast(AstNode*)b.right);

		// constant folding
		if (b.left.irRef.isCon && b.right.irRef.isCon)
		{
			long arg0 = ir.constants[b.left.irRef.constIndex].i64;
			long arg1 = ir.constants[b.right.irRef.constIndex].i64;

			switch(b.op)
			{
				case BinOp.EQUAL_EQUAL: b.irRef = builder.put(arg0 == arg1); return;
				case BinOp.NOT_EQUAL: b.irRef = builder.put(arg0 != arg1); return;
				case BinOp.GREATER: b.irRef = builder.put(arg0 > arg1); return;
				case BinOp.GREATER_EQUAL: b.irRef = builder.put(arg0 >= arg1); return;
				case BinOp.LESS: b.irRef = builder.put(arg0 < arg1); return;
				case BinOp.LESS_EQUAL: b.irRef = builder.put(arg0 <= arg1); return;
				case BinOp.PLUS: b.irRef = builder.put(arg0 + arg1); return;
				case BinOp.MINUS: b.irRef = builder.put(arg0 - arg1); return;
				default:
					context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op);
					return;
			}
		}

		auto lRef = b.left.irRef;
		auto rRef = b.right.irRef;
		switch(b.op)
		{
			case BinOp.EQUAL_EQUAL: b.irRef = builder.emitInstrCmp(IrCond.eq, lRef, rRef); break;
			case BinOp.NOT_EQUAL: b.irRef = builder.emitInstrCmp(IrCond.ne, lRef, rRef); break;
			case BinOp.GREATER: b.irRef = builder.emitInstrCmp(IrCond.g, lRef, rRef); break;
			case BinOp.GREATER_EQUAL: b.irRef = builder.emitInstrCmp(IrCond.ge, lRef, rRef); break;
			case BinOp.LESS: b.irRef = builder.emitInstrCmp(IrCond.l, lRef, rRef); break;
			case BinOp.LESS_EQUAL: b.irRef = builder.emitInstrCmp(IrCond.le, lRef, rRef); break;

			case BinOp.PLUS: b.irRef = builder.emitInstr2(IrOpcode.o_add, lRef.type, lRef, rRef); break;
			case BinOp.MINUS: b.irRef = builder.emitInstr2(IrOpcode.o_sub, lRef.type, lRef, rRef); break;

			default: context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op); break;
		}
	}
	void visit(CallExprNode* c)
	{
		foreach (i, ExpressionNode* arg; c.args) _visit(arg);

		foreach (uint i, ExpressionNode* arg; c.args)
		{
			auto instr = IrInstruction(IrOpcode.o_call_arg);
			instr._args[0] = arg.irRef;
			instr.type = arg.type.irType(context);
			instr.argIndex = i;
			auto irRef = builder.put(instr);
		}

		auto callInstr = IrInstruction(IrOpcode.o_call, c.type.irType(context));
		FunctionDeclNode* callee = c.getSym.funcDecl;
		callInstr.callee = callee;
		if (!callee.returnType.isVoid)
			callInstr.returnsValue = true;
		c.irRef = builder.put(callInstr);
	}
	void visit(IndexExprNode* i) {
		auto parentAssignSide = currentAssignSide; // save
		scope(exit) currentAssignSide = parentAssignSide; // restore
		currentAssignSide = CurrentAssignSide.rightSide;
		_visit(i.array);
		_visit(i.index);

		IrRef address;
		if (i.index.irRef.isLiteral)
		{
			ulong elemSize = i.type.size;
			ulong index = ir.constants[i.index.irRef.constIndex].i64;
			if (index == 0)
				address = i.array.irRef;
			else
			{
				IrRef offset = builder.put(index * elemSize);
				address = builder.emitInstr2(IrOpcode.o_add, IrValueType.ptr, i.array.irRef, offset);
			}
		}
		else
		{
			IrRef scale = builder.put(i.type.size);
			IrRef offset = builder.emitInstr2(IrOpcode.o_mul, i.index.irRef.type, i.index.irRef, scale);
			address = builder.emitInstr2(IrOpcode.o_add, IrValueType.ptr, i.array.irRef, offset);
		}

		if (parentAssignSide == CurrentAssignSide.rightSide) {
			i.irRef = builder.emitInstr1(IrOpcode.o_load, i.type.irType(context), address);
		}
		else
		{
			i.irRef = address;
		}
	}
	void visit(TypeConvExprNode* t) {
		_visit(cast(AstNode*)t.expr);

		IrValueType to = t.type.irType(context);
		IrValueType from = t.expr.type.irType(context);
		if (t.expr.irRef.isLiteral || from == IrValueType.i32 && to == IrValueType.i64)
		{
			t.irRef = t.expr.irRef;
			t.irRef.type = to;
		}
		else
		{
			t.irRef = builder.emitInstr1(IrOpcode.o_conv, to, t.expr.irRef);
		}
	}
	void visit(BasicTypeNode* t) {}
	void visit(PtrTypeNode* t) {}
	void visit(StaticArrayTypeNode* t) {}
	void visit(UserTypeNode* t) {}
}

struct OptimizeIrPass {
	CompilationContext* ctx;

	void visit(IrModule* m) {
		ctx.assertf(m !is null, "Module IR is null");
		foreach (func; m.functions) visit(func);
	}

	void visit(IrFunction* func)
	{
	}
}

//                               #####   ######
//                                 #     #     #
//                                 #     #     #
//                                 #     ######
//                                 #     #   #
//                                 #     #    #
//                               #####   #     #
// -----------------------------------------------------------------------------



//    #         #####   ##   ##  #######  #     #  #######   #####    #####
//    #           #      #   #   #        ##    #  #        #     #  #     #
//    #           #      #   #   #        # #   #  #        #        #
//    #           #       # #    #####    #  #  #  #####     #####    #####
//    #           #       # #    #        #   # #  #              #        #
//    #           #        #     #        #    ##  #        #     #  #     #
//    ######    #####      #     #######  #     #  #######   #####    #####
// -----------------------------------------------------------------------------


/// Implementation of:
/// "Linear Scan Register Allocation on SSA Form"
/*
// buildIntervals
for each block b in reverse order do
	live = union of successor.liveIn for each successor of b

	for each phi function phi of successors of b do
		live.add(phi.inputOf(b))

	for each opd in live do
		intervals[opd].addRange(b.from, b.to)

	for each operation op of b in reverse order do
		for each output operand opd of op do
			intervals[opd].setFrom(op.id)
			live.remove(opd)
		for each input operand opd of op do
			intervals[opd].addRange(b.from, op.id)
			live.add(opd)

	for each phi function phi of b do
		live.remove(phi.output)

	if b is loop header then
		loopEnd = last block of the loop starting at b
		for each opd in live do
			intervals[opd].addRange(b.from, loopEnd.to)
	b.liveIn = live
*/
void pass_live_intervals(ref CompilationContext ctx)
{
	import std.bitmanip : BitArray;
	// We store a bit array per basic block. Each bit shows liveness of operand per block
	// Padding aligns number of bits to multiple of size_t bits.
	// This way there is a whole number of size_ts per basic block
	// With padding we can copy size_t's directly between live and liveIn, without bit twiddling

	// [block0:[IrArgumentId..., padding], block1:[IrArgumentId..., padding]]
	BitArray liveIn;
	size_t[] liveInData;
	size_t[] liveInBuckets;

	// [IrArgumentId..., padding]
	BitArray live;
	size_t[] liveData;
	size_t[] liveBuckets;

	void allocSets(size_t numBucketsPerBlock, size_t numBlocks) {
		//writefln("alloc buckets %s blocks %s", numBucketsPerBlock, numBlocks);
		if (liveData.length < numBucketsPerBlock)
			liveData.length = numBucketsPerBlock;
		liveBuckets = liveData[0..numBucketsPerBlock];
		// liveData is nulled for each basic block, so we skip nulling
		live = BitArray(liveData, numBucketsPerBlock * size_t.sizeof * 8);

		size_t numBucketsTotal = numBucketsPerBlock * numBlocks;
		if (liveInData.length < numBucketsTotal)
			liveInData.length = numBucketsTotal;
		liveInData[] = 0; // unset all bits
		liveInBuckets = liveInData[0..numBucketsTotal];
		liveIn = BitArray(liveInData, numBucketsTotal * size_t.sizeof * 8);
	}

	foreach (IrFunction* fun; ctx.mod.irModule.functions)
	{
		fun.assignSequentialBlockIndices();
		size_t numValues = fun.operands.length;
		size_t numBucketsPerBlock = divCeil(numValues, size_t.sizeof * 8);
		allocSets(numBucketsPerBlock, fun.basicBlocks.length);
		fun.liveIntervals.initIntervals(numValues, numValues, ctx);

		void liveAdd(IrOperandId opdId)
		{
			if (opdId.isNull) return;
			live[opdId] = true;
		}

		void liveRemove(IrOperandId opdId) {
			if (opdId.isNull) return;
			live[opdId] = false;
		}

		size_t[] blockLiveIn(BasicBlockIndex blockIndex)
		{
			size_t from = fun.basicBlocks[blockIndex].seqIndex * numBucketsPerBlock;
			size_t to = from + numBucketsPerBlock;
			return liveInBuckets[from..to];
		}

		// algorithm start
		// for each block b in reverse order do
		foreach (ref IrBasicBlock block; fun.basicBlocks.range)
		{
			// live = union of successor.liveIn for each successor of block
			liveData[] = 0;
			foreach (BasicBlockIndex succIndex; block.successors)
			{
				foreach (size_t i, size_t bucket; blockLiveIn(succIndex))
					liveBuckets[i] |= bucket;
			}

			// for each phi function phi of successors of block do
			//     live.add(phi.inputOf(block))
			foreach (BasicBlockIndex succIndex; block.successors)
				foreach (ref IrRef phiRef; fun.basicBlocks[succIndex].phis)
					foreach (ref IrPhiArg arg; fun.phis[phiRef.index].args)
						if (arg.blockIndex == block.index)
							liveAdd(fun.getOperand(arg.value));

			//writef("in @%s live:", block.index);
			//foreach (size_t index; live.bitsSet)
			//	writef(" %s", index);
			//writeln;

			// for each opd in live do
			foreach (size_t index; live.bitsSet)
			{
				// intervals[opd].addRange(block.from, block.to)
				IntervalId intId = fun.liveIntervals.intervalId(IrOperandId(cast(uint)index));
				fun.liveIntervals.addRange(intId, cast(int)block.firstInstr, cast(int)block.lastInstr);
			}

			void eachArg(IrRef opd, size_t opId) {
				IrOperandId opdId = fun.getOperand(opd);
				if (opdId.isNull) return;

				// intervals[opd].addRange(block.from, op.id)
				IntervalId intId = fun.liveIntervals.intervalId(opdId);
				fun.liveIntervals.addRange(intId, cast(int)block.firstInstr, cast(int)opId);

				// live.add(opd)
				liveAdd(opdId);
			}

			// for each operation op of b in reverse order do
				// last instr, no return operand
				switch(block.exit.type) with(IrJump.Type) {
					case ret1, branch: eachArg(block.exit.value, block.lastInstr); break;
					default: break;
				}
			foreach_reverse(blk_i, ref instr; fun.instructions[block.firstInstr..block.lastInstr+1])
			{
				size_t index = block.firstInstr + blk_i;
				// for each output operand opd of op do
				if (instr.returnsValue)
				{
					// intervals[opd].setFrom(op.id)
					fun.liveIntervals.addDefinition(instr.result, instr.type, cast(int)index);
					// live.remove(opd)
					liveRemove(instr.result);
				}

				// for each input operand opd of op do
				foreach(IrRef opd; instr.args)
				{
					// intervals[opd].addRange(b.from, op.id)
					// live.add(opd)
					eachArg(opd, index);
				}

				switch(instr.op) with(IrOpcode)
				{
					case o_call:
						foreach (reg; fun.callingConvention.volatileRegs)
						{
							IntervalId intId = fun.liveIntervals.intervalId(reg);
							fun.liveIntervals.addRange(intId, cast(int)index, cast(int)index);
						}
						break;

					default:
						break;
				}
			}

			// for each phi function phi of b do
			foreach (ref IrRef phiRef; block.phis)
			{
				// live.remove(phi.output)
				liveRemove(fun.phis[phiRef.index].result);
			}

			// TODO
			// if b is loop header then
			//     loopEnd = last block of the loop starting at b
			//     for each opd in live do
			//         intervals[opd].addRange(b.from, loopEnd.to)

			// b.liveIn = live
			blockLiveIn(block.index)[] = liveBuckets;
		}
	}
}

///
struct FunctionLiveIntervals
{
	// invariant: all ranges of one interval are sorted by `from` and do not intersect
	Buffer!LiveRange ranges;
	LiveInterval[] intervals;
	size_t numFixedIntervals;

	this(this)
	{
		intervals = intervals.dup;
	}

	LiveInterval[] virtualIntervals() { return intervals[numFixedIntervals..$]; }
	LiveInterval[] physicalIntervals() { return intervals[0..numFixedIntervals]; }

	void initIntervals(size_t numIntervals, size_t reserveRanges, ref CompilationContext ctx) {
		numFixedIntervals = ctx.machineInfo.numIntegerRegisters;
		intervals.length = numFixedIntervals + numIntervals;
		foreach (i, ref it; physicalIntervals)
		{
			it.reg = ctx.machineInfo.regs[i];
			it.isFixed = true;
		}
		ranges.reserve(reserveRanges);
	}

	/// Set hint for register allocator
	void setStorageHint(IrOperandId opdId, StorageHint storageHint) {
		this[opdId].storageHint = storageHint;
	}

	bool intervalCoversPosition(NodeIndex cur, int position)
	{
		while (!cur.isNull)
		{
			auto r = &ranges[cur];
			if (position < r.from)
				return false;
			else if (position >= r.to)
				cur = r.nextIndex;
			else // from <= position < to
				return true;
		}
		return false;
	}

	int firstIntersection(NodeIndex a, NodeIndex b)
	{
		while (true)
		{
			auto ra = &ranges[a];
			auto rb = &ranges[b];
			if (ra.intersectsWith(*rb)) {
				return max(ra.from, rb.from);
			}
			else if (ra.from < rb.from) {
				a = ra.nextIndex;
				if (a.isNull) return int.max;
			}
			else { // rb.from > ra.from
				b = rb.nextIndex;
				if (b.isNull) return int.max;
			}
		}
	}

	ref LiveInterval opIndex(IntervalId intId) { return intervals[intId]; }
	ref LiveInterval opIndex(IrOperandId opdId) { return intervals[numFixedIntervals + opdId]; }
	ref LiveInterval opIndex(NodeIndex rangeId) { return intervals[ranges[rangeId].intervalId]; }

	IntervalId intervalId(NodeIndex rangeId) { return ranges[rangeId].intervalId; }
	IntervalId intervalId(IrOperandId opdId) { return IntervalId(numFixedIntervals + opdId); }
	IntervalId intervalId(RegisterRef regRef) { return IntervalId(regRef.index); }
	IrOperandId operandId(NodeIndex rangeId) { return operandId(ranges[rangeId].intervalId); }
	IrOperandId operandId(IntervalId intId) { assert(intId >= numFixedIntervals); return IrOperandId(intId- numFixedIntervals); }
	// returns rangeId pointing to range covering position or one to the right of pos.
	// returns -1 if no ranges left after pos.
	NodeIndex advanceRangeId(NodeIndex cur, int position)
	{
		while (!cur.isNull)
		{
			auto r = &ranges[cur];
			if (position < r.from)
				return cur;
			else if (position >= r.to)
				cur = r.nextIndex;
			else // from <= position < to
				return cur;
		}
		return cur;
	}

	// bounds are from block start to block end of the same block
	// from is always == to block start
	// checks for interval being null
	void addRange(IntervalId interval, int from, int to)
	{
		if (interval.isNull) return;
		LiveRange newRange = LiveRange(from, to, interval);
		NodeIndex firstMergeId;

		// merge all intersecting ranges into one
		NodeIndex cur = intervals[interval].first;
		while (!cur.isNull)
		{
			auto r = &ranges[cur];

			if (r.canBeMergedWith(newRange))
			{
				if (firstMergeId.isNull)
				{
					// first merge
					firstMergeId = cur;
					r.merge(newRange);
				}
				else
				{
					// second+ merge
					ranges[firstMergeId].merge(*r);
					cur = deleteRange(cur); // sets cur to next range
					continue;
				}
			}
			else if (to < r.from)
			{
				if (!firstMergeId.isNull) return; // don't insert after merge

				// we found insertion point before cur, no merge was made
				insertRangeBefore(cur, newRange);
				return;
			}

			cur = r.nextIndex;
		}

		if (firstMergeId.isNull)
		{
			// insert after last (cur is NULL), no merge/insertion was made
			appendRange(interval, newRange);
		}
	}

	// sets the definition position
	void addDefinition(IrOperandId opdId, IrValueType type, int from) {
		IntervalId interval = intervalId(opdId);
		NodeIndex cur = intervals[interval].first;
		intervals[interval].regClass = typeToRegClass(type);
		if (!cur.isNull) {
			ranges[cur].from = from;
		}
	}

	void insertRangeBefore(NodeIndex beforeRange, LiveRange range)
	{
		NodeIndex index = NodeIndex(ranges.length);

		LiveRange* next = &ranges[beforeRange];
		range.prevIndex = next.prevIndex;
		range.nextIndex = beforeRange;
		next.prevIndex = index;

		if (!range.prevIndex.isNull)
			ranges[range.prevIndex].nextIndex = index;
		else
			intervals[range.intervalId].first = index;

		ranges.put(range);
	}

	void appendRange(IntervalId interval, LiveRange range)
	{
		NodeIndex last = intervals[interval].last;
		NodeIndex index = NodeIndex(ranges.length);

		if (last.isNull)
		{
			intervals[range.intervalId].first = index;
			intervals[range.intervalId].last = index;
		}
		else
		{
			LiveRange* prev = &ranges[last];
			range.prevIndex = last;
			range.nextIndex = prev.nextIndex;
			prev.nextIndex = index;

			if (!range.nextIndex.isNull)
				ranges[range.nextIndex].prevIndex = index;
		}

		ranges.put(range);
	}

	void moveRange(NodeIndex fromIndex, NodeIndex toIndex)
	{
		if (fromIndex == toIndex) return;
		ranges[toIndex] = ranges[fromIndex];
		auto range = &ranges[toIndex];
		if (!range.prevIndex.isNull) ranges[range.prevIndex].nextIndex = toIndex;
		if (!range.nextIndex.isNull) ranges[range.nextIndex].prevIndex = toIndex;
		auto it = &intervals[range.intervalId];
		if (fromIndex == it.first) it.first = toIndex;
		if (fromIndex == it.last) it.last = toIndex;
	}

	// returns rangeId of the next range
	NodeIndex deleteRange(NodeIndex rangeIndex)
	{
		auto range = &ranges[rangeIndex];

		auto it = &intervals[range.intervalId];
		if (rangeIndex == it.first) it.first = range.nextIndex;
		if (rangeIndex == it.last) it.last = range.prevIndex;

		NodeIndex lastIndex = NodeIndex(ranges.length-1);
		NodeIndex nextIndex = range.nextIndex;

		if (range.prevIndex != NodeIndex.NULL)
			ranges[range.prevIndex].nextIndex = range.nextIndex;
		if (range.nextIndex != NodeIndex.NULL)
			ranges[range.nextIndex].prevIndex = range.prevIndex;

		if (nextIndex == lastIndex) nextIndex = rangeIndex;

		moveRange(lastIndex, rangeIndex);
		ranges.unput(1);

		return nextIndex;
	}

	void dump(ref TextSink sink) {
		void dumpSub(LiveInterval[] intervals)
		{
			foreach (i, it; intervals) {
				NodeIndex cur = it.first;
				if (cur.isNull) {
					sink.putfln("% 3s: null", i);
					continue;
				}
				if (it.reg.isNull)
					sink.putf("% 3s [no reg]:", i);
				else
					sink.putf("% 3s [%s %2s]:", i, it.reg.regClass, it.reg);

				while (!cur.isNull)
				{
					auto r = &ranges[cur];
					sink.putf(" [%s; %s)", r.from, r.to);
					cur = r.nextIndex;
				}
				sink.putln;
			}
		}
		sink.putln("fixed intervals:");
		dumpSub(intervals[0..numFixedIntervals]);
		sink.putln("virtual intervals:");
		dumpSub(intervals[numFixedIntervals..$]);
	}
}

///
struct LiveInterval
{
	NodeIndex first;
	NodeIndex last;
	RegisterRef reg;
	RegClass regClass;
	bool isFixed;
	StorageHint storageHint;
}

struct IntervalId
{
	this(size_t idx) { index = cast(uint)idx; }
	uint index = uint.max;
	alias index this;
	enum NULL = IntervalId();
	bool isNull() { return index == uint.max; }
}

/// [from; to)
struct LiveRange
{
	int from;
	int to;
	IntervalId intervalId;
	NodeIndex prevIndex = NodeIndex.NULL;
	NodeIndex nextIndex = NodeIndex.NULL;
	bool isLast() { return nextIndex == NodeIndex.NULL; }
	bool contains(int pos) {
		if (pos < from) return false;
		if (pos >= to) return false;
		return true;
	}
	void merge(LiveRange other) {
		from = min(from, other.from);
		to = max(to, other.to);
	}
	bool canBeMergedWith(const LiveRange other) {
		if (to < other.from) return false;
		if (from > other.to) return false;
		return true;
	}

	bool intersectsWith(const LiveRange other) {
		if (to <= other.from) return false;
		if (from >= other.to) return false;
		return true;
	}
}

//     ######   #######    ####      #     #        #          ###      ####
//     #     #  #         #    #     #     #        #         #   #    #    #
//     #     #  #        #          ###    #        #        #     #  #
//     ######   #####    #   ###    # #    #        #        #     #  #
//     #   #    #        #     #   #####   #        #        #     #  #
//     #    #   #         #    #   #   #   #        #         #   #    #    #
//     #     #  #######    #####  ##   ##  ######   ######     ###      ####
// -----------------------------------------------------------------------------


/// Does linear scan register allocation.
/// Uses live intervals produced by pass_live_intervals
///
void pass_linear_scan(ref CompilationContext ctx) {
	LinearScan linearScan;
	linearScan.setup(&ctx);
	foreach (fun; ctx.mod.irModule.functions) {
		linearScan.assignHints(*fun, *fun.callingConvention);
		linearScan.scanFun(*fun);
		linearScan.resolve(*fun);
	}
}

/// Implementation of:
/// "Optimized Interval Splitting in a Linear Scan Register Allocator"
/// "Linear Scan Register Allocation on SSA Form"
struct LinearScan
{
	NodeIndex[] unhandledStorage;
	Buffer!NodeIndex active;
	Buffer!NodeIndex inactive;
	Buffer!NodeIndex handled;
	PhysRegisters physRegs;
	CompilationContext* ctx;
	FunctionLiveIntervals* live;

	void setup(CompilationContext* ctx)
	{
		this.ctx = ctx;
		physRegs.setup;
	}

	void assignHints(ref IrFunction fun, ref CallConv callConv)
	{
		// assign paramter registers and memory locations
		foreach(i, ref instr; fun.parameters)
		{
			callConv.setParamHint(fun, instr, i);
		}

		// assign register hint to return values according to call conv
		foreach (ref IrBasicBlock block; fun.basicBlocks.range)
		{
			if (block.exit.type == IrJump.Type.ret1)
			{
				callConv.setReturnHint(fun, block.exit.value);
			}
		}
	}

	/*
		LinearScan
		unhandled = list of intervals sorted by increasing start positions
		active = { }; inactive = { }; handled = { }

		while unhandled != { } do
			current = pick and remove first interval from unhandled
			position = start position of current

			// check for intervals in active that are handled or inactive
			for each interval it in active do
				if it ends before position then
					move it from active to handled
				else if it does not cover position then
					move it from active to inactive

			// check for intervals in inactive that are handled or active
			for each interval it in inactive do
				if it ends before position then
					move it from inactive to handled
				else if it covers position then
					move it from inactive to active

			// find a register for current
			TryAllocateFreeReg
			if allocation failed then AllocateBlockedReg

			if current has a register assigned then add current to active
	*/
	void scanFun(ref IrFunction fun)
	{
		import std.container.binaryheap;

		live = &fun.liveIntervals;
		scope(exit) live = null;

		int cmp(NodeIndex a, NodeIndex b) {
			return live.ranges[a].from > live.ranges[b].from;
		}

		// unhandled = list of intervals sorted by increasing start positions
		// active = { }; inactive = { }; handled = { }
		BinaryHeap!(NodeIndex[], cmp) unhandled;
		unhandled.assume(assumeSafeAppend(unhandledStorage), 0);
		active.clear;
		inactive.clear;
		handled.clear;

		unhandled.acquire(null);
		foreach (it; live.virtualIntervals)
			if (!it.first.isNull)
				unhandled.insert(it.first);
		//writefln("unhandled %s", unhandled);

		foreach (it; live.physicalIntervals)
			if (!it.first.isNull)
				inactive.put(it.first);

		// while unhandled != { } do
		while (!unhandled.empty)
		{
			// current = pick and remove first interval from unhandled
			NodeIndex currentId = unhandled.front;
			unhandled.removeFront;

			// position = start position of current
			int position = live.ranges[currentId].from;
			//writefln("current %s pos %s", live.ranges[currentId].intervalId, position);

			size_t index;
			// // check for intervals in active that are handled or inactive
			// for each interval it in active do
			while (index < active.length)
			{
				NodeIndex rangeId0 = active[index];
				NodeIndex rangeId = active[index] = live.advanceRangeId(rangeId0, position);
				LiveRange range = live.ranges[rangeId0];

				// if it ends before position then
				if (rangeId.isNull)
				{
					// move it from active to handled
					//writefln("move %s active -> handled", range.intervalId);
					//handled.put(rangeId);
					active.removeInPlace(index);
				}
				// else if it does not cover position then
				else if(!live.intervalCoversPosition(rangeId, position))
				{
					// move it from active to inactive
					//writefln("%s isLast %s less %s", rangeId, range.isLast, range.to < position);
					//writefln("move %s active -> inactive", range.intervalId);
					inactive.put(rangeId);
					active.removeInPlace(index);
				}
				else
					++index;
			}

			index = 0;
			// check for intervals in inactive that are handled or active
			// for each interval it in inactive do
			while (index < inactive.length)
			{
				NodeIndex rangeId = inactive[index];

				// if it ends before position then
				if (live.ranges[rangeId].isLast && live.ranges[rangeId].to < position)
				{
					// move it from inactive to handled
					//writefln("move %s inactive -> handled", live.ranges[rangeId].intervalId);
					//handled.put(rangeId);
					inactive.removeInPlace(index);
				}
				// 	else if it covers position then
				else if(live.intervalCoversPosition(rangeId, position))
				{
					// move it from inactive to active
					//writefln("move %s inactive -> active", live.ranges[rangeId].intervalId);
					active.put(rangeId);
					inactive.removeInPlace(index);
				}
				else
					++index;
			}

			// find a register for current
			bool success = tryAllocateFreeReg(fun, currentId);

			// if allocation failed then AllocateBlockedReg
			if (!success)
				allocateBlockedReg(fun, currentId, position);

			// if current has a register assigned then add current to active
			if (!(*live)[currentId].reg.isNull)
			{
				//writefln("move current %s -> active", live.ranges[currentId].intervalId);
				active.put(currentId);
			}
		}

		unhandledStorage = unhandled.release;
	}

	/*
	TRYALLOCATEFREEREG
		set freeUntilPos of all physical registers to maxInt

		for each interval it in active do
			freeUntilPos[it.reg] = 0
		for each interval it in inactive intersecting with current do
			freeUntilPos[it.reg] = next intersection of it with current

		reg = register with highest freeUntilPos
		if freeUntilPos[reg] = 0 then
			// no register available without spilling
			allocation failed
		else if current ends before freeUntilPos[reg] then
			// register available for the whole interval
			current.reg = reg
		else
			// register available for the first part of the interval
			current.reg = reg
			split current before freeUntilPos[reg]
	*/
	bool tryAllocateFreeReg(ref IrFunction fun, NodeIndex currentId)
	{
		// set freeUntilPos of all physical registers to maxInt
		physRegs.resetFreeUntilPos;

		LiveInterval* currentIt = &fun.liveIntervals[currentId];
		int currentEnd = live.ranges[currentIt.last].to;

		// for each interval it in active do
		//     freeUntilPos[it.reg] = 0
		foreach (rangeId; active.data)
		{
			auto it = fun.liveIntervals[rangeId];
			assert(!it.reg.isNull);
			physRegs[it.reg].freeUntilPos = 0;
		}

		// check inactive : TODO

		// reg = register with highest freeUntilPos
		int maxPos = 0;
		RegisterRef reg;

		// reg stored in hint
		RegisterRef hintReg = currentIt.storageHint.getRegHint;

		PhysRegister[] candidates = physRegs.getClassRegs(currentIt.regClass);
		foreach (i, ref PhysRegister r; candidates)
		{
			if (r.freeUntilPos > maxPos) {
				maxPos = r.freeUntilPos;
				reg = r.regRef;
			}
		}

		if (maxPos == 0)
		{
			return false;
		}
		else
		{
			if (!hintReg.isNull)
			{
				if (currentEnd < physRegs[hintReg].freeUntilPos)
					reg = hintReg;
			}

			if (currentEnd < maxPos)
			{
				currentIt.reg = reg;
				return true;
			}
			else
			{
				// split
				return false;
			}
		}
	}

	int nextUseAfter(ref IrFunction fun, IrOperandId opdId, int after)
	{
		ListInfo!NodeIndex usesList = fun.operands[opdId].usesList;
		int closest = int.max;
		for (NodeIndex cur = usesList.first; !cur.isNull; cur = fun.opdUses.nodes[cur].nextIndex)
		{
			IrRef userRef = fun.opdUses.nodes[cur].data;
			int pos = fun.linearPosition(userRef);
			if (pos > after && pos < closest)
			{
				closest = pos;
			}
		}
		return closest;
	}

	/*
	ALLOCATEBLOCKEDREG
		set nextUsePos of all physical registers to maxInt

		for each interval it in active do
			nextUsePos[it.reg] = next use of it after start of current
		for each interval it in inactive intersecting with current do
			nextUsePos[it.reg] = next use of it after start of current

		reg = register with highest nextUsePos
		if first usage of current is after nextUsePos[reg] then
			// all other intervals are used before current,
			// so it is best to spill current itself
			assign spill slot to current
			split current before its first use position that requires a register
		else
			// spill intervals that currently block reg
			current.reg = reg
			split active interval for reg at position
			split any inactive interval for reg at the end of its lifetime hole

		// make sure that current does not intersect with
		// the fixed interval for reg
		if current intersects with the fixed interval for reg then
			split current before this intersection
	*/
	void allocateBlockedReg(ref IrFunction fun, NodeIndex currentRangeId, int currentStart)
	{
		writefln("allocateBlockedReg of range %s, start %s", currentRangeId, currentStart);
		// set nextUsePos of all physical registers to maxInt
		physRegs.resetNextUsePos();

		// for each interval it in active do
		//     nextUsePos[it.reg] = next use of it after start of current
		foreach (rangeId; active.data)
		{
			LiveInterval* it = &fun.liveIntervals[rangeId];
			assert(!it.reg.isNull);
			if (it.isFixed)
			{
				physRegs[it.reg].nextUsePos = currentStart;
			}
			else
			{
				IrOperandId opdId = live.operandId(rangeId);
				physRegs[it.reg].nextUsePos = nextUseAfter(fun, opdId, currentStart);
			}
			writefln("nextUsePos of %s is %s", rangeId, physRegs[it.reg].nextUsePos);
		}

		// for each interval it in inactive intersecting with current do
		//     nextUsePos[it.reg] = next use of it after start of current
		foreach (rangeId; inactive.data)
		{
			LiveInterval* it = &fun.liveIntervals[rangeId];
			assert(!it.reg.isNull);
			if (it.isFixed)
			{
				int fistrIntersection = live.firstIntersection(currentRangeId, rangeId);
				physRegs[it.reg].nextUsePos = min(fistrIntersection, physRegs[it.reg].nextUsePos);
			}
			else
			{
				IrOperandId opdId = live.operandId(rangeId);
				physRegs[it.reg].nextUsePos = nextUseAfter(fun, opdId, currentStart);
			}
			writefln("nextUsePos of %s is %s", rangeId, physRegs[it.reg].nextUsePos);
		}

		// reg = register with highest nextUsePos
		LiveInterval* currentIt = &fun.liveIntervals[currentRangeId];
		PhysRegister[] regs = physRegs.getClassRegs(currentIt.regClass);
		int maxUsePos = 0;
		RegisterRef reg;
		foreach (i, ref PhysRegister r; regs)
		{
			if (r.nextUsePos > maxUsePos) {
				maxUsePos = r.nextUsePos;
				reg = r.regRef;
			}
		}

		ctx.unreachable;
		assert(false);
	}

	/*
	// Resolve
	for each control flow edge from predecessor to successor do
		for each interval it live at begin of successor do
			if it starts at begin of successor then
				phi = phi function defining it
				opd = phi.inputOf(predecessor)
				if opd is a constant then
					moveFrom = opd
				else
					moveFrom = location of intervals[opd] at end of predecessor
			else
				moveFrom = location of it at end of predecessor
			moveTo = location of it at begin of successor
			if moveFrom ≠ moveTo then
				mapping.add(moveFrom, moveTo)
		mapping.orderAndInsertMoves()
	*/
	void resolve(ref IrFunction fun)
	{

	}
}

RegClass typeToRegClass(IrValueType type)
{
	if (type == IrValueType.i1) return RegClass.flags;
	return RegClass.gpr;
}

enum RegClass : ubyte
{
	gpr,
	flags
}

struct PhysRegister
{
	RegisterRef regRef;
	int freeUntilPos;
	int nextUsePos;
}

struct PhysRegisters
{
	PhysRegister[] gpr;
	PhysRegister[] flags;

	ref PhysRegister opIndex(RegisterRef reg) {
		final switch(reg.regClass) {
			case RegClass.gpr: return gpr[reg];
			case RegClass.flags: return flags[reg];
		}
	}

	void setup()
	{
		gpr.length = 0;
		flags ~= PhysRegister(RegisterRef(0, 0, RegClass.flags));
		gpr ~= PhysRegister(RegisterRef(0, Register.AX, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(1, Register.CX, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(2, Register.DX, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(3, Register.R8, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(4, Register.R9, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(5, Register.R10, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(6, Register.R11, RegClass.gpr));
	}

	void resetFreeUntilPos()
	{
		foreach (ref reg; chain(gpr, flags)) reg.freeUntilPos = int.max;
	}

	void resetNextUsePos()
	{
		foreach (ref reg; chain(gpr, flags)) reg.nextUsePos = int.max;
	}

	PhysRegister[] getClassRegs(RegClass cls)
	{
		final switch(cls) {
			case RegClass.gpr: return gpr;
			case RegClass.flags: return flags;
		}
	}
}

struct OperandLocation
{
	this(RegisterRef reg) {
		type = OpdLocType.physicalRegister;
		this.reg = reg;
	}
	static OperandLocation makeConstant(IrRef irRef) {
		assert(irRef.kind == IrValueKind.con);
		OperandLocation res;
		final switch(irRef.constKind)
		{
			case IrConstKind.literal: res.type = OpdLocType.constant; break;
			case IrConstKind.stackSlotId: res.type = OpdLocType.stackSlot; break;
		}

		res.irRef = irRef;
		return res;
	}

	bool isConstant() { return type == OpdLocType.constant; }

	OpdLocType type;
	union
	{
		IrRef irRef; // ref to instruction, phi or constant
		RegisterRef reg;
	}
}
enum OpdLocType : ubyte
{
	constant,
	virtualRegister,
	physicalRegister,
	memoryAddress,
	stackSlot,
}
MoveType calcMoveType(OpdLocType dst, OpdLocType src)
{
	final switch(dst) with(OpdLocType) {
		case constant: return MoveType.invalid;
		case virtualRegister: return MoveType.invalid;
		case physicalRegister:
			final switch(src) with(OpdLocType) {
				case constant: return MoveType.const_to_reg;
				case virtualRegister: return MoveType.invalid;
				case physicalRegister: return MoveType.reg_to_reg;
				case memoryAddress: return MoveType.mem_to_reg;
				case stackSlot: return MoveType.stack_to_reg;
			}
		case memoryAddress:
			final switch(src) with(OpdLocType) {
				case constant: return MoveType.const_to_mem;
				case virtualRegister: return MoveType.invalid;
				case physicalRegister: return MoveType.reg_to_mem;
				case memoryAddress: return MoveType.invalid;
				case stackSlot: return MoveType.invalid;
			}
		case stackSlot:
			final switch(src) with(OpdLocType) {
				case constant: return MoveType.const_to_stack;
				case virtualRegister: return MoveType.invalid;
				case physicalRegister: return MoveType.reg_to_stack;
				case memoryAddress: return MoveType.invalid;
				case stackSlot: return MoveType.invalid;
			}
	}
}

enum MoveType
{
	invalid,
	const_to_reg,
	const_to_stack,
	reg_to_reg,
	reg_to_stack,
	stack_to_reg,
	const_to_mem,
	reg_to_mem,
	mem_to_reg,
}

struct StorageHint
{
	union {
		RegisterRef reg;       // if hintType == register
		StackSlotId stackSlot; // if hintType == stack
	}
	RegisterRef getRegHint() {
		if (isSet && hintType == StorageHintType.register) return reg;
		return RegisterRef();
	}
	mixin(bitfields!(
		/// Value came from memory location. No need to spill.
		/// True for parameters that are passed through stack, not regs
		bool,            "defByMem",   1,
		bool,            "isSet",      1,
		bool,            "isParameter",1,
		StorageHintType, "hintType",   1,
		uint,            "",           4,
	));
}

enum StorageHintType : ubyte
{
	register,
	stack
}

/// Is used in IrValue to indicate the register that stores given IrValue
struct RegisterRef
{
	this(size_t index, ubyte nativeReg, RegClass regClass) {
		this.index = cast(ubyte)index;
		this.nativeReg = nativeReg;
		this.regClass = regClass;
	}
	ubyte index = ubyte.max;
	ubyte nativeReg; // stores actual id of register (EAX, EDX etc)
	RegClass regClass;
	alias index this;
	bool isNull() { return index == ubyte.max; }
}


//                    #####   #######     #       ####   #    #
//                   #     #     #        #      #    #  #   #
//                   #           #       ###    #        #  #
//                    #####      #       # #    #        ###
//                         #     #      #####   #        #  #
//                   #     #     #      #   #    #    #  #   #
//                    #####      #     ##   ##    ####   #    #
// -----------------------------------------------------------------------------


/// Arranges items on the stack according to calling convention
void pass_stack_layout(ref CompilationContext ctx) {
	IrModule* mod = &ctx.mod.irModule;
	foreach (IrFunction* func; mod.functions)
	{
		enum STACK_ITEM_SIZE = 8; // x86_64
		int numParams = cast(int)func.numParameters;

		auto layout = &func.stackLayout;
		layout.reservedBytes = layout.numLocals * STACK_ITEM_SIZE;
		//writefln("%s", layout.numLocals);
		//writefln("%s", layout.numParams);

		// ++        slot index
		// param2    1  rsp + 20     \
		// param1    0  rsp + 18     / numParams = 2
		// ret addr     rsp + 10
		// local1    2  rsp +  8     \
		// local2    3  rsp +  0     / numLocals = 2   <-- RSP
		// --

		//writefln("numSlots %s numLocals %s numParams %s", numSlots, numLocals, numParams);
		//writefln("layout %s", layout.reservedBytes);

		/*
		if (USE_FRAME_POINTER)
		{
			// ++        varIndex
			// param2    1              \
			// param1    0  rbp + 2     / numParams = 2
			// ret addr     rbp + 1
			// rbp      <-- rbp + 0
			// local1    2  rbp - 1     \
			// local2    3  rbp - 2     / numLocals = 2
			// --
			if (isParameter) // parameter
			{
				index = 2 + varIndex;
			}
			else // local variable
			{
				index = -(varIndex - numParams + 1);
			}
			baseReg = Register.BP;
		}*/

		int localIndex = 0;
		foreach (ref slot; layout.slots)
		{
			if (slot.isParameter)
			{
				slot.offset = (layout.numLocals + slot.paramIndex + 1) * STACK_ITEM_SIZE;
			}
			else
			{
				slot.offset = (layout.numLocals - localIndex - 1) * STACK_ITEM_SIZE;
				++localIndex;
			}
		}
	}
}

struct StackLayout
{
	int reservedBytes;
	int numParams() { return cast(uint)slots.length - numLocals; }
	int numLocals;
	StackSlot[] slots;

	this(this)
	{
		slots = slots.dup;
	}

	/// paramIndex == -1 for non-params
	IrRef addStackItem(ulong size, ulong alignment, bool isParameter, ushort paramIndex)
	{
		assert(size > 0);
		assert(alignment > 0);

		auto id = StackSlotId(cast(uint)(slots.length));
		auto slot = StackSlot(size, alignment, isParameter, paramIndex);

		if (!isParameter) ++numLocals;

		slots ~= slot;
		return IrRef(id);
	}
}

struct StackSlot
{
	ulong size;
	ulong alignment;
	bool isParameter;
	ushort paramIndex;
	ushort numUses;
	int offset;
	void addUser() { ++numUses; }
}

struct StackSlotId
{
	uint id = uint.max;
	alias id this;
	bool isNull() { return id == uint.max; }
}

struct MachineInfo
{
	size_t numIntegerRegisters() { return regs.length; }
	RegisterRef[] regs;
}

__gshared MachineInfo mach_info_x86_64 = MachineInfo(
	[RegisterRef(0, Register.AX, RegClass.gpr),
	RegisterRef(1, Register.CX, RegClass.gpr),
	RegisterRef(2, Register.DX, RegClass.gpr),
	RegisterRef(3, Register.R8, RegClass.gpr),
	RegisterRef(4, Register.R9, RegClass.gpr),
	RegisterRef(5, Register.R10, RegClass.gpr),
	RegisterRef(6, Register.R11, RegClass.gpr)]);

/// Info needed for calling convention implementation
struct CallConv
{
	RegisterRef[] paramsInRegs;
	RegisterRef returnReg;
	RegisterRef[] volatileRegs;

	bool isParamOnStack(size_t parIndex) {
		return parIndex >= paramsInRegs.length;
	}
	void setParamHint(ref IrFunction fun, ref IrInstruction instr, size_t parIndex) {
		StorageHint hint;
		hint.isSet = true;
		if (parIndex < paramsInRegs.length)
		{
			hint.hintType = StorageHintType.register;
			hint.reg = paramsInRegs[parIndex];
		}
		else
		{
			hint.defByMem = true;
			hint.hintType = StorageHintType.stack;
			hint.stackSlot = StackSlotId(cast(uint)(parIndex - paramsInRegs.length));
		}
		hint.isParameter = true;
		fun.liveIntervals.setStorageHint(instr.result, hint);
	}
	void setReturnHint(ref IrFunction fun, IrRef irRef) {
		StorageHint hint;
		hint.isSet = true;
		hint.reg = returnReg;
		fun.setStorageHint(irRef, hint);
	}
}

__gshared CallConv win64_call_conv = CallConv(
	[RegisterRef(1, Register.CX, RegClass.gpr), // indicies into PhysRegisters.gpr
	RegisterRef(2, Register.DX, RegClass.gpr),
	RegisterRef(3, Register.R8, RegClass.gpr),
	RegisterRef(4, Register.R9, RegClass.gpr)],
	RegisterRef(0, Register.AX, RegClass.gpr),  // return reg

	[RegisterRef(0, Register.AX, RegClass.gpr), // volatile regs
	RegisterRef(1, Register.CX, RegClass.gpr),
	RegisterRef(2, Register.DX, RegClass.gpr),
	RegisterRef(3, Register.R8, RegClass.gpr),
	RegisterRef(4, Register.R9, RegClass.gpr),
	RegisterRef(5, Register.R10, RegClass.gpr),
	RegisterRef(6, Register.R11, RegClass.gpr)]
);
/*
/// State of single machine register
struct RegisterState
{
	IrRef storedValue;
	mixin(bitfields!(
		/// True if value in register needs to be spilled before using register
		bool,        "isDirty",  1,
		uint,        "",         7,
	));
	bool hasValue() { return storedValue.isDefined; }
}

/// Stores info about all avaliable registers of the same class
/// Classes are (general purpose aka GPR, floating point FP, flags)
struct RegisterClass
{
	enum MAX_REGS = 16;
	// Index of register here corrensponds to machine-specific register index
	RegisterState[MAX_REGS] regStates;
	// Stores indicies of registers that hold n first parameters in calling convention
	RegisterRef[MAX_REGS] paramRegs;
	int numParamRegs;
	// Stores registers that can be used to load a value. Can contain value at the same time
	RegisterRef[MAX_REGS] freeRegsStack;
	int numFreeRegs;

	// Mark register `reg` as used by value `valueRef`
	void markAsUsed(RegisterRef reg, IrRef valueRef)
	{
		writefln("markAsUsed R%s %%%s", cast(Register)reg.index, valueRef.index);
		assert(reg.isDefined);
		assert(valueRef.isDefined);
		regStates[reg.index].storedValue = valueRef;
	}

	void markAsDirty(RegisterRef reg)
	{
		writefln("markAsDirty R%s", cast(Register)reg.index);
		regStates[reg.index].isDirty = true;
	}

	void markAsFree(RegisterRef reg)
	{
		writefln("markAsFree R%s dirty %s", cast(Register)reg.index, regStates[reg.index].isDirty);
		regStates[reg.index].isDirty = false;
		freeRegsStack[numFreeRegs] = reg;
		++numFreeRegs;
	}

	// Does linear search and removes register from stack of free registers
	void removeFromFree(RegisterRef removedReg)
	{
		writefln("removeFromFree R%s", cast(Register)removedReg.index);
		foreach(i, regRef; freeRegsStack[0..numFreeRegs])
		if (regRef == removedReg)
		{
			freeRegsStack[i] = freeRegsStack[numFreeRegs-1];
			--numFreeRegs;
			return;
		}
		assert(false, "Removed register is not found");
	}

	// Returns free register from stack, or undefined otherwise
	RegisterRef tryAlloc(IrRef valueRef)
	{
		if (numFreeRegs == 0) return RegisterRef(); // return undefined ref
		--numFreeRegs;
		RegisterRef regRef = freeRegsStack[numFreeRegs];
		writefln("tryAlloc R%s", cast(Register)regRef.index);
		//assert(!regStates[regRef.index].isDirty);
		regStates[regRef.index].storedValue = valueRef;
		return regRef;
	}

	// Searches all register for spill candidate
	RegisterRef spillAlloc()
	{
		if (numFreeRegs == 0) return RegisterRef(); // return undefined ref
		return freeRegsStack[--numFreeRegs];
	}

	void printFree() {
		writef("free %s regs : ", numFreeRegs);
		foreach(reg; freeRegsStack[0..numFreeRegs])
		{
			writef("%s ", cast(Register)reg.index);
		}
	}
}

struct MachineState
{
	void setup() {
		// Microsoft x64 calling convention
		// The registers RAX, RCX, RDX, R8, R9, R10, R11 are considered volatile (caller-saved).
		// The registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, and R15 are considered nonvolatile (callee-saved).
		gprRegs.numFreeRegs = 7;
		gprRegs.freeRegsStack[0..gprRegs.numFreeRegs] = [
			cast(RegisterRef)Register.AX,
			cast(RegisterRef)Register.CX,
			cast(RegisterRef)Register.DX,
			cast(RegisterRef)Register.R8,
			cast(RegisterRef)Register.R9,
			cast(RegisterRef)Register.R10,
			cast(RegisterRef)Register.R11,
		];

		gprRegs.numParamRegs = 4;
		gprRegs.paramRegs[0..gprRegs.numParamRegs] = [
			cast(RegisterRef)Register.CX,
			cast(RegisterRef)Register.DX,
			cast(RegisterRef)Register.R8,
			cast(RegisterRef)Register.R9,
		];

		flagsReg.numFreeRegs = 1;
		flagsReg.freeRegsStack[0] = RegisterRef(0);
	}

	/// General purpose registers. Store integers, pointers, bools
	RegisterClass gprRegs;
	RegisterClass flagsReg;
}
*/


//                           #         #####   ######
//                           #           #     #     #
//                           #           #     #     #
//                           #           #     ######
//                           #           #     #   #
//                           #           #     #    #
//                           ######    #####   #     #
// -----------------------------------------------------------------------------

/// Generates LIR from IR
void pass_lir_gen(ref CompilationContext ctx) {
	//LirGen gen;
	//gen.ctx = &ctx;
	//gen.visit(&ctx.mod.irModule);
}
