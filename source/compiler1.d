/**
Copyright: Copyright (c) 2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// This is the simplest complete (not yet) compiler for C-like lang
module compiler1;

import std.string : format;
import std.typecons : Flag, Yes, No;
import std.stdio : writeln, write, writef, writefln;
import std.format : formattedWrite;
import std.range : repeat;

// Grammar

/**
	<module> = <declaration>* EOF
	<declaration> = <func_decl> / <var_decl> / <struct_decl>

	<func_decl> = <type> <identifier> "(" <param_list> ")" <block_statement>
	<param_list> = <parameter> "," <parameter_list> / <parameter>?
	<parameter> = <type> <identifier>

	<var_decl> = <type> <identifier> ";"
	<struct_decl> = "struct" <identifier> "{" <declaration>* "}"

	<statement> = "if" <paren_expression> <statement> ("else" <statement>)?
	              "while" <paren_expression> <statement> /
	              "do" <statement> "while" <paren_expression> ";" /
	              "return" <expression>? ";" /
	              "break" ";" /
	              "continue" ";" /
	              <block_statement> /
	              <expression> ";" /
	              <declaration_statement>

	<declaration_statement> = <declaration>
	<block_statement> = "{" <statement>* "}"

	<expression> = <test> | <identifier> "=" <expression>
	<test> = <sum> | <sum> ("=="|"!="|"<"|">"|"<="|"<=") <sum>
	<sum> = <term> / <sum> "+" <term> / <sum> "-" <term>
	<term> = <identifier> "(" <expression_list> ")" / <identifier> / <int_literal> / <paren_expression>
	<paren_expression> = "(" <expression> ")"

	<expression_list> = (<expression> ",")*
	<identifier> = [_a-zA-Z] [_a-zA-Z0-9]*

	<type> = <type_basic> / <type_user>
	<type_basic> = ("i8" | "i16" | "i32" | "i64" | "isize" |
		"u8" | "u16" | "u32" | "u64" | "usize" | "void" | "f32" | "f64")

	<type_user> = <identifier>

	<int_literal> = <literal_dec_int> / <literal_hex_int>
	<literal_dec_int> = 0|[1-9][0-9_]*
	<literal_hex_int> = ("0x"|"0X")[0-9A-Fa-f_]+
	<literal_bin_int> = ("0b"|"0B")[01_]+
	<literal_oct_int> = 0[01_]*
*/

//                  #     #     #      #####   #     #
//                  ##   ##     #        #     ##    #
//                  # # # #    ###       #     # #   #
//                  #  #  #    # #       #     #  #  #
//                  #     #   #####      #     #   # #
//                  #     #   #   #      #     #    ##
//                  #     #  ##   ##   #####   #     #
// -----------------------------------------------------------------------------
void main()
{
	string input =
		q{
		i32 a;
		struct structWIP {
			i64 e;
			i32 member(i32 param) {
				i32 c;
				i32 d;
				a = b + c + d + e + g;
			}
			i32 g;
		}
		i32 b;
	};

	string input2 = q{
		void e()
		{
			i32 a;
			i32 b;
			i32 c;

			void f() {
				i32 a;
				i32 b;
				i32 c;
				a = b + c;
			}

			void g() {
				i32 a;
				i32 b;

				void h() {
					i32 c;
					i32 d;
					c = a + d;
				}

				void i() {
					i32 b;
					i32 d;
					b = a + c;
				}

				b = a + c;
			}

			a = b + c;
		}
	};

	string input3 = q{
		A b;
		struct A{
			void fun(i32 param) {
				//a = 1;
				i32 a;
				a = (param + 1) - var + fun42();
			}
		}
		A a;
		i32 var;
		i32 fun42() { return 42; }
	};

	string input4 = q{
		struct A {
			int x;
			struct B { int y; }
			B b;
		}

		int i=0;
		int j=0;

		void f() {
			A a;
			a.x = 1+i*j;
			a.b.y = 2;
			bool b = 3 == a.x;
			if ( i < j ) f();
		}
	};

	// test implicit casting
	string input5 = q{
		void f() {
			//struct A{}
			//A a;
			//if (a){} // error
			f32 var_f32;
			f64 var_f64;
			//var_f32 = var_f64; // error
			var_f64 = var_f32;

			i8 var_i8;
			if (var_i8){}
			i16 var_i16;
			if (var_i16){}
			i32 var_i32;
			if (var_i32){}
			i64 var_i64;
			if (var_i64){}

			u8 var_u8;
			if (var_u8){}
			u16 var_u16;
			if (var_u16){}
			u32 var_u32;
			if (var_u32){}
			u64 var_u64;
			if (var_u64){}
		}
	};

	string input6 = q{
		void f() {
			f32 var_f32;
			f64 var_f64;
			var_f64 = var_f32;
			i8 var_i8;
			i16 var_i16;
			i32 var_i32;
			i64 var_i64;
			u8 var_u8;
			u16 var_u16;
			u32 var_u32;
			u64 var_u64;

			var_i8 + var_i16;
			var_i8 + var_i32;
			var_i8 + var_i64;
			var_i8 + var_u8;
			var_i8 + var_u16;
			var_i8 + var_u32;
			var_i8 + var_u64;
			var_i8 + var_f32;
			var_i8 + var_f64;

			var_i16 + var_i32;
			var_i16 + var_i64;
			var_i16 + var_u8;
			var_i16 + var_u16;
			var_i16 + var_u32;
			var_i16 + var_u64;
			var_i16 + var_f32;
			var_i16 + var_f64;

			var_i32 + var_i32;
			var_i32 + var_i64;
			var_i32 + var_u8;
			var_i32 + var_u16;
			var_i32 + var_u32;
			var_i32 + var_u64;
			var_i32 + var_f32;
			var_i32 + var_f64;

			var_i64 + var_i64;
			var_i64 + var_u8;
			var_i64 + var_u16;
			var_i64 + var_u32;
			var_i64 + var_u64;
			var_i64 + var_f32;
			var_i64 + var_f64;

			var_u8 + var_u8;
			var_u8 + var_u16;
			var_u8 + var_u32;
			var_u8 + var_u64;
			var_u8 + var_f32;
			var_u8 + var_f64;

			var_u16 + var_u16;
			var_u16 + var_u32;
			var_u16 + var_u64;
			var_u16 + var_f32;
			var_u16 + var_f64;

			var_u32 + var_u32;
			var_u32 + var_u64;
			var_u32 + var_f32;
			var_u32 + var_f64;

			var_u64 + var_u64;
			var_u64 + var_f32;
			var_u64 + var_f64;

			var_f32 + var_f32;
			var_f32 + var_f64;
		}
	};

	string input7 =
	q{i32 fib(i32 number)
	{
		if (number < 1) return 0;
		if (number < 3) return 1;
		return fib(number-1) + fib(number-2);
	}};

	string input8 =
q{i32 isNegative(i32 number)
{
	i32 result;
	if (number < 0) result = 1;
	else result = 0;
	return result;
}};

	auto time0 = currTime;
	IdentifierMap idMap;
	auto context = CompilationContext(input8, &idMap);

	try {
		Lexer lexer = Lexer(context.input);
		//foreach (tok; Lexer(input)) writefln("%s", tok.type);
		Parser parser = Parser(&lexer, &context);
		ModuleDeclNode* mod = parser.parseModule();
		context.throwOnErrors;

		auto time1 = currTime;
			auto astPrinter = AstPrinter(&context, 2);
			//astPrinter.printAst(cast(AstNode*)mod);
			context.throwOnErrors;

		auto time2 = currTime;
			auto scopeStack = ScopeStack(&context);
			auto sem1 = SemanticDeclarations(&context, &scopeStack);
			sem1.visit(mod);
			context.throwOnErrors;

		auto time3 = currTime;
			auto sem2 = SemanticLookup(&context, &scopeStack);
			sem2.visit(mod);
			context.throwOnErrors;

		auto time4 = currTime;
			auto sem3 = SemanticStaticTypes(&context);
			sem3.visit(mod);
			context.throwOnErrors;

		auto time5 = currTime;
			//astPrinter.printAst(cast(AstNode*)mod);
			context.throwOnErrors;

		auto time6 = currTime;
			//auto text_irgen = TextIrGenerationVisitor(&context);
			//text_irgen.visit(mod);
			//context.throwOnErrors;

		auto time7 = currTime;
			auto bin_irgen = BinIrGenerationVisitor(&context);
			bin_irgen.visit(mod);
			context.throwOnErrors;

		auto time8 = currTime;
			writeln("// source code");
			writeln(context.input);
			//writeln;
			//writeln("// Generated from AST directly");
			//writeln(text_irgen.sink.sink.text);
			TextSink sink;
			bin_irgen.irModule.dump(sink, &context);
			writeln;
			writeln("// Generated from binary IR");
			writeln(sink.text);

		auto time9 = currTime;

		writeln;
		writeln("// timing");
		writefln("parse %ss", scaledNumberFmt(time1-time0));
		writefln("print %ss", scaledNumberFmt(time2-time1));
		writefln("semantic insert %ss", scaledNumberFmt(time3-time2));
		writefln("semantic lookup %ss", scaledNumberFmt(time4-time3));
		writefln("semantic types %ss", scaledNumberFmt(time5-time4));
		writefln("print2 %ss", scaledNumberFmt(time6-time5));
		//writefln("IR text gen %ss", scaledNumberFmt(time7-time6));
		writefln("IR bin gen %ss", scaledNumberFmt(time8-time7));
	}
	catch(CompilationException e) {
		writeln(context.sink.text);
	}
}

struct CompilationContext
{
	string input;
	IdentifierMap* idMap;
	bool hasErrors;
	TextSink sink;

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

	void internal_error(Args...)(SourceLocation loc, string format, Args args)
	{
		sink.putf("file(%s, %s): ICE: ", loc.line+1, loc.col+1);
		sink.putfln(format, args);
		hasErrors = true;
		throw new CompilationException();
	}

	void throwOnErrors() {
		if (hasErrors) throw new CompilationException();
	}
}

class CompilationException : Exception { this(){ super(null); }}


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
	void toString()(scope void delegate(const(char)[]) sink) const {
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
	long getTokenNumber() { return numberRep; }

	int opApply(scope int delegate(Token) dg)
	{
		Token tok;
		while ((tok = nextToken()).type != TokenType.EOI)
			if (auto res = dg(tok))
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

	string get(Identifier id) {
		return strings[id];
	}

	Identifier find(string str) {
		return map.get(str, uint.max);
	}

	Identifier getOrReg(string str) {
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
	decl_parameter,
	decl_struct,

	stmt_block,
	stmt_if,
	stmt_while,
	stmt_do_while,
	stmt_return,
	stmt_break,
	stmt_continue,

	expr_var,
	expr_literal,
	expr_bin_op,
	expr_call,
	expr_type_conv,

	type_basic,
	type_user,
}

enum AstFlags {
	isDeclaration = 1 << 0,
	isScope       = 1 << 1,
	isExpression  = 1 << 2,
	isStatement   = 1 << 3,
	isType        = 1 << 4,
	isSymResolved = 1 << 5,
	isParameter   = 1 << 6,
}

mixin template AstNodeData(AstType _astType = AstType.abstract_node, int default_flags = 0) {
	//pragma(msg, format("%s %s", typeof(this).stringof, _astType));
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

mixin template TypeNodeData(AstType _astType) {
	mixin AstNodeData!(_astType, AstFlags.isType);
}

struct TypeNode {
	mixin AstNodeData!(AstType.abstract_node, AstFlags.isType);

	string typeName(CompilationContext* context) {
		if (&this == null) return null;
		assert(isType);
		if (astType == AstType.type_basic)
			return (cast(BasicTypeNode*)&this).strId;
		else if (astType == AstType.type_user)
			return (cast(UserTypeNode*)&this).strId(context);
		else assert(false, format("got %s", astType));
	}

	bool sameType(TypeNode* t2) {
		assert(isType, format("this is %s, not type", astType));
		assert(t2.isType, format("t2 is %s, not type", t2.astType));
		if (astType != t2.astType) return false;

		if (astType == AstType.type_basic)
			return (cast(BasicTypeNode*)&this).basicType == (cast(BasicTypeNode*)t2).basicType;
		else if (astType == AstType.type_user)
			return cast(void*)(&this) == cast(void*)(t2);

		assert(false, format("got %s %s", astType, t2.astType));
	}

	bool isVoid() {
		return astType == AstType.type_basic &&
			(cast(BasicTypeNode*)&this).basicType == BasicType.t_void;
	}
	bool isError() {
		return astType == AstType.type_basic &&
			(cast(BasicTypeNode*)&this).basicType == BasicType.t_error;
	}
}

enum BasicTypeFlag : ubyte {
	isFloat    = 1 << 0,
	isInteger  = 1 << 1,
	isUnsigned = 1 << 2,
	isBoolean  = 1 << 3,
}

BasicTypeNode basicTypeNode(int size, BasicType basicType, int typeFlags = 0)
{
	return BasicTypeNode(SourceLocation(), cast(int)size, basicType, cast(ubyte)typeFlags);
}

struct BasicTypeNode {
	mixin TypeNodeData!(AstType.type_basic);
	int size; // -1 arch dependent
	BasicType basicType;
	ubyte typeFlags;
	string strId() { return basicTypeNames[basicType]; }

	bool isFloat() { return cast(bool)(typeFlags & BasicTypeFlag.isFloat); }
	bool isInteger() { return cast(bool)(typeFlags & BasicTypeFlag.isInteger); }
	bool isUnsigned() { return cast(bool)(typeFlags & BasicTypeFlag.isUnsigned); }
	bool isBoolean() { return cast(bool)(typeFlags & BasicTypeFlag.isBoolean); }
}

struct UserTypeNode {
	mixin TypeNodeData!(AstType.type_user);
	mixin SymRefNodeData;
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
	ParameterDeclNode*[] parameters;
	BlockStmtNode* block_stmt;
	Scope* _scope;
	IrFunction irData;
}

struct VariableDeclNode {
	mixin AstNodeData!(AstType.decl_var, AstFlags.isDeclaration);
	mixin SymRefNodeData;
	TypeNode* type;
}

struct ParameterDeclNode {
	mixin AstNodeData!(AstType.decl_parameter, AstFlags.isDeclaration);
	mixin SymRefNodeData;
	TypeNode* type;
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

// ------------------------------- Expressions ---------------------------------
// -----------------------------------------------------------------------------
mixin template ExpressionNodeData(AstType _astType) {
	mixin AstNodeData!(_astType, AstFlags.isExpression);
	TypeNode* type;
	IrVar irVarData;
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
	EQUAL,
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
enum BinOp BIN_OP_ARITH_FIRST = BinOp.EQUAL;
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
			case decl_parameter: auto p = cast(ParameterDeclNode*)n; visit(p); break;
			case decl_struct: auto s = cast(StructDeclNode*)n; visit(s); break;
			case stmt_block: auto b = cast(BlockStmtNode*)n; visit(b); break;
			case stmt_if: auto i = cast(IfStmtNode*)n; visit(i); break;
			case stmt_while: auto w = cast(WhileStmtNode*)n; visit(w); break;
			case stmt_do_while: auto d = cast(DoWhileStmtNode*)n; visit(d); break;
			case stmt_return: auto r = cast(ReturnStmtNode*)n; visit(r); break;
			case stmt_break: auto b = cast(BreakStmtNode*)n; visit(b); break;
			case stmt_continue: auto c = cast(ContinueStmtNode*)n; visit(c); break;
			case expr_var: auto v = cast(VariableExprNode*)n; visit(v); break;
			case expr_literal: auto l = cast(LiteralExprNode*)n; visit(l); break;
			case expr_bin_op: auto b = cast(BinaryExprNode*)n; visit(b); break;
			case expr_call: auto c = cast(CallExprNode*)n; visit(c); break;
			case expr_type_conv: auto t = cast(TypeConvExprNode*)n; visit(t); break;
			case type_basic: auto t = cast(BasicTypeNode*)n; visit(t); break;
			case type_user: auto t = cast(UserTypeNode*)n; visit(t); break;
		}
	}
}

/*	// Visitor code
	void visit(ModuleDeclNode* m) {
		foreach (decl; m.declarations) _visit(decl); }
	void visit(FunctionDeclNode* f) {
		foreach (param; f.parameters) visit(param);
		visit(f.block_stmt); }
	void visit(VariableDeclNode* v) {}
	void visit(ParameterDeclNode* p) {}
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
	void visit(VariableExprNode* v) {}
	void visit(LiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {
		_visit(cast(AstNode*)b.left);
		_visit(cast(AstNode*)b.right); }
	void visit(CallExprNode* c) {
		foreach (arg; c.args) _visit(arg); }
	void visit(TypeConvExprNode* t) {
		_visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
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
		print("FUNC ", f.returnType.typeName(context), " ", f.strId(context));
		foreach (param; f.parameters) pr_node(cast(AstNode*)param);
		pr_node(cast(AstNode*)f.block_stmt);
	}
	void visit(VariableDeclNode* v) {
		print("VAR ", v.type.typeName(context), " ", v.strId(context));
	}
	void visit(ParameterDeclNode* p) {
		print("PARAM ", p.type.typeName(context), " ", p.strId(context)); }
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
	void visit(VariableExprNode* v) {
		if (v.isSymResolved)
			print("VAR_USE ", v.getSym.getType.typeName(context), " ", v.strId(context));
		else
			print("VAR_USE ", v.strId(context));
	}
	void visit(LiteralExprNode* c) { print("LITERAL ", c.type.typeName(context), " ", c.value); }
	void visit(BinaryExprNode* b) {
		if (b.type) print("BINOP ", b.type.typeName(context), " ", b.op);
		else print("BINOP ", b.op);
		pr_node(cast(AstNode*)b.left);
		pr_node(cast(AstNode*)b.right); }
	void visit(CallExprNode* c) {
		if (c.isSymResolved)
			print("CALL ", c.strId(context), " ", c.getSym.getType.typeName(context));
		else print("CALL ", c.strId(context));
		foreach (arg; c.args) pr_node(cast(AstNode*)arg); }
	void visit(TypeConvExprNode* t) {
		print("CAST to ", t.type.typeName(context));
		pr_node(cast(AstNode*)t.expr); }
	void visit(BasicTypeNode* t) { print("TYPE ", t.strId); }
	void visit(UserTypeNode* t) { print("TYPE ", t.strId(context)); }

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
//version = print_parse;
struct Parser {
	Lexer* lexer;
	CompilationContext* context;

	Token tok;

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
	T* makeExpr(T, Args...)(SourceLocation start, Args args) { return new T(start, null, IrVar(), IrRef(), args); }

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
		return context.idMap.getOrReg(name);
	}

	// ------------------------------ PARSING ----------------------------------

	ModuleDeclNode* parseModule() { // <module> ::= <declaration>*
		version(print_parse) writeln("parseModule");
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
		version(print_parse) writeln("parse_declaration");
		SourceLocation start = tok.loc;
		if (tok.type == TokenType.STRUCT_SYM) // <struct_declaration> ::= "struct" <id> "{" <declaration>* "}"
		{
			nextToken();
			Identifier structId = expectIdentifier();
			expectAndConsume(TokenType.LCURLY);
			AstNode*[] declarations = parse_declarations(TokenType.RCURLY);
			expectAndConsume(TokenType.RCURLY);
			return cast(AstNode*)make!StructDeclNode(start, declarations, structId);
		}
		else // <func_declaration> / <var_declaration>
		{
			TypeNode* type = parse_type();
			if (type is null) return null;
			Identifier declarationId = expectIdentifier();

			if (tok.type == TokenType.SEMICOLON) // <var_declaration> ::= <type> <id> ";"
			{
				nextToken();
				return cast(AstNode*)make!VariableDeclNode(start, declarationId, type);
			}
			else if (tok.type == TokenType.LPAREN) // <func_declaration> ::= <type> <id> "(" <param_list> ")" <block_statement>
			{
				expectAndConsume(TokenType.LPAREN);
				ParameterDeclNode*[] params;
				while (tok.type != TokenType.RPAREN)
				{
					TypeNode* paramType = parse_type_expected();
					Identifier paramId = expectIdentifier();
					params ~= make!ParameterDeclNode(start, paramId, paramType);
					if (tok.type == TokenType.COMMA) nextToken();
					else break;
				}
				expectAndConsume(TokenType.RPAREN);
				BlockStmtNode* block = block_stmt();
				return cast(AstNode*)make!FunctionDeclNode(start, declarationId, type, params, block);
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
		version(print_parse) writeln("parse_type_expected");
		auto type = parse_type();
		if (type is null) context.unrecoverable_error(tok.loc, "Expected basic type, while got '%s'", lexer.getTokenString(tok));
		return type;
	}

	TypeNode* parse_type()
	{
		version(print_parse) writeln("parse_type");
		SourceLocation start = tok.loc;
		if (tok.type == TokenType.IDENTIFIER) {
			Identifier id = expectIdentifier();
			return cast(TypeNode*)make!UserTypeNode(start, id);
		} else if (isBasicTypeToken(tok.type)) {
			return context.basicTypeNodes(parse_type_basic());
		}
		return null;
	}

	BasicType parse_type_basic()
	{
		version(print_parse) writeln("parse_type_basic");
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
		version(print_parse) writeln("block_stmt");
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
		version(print_parse) writeln("statement");
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
				return cast(AstNode*)make!BlockStmtNode(start, null); // TODO: make this error
			case TokenType.LCURLY:  /* "{" { <statement> } "}" */
				return cast(AstNode*)block_stmt();
			default:
			{
				if (isBasicTypeToken(tok.type) || tok.type == TokenType.STRUCT_SYM) // declaration
				{
					AstNode* decl = parse_declaration;
					return decl;
				}
				else if (tok.type == TokenType.IDENTIFIER)
				{
					stashToken(); // copy to stash
					lexToken(); // lex into tok

					if (tok.type == TokenType.IDENTIFIER) // declaration
					{
						stashToken();
						nextToken(); // move from stash to tok
						AstNode* decl = parse_declaration;
						return decl;
					}
					else // expression
					{
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
		version(print_parse) writeln("paren_expr");
		expectAndConsume(TokenType.LPAREN);
		auto res = expr();
		expectAndConsume(TokenType.RPAREN);
		return res;
	}

	ExpressionNode* expr() { /* <expr> ::= <test> | <id> "=" <expr> */
		version(print_parse) writeln("expr");
		SourceLocation start = tok.loc;
		ExpressionNode* t, n;
		if (tok.type != TokenType.IDENTIFIER) return test();
		n = test();
		if (n.astType == AstType.expr_var && tok.type == TokenType.EQUAL)
		{
			nextToken();
			t = n;
			n = cast(ExpressionNode*)makeExpr!BinaryExprNode(start, BinOp.EQUAL, t, expr());
		}
		return n;
	}

	ExpressionNode* test() { /* <test> ::= <sum> | <sum> "<" <sum> */
		version(print_parse) writeln("test");
		SourceLocation start = tok.loc;
		ExpressionNode* t, n = sum();


/*
		switch(tok.type)
		{
			case TokenType.LESS: goto case;
			case TokenType.LESS_EQUAL: goto case;
			case TokenType.GREATER: goto case;
			case TokenType.GREATER_EQUAL:
				nextToken();
				t = n;
				n = cast(ExpressionNode*)makeExpr!BinaryExprNode(start, BinOp.LESS, t, sum());
				break;
		}*/
		if (tok.type == TokenType.LESS)
		{
			nextToken();
			t = n;
			n = cast(ExpressionNode*)makeExpr!BinaryExprNode(start, BinOp.LESS, t, sum());
		}
		return n;
	}

	ExpressionNode* sum() { /* <sum> ::= <term> | <sum> "+" <term> | <sum> "-" <term> */
		version(print_parse) writeln("sum");
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
			n = cast(ExpressionNode*)makeExpr!BinaryExprNode(start, op, t, term());
		}
		return n;
	}

	ExpressionNode* term() /* <term> ::= <id> | <id> "(" <expr_list> ")" | <int> | <paren_expr> */
	{
		version(print_parse) writeln("term");
		SourceLocation start = tok.loc;
		if (tok.type == TokenType.IDENTIFIER)
		{
			Identifier id = expectIdentifier();
			if (tok.type == TokenType.LPAREN) // <term> ::= <id> "(" <expr_list> ")"
			{
				expectAndConsume(TokenType.LPAREN);
				ExpressionNode*[] args = expr_list();
				expectAndConsume(TokenType.RPAREN);
				return cast(ExpressionNode*)makeExpr!CallExprNode(start, id, args);
			}
			return cast(ExpressionNode*)makeExpr!VariableExprNode(start, id);
		}
		else if (tok.type == TokenType.INT_LITERAL)
		{
			long value = lexer.getTokenNumber();
			nextToken();
			TypeNode* type = context.basicTypeNodes(BasicType.t_i32);
			return cast(ExpressionNode*)make!LiteralExprNode(start, type, IrVar(), IrRef(), value);
		}
		else return paren_expr();
	}

	ExpressionNode*[] expr_list() // <expr_list> ::= (<expr> ",")*
	{
		version(print_parse) writeln("expr_list");
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
	IrRef irRef;

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
			case decl_parameter: return (cast(ParameterDeclNode*)node).type;
			case expr_var: .. case expr_call:
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
	Scope*[] childrenScopes;
	Symbol**[] fixups;
	string debugName;
	bool isOrdered;
}

/// Used for semantic analysis
struct ScopeStack
{
	CompilationContext* context;
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

/// Register
struct SemanticDeclarations {
	mixin AstVisitorMixin;

	CompilationContext* context;
	ScopeStack* scopeStack;

	void visit(ModuleDeclNode* m) {
		m._scope = scopeStack.pushScope("Module", No.ordered);
		foreach (decl; m.declarations) _visit(decl);
		scopeStack.popScope1;
	}
	void visit(FunctionDeclNode* f) {
		f.resolveSymbol = scopeStack.insert(f.id, f.loc, SymbolClass.c_function, cast(AstNode*)f);
		f._scope = scopeStack.pushScope(context.idString(f.id), Yes.ordered);
		foreach (param; f.parameters) visit(param);
		visit(f.block_stmt);
		scopeStack.popScope1;
	}
	void visit(VariableDeclNode* v) {
		v.resolveSymbol = scopeStack.insert(v.id, v.loc, SymbolClass.c_variable, cast(AstNode*)v);
	}
	void visit(ParameterDeclNode* p) {
		p.resolveSymbol = scopeStack.insert(p.id, p.loc, SymbolClass.c_variable, cast(AstNode*)p);
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
	void visit(VariableExprNode* v) {}
	void visit(LiteralExprNode* c) {}
	void visit(BinaryExprNode* b) {}
	void visit(CallExprNode* c) {}
	void visit(TypeConvExprNode* c) {}
	void visit(BasicTypeNode* t) {}
	void visit(UserTypeNode* t) {}
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
		visit(f.block_stmt);
		scopeStack.popScope2;
	}
	void visit(VariableDeclNode* v) { _visit(v.type); }
	void visit(ParameterDeclNode* p) { _visit(p.type); }
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
	void visit(VariableExprNode* v) { v.resolveSymbol = scopeStack.lookup(v.id, v.loc); }
	void visit(LiteralExprNode* c) {}
	void visit(BinaryExprNode* b) { _visit(b.left); _visit(b.right); }
	void visit(CallExprNode* c) {
		c.resolveSymbol = scopeStack.lookup(c.id, c.loc);
		foreach (arg; c.args) _visit(arg); }
	void visit(TypeConvExprNode* t) { _visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
	void visit(UserTypeNode* t) { t.resolveSymbol = scopeStack.lookup(t.id, t.loc); }
}

/// Annotates all expression nodes with their type
struct SemanticStaticTypes {
	mixin AstVisitorMixin;

	CompilationContext* context;

	bool isBool(TypeNode* type)
	{
		return
			type.astType == AstType.type_basic &&
			(cast(BasicTypeNode*)&this).basicType == BasicType.t_bool;
	}

	/// Returns true if types are equal or were converted to common type. False otherwise
	bool autoconvToCommonType(ref ExpressionNode* left, ref ExpressionNode* right)
	{
		if (left.type.astType == AstType.type_basic && right.type.astType == AstType.type_basic)
		{
			BasicTypeNode* leftType = cast(BasicTypeNode*)left.type;
			BasicTypeNode* rightType = cast(BasicTypeNode*)right.type;

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
			BasicType fromType = (cast(BasicTypeNode*)expr.type).basicType;
			bool canConvert = isAutoConvertibleFromToBasic[fromType][toType];
			if (canConvert || force)
			{
				expr = cast(ExpressionNode*) new TypeConvExprNode(expr.loc, type, IrVar(), IrRef(), expr);
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
			BasicType fromType = (cast(BasicTypeNode*)expr.type).basicType;
			BasicType toType = (cast(BasicTypeNode*)type).basicType;
			bool canConvert = isAutoConvertibleFromToBasic[fromType][toType];
			if (canConvert)
			{
				expr = cast(ExpressionNode*) new TypeConvExprNode(expr.loc, type, IrVar(), IrRef(), expr);
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
			case EQUAL_EQUAL: goto case;
			case NOT_EQUAL: goto case;
			case GREATER: goto case;
			case GREATER_EQUAL: goto case;
			case LESS: goto case;
			case LESS_EQUAL:
				if (b.left.type is b.right.type)
					resRype = context.basicTypeNodes(BasicType.t_bool);
				else
					context.error(b.left.loc, "Cannot compare `%s` and `%s`",
						b.left.type.typeName(context),
						b.right.type.typeName(context));
				break;

			// arithmetic op int float
			case MINUS: goto case;
			case PLUS: goto case;
			case SLASH: goto case;
			case STAR:
				if (autoconvToCommonType(b.left, b.right))
					resRype = b.left.type;
				else
				{
					context.error(b.left.loc, "Cannot perform `%s` %s `%s` operation",
						b.left.type.typeName(context), b.op,
						b.right.type.typeName(context));
				}
				break;

			case EQUAL: // int float
				autoconvTo(b.right, b.left.type);
				resRype = b.right.type;
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
	}

	void calcType(BinaryExprNode* b)
	{
		assert(b.left.type, format("left(%s).type: is null", b.left.astType));
		assert(b.right.type, format("right(%s).type: is null", b.right.astType));

		setResultType(b);
	}

	void visit(ModuleDeclNode* m) {
		foreach (decl; m.declarations) _visit(decl);
	}
	void visit(FunctionDeclNode* f) {
		foreach (param; f.parameters) visit(param);
		visit(f.block_stmt);
	}
	void visit(VariableDeclNode* v) { _visit(v.type); }
	void visit(ParameterDeclNode* p) {}
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
	void visit(ReturnStmtNode* r) { if (r.expression) _visit(r.expression); }
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	// Get type from variable declaration
	void visit(VariableExprNode* v) {
		v.type = v.getSym.getType;
	}
	void visit(LiteralExprNode* c) {
		//v.type =
	}
	void visit(BinaryExprNode* b) { _visit(b.left); _visit(b.right); calcType(b); }
	// Get type from function declaration
	void visit(CallExprNode* c) {
		foreach (arg; c.args) _visit(arg);
		c.type = c.getSym.getType;
	}
	void visit(TypeConvExprNode* t) { _visit(t.type); _visit(t.expr); }
	void visit(BasicTypeNode* t) {}
	void visit(UserTypeNode* t) {}
}


//                               #####   ######
//                                 #     #     #
//                                 #     #     #
//                                 #     ######
//                                 #     #   #
//                                 #     #    #
//                               #####   #     #
// -----------------------------------------------------------------------------
struct IrModule
{
	IrFunction*[] functions;

	void addFunction(IrFunction* fun)
	{
		functions ~= fun;
	}

	void dump(ref TextSink sink, CompilationContext* context)
	{
		foreach (func; functions) func.dump(sink, context);
	}
}

struct IrFunction
{
	IrName name;

	TypeNode* returnType;

	/// The first Basic Block of the function
	/// Also the first node in the linked list of Basic Blocks
	IrBasicBlock* start;

	/// This is the last Basic Block in the linked list of blocks of this function
	IrBasicBlock* last;

	/// This array is indexed with -int.max..-1 IrRef values. -1 is index 0, -2 is 1 etc.
	IrConstant[] constants;

	/// This array begins with all parameters
	/// This array is indexed with 0..int.max IrRef values
	IrVar[] variables;

	/// Parameters are saved as first `numParameters` items of `variables` array
	size_t numParameters;

	IrVar[] parameters() { return variables[0..numParameters]; }

	IrVarOrConstant deref(IrRef reference) {
		if (reference.index > 0) return IrVarOrConstant(variables[ reference.index-1]);
		if (reference.index < 0) return IrVarOrConstant(constants[-reference.index-1]);
		assert(false, "Attempting to deref zero reference");
	}

	/// Automatically sets `start`, sets last and links blocks together
	IrBasicBlock* addBasicBlock(IrName name)
	{
		auto newBlock = new IrBasicBlock(name);
		if (start is null) start = newBlock;
		else last.next = newBlock;
		last = newBlock;
		return newBlock;
	}

	IrRef addConstant(IrConstant constant)
	{
		constants ~= constant;
		int index = -cast(int)constants.length;
		return IrRef(index);
	}

	IrRef addVariable(IrVar variable)
	{
		variables ~= variable;
		int index = cast(int)variables.length;
		return IrRef(index);
	}

	void dump(ref TextSink sink, CompilationContext* context)
	{
		sink.putf("function %s $%s (", returnType.typeName(context), name);
		foreach (i, param; parameters)
		{
			sink.putf("%s %%%s", param.type.typeName(context), param.name);
			if (i+1 < parameters.length) sink.put(", ");
		}
		sink.putln(") {");

		for (IrBasicBlock* block = start; block; block = block.next)
		{
			sink.putfln("  @%s", block.name);

			// print all instructions
			foreach(instr_i, ref instr; block.instructions)
			{
				final switch(instr.op) with(IrOpcode) {
					case o_eq: goto case;
					case o_ne: goto case;
					case o_gt: goto case;
					case o_ge: goto case;
					case o_lt: goto case;
					case o_le: goto case;
					// Arithmetic
					case o_add:  goto case;
					case o_sub:  goto case;
					case o_div:  goto case;
					case o_rem:  goto case;
					case o_udiv: goto case;
					case o_urem: goto case;
					case o_mul:  goto case;
					case o_assign:
						sink.putfln("    %s = %s %s, %s",
							deref(instr.to),
							irOpcodeNames[instr.op],
							deref(instr.arg0),
							deref(instr.arg1));
						break;
					case o_call:
						if (instr.to.isZero)
							sink.putf("    call $%s(", context.idString(instr.callee.id));
						else
							sink.putf("    %s = call $%s(", deref(instr.to), context.idString(instr.callee.id));
						foreach (arg_i, ref arg; block.instructions[instr_i+1..instr_i+instr.numArgs+1])
						{
							sink.putf("%s", deref(arg.arg0));
							if (arg_i+1 < instr.numArgs) sink.put(", ");
						}
						sink.putln(")");
						break;
					case o_arg: break;
				}
			}

			// print jump/return at the end of block
			final switch(block.exit.type) with(IrJump.Type) {
				case none: sink.putln("  // block not sealed"); break;
				case ret0: sink.putln("    return"); break;
				case ret1: sink.putfln("    return %s", deref(block.exit.value)); break;
				case jmp:  sink.putfln("    jmp @%s", block.outs[0].name); break;
				case jnz:  sink.putfln("    jnz %s @%s, @%s",
					deref(block.exit.value), block.outs[0].name, block.outs[1].name); break;
			}
		}
		sink.putln("}");
	}
}

struct IrVarOrConstant
{
	this(ref IrVar var) { isVar = true; varPtr = &var; }
	this(ref IrConstant con) { isVar = false; conPtr = &con; }
	bool isVar;
	union {
		IrVar* varPtr;
		IrConstant* conPtr;
	}
	void toString()(scope void delegate(const(char)[]) sink) {
		if (isVar) { sink("%"); varPtr.name.toString(sink); }
		else conPtr.toString(sink);
	}
}

struct IrConstant
{
	long value;
	void toString()(scope void delegate(const(char)[]) sink) {
		sink.formattedWrite("%s", value);
	}
}

struct IrBasicBlock
{
	IrName name;

	/// BBs that jump to this block
	IrBasicBlock*[] ins;

	/// BBs this block jumps to
	IrBasicBlock*[] outs;

	/// The jump or return that must be the last instruction is stored in `exit`
	IrInstruction[] instructions;

	/// Specifies the last instruction of this Basic Block
	/// Jumps use `outs` for targets
	IrJump exit;

	/// Next node in linked listed of Basic Blocks of the function
	IrBasicBlock* next;

	void emit(IrInstruction instr)
	{
		instructions ~= instr;
	}

	bool isFinished() { return exit.type != IrJump.Type.none; }
}

/// Jumps use information stored in basic block
struct IrJump
{
	enum Type
	{
		none, /// Basic Block is not yet complete
		ret0, /// without return value
		ret1, /// with return value
		jmp,  /// unconditional jump
		jnz   /// conditional jump
	}

	Type type;
	IrRef value; /// return value reference, or condition for jnz
}

enum IrOpcode : ubyte
{
	o_eq,
	o_ne,
	o_gt,
	o_ge,
	o_lt,
	o_le,

	// Arithmetic
	o_add,
	o_sub,
	o_div,
	o_rem,
	o_udiv,
	o_urem,
	o_mul,

	o_call,
	o_arg,
	o_assign,

	//o_and,
	//o_or,
	//o_xor,
	//o_sar,
	//o_shr,
	//o_shl,
}

enum IrOpcode_num_opcodes = IrOpcode.max+1;

string[IrOpcode_num_opcodes] irOpcodeNames = ["eq", "ne", "gt", "ge", "lt", "le", "add", "sub",
"div", "rem", "udiv", "urem", "mul", "call", "arg", "assign", ];

immutable IrOpcode[] binOpToIrOpcode = [
	IrOpcode.o_eq,
	IrOpcode.o_ne,
	IrOpcode.o_gt,
	IrOpcode.o_ge,
	IrOpcode.o_lt,
	IrOpcode.o_le,

	IrOpcode.o_assign,
	IrOpcode.o_sub,
	IrOpcode.o_add,
	IrOpcode.o_div,
	IrOpcode.o_mul];

struct IrInstruction
{
	this(IrOpcode op, IrRef to) { this.op = op; this.to = to; }
	this(IrOpcode op, IrRef to, IrRef arg0) { this.op = op; this.to = to; this.arg0 = arg0; }
	this(IrOpcode op, IrRef to, IrRef arg0, IrRef arg1) { this.op = op; this.to = to; this.arg0 = arg0; this.arg1 = arg1; }
	//this(IrOpcode op, IrRef to, size_t value) { this.op = op; this.to = to; this.value = value; }
	this(IrOpcode op, IrRef to, size_t numArgs, Symbol* callee) { this.op = op; this.to = to; this.numArgs = numArgs; this.callee = callee; }

	IrOpcode op;
	IrRef to;
	union {
		struct { IrRef arg0, arg1; }
		//struct { size_t value;     }
		struct { size_t numArgs; Symbol* callee; }
	}
}

struct IrRef
{
	/// Negative indicies represent constants
	/// Positive represent a result of an instruction
	/// Zero is used when no ref is given
	int index;

	/// Returns true if reference has no destination
	bool isZero() { return index == 0; }
}

struct IrName
{
	string name;
	int suffix;
	void toString()(scope void delegate(const(char)[]) sink) {
		if (suffix == 0) sink.formattedWrite("%s", name);
		else sink.formattedWrite("%s_%s", name, suffix);
	}
}

struct IrVar
{
	IrName name;
	TypeNode* type;
}

struct TextIrGenerationVisitor {
	mixin AstVisitorMixin;
	CompilationContext* context;

	IndentTextSink sink;
	int varSuffixCounter;
	int uniqueSuffix() { return ++varSuffixCounter; }

	IrVar makeVar(string name) { return IrVar(IrName(name, uniqueSuffix)); }
	IrVar makeVar(Identifier id) { return IrVar(IrName(context.idString(id), uniqueSuffix)); }

	void visit(ModuleDeclNode* m) {
		foreach (decl; m.declarations) _visit(decl); }
	void visit(FunctionDeclNode* f) {
		sink.putf("function %s $%s (",
			f.returnType.typeName(context),
			context.idString(f.id));
		foreach (i, param; f.parameters)
		{
			visit(param);
			if (i+1 < f.parameters.length) sink.sink.put(", ");
		}
		sink.sink.putln(")");
		visit(f.block_stmt);
	}
	void visit(VariableDeclNode* v) {}
	void visit(ParameterDeclNode* p) {
		sink.sink.putf("%s %s", p.type.typeName(context), context.idString(p.id));
	}
	void visit(StructDeclNode* s) {
		sink.putln("{"); sink.push;
		foreach (decl; s.declarations) _visit(decl);
		sink.pop; sink.putln("}");
	}
	void visit(BlockStmtNode* b) {
		sink.putln("{"); sink.push;
		sink.putln("@start");
		foreach (stmt; b.statements) _visit(stmt);
		sink.pop; sink.putln("}");
	}
	void visit(IfStmtNode* i) {
		int suffix = uniqueSuffix;
		sink.putfln("@ifstmt_%s", suffix);
		sink.push;
		_visit(cast(AstNode*)i.condition);
		if (i.elseStatement)
			sink.putfln("jnz %s, @ift_%s, @iff_%s", i.condition.irVarData.name, suffix, suffix);
		else
			sink.putfln("jnz %s, @ift_%s, @ifend_%s", i.condition.irVarData.name, suffix, suffix);
		sink.pop;
		sink.putfln("@ift_%s", suffix);
		sink.push;
		_visit(cast(AstNode*)i.thenStatement);
		sink.pop;
		sink.push;
		if (i.elseStatement)
		{
			sink.putfln("@iff_%s", suffix);
			_visit(i.elseStatement);
		}
		sink.pop;
		sink.putfln("@ifend_%s", suffix);
	}
	void visit(WhileStmtNode* w) {
		_visit(cast(AstNode*)w.condition);
		_visit(cast(AstNode*)w.statement); }
	void visit(DoWhileStmtNode* d) {
		_visit(cast(AstNode*)d.condition);
		_visit(cast(AstNode*)d.statement); }
	void visit(ReturnStmtNode* r) {
		if (r.expression)
		{
			_visit(r.expression);
			sink.putfln("ret %s", r.expression.irVarData.name);
		}
		else sink.putln("ret");
	}
	void visit(BreakStmtNode* r) {}
	void visit(ContinueStmtNode* r) {}
	void visit(VariableExprNode* v) {
		v.irVarData = makeVar(v.id);
	}
	void visit(LiteralExprNode* c) {
		c.irVarData = makeVar("lit");
		sink.putfln("%s = %s", c.irVarData.name, c.value);
	}
	void visit(BinaryExprNode* b) {
		_visit(cast(AstNode*)b.left);
		_visit(cast(AstNode*)b.right);
		b.irVarData = makeVar("temp");
		sink.putfln("%s = %s %s, %s", b.irVarData.name, b.op, b.left.irVarData.name, b.right.irVarData.name);
	}
	void visit(CallExprNode* c) {
		foreach (arg; c.args) _visit(arg);
		c.irVarData = makeVar("res");
		if (c.type.isVoid) sink.putf("call %s (", c.strId(context));
		else sink.putf("%s = call %s (", c.irVarData.name, c.strId(context));
		foreach (i, arg; c.args)
		{
			sink.sink.putf("%s", arg.irVarData.name);
			if (i+1 < c.args.length) sink.sink.put(", ");
		}
		sink.sink.putln(")");
	}
	void visit(TypeConvExprNode* t) {
		_visit(t.type); _visit(t.expr);
		t.irVarData = makeVar("conv");
		sink.putfln("%s = cast(%s) %s", t.irVarData.name,
			t.type.typeName(context), t.expr.irVarData.name);
	}
	void visit(BasicTypeNode* t) {}
	void visit(UserTypeNode* t) {}
}

struct BinIrGenerationVisitor {
	mixin AstVisitorMixin;
	CompilationContext* context;

	IrModule irModule;
	IrFunction* currentFunction;
	IrBasicBlock* currentBB;

	Identifier tempId;

	int varSuffixCounter;
	int bbCounter;
	int thenCounter;
	int elseCounter;
	int uniqueSuffix() { return ++varSuffixCounter; }

	IrVar makeVar(string name, TypeNode* type) { return IrVar(IrName(name, uniqueSuffix), type); }
	IrVar makeVar(Identifier id, TypeNode* type) { return IrVar(IrName(context.idString(id)), type); }
	IrVar makeVarSuf(Identifier id, TypeNode* type) { return IrVar(IrName(context.idString(id), uniqueSuffix), type); }

	void visit(ModuleDeclNode* m)
	{
		tempId = context.idMap.getOrReg("__tmp");
		foreach (decl; m.declarations) _visit(decl);
	}
	void visit(FunctionDeclNode* f)
	{
		// create new function
		f.irData = IrFunction(IrName(context.idString(f.id)), f.returnType);

		// save previous function
		auto prevFunc = currentFunction;

		// set new function as current
		currentFunction = &f.irData;
		irModule.addFunction(&f.irData);

		foreach (i, param; f.parameters)
		{
			visit(param);
		}

		// create Basic Block for function body
		// other code will use `last` Basic Block
		currentBB = currentFunction.addBasicBlock(IrName("start"));

		visit(f.block_stmt);

		// restore previous function
		currentFunction = prevFunc;
	}
	void visit(VariableDeclNode* v)
	{
		v.getSym.irRef = currentFunction.addVariable(makeVar(v.id, v.type));
	}
	void visit(ParameterDeclNode* p)
	{
		p.getSym.irRef = currentFunction.addVariable(makeVar(p.id, p.type));
		++currentFunction.numParameters;

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
			if (currentBB.isFinished) break;
		}
	}
	void visit(IfStmtNode* i)
	{
		_visit(cast(AstNode*)i.condition);
		// prevBB links to thenBB and (afterBB or elseBB)
		auto prevBB = currentBB;
		prevBB.exit = IrJump(IrJump.Type.jnz, i.condition.irRef);

		// create Basic Block for then statement. It links to afterBB
		currentBB = currentFunction.addBasicBlock(IrName("then", ++thenCounter));
		auto thenBB = currentBB;
		prevBB.outs ~= thenBB;

		_visit(cast(AstNode*)i.thenStatement);

		if (i.elseStatement) currentBB = currentFunction.addBasicBlock(IrName("else", ++elseCounter));

		auto afterBB = currentFunction.addBasicBlock(IrName("blk", ++bbCounter));
		if (!thenBB.isFinished)
		{
			thenBB.exit = IrJump(IrJump.Type.jmp);
			thenBB.outs ~= afterBB;
		}

		if (i.elseStatement)
		{
			auto elseBB = currentBB;
			_visit(i.elseStatement);
			prevBB.outs ~= elseBB;
			if (!elseBB.isFinished)
			{
				elseBB.exit = IrJump(IrJump.Type.jmp);
				elseBB.outs ~= afterBB;
			}
		}
		else
		{
			prevBB.outs ~= afterBB;
		}

		currentBB = afterBB;
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
			currentBB.exit = IrJump(IrJump.Type.ret1, r.expression.irRef);
		}
		else currentBB.exit = IrJump(IrJump.Type.ret0);
		//currentBB = currentFunction.addBasicBlock(IrName("unreachable"));
	}
	void visit(BreakStmtNode* b) {}
	void visit(ContinueStmtNode* c) {}
	void visit(VariableExprNode* v) {
		//writefln("VariableExprNode %s", currentFunction.deref(v.getSym.irRef));
		v.irRef = v.getSym.irRef;
	}
	void visit(LiteralExprNode* c) {
		c.irRef = currentFunction.addConstant(IrConstant(c.value));
		//writefln("LiteralExprNode %s", currentFunction.deref(c.irRef));
	}
	void visit(BinaryExprNode* b) {
		_visit(cast(AstNode*)b.left);
		_visit(cast(AstNode*)b.right);
		b.irRef = currentFunction.addVariable(makeVarSuf(tempId, b.type));
		auto instr = IrInstruction(binOpToIrOpcode[b.op], b.irRef, b.left.irRef, b.right.irRef);
		currentBB.emit(instr);
	}
	void visit(CallExprNode* c) {
		foreach (arg; c.args) _visit(arg);
		c.irRef = currentFunction.addVariable(makeVarSuf(tempId, c.getSym.getType));
		IrRef destination;
		if (!c.type.isVoid) destination = c.irRef;
		currentBB.emit(IrInstruction(IrOpcode.o_call, destination, c.args.length, c.getSym));
		foreach (arg; c.args)
		{
			currentBB.emit(IrInstruction(IrOpcode.o_arg, IrRef(), arg.irRef));
		}
	}
	void visit(TypeConvExprNode* t) {
		//_visit(t.type); _visit(t.expr);
		//t.irVarData = makeVarSuf("conv");
		//sink.putfln("%s = cast(%s) %s", t.irVarData,
		//	t.type.typeName(context), t.expr.irVarData);
	}
	void visit(BasicTypeNode* t) {}
	void visit(UserTypeNode* t) {}
}

struct IrToSSAVisitor
{
	CompilationContext* context;

	void visit(IrModule* m) { foreach (func; m.functions) visit(m); }

	void visit(IrFunction* func)
	{
		for (IrBasicBlock* block = func.start; block; block = block.next)
		{
			localValueNumbering(func, block);
		}
	}

	void localValueNumbering(IrFunction* func, IrBasicBlock* block)
	{

	}
}


//              #     #  #######   #####   #         #####
//              #     #     #        #     #        #     #
//              #     #     #        #     #        #
//              #     #     #        #     #         #####
//              #     #     #        #     #              #
//              #     #     #        #     #        #     #
//               #####      #      #####   ######    #####
// -----------------------------------------------------------------------------
import std.datetime : MonoTime, Duration, usecs, dur;
MonoTime currTime() { return MonoTime.currTime(); }

struct ScaledNumberFmt(T)
{
	T value;
	void toString()(scope void delegate(const(char)[]) sink)
	{
		int scale = calcScale(value);
		auto scaledValue = scaled(value, -scale);
		int digits = numDigitsInNumber(scaledValue);
		import std.format : formattedWrite;
		sink.formattedWrite("%*.*f%s", digits, 3-digits, scaledValue,
			scaleSuffixes[scaleToScaleIndex(scale)]);
	}
}

auto scaledNumberFmt(T)(T value)
{
	return ScaledNumberFmt!T(value);
}

auto scaledNumberFmt(Duration value, double scale = 1)
{
	double seconds = value.total!"hnsecs" / 10_000_000.0;
	return ScaledNumberFmt!double(seconds * scale);
}

// -24 .. 24, with step of 3. Or -8 to 8 with step of 1
immutable string[] scaleSuffixes = ["y","z","a","f","p","n","u","m","",
"K","M","G","T","P","E","Z","Y"];

int numDigitsInNumber(Num)(const Num val)
{
	import std.math: abs;
	ulong absVal = cast(ulong)abs(val);
	int numDigits = 1;

	while (absVal >= 10)
	{
		absVal /= 10;
		++numDigits;
	}

	return numDigits;
}

int calcScale(Num)(Num val)
{
	import std.algorithm: clamp;
	import std.math: abs, floor, ceil, log10;
	static int signum(T)(const T x) nothrow
	{
	    return (x > 0) - (x < 0);
	}

	auto lg = log10(abs(val));
	int logSign = signum(lg);
	double absLog = abs(lg);

	int scale;
	if (lg < 0)
		scale = cast(int)(ceil(absLog/3.0))*3;
	else
		scale = cast(int)(floor(absLog/3.0))*3;

	int clampedScale = clamp(scale * logSign, -24, 24);

	return clampedScale;
}

int scaleToScaleIndex(int scale)
{
	return scale / 3 + 8; // -24..24 -> -8..8 -> 0..16
}

double scaled(Num)(Num num, int scale)
{
	import std.math: pow;
	return num * pow(10.0, scale);
}

T nextPOT(T)(T x)
{
	--x;
	x |= x >> 1;  // handle  2 bit numbers
	x |= x >> 2;  // handle  4 bit numbers
	x |= x >> 4;  // handle  8 bit numbers
	static if (T.sizeof >= 2) x |= x >> 8;  // handle 16 bit numbers
	static if (T.sizeof >= 4) x |= x >> 16; // handle 32 bit numbers
	static if (T.sizeof >= 8) x |= x >> 32; // handle 64 bit numbers
	++x;

	return x;
}

struct IndentTextSink
{
	TextSink sink;
	int indentSize = 2;
	private int indent;

	void putIndent() { sink.putf("%s", ' '.repeat(indent)); }
	void put(in char[] str) { putIndent; sink.put(str); }
	void putf(Args...)(const(char)[] fmt, Args args) { putIndent; sink.putf(fmt, args); }
	void putfln(Args...)(const(char)[] fmt, Args args) { putIndent; sink.putfln(fmt, args); }
	void putln(const(char)[] str = null) { putIndent; sink.putln(str); }

	void push() { indent += indentSize; }
	void pop() { indent -= indentSize; }
}

struct TextSink
{
	import std.format : formattedWrite;
	import std.string : stripRight;

	Buffer!char data;

	void clear() { data.clear(); }
	string text() { return stripRight(cast(string)data.data); }

	void put(in char[] str)
	{
		if (str.length == 0) return;
		data.put(str);
		data.stealthPut('\0');
	}

	void putf(Args...)(const(char)[] fmt, Args args) { formattedWrite(&this, fmt, args); }
	void putfln(Args...)(const(char)[] fmt, Args args) { formattedWrite(&this, fmt, args); put("\n"); }
	void putln(const(char)[] str = null) { put(str); put("\n"); }
}

struct Buffer(T)
{
	import std.experimental.allocator.gc_allocator;
	alias allocator = GCAllocator.instance;

	T[] buf;
	// Must be kept private since it can be used to check for avaliable space
	// when used as output range
	private size_t length;

	bool empty() { return length == 0; }

	void put(T[] items ...)
	{
		reserve(items.length);
		buf[length..length+items.length] = items;
		length += items.length;
	}

	void put(R)(R itemRange)
	{
		foreach(item; itemRange)
			put(item);
	}

	void stealthPut(T item)
	{
		reserve(1);
		buf[length] = item;
	}

	ref T opIndex(size_t at)
	{
		return buf[at];
	}

	ref T back() { return buf[length-1]; }

	inout(T[]) data() inout {
		return buf[0..length];
	}

	void clear() nothrow {
		length = 0;
	}

	size_t capacity() const @property {
		return buf.length;
	}

	void reserve(size_t items)
	{
		if (buf.length - length < items)
		{
			import core.memory;
			GC.removeRange(buf.ptr);
			size_t newCapacity = nextPOT(buf.length + items);
			void[] tmp = buf;
			allocator.reallocate(tmp, newCapacity*T.sizeof);
			buf = cast(T[])tmp;
			GC.addRange(buf.ptr, buf.length * T.sizeof, typeid(T));
		}
	}

	void unput(size_t numItems)
	{
		length -= numItems;
	}
}
