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
import std.bitmanip : bitfields;

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
q{i32 sign(i32 number)
{
	i32 result;
	if (number < 0) result = 0-1;
	else if (number > 0) result = 1;
	else result = 0;
	return result;
}};

	auto time0 = currTime;
	IdentifierMap idMap;
	auto context = CompilationContext(input8, &idMap);
	//context.crashOnICE = true;
	ubyte[] codeBuffer = alloc_executable_memory(PAGE_SIZE * 8);

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
			//writeln;
			context.throwOnErrors;

		auto time6 = currTime;

		auto time7 = currTime;
			auto bin_irgen = BinIrGenerationVisitor(&context);
			bin_irgen.visit(mod);
			context.assertf(bin_irgen.irModule !is null, "Module IR is null");
			context.throwOnErrors;

		auto time8 = currTime;
			auto opt_pass = OptimizeIrPass(&context);
			opt_pass.visit(bin_irgen.irModule);
			context.throwOnErrors;

		auto time9 = currTime;
			bin_irgen.irModule.codeBuffer = codeBuffer;
			auto codegen_pass = IrToAmd64(&context);
			codegen_pass.visit(bin_irgen.irModule);

		auto time10 = currTime;
			auto funDecl = mod.findFunction("sign", &context);
			alias FunType = extern(C) int function(int);
			FunType fun;
			int res1;
			int res2;
			int res3;
			if (funDecl.irData.funcPtr)
			{
				fun = cast(FunType)funDecl.irData.funcPtr;
				foreach(_; 0..10_000)
				{
					res1 = fun(10);
					res2 = fun(0);
					res3 = fun(-10);
				}
			}

		auto time11 = currTime;

		auto time12 = currTime;
			// Text dump
			writeln("// source code");
			writeln(context.input);

			TextSink sink;
			bin_irgen.irModule.dump(sink, &context, false);
			writeln;
			writeln("// Generated from binary IR");
			writeln(sink.text);
			writeln;
			writeln("// Code");
			printHex(bin_irgen.irModule.code, 16);

		writeln;
		writeln("// timing");
		writefln("parse %ss", scaledNumberFmt(time1-time0));
		writefln("print %ss", scaledNumberFmt(time2-time1));
		writefln("semantic insert %ss", scaledNumberFmt(time3-time2));
		writefln("semantic lookup %ss", scaledNumberFmt(time4-time3));
		writefln("semantic types %ss", scaledNumberFmt(time5-time4));
		writefln("print2 %ss", scaledNumberFmt(time6-time5));
		writefln("IR bin gen %ss", scaledNumberFmt(time8-time7));
		writefln("IR opt %ss", scaledNumberFmt(time9-time8));
		writefln("Codegen %ss", scaledNumberFmt(time10-time9));
		/*
		writefln("fun run x3 %ss", scaledNumberFmt(time11-time10));
		writefln("codeBuffer %s", codeBuffer.ptr);
		writefln("fun %s", fun);
		writefln("sign(10) -> %s", res1);
		writefln("sign(0) -> %s", res2);
		writefln("sign(-10) -> %s", res3);
		*/
	}
	catch(CompilationException e) {
		writeln(context.sink.text);
		if (e.isICE)
			writeln(e);
	}
	//testNativeFun;
}

void testNativeFun()
{
	auto time0 = currTime;
	int res1;
	int res2;
	int res3;
	fun = &sign;
	foreach(_; 0..10_000)
	{
		res1 = fun(10);
		res2 = fun(0);
		res3 = fun(-10);
	}
	auto time1 = currTime;
	writefln("sign(10) -> %s", res1);
	writefln("sign(0) -> %s", res2);
	writefln("sign(-10) -> %s", res3);
	writefln("native fun run x10k %ss", scaledNumberFmt(time1-time0));
}

__gshared int function(int) fun;

int sign(int number)
{
	int result;
	if (number < 0) result = -1;
	else if (number > 0) result = 1;
	else result = 0;
	return result;
}

// Parsing expressions - http://effbot.org/zone/simple-top-down-parsing.htm

struct CompilationContext
{
	string input;
	IdentifierMap* idMap;
	bool hasErrors;
	TextSink sink;
	bool crashOnICE;

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

	void assertf(string file = __FILE__, int line = __LINE__, Args...)(bool cond, string fmt, lazy Args args)
	{
		if (cond) return;
		sink.putf("%s(%s): ICE: Assertion failure: ", file, line);
		sink.putfln(fmt, args);
		hasErrors = true;
		tryCrashOnICE;
		throw new CompilationException(true);
	}

	void internal_error(Args...)(SourceLocation loc, string format, Args args)
	{
		sink.putf("file(%s, %s): ICE: ", loc.line+1, loc.col+1);
		sink.putfln(format, args);
		hasErrors = true;
		tryCrashOnICE;
		throw new CompilationException(true);
	}

	void internal_error(Args...)(string format, Args args)
	{
		sink.put("ICE: ");
		sink.putfln(format, args);
		hasErrors = true;
		tryCrashOnICE;
		throw new CompilationException(true);
	}

	private void tryCrashOnICE() {
		if (crashOnICE)
		{
			//ubyte* nullPtr = null;
			//nullPtr[0] = nullPtr[0];
			assert(false, sink.text);
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

	void assertImplemented(SourceLocation loc, CompilationContext* context) {
		if (!isImplemented)
			context.error(loc, "Type is not implemented `%s`",
				typeName(context));
	}

	bool isImplemented() {
		if (astType == AstType.type_basic)
		{
			switch((cast(BasicTypeNode*)&this).basicType)
			{
				case BasicType.t_bool: return true;
				case BasicType.t_i32: return true;
				case BasicType.t_i64: return true;
				case BasicType.t_f32: return false;
				case BasicType.t_f64: return false;
				default: return false;
			}
		}
		return false;
	}

	IrValueType irType(CompilationContext* context) {
		if (astType == AstType.type_basic)
		{
			switch((cast(BasicTypeNode*)&this).basicType)
			{
				case BasicType.t_bool: return IrValueType.i32;
				case BasicType.t_i32: return IrValueType.i32;
				case BasicType.t_i64: return IrValueType.i64;
				//case BasicType.t_f32: return IrValueType.f32;
				//case BasicType.t_f64: return IrValueType.f64;
				default: break;
			}
		}
		context.internal_error(loc, "Cannot convert `%s` to IrValueType", astType);
		assert(false);
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
	IrModule irModule;

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
	IrValueRef irRef;
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
	T* makeExpr(T, Args...)(SourceLocation start, Args args) { return new T(start, null, IrValueRef(), args); }

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

		BinOp op;
		switch(tok.type)
		{
			case TokenType.LESS: op = BinOp.LESS; break;
			case TokenType.LESS_EQUAL: op = BinOp.LESS_EQUAL; break;
			case TokenType.GREATER: op = BinOp.GREATER; break;
			case TokenType.GREATER_EQUAL: op = BinOp.GREATER_EQUAL; break;
			default: return n;
		}

		nextToken();
		t = n;
		n = cast(ExpressionNode*)makeExpr!BinaryExprNode(start, op, t, sum());

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
			return cast(ExpressionNode*)make!LiteralExprNode(start, type, IrValueRef(), value);
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
	IrValueRef irRef;

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
	FunctionDeclNode* curFunc;

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
				expr = cast(ExpressionNode*) new TypeConvExprNode(expr.loc, type, IrValueRef(), expr);
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
				expr = cast(ExpressionNode*) new TypeConvExprNode(expr.loc, type, IrValueRef(), expr);
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

			case EQUAL: // int float
				if (b.left.astType == AstType.expr_var)
				{
					autoconvTo(b.right, b.left.type);
					resRype = b.right.type;
				}
				else
					context.error(b.left.loc, "Cannot perform assignment into %s", b.left.astType);
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

	void checkReturnType(FunctionDeclNode* f) {
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
		visit(f.block_stmt);
		checkReturnType(f);
		curFunc = prevFunc;
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
		foreach (arg; c.args) _visit(arg);
		c.type = c.getSym.getType;
	}
	void visit(TypeConvExprNode* t) {
		_visit(t.type);
		_visit(t.expr);
		t.type.assertImplemented(t.loc, context);
	}
	void visit(BasicTypeNode* t) {}
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

/// Converts AST to in-memory linear IR
struct BinIrGenerationVisitor {
	mixin AstVisitorMixin;
	CompilationContext* context;

	IrModule* irModule;
	IrFunction* curFunc; // current function
	IrBasicBlock* currentBB;

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
		tempId = context.idMap.getOrReg("__tmp");
		startId = context.idMap.getOrReg("start");
		thenId = context.idMap.getOrReg("then");
		elseId = context.idMap.getOrReg("else");
		blkId = context.idMap.getOrReg("blk");
		foreach (decl; m.declarations) _visit(decl);
	}
	void visit(FunctionDeclNode* f)
	{
		// create new function
		f.irData = IrFunction(IrName(f.id), f.returnType.irType(context));

		// save previous function
		auto prevFunc = curFunc;

		// set new function as current
		curFunc = &f.irData;
		irModule.addFunction(&f.irData);

		foreach (i, param; f.parameters)
		{
			visit(param);
		}

		// create Basic Block for function body
		// other code will use `last` Basic Block
		currentBB = curFunc.addBasicBlock(IrName(startId));

		visit(f.block_stmt);

		if (!curFunc.last.isFinished)
		{
			// Return must present in case of non-void function
			curFunc.last.exit = IrJump(IrJump.Type.ret0);
		}

		// restore previous function
		curFunc = prevFunc;
	}
	void visit(VariableDeclNode* v)
	{
		v.getSym.irRef = curFunc.addVariable(v.type.irType(context), IrName(v.id));
	}
	void visit(ParameterDeclNode* p)
	{
		p.getSym.irRef = curFunc.addVariable(p.type.irType(context), IrName(p.id));
		++curFunc.numParameters;
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
		IrValueRef condRef = i.condition.irRef;

		// invert condition, so that we jump to else on success
		auto lastInstr = &currentBB.instructions[$-1];
		if (lastInstr.op == IrOpcode.o_icmp)
		{
			lastInstr.condition = complementaryCondition(lastInstr.condition);
		}
		else
		{
			auto notCond = curFunc.addVariable(i.condition.type.irType(context), IrName(tempId, uniqueSuffix));
			auto instr = IrInstruction(IrOpcode.o_not, notCond, condRef);
			currentBB.emit(instr);
			//writefln("incValueUsage cond1 %s %s", condRef.kind, condRef.index);
			curFunc.incValueUsage(condRef);
			condRef = notCond;
		}

		// prevBB links to elseBB and (afterBB or thenBB)
		IrBasicBlock* prevBB = currentBB;
		prevBB.exit = IrJump(IrJump.Type.branch, condRef);
		//writefln("incValueUsage cond2 %s %s", condRef.kind, condRef.index);
		curFunc.incValueUsage(condRef);

		// create Basic Block for then statement. It links to afterBB
		IrBasicBlock* then_StartBB = curFunc.addBasicBlock(IrName(thenId, ++thenCounter));
		currentBB = then_StartBB;

		_visit(cast(AstNode*)i.thenStatement);
		IrBasicBlock* then_EndBB = currentBB;

		IrBasicBlock* else_StartBB;
		IrBasicBlock* else_EndBB;
		if (i.elseStatement)
		{
			currentBB = curFunc.addBasicBlock(IrName(elseId, ++elseCounter));
			else_StartBB = currentBB;
			prevBB.outs ~= else_StartBB;
			_visit(i.elseStatement);
			else_EndBB = currentBB;
		}

		IrBasicBlock* afterBB = curFunc.addBasicBlock(IrName(blkId, ++bbCounter));
		currentBB = afterBB;

		if (!then_EndBB.isFinished)
		{
			then_EndBB.exit = IrJump(IrJump.Type.jmp);
			then_EndBB.outs ~= afterBB;
		}

		if (i.elseStatement)
		{
			if (!else_EndBB.isFinished)
			{
				else_EndBB.exit = IrJump(IrJump.Type.jmp);
				else_EndBB.outs ~= afterBB;
			}
		}
		else
		{
			prevBB.outs ~= afterBB;
		}
		prevBB.outs ~= then_StartBB;
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
			//writefln("incValueUsage ret %s %s", r.expression.irRef.kind, r.expression.irRef.index);
			curFunc.incValueUsage(r.expression.irRef);
		}
		else currentBB.exit = IrJump(IrJump.Type.ret0);
	}
	void visit(BreakStmtNode* b) {}
	void visit(ContinueStmtNode* c) {}
	void visit(VariableExprNode* v) {
		v.irRef = v.getSym.irRef;
	}
	void visit(LiteralExprNode* c) {
		c.irRef = curFunc.addConstant(IrValueType.i32, c.value);
	}
	void visit(BinaryExprNode* b) {
		_visit(cast(AstNode*)b.left);
		_visit(cast(AstNode*)b.right);
		curFunc.incValueUsage(b.left.irRef);
		curFunc.incValueUsage(b.right.irRef);
		//writefln("incValueUsage expr1 %s %s", b.left.irRef.kind, b.left.irRef.index);
		//writefln("incValueUsage expr2 %s %s", b.right.irRef.kind, b.right.irRef.index);
		Condition condition;
		switch(b.op)
		{
			case BinOp.EQUAL:
				auto instr = IrInstruction(IrOpcode.o_assign, b.left.irRef, b.right.irRef);
				currentBB.emit(instr);
				break;
			case BinOp.EQUAL_EQUAL: condition = Condition.E; goto case BinOp.MINUS;
			case BinOp.NOT_EQUAL: condition = Condition.NE; goto case BinOp.MINUS;
			case BinOp.GREATER: condition = Condition.G; goto case BinOp.MINUS;
			case BinOp.GREATER_EQUAL: condition = Condition.GE; goto case BinOp.MINUS;
			case BinOp.LESS: condition = Condition.L; goto case BinOp.MINUS;
			case BinOp.LESS_EQUAL: condition = Condition.LE; goto case BinOp.MINUS;
			case BinOp.PLUS: goto case BinOp.MINUS;
			case BinOp.MINUS:
				b.irRef = curFunc.addVariable(b.type.irType(context), IrName(tempId, uniqueSuffix));
				auto instr = IrInstruction(binOpToIrOpcode[b.op], b.irRef, b.left.irRef, b.right.irRef);
				instr.condition = condition;
				currentBB.emit(instr);
				break;
			default:
				context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op);
				break;
		}
	}
	void visit(CallExprNode* c) {
		foreach (arg; c.args) _visit(arg);
		if (!c.type.isVoid) {
			c.irRef = curFunc.addVariable(c.getSym.getType.irType(context), IrName(tempId, uniqueSuffix));
		}
		currentBB.emit(IrInstruction(IrOpcode.o_call, c.irRef, c.args.length, c.getSym));
		foreach (arg; c.args)
		{
			currentBB.emit(IrInstruction(IrOpcode.o_arg, IrValueRef(), arg.irRef));
			//writefln("incValueUsage arg %s %s", arg.irRef.kind, arg.irRef.index);
			curFunc.incValueUsage(arg.irRef);
		}
	}
	void visit(TypeConvExprNode* t) {
		context.internal_error(t.loc, "Type conversion is not implemented");
		//_visit(t.type); _visit(t.expr);
		//t.irVarData = makeVarSuf("conv");
		//sink.putfln("%s = cast(%s) %s", t.irVarData,
		//	t.type.typeName(context), t.expr.irVarData);
	}
	void visit(BasicTypeNode* t) {}
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
		for (IrBasicBlock* block = func.start; block; block = block.next)
		{
			/// If jump target is a basic block that only jumps to another block,
			/// jump straight to it recursively
			void skipEmptyBlockJump(ref IrBasicBlock* target)
			{
				while(target && target.isRedundant)
				{
					func.removeBasicBlock(target);
					// copy jmp or ret0 type
					block.exit.type = target.exit.type;
					// copy jmp target or null
					target = target.outs[0];
				}
			}

			switch(block.exit.type) with(IrJump.Type) {
				case branch:
					skipEmptyBlockJump(block.outs[0]);
					skipEmptyBlockJump(block.outs[1]);
					break;
				case jmp:
					skipEmptyBlockJump(block.outs[0]);
					break;
				default: break;
			}
		}
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
struct IrModule
{
	IrFunction*[] functions;

	ubyte[] codeBuffer;
	ubyte[] code;

	void addFunction(IrFunction* fun)
	{
		functions ~= fun;
	}

	void dump(ref TextSink sink, CompilationContext* context, bool printVars = false)
	{
		foreach (func; functions) dumpFunction(*func, sink, context, printVars);
	}
}

struct IrFunction
{
	IrName name;

	IrValueType returnType;

	/// Position in buffer or in memory
	void* funcPtr;

	/// The first Basic Block of the function
	/// Also the first node in the linked list of Basic Blocks
	IrBasicBlock* start;

	/// This is the last Basic Block in the linked list of blocks of this function
	IrBasicBlock* last;

	size_t numBasicBlocks;

	/// This values are of kind IrValueKind.con
	IrValue[] constants;
	IrConstantData[] constantData;

	/// This array begins with all parameters
	/// This values are of kind IrValueKind.var
	IrValue[] values;

	/// Contains names for `variables`. Is either empty or the same length as `variables`
	IrName[] variableNames;

	/// Parameters are saved as first `numParameters` items of `variables` array
	size_t numParameters;

	IrValue[] parameters() { return values[0..numParameters]; }

	ValuePrintProxy valueText(CompilationContext* ctx, IrValueRef reference) {
		auto result = ValuePrintProxy(ctx, reference);
		final switch(reference.kind) {
			case IrValueKind.con: result.constData = constantData[reference.index].value; break;
			case IrValueKind.var: result.name = variableNames[reference.index]; break;
		}
		return result;
	}

	IrValue deref(IrValueRef reference) {
		assert(reference.isDefined, "Attempting to deref zero reference");
		final switch(reference.kind) {
			case IrValueKind.con: return constants[reference.index];
			case IrValueKind.var: return values[reference.index];
		}
	}

	/// Automatically sets `start`, sets last and links blocks together
	IrBasicBlock* addBasicBlock(IrName name) {
		++numBasicBlocks;
		auto newBlock = new IrBasicBlock(name);
		if (start is null) start = newBlock;
		else
		{
			newBlock.prev = last;
			last.next = newBlock;
		}
		last = newBlock;
		return newBlock;
	}

	void removeBasicBlock(IrBasicBlock* bb) {
		if (bb.prev)
		{
			if (bb.next)
			{
				bb.prev.next = bb.next;
				bb.next.prev = bb.prev;
			}
			else
			{
				assert(last == bb);
				last = bb.prev;
				last.next = null;
			}
		}
		else if (bb.next)
		{
			assert(start == bb);
			start = bb.next;
			start.prev = null;
		}
		else
		{
			// This may be an already deleted block
			if (bb == start)
			{
				start = null;
				assert(last == bb);
				last = null;
			}
		}
		bb.next = null;
		bb.prev = null;
	}

	/// Increments numUses of value
	void incValueUsage(IrValueRef reference) {
		assert(reference.isDefined, "Attempting to incValueUsage zero reference");

		final switch(reference.kind) {
			case IrValueKind.con: ++constants[reference.index].numUses; break;
			case IrValueKind.var: ++values[reference.index].numUses; break;
		}
	}

	IrValueRef addConstant(IrValueType type, ulong data) {
		IrValue val = IrValue(IrValueKind.con, type, 0);
		uint index = cast(uint)constants.length;
		constants ~= val;
		constantData ~= IrConstantData(data);
		return IrValueRef(index, IrValueKind.con, type);
	}

	IrValueRef addVariable(IrValueType type, IrName name) {
		IrValue val = IrValue(IrValueKind.var, type, 0);
		uint index = cast(uint)values.length;
		values ~= val;
		variableNames ~= name;
		return IrValueRef(index, IrValueKind.var, type);
	}
}

void dumpFunction(ref IrFunction func, ref TextSink sink, CompilationContext* ctx, bool printVars = false)
{
	sink.putf("function %s $%s (", func.returnType, IrNameProxy(ctx, func.name));
	foreach (i, param; func.parameters)
	{
		sink.putf("%s %%%s", param.type, IrNameProxy(ctx, func.variableNames[i]));
		if (i+1 < func.parameters.length) sink.put(", ");
	}
	sink.putln(") {");

	if (printVars)
	{
		sink.putln("constants:");
		sink.putln("  #   T uses value");
		foreach (i, value; func.constants)
		{
			sink.putfln("% 3s %s% 5s % 5s",
				i, value.type, value.numUses, func.constantData[i].value);
		}
		sink.putln;

		sink.putln("variables:");
		sink.putln("  #   T uses id");
		foreach (i, value; func.values)
		{
			sink.putfln("% 3s %s% 5s %s",
				i, value.type, value.numUses, IrNameProxy(ctx, func.variableNames[i]));
		}
		sink.putln;
	}

	for (IrBasicBlock* block = func.start; block; block = block.next)
	{
		sink.putfln("  @%s", IrNameProxy(ctx, block.name));

		// print all instructions
		foreach(instr_i, ref instr; block.instructions)
		{
			final switch(instr.op) with(IrOpcode) {
				case o_nop:
					sink.putfln("    nop");
					break;
				case o_icmp:
					sink.putfln("    %s = icmp %s %s, %s",
						func.valueText(ctx, instr.to),
						instr.condition,
						func.valueText(ctx, instr.arg0),
						func.valueText(ctx, instr.arg1));
					break;
				case o_not:
					sink.putfln("    %s = not %s",
						func.valueText(ctx, instr.to),
						func.valueText(ctx, instr.arg0));
					break;
				// Arithmetic
				case o_add, o_sub, o_div, o_rem, o_udiv, o_urem, o_mul:
					sink.putfln("    %s = %s %s, %s",
						func.valueText(ctx, instr.to),
						irOpcodeNames[instr.op],
						func.valueText(ctx, instr.arg0),
						func.valueText(ctx, instr.arg1));
					break;
				case o_assign:
					sink.putfln("    %s = %s",
						func.valueText(ctx, instr.to),
						func.valueText(ctx, instr.arg0));
					break;
				case o_call:
					if (!instr.to.isDefined)
						sink.putf("    call $%s(", ctx.idString(instr.callee.id));
					else
						sink.putf("    %s = call $%s(", func.valueText(ctx, instr.to), ctx.idString(instr.callee.id));
					foreach (arg_i, ref arg; block.instructions[instr_i+1..instr_i+instr.numArgs+1])
					{
						sink.putf("%s", func.valueText(ctx, arg.arg0));
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
			case ret1: sink.putfln("    return %s", func.valueText(ctx, block.exit.value)); break;
			case jmp:  sink.putfln("    jmp @%s", IrNameProxy(ctx, block.outs[0].name)); break;
			case branch:  sink.putfln("    branch %s @%s, @%s",
				func.valueText(ctx, block.exit.value), IrNameProxy(ctx, block.outs[0].name), IrNameProxy(ctx, block.outs[1].name)); break;
		}
	}
	sink.putln("}");
}

enum IrValueKind : ubyte
{
	con, // constant
	var, // variable
}

/// According to WebAssembly spec
enum IrValueType : ubyte
{
	i32,
	i64,
	//f32,
	//f64,
}

/// Represents a variable, temporary or constant in a function
struct IrValue
{
	this(IrValueKind kind, IrValueType type, ushort numUses) {
		this.kind = kind;
		this.type = type;
		this.numUses = numUses;
	}

	mixin(bitfields!(
		IrValueKind, "kind",     2, // const, var
		IrValueType, "type",     2, // int, float, bool
		bool,        "isDirty",  1, // true if value in register is newer than value in memory
		uint,        "",         3)
	);
	/// Used by register allocation code to indicate register that contains this value
	RegisterRef reg;
	/// Indicates number of uses. There can be multiple uses per instruction.
	/// 0 means unused, 1 means temporary, or more
	ushort numUses;

	/// index in values, constants, constantData, variableNames
	ushort index;

	bool isVar() { return kind == IrValueKind.var; }
	bool isCon() { return kind == IrValueKind.con; }
}

struct IrValueRef
{
	this(uint index, IrValueKind kind, IrValueType type) {
		this.index = index;
		this.kind = kind;
		this.type = type;
		this.isDefined = true;
	}
	mixin(bitfields!(
		/// Negative indicies represent constants
		/// Positive represent a result of an instruction
		/// Zero is used when no ref is given
		uint,        "index",     27,
		/// con, var
		IrValueKind, "kind",       2,
		IrValueType, "type",       2, // int, float, bool
		bool,        "isDefined",  1,
	));

	/// Returns true if reference has no destination
	bool isVar() { return kind == IrValueKind.var; }
	bool isCon() { return kind == IrValueKind.con; }
}

/// Stores data for the IrValue of `con` kind with the same index
struct IrConstantData
{
	long value; // raw bits of constant data
	void toString()(scope void delegate(const(char)[]) sink) {
		sink.formattedWrite("%s", value);
	}
}

struct IrName
{
	Identifier id;
	uint suffix;
}

struct IrNameProxy
{
	CompilationContext* ctx;
	IrName name;
	void toString(scope void delegate(const(char)[]) sink) {
		if (name.suffix == 0) sink.formattedWrite("%s", ctx.idString(name.id));
		else sink.formattedWrite("%s_%s", ctx.idString(name.id), name.suffix);
	}
}

struct ValuePrintProxy
{
	CompilationContext* ctx;
	IrValueRef reference;
	union {
		IrName name;
		long constData;
	}
	void toString(scope void delegate(const(char)[]) sink) {
		if (reference.isDefined)
		{
			final switch(reference.kind) {
				case IrValueKind.con:
					sink.formattedWrite("%s", constData);
					break;
				case IrValueKind.var:
					if (name.suffix == 0) sink.formattedWrite("%%%s", ctx.idString(name.id));
					else sink.formattedWrite("%%%s_%s", ctx.idString(name.id), name.suffix);
					break;
			}
		}
		else sink("<undefined>");
	}
}

struct IrBasicBlock
{
	IrName name;

	/// Address of this Basic Block in generated code
	PC startPC;

	/// BBs that jump to this block
	//IrBasicBlock*[] ins; // unused

	/// BBs this block jumps or branches to. Null if ends with no jump/branch
	IrBasicBlock*[] outs;

	/// The jump or return that must be the last instruction is stored in `exit`
	IrInstruction[] instructions;

	/// This variables are live at the end of this Basic Block
	IrValueRef[] liveVars;

	/// Specifies the last instruction of this Basic Block
	/// Jumps use `outs` for targets
	IrJump exit;

	/// Next node in linked listed of Basic Blocks of the function
	IrBasicBlock* next;
	/// Prev node in linked listed of Basic Blocks of the function
	IrBasicBlock* prev;

	void emit(IrInstruction instr)
	{
		instructions ~= instr;
	}

	bool isFinished() { return exit.type != IrJump.Type.none; }
	bool isRedundant() {
		return instructions.length == 0 &&
			(exit.type == IrJump.Type.jmp || exit.type == IrJump.Type.ret0);
	}
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
		branch   /// conditional jump
	}

	Type type;
	IrValueRef value; /// return value reference, or condition for branch
	/// Used by backend, for:
	///   - first `branch` instruction
	///   - `jmp` instruction
	///   - `ret0`, `ret1` jump instruction when jumping to the end of function
	PC fixup0;
	// Used by backend, for second target of `branch` instruction
	PC fixup1;

	/// Used for branch instruction fixup
	Condition condition = Condition.NZ;

	/// Is set to true by cmp instruction if it precedes branch
	bool useFlagForCmp;
}

enum IrOpcode : ubyte
{
	o_nop,

	o_icmp,
	o_not,

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

string[IrOpcode_num_opcodes] irOpcodeNames = ["nop", "icmp", "not", "add", "sub",
"div", "rem", "udiv", "urem", "mul", "call", "arg", "assign", ];

immutable IrOpcode[] binOpToIrOpcode = [
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,

	IrOpcode.o_assign,
	IrOpcode.o_sub,
	IrOpcode.o_add,
	IrOpcode.o_div,
	IrOpcode.o_mul];

struct IrInstruction
{
	this(IrOpcode op, IrValueRef to) { this.op = op; this.to = to; }
	this(IrOpcode op, IrValueRef to, IrValueRef arg0) { this.op = op; this.to = to; this.arg0 = arg0; }
	this(IrOpcode op, IrValueRef to, IrValueRef arg0, IrValueRef arg1) { this.op = op; this.to = to; this.arg0 = arg0; this.arg1 = arg1; }
	//this(IrOpcode op, IrValueRef to, size_t value) { this.op = op; this.to = to; this.value = value; }
	this(IrOpcode op, IrValueRef to, size_t numArgs, Symbol* callee) { this.op = op; this.to = to; this.numArgs = numArgs; this.callee = callee; }

	IrOpcode op;
	Condition condition;
	IrValueRef to;
	union {
		struct { IrValueRef arg0, arg1; }
		//struct { size_t value;     }
		struct { size_t numArgs; Symbol* callee; }
	}
}

/*
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
}*/


//          ####     ###    #####    #######    ####   #######  #     #
//         #    #   #   #   #    #   #         #    #  #        ##    #
//        #        #     #  #     #  #        #        #        # #   #
//        #        #     #  #     #  #####    #   ###  #####    #  #  #
//        #        #     #  #     #  #        #     #  #        #   # #
//         #    #   #   #   #    #   #         #    #  #        #    ##
//          ####     ###    #####    #######    #####  #######  #     #
// -----------------------------------------------------------------------------

enum MAX_REGS = 14;

/// Stores info about all avaliable registers of the same class
/// Classes are (general purpose aka GPR, floating point FP, flags)
struct RegisterClass
{
	int[MAX_REGS] Name;
	int[MAX_REGS] Next;
	Bits free;
	int[MAX_REGS] Stack;
	int StackTop;
}

struct Bits
{
	ulong data;
	bool get(size_t i) { return cast(bool)(data & (1 << i)); }
	void set(size_t i) { data |= (1 << i); }
	void unset(size_t i) { data &= ~(1 << i); }
}


struct RegisterRef
{
	byte index = -1; /// -1 means undefined
	bool isDefined() { return index >= 0; }
}

/// State of single machine register
struct RegisterState
{
	IrValueRef storedValue;
	bool isUsed() { return storedValue.isDefined; }
}

struct MachineState
{
	RegisterState[MAX_REGS] regs;
}

struct IrToAmd64
{
	CompilationContext* context;

	enum RET_REG = Register.AX;
	enum TEMP_REG_0 = Register.CX;
	enum TEMP_REG_1 = Register.DX;
	enum STACK_ITEM_SIZE = 8; // x86_64
	enum USE_FRAME_POINTER = true;
	CodeGen_x86_64 gen;

	/// Those two store a state of variables and registers
	private IrFunction* curFunc;
	private MachineState machineState;

	static struct CallFixup {
		Fixup call_fixup;
		IrFunction* callee;
	}

	private Buffer!CallFixup callFixups;

	private int numParams;
	private int numLocals;
	private int numVars; // numLocals + numParams
	private int reservedBytes;

	void visit(IrModule* m) {
		context.assertf(m !is null, "Module IR is null");

		gen.encoder.setBuffer(m.codeBuffer);

		foreach (func; m.functions) visit(func);

		fixCalls();
		m.code = gen.encoder.code;
	}

	void visit(IrFunction* func)
	{
		curFunc = func;
		curFunc.funcPtr = gen.pc;
		compileFuncProlog();
		compileFuncBody();
		fixJumpsAndReturns(gen.pc);
		compileFuncEpilog();
	}

	void fixCalls()
	{
		foreach (fixup; callFixups.data) {
			genCall(fixup.call_fixup, fixup.callee);
			writefln("fix call to %s", cast(void*)fixup.callee.funcPtr);
		}
		callFixups.clear();
	}

	void compileFuncProlog()
	{
		numParams = cast(int)curFunc.numParameters;
		numLocals = cast(int)(curFunc.values.length - numParams);
		numVars = numLocals + numParams;

		// Establish frame pointer
		if (USE_FRAME_POINTER)
		{
			gen.pushq(Register.BP);
			gen.movq(Register.BP, Register.SP);
		}
		reservedBytes = cast(int)(numLocals * STACK_ITEM_SIZE);
		if (reservedBytes) // Reserve space for locals
		{
			if (reservedBytes > byte.max) gen.subq(Register.SP, Imm32(reservedBytes));
			else gen.subq(Register.SP, Imm8(cast(byte)reservedBytes));
		}

		// init locals
		if (numLocals)
		{
			gen.xorq(RET_REG, RET_REG);
			foreach(i; 0..numLocals) gen.movq(localVarMemAddress(numParams+i), RET_REG);
		}

		/* save parameters to stack after setting frame ptr */
		if (numParams > 3) gen.movq(localVarMemAddress(3), Register.R9); // save fourth parameter
		if (numParams > 2) gen.movq(localVarMemAddress(2), Register.R8); // save third parameter
		if (numParams > 1) gen.movq(localVarMemAddress(1), Register.DX); // save second parameter
		if (numParams > 0) gen.movq(localVarMemAddress(0), Register.CX); // save first parameter
	}

	void saveDirtyRegisters()
	{

	}

	Register ensureLoaded(IrValueRef value)
	{
		//value.
		return RET_REG;
	}

	Register loadValue(IrValueRef value, Register reg)
	{
		assert(value.isDefined);
		final switch(value.type)
		{
			case IrValueType.i32:
				if (value.isVar) gen.movd(reg, localVarMemAddress(value));
				else gen.movd(reg, Imm32(cast(uint)curFunc.constantData[value.index].value));
				break;
			case IrValueType.i64:
				if (value.isVar) gen.movq(reg, localVarMemAddress(value));
				else gen.movq(reg, Imm64(curFunc.constantData[value.index].value));
				break;
		}
		return reg;
	}

	void storeValue(IrValueRef dest, Register reg)
	{
		if (!dest.isVar) context.internal_error("Assignment into a constant");
		final switch(dest.type)
		{
			case IrValueType.i32: gen.movd(localVarMemAddress(dest), reg); break;
			case IrValueType.i64: gen.movq(localVarMemAddress(dest), reg); break;
		}
	}

	/// At the start all values are in memory
	void compileFuncBody()
	{
		for (IrBasicBlock* block = curFunc.start; block; block = block.next)
		{
			block.startPC = gen.pc;
			foreach (instr_i, ref instr; block.instructions)
			{
				bool nextIsBranch() {
					return instr_i == block.instructions.length-1 &&
						block.exit.value == instr.to &&
						block.exit.type == IrJump.Type.branch;
				}
				switch (instr.op) with(IrOpcode) {
					case o_nop: break;
					case o_icmp:
						auto arg0 = curFunc.deref(instr.arg0);
						auto arg1 = curFunc.deref(instr.arg1);
						if (arg0.type != arg1.type)
							context.internal_error("Type mismatch: %s != %s. In BB `%s`",
								arg0.type, arg1.type, block.name);
						auto reg0 = loadValue(instr.arg0, TEMP_REG_0);
						auto reg1 = loadValue(instr.arg1, TEMP_REG_1);
						final switch(arg0.type)
						{
							case IrValueType.i32: gen.cmpd(reg0, reg1); break;
							case IrValueType.i64: gen.cmpq(reg0, reg1); break;
						}

						// Check if comparison result is used for jump
						if (nextIsBranch)
						{
							// set condition for jump (otherwise it is NZ)
							block.exit.condition = instr.condition;
							block.exit.useFlagForCmp = true;
							break;
						}

						// otherwise store in variable
						gen.setcc(instr.condition, reg0);
						gen.movzx_btod(reg0, reg0);
						storeValue(instr.to, reg0);
						break;

					case o_not:
						auto reg0 = loadValue(instr.arg0, TEMP_REG_0);
						final switch(instr.arg0.type) {
							case IrValueType.i32: gen.notd(reg0); break;
							case IrValueType.i64: gen.notq(reg0); break;
						}
						storeValue(instr.to, reg0);
						break;

					// Arithmetic
					case o_add, o_sub:
						auto reg0 = loadValue(instr.arg0, TEMP_REG_0);
						auto reg1 = loadValue(instr.arg1, TEMP_REG_1);

						switch(instr.op)
						{
							case IrOpcode.o_add:
								final switch(instr.arg0.type) {
									case IrValueType.i32: gen.addd(reg0, reg1); break;
									case IrValueType.i64: gen.addq(reg0, reg1); break;
								}
								break;
							case IrOpcode.o_sub:
								final switch(instr.arg0.type) {
									case IrValueType.i32: gen.subd(reg0, reg1); break;
									case IrValueType.i64: gen.subq(reg0, reg1); break;
								}
								break;
							default: break;
						}

						final switch(instr.arg0.type)
						{
							case IrValueType.i32:
								gen.movd(localVarMemAddress(instr.to), TEMP_REG_0);
								break;
							case IrValueType.i64:
								gen.movq(localVarMemAddress(instr.to), TEMP_REG_0);
								break;
						}
						break;
					case o_assign:
						auto reg0 = loadValue(instr.arg0, TEMP_REG_0);
						storeValue(instr.to, reg0);
						break;
					default:
						context.internal_error("Compilation of `%s` is not implemented. In BB `%s`",
							instr.op, block.name);
						break;
				}
			}

			final switch(block.exit.type) with(IrJump.Type) {
				case none: context.internal_error("Compilation non-sealed basic block `%s`", block.name); break;
				case ret1:
					auto reg0 = loadValue(block.exit.value, RET_REG);
					goto case;
				case ret0:
					// ignore jump in the last Basic Block
					if (block.next is null) break;
					block.exit.fixup0 = gen.pc;
					gen.jmp(Imm32(0));
					break;
				case jmp:
					if (block.next != block.outs[0]) {
						block.exit.fixup0 = gen.pc;
						gen.jmp(Imm32(0));
					}
					break;
				case branch:
					block.exit.fixup0 = gen.pc;
					if (!block.exit.useFlagForCmp)
					{
						auto reg0 = loadValue(block.exit.value, TEMP_REG_0);
						gen.testd(TEMP_REG_0, TEMP_REG_0);
					}
					gen.jcc(Condition.NZ, Imm32(0));
					if (block.next != block.outs[1]) {
						block.exit.fixup1 = gen.pc;
						gen.jmp(Imm32(0));
					}
					break;
			}
		}
	}

	void fixJumpsAndReturns(PC returnTarget)
	{
		for (IrBasicBlock* block = curFunc.start; block; block = block.next)
		{
			final switch(block.exit.type) with(IrJump.Type) {
				case none: context.internal_error("Compilation non-sealed basic block `%s`", block.name); return;
				case ret0, ret1:
					// ignore jump in the last Basic Block
					if (block.next is null) break;
					auto fixup = gen.fixupAt(block.exit.fixup0);
					fixup.jmpAbs(returnTarget);
					break;
				case jmp:
					if (block.next != block.outs[0]) {
						auto fixup = gen.fixupAt(block.exit.fixup0);
						fixup.jmpAbs(block.outs[0].startPC);
					}
					break;
				case branch:
					// Jump to not zero
					auto fixup0 = gen.fixupAt(block.exit.fixup0);
					fixup0.jccAbs(block.exit.condition, block.outs[0].startPC);
					// fallthrough or jump to zero
					if (block.next != block.outs[1]) {
						auto fixup1 = gen.fixupAt(block.exit.fixup1);
						fixup1.jmpAbs(block.outs[1].startPC);
					}
					break;
			}
		}
	}

	void compileFuncEpilog()
	{
		if (reservedBytes)
		{
			if (reservedBytes > byte.max) gen.addq(Register.SP, Imm32(reservedBytes));
			else gen.addq(Register.SP, Imm8(cast(byte)reservedBytes));
		}

		if (USE_FRAME_POINTER)
		{
			// Restore frame pointer
			gen.popq(Register.BP);
		}

		gen.ret();
	}

	MemAddress localVarMemAddress(IrValueRef varRef)
	{
		assert(varRef.isVar);
		return localVarMemAddress(varRef.index);
	}

	MemAddress localVarMemAddress(int varIndex)
	{
		bool isParameter = varIndex < numParams;
		Register baseReg;

		int index;
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
		}
		else
		{
			if (isParameter) // parameter
			{
				// Since return address is saved between locals and parameters, we need to add 1 to index for parameters
				index = numLocals + varIndex + 1;
			}
			else // local variable
			{
				// count from RSP, so last var has index of 0
				index = numVars - varIndex - 1;
			}
			baseReg = Register.SP;
		}
		int displacement = index * STACK_ITEM_SIZE;
		return minMemAddrBaseDisp(baseReg, displacement);
	}

	void genCall(Gen)(ref Gen gen, IrFunction* callee)
	{
		/*
		if (callee.native)
		{
			// hardcoded size of instruction (6)
			int disp32 = cast(int)(&nativeFunctionTable[callee.index] - cast(void*)gen.pc - 6);
			gen.call(memAddrRipDisp32(cast(uint)disp32));
		}
		else
		{
			if (!callee.funcPtr) throw new Error("Invalid funcPtr");
			gen.call(cast(PC)callee.funcPtr);
		}*/
	}

	void genFixup(IrFunction* callee)
	{/*
		callFixups.put(CallFixup(gen.saveFixup(), callee));
		if (callee.native)
		{
			gen.call(memAddrRipDisp32(0));
		}
		else
		{
			gen.call(gen.stubPC);
		}*/
	}
}

//                                                                  ###       ##
//    #      #####   #     #              #     #     #  #####     #          ##
//    #     #     #  ##   ##              #     ##   ##  #    #   #          # #
//   ###    #        # # # #             ###    # # # #  #     #  #####      # #
//   # #     #####   #  #  #             # #    #  #  #  #     #  #    #    #  #
//  #####         #  #     #            #####   #     #  #     #  #    #   ######
//  #   #   #     #  #     #            #   #   #     #  #    #   #    #       #
// ##   ##   #####   #     #           ##   ##  #     #  #####     ####       ###
//                            #######
// -----------------------------------------------------------------------------

enum Register : ubyte {AX, CX, DX, BX, SP, BP, SI, DI, R8, R9, R10, R11, R12, R13, R14, R15}
enum RegisterMax  = cast(Register)(Register.max+1);

bool is_SP_or_R12(Register reg) { return (reg & 0b111) == 0b100; }
bool is_BP_or_R13(Register reg) { return (reg & 0b111) == 0b101; }

enum ArgType : ubyte { BYTE, WORD, DWORD, QWORD }

// ensures REX prefix for ah ch dh bh
bool regNeedsRexPrefix(ArgType argType)(Register reg) {
	static if (argType == ArgType.BYTE) return reg >= 4;
	else return false;
}

import std.string : format;
struct Imm8  { ubyte  value; enum argT = ArgType.BYTE;  string toString(){ return format("0X%02X", value); } }
struct Imm16 { ushort value; enum argT = ArgType.WORD;  string toString(){ return format("0X%02X", value); } }
struct Imm32 { uint   value; enum argT = ArgType.DWORD; string toString(){ return format("0X%02X", value); } }
struct Imm64 { ulong  value; enum argT = ArgType.QWORD; string toString(){ return format("0X%02X", value); } }
enum bool isAnyImm(I) = is(I == Imm64) || is(I == Imm32) || is(I == Imm16) || is(I == Imm8);


enum ubyte REX_PREFIX = 0b0100_0000;
enum ubyte REX_W      = 0b0000_1000;
enum ubyte REX_R      = 0b0000_0100;
enum ubyte REX_X      = 0b0000_0010;
enum ubyte REX_B      = 0b0000_0001;

enum LegacyPrefix : ubyte {
	// Prefix group 1
	LOCK = 0xF0, // LOCK prefix
	REPN = 0xF2, // REPNE/REPNZ prefix
	REP  = 0xF3, // REP or REPE/REPZ prefix
	// Prefix group 2
	CS = 0x2E, // CS segment override
	SS = 0x36, // SS segment override
	DS = 0x3E, // DS segment override
	ES = 0x26, // ES segment override
	FS = 0x64, // FS segment override
	GS = 0x65, // GS segment override
	BNT = 0x2E, // Branch not taken
	BT = 0x3E, // Branch taken
	// Prefix group 3
	OPERAND_SIZE = 0x66, // Operand-size override prefix
	// Prefix group 4
	ADDRESS_SIZE = 0x67, // Address-size override prefix
}

/// The terms "less" and "greater" are used for comparisons of signed integers.
/// The terms "above" and "below" are used for unsigned integers.
enum Condition : ubyte {
	O   = 0x0, /// overflow (OF=1).
	NO  = 0x1, /// not overflow (OF=0).
	B   = 0x2, /// below (CF=1).
	C   = 0x2, /// carry (CF=1).
	NAE = 0x2, /// not above or equal (CF=1).
	AE  = 0x3, /// above or equal (CF=0).
	NB  = 0x3, /// not below (CF=0).
	NC  = 0x3, /// not carry (CF=0).
	E   = 0x4, /// equal (ZF=1).
	Z   = 0x4, /// zero (ZF = 1).
	NE  = 0x5, /// not equal (ZF=0).
	NZ  = 0x5, /// not zero (ZF=0).
	BE  = 0x6, /// below or equal (CF=1 or ZF=1).
	NA  = 0x6, /// not above (CF=1 or ZF=1).
	A   = 0x7, /// above (CF=0 and ZF=0).
	NBE = 0x7, /// not below or equal (CF=0 andZF=0).
	S   = 0x8, /// sign (SF=1).
	NS  = 0x9, /// not sign (SF=0).
	P   = 0xA, /// parity (PF=1).
	PE  = 0xA, /// parity even (PF=1).
	NP  = 0xB, /// not parity (PF=0).
	PO  = 0xB, /// parity odd (PF=0).
	L   = 0xC, /// less (SF≠ OF).
	NGE = 0xC, /// not greater or equal (SF≠ OF).
	GE  = 0xD, /// greater or equal (SF=OF).
	NL  = 0xD, /// not less (SF=OF).
	LE  = 0xE, /// less or equal (ZF=1 or SF≠ OF).
	NG  = 0xE, /// not greater (ZF=1 or SF≠ OF).
	G   = 0xF, /// greater (ZF=0 and SF=OF).
	NLE = 0xF, /// not less or equal (ZF=0 andSF=OF).
}

Condition complementaryCondition(Condition cond) { return cast(Condition)(cond ^ 1);}

// place 1 MSB of register into appropriate bit field of REX prefix
ubyte regTo_Rex_W(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 0; } // 1000 WRXB
ubyte regTo_Rex_R(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 1; } // 0100 WRXB
ubyte regTo_Rex_X(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 2; } // 0010 WRXB
ubyte regTo_Rex_B(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 3; } // 0001 WRXB

// place 3 LSB of register into appropriate bit field of ModR/M byte
ubyte regTo_ModRm_Reg(Register reg) pure nothrow @nogc { return (reg & 0b0111) << 3; }
ubyte regTo_ModRm_Rm(Register reg) pure nothrow @nogc { return (reg & 0b0111) << 0; }

struct SibScale { ubyte bits; ubyte value() { return cast(ubyte)(1 << bits); } }
struct ModRmMod { ubyte bits; }

ubyte encodeSibByte(SibScale ss, Register index, Register base) pure nothrow @nogc {
	return cast(ubyte)(ss.bits << 6) | (index & 0b0111) << 3 | (base & 0b0111);
}

ubyte encodeModRegRmByte(ModRmMod mod, Register reg, Register rm) pure nothrow @nogc {
	return cast(ubyte)(mod.bits << 6) | (reg & 0b0111) << 3 | (rm & 0b0111);
}

enum MemAddrType : ubyte {
	disp32,           // [                     disp32]
	indexDisp32,      // [       (index * s) + disp32]
	base,             // [base                       ]
	baseDisp32,       // [base +             + disp32]
	baseIndex,        // [base + (index * s)         ]
	baseIndexDisp32,  // [base + (index * s) + disp32]
	baseDisp8,        // [base +             + disp8 ]
	baseIndexDisp8,   // [base + (index * s) + disp8 ]
	ripDisp32         // [RIP  +             + disp32]
}
ubyte sibAddrType(MemAddrType type) { return 0b1_0000 | type; }

ubyte[9] memAddrType_to_mod = [0,0,0,2,0,2,1,1,0];
ubyte[9] memAddrType_to_dispType = [1,1,0,1,0,1,2,2,1]; // 0 - none, 1 - disp32, 2 - disp8

// memory location that can be passed to assembly instructions
struct MemAddress {
	ubyte typeStorage; // MemAddrType | 0b1_0000;
	Register indexReg = Register.SP;
	Register baseReg  = Register.BP;
	SibScale scale;
	uint disp; // disp8 is stored here too

	MemAddrType type() { return cast(MemAddrType)(typeStorage & 0b1111); }
	Imm32 disp32() @property { return Imm32(disp); }
	Imm8 disp8() @property { return Imm8(cast(ubyte)(disp & 0xFF)); }

	ubyte rexBits() { return regTo_Rex_X(indexReg) | regTo_Rex_B(baseReg); }
	ubyte modRmByte(ubyte reg = 0) {
		return encodeModRegRmByte(ModRmMod(memAddrType_to_mod[type]), cast(Register)reg, hasSibByte ? Register.SP : baseReg);
	}
	ModRmMod mod() { return ModRmMod(memAddrType_to_mod[type]); }
	ubyte sibByte() { return encodeSibByte(scale, indexReg, baseReg); }
	bool hasDisp32() { return memAddrType_to_dispType[type] == 1; }
	bool hasDisp8 () { return memAddrType_to_dispType[type] == 2; }
	bool hasSibByte() { return cast(bool)(typeStorage & 0b1_0000); }

	string toString() {
		final switch(type) {
			case MemAddrType.disp32: return format("[0x%x]", disp32.value);
			case MemAddrType.indexDisp32: return format("[(%s*%s) + 0x%x]", indexReg, scale.value, disp32.value);
			case MemAddrType.base: return format("[%s]", baseReg);
			case MemAddrType.baseDisp32: return format("[%s + 0x%x]", baseReg, disp32.value);
			case MemAddrType.baseIndex: return format("[%s + (%s*%s)]", baseReg, indexReg, scale.value);
			case MemAddrType.baseIndexDisp32: return format("[%s + (%s*%s) + 0x%x]", baseReg, indexReg, scale.value, disp32.value);
			case MemAddrType.baseDisp8: return format("[%s + 0x%x]", baseReg, disp8.value);
			case MemAddrType.baseIndexDisp8: return format("[%s + (%s*%s) + 0x%x]", baseReg, indexReg, scale.value, disp8.value);
			case MemAddrType.ripDisp32: return format("[RIP + 0x%x]", disp32.value);
		}
	}
}

// variant 1  [disp32]
MemAddress memAddrDisp32(uint disp32) {
	return MemAddress(sibAddrType(MemAddrType.disp32), Register.SP, Register.BP, SibScale(), disp32); // with SIB
}
// variant 2  [(index * s) + disp32]
MemAddress memAddrIndexDisp32(Register indexReg, SibScale scale, uint disp32) {
	assert(indexReg != Register.SP, "Cannot encode [RSP * scale + disp32]");
	return MemAddress(sibAddrType(MemAddrType.indexDisp32), indexReg, Register.BP, scale, disp32); // with SIB
}
// variant 3  [base]
MemAddress memAddrBase(Register baseReg) {
	if (is_BP_or_R13(baseReg)) // fallback to variant 7 [base + 0x0]
		return memAddrBaseDisp8(baseReg, 0); // with or without SIB
	else if (is_SP_or_R12(baseReg)) // cannot encode SP,R12 without SIB
		return MemAddress(sibAddrType(MemAddrType.base), Register.SP, baseReg); // with SIB
	else
		return MemAddress(MemAddrType.base, Register.SP, baseReg); // no SIB
}
// variant 4  [base + disp32]
MemAddress memAddrBaseDisp32(Register baseReg, uint disp32) {
	if (is_SP_or_R12(baseReg))
		return MemAddress(sibAddrType(MemAddrType.baseDisp32), Register.SP, baseReg, SibScale(), disp32); // with SIB
	else
		return MemAddress(MemAddrType.baseDisp32, Register.SP, baseReg, SibScale(), disp32); // no SIB
}
// variant 5  [base + index * s]
MemAddress memAddrBaseIndex(Register baseReg, Register indexReg, SibScale scale) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale]");
	if (is_BP_or_R13(baseReg)) // fallback to variant 8 [base + (index * s) + disp8]
		return memAddrBaseIndexDisp8(baseReg, indexReg, scale, 0); // with SIB
	else
		return MemAddress(sibAddrType(MemAddrType.baseIndex), indexReg, baseReg, scale); // with SIB
}
// variant 6  [base + index * s + disp32]
MemAddress memAddrBaseIndexDisp32(Register baseReg, Register indexReg, SibScale scale, uint disp32) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale + disp32]");
	return MemAddress(sibAddrType(MemAddrType.baseIndexDisp32), indexReg, baseReg, scale, disp32); // with SIB
}
// variant 7  [base + disp8]
MemAddress memAddrBaseDisp8(Register baseReg, ubyte disp8) {
	if (is_SP_or_R12(baseReg)) // cannot encode SP,R12 without SIB
		return MemAddress(sibAddrType(MemAddrType.baseDisp8), Register.SP, baseReg, SibScale(), disp8); // with SIB
	else
		return MemAddress(MemAddrType.baseDisp8, Register.SP, baseReg, SibScale(), disp8); // no SIB
}
// variant 8  [base + (index * s) + disp8]
MemAddress memAddrBaseIndexDisp8(Register baseReg, Register indexReg, SibScale scale, ubyte disp8) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale + disp8]");
	return MemAddress(sibAddrType(MemAddrType.baseIndexDisp8), indexReg, baseReg, scale, disp8); // with SIB
}

// variant 9  [RIP + disp32]
MemAddress memAddrRipDisp32(uint disp32) {
	return MemAddress(MemAddrType.ripDisp32, Register.SP, Register.BP, SibScale(), disp32); // with SIB
}

// Shortcut for memAddrBaseDisp32 and memAddrBaseDisp8. memAddrBaseDisp8 is used when possible.
MemAddress minMemAddrBaseDisp(Register baseReg, int displacement)
{
	if (displacement < byte.min || displacement > byte.max)
		return memAddrBaseDisp32(baseReg, displacement);
	else
		return memAddrBaseDisp8(baseReg, cast(byte)displacement);
}

// Opcode structures for 1-byte and 2-byte encodings
struct OP1 { enum size = 1; ubyte op0; }
struct OP2 { enum size = 2; ubyte op0; ubyte op1; }
enum bool isAnyOpcode(O) = is(O == OP1) || is(O == OP2);

alias PC = ubyte*;

struct Encoder
{
	private ubyte[] mem;
	private PC pc;

	uint pcOffset() { return cast(uint)(pc - mem.ptr); }
	void setBuffer(ubyte[] buf) { mem = buf; pc = mem.ptr; }
	ubyte[] freeBuffer() { scope(exit) { mem = null; resetPC; } return mem; }
	void resetPC() { pc = mem.ptr; }
	ubyte[] code() { return mem[0..pc - mem.ptr]; }

	void sink_put(T)(T value)
	{
		*(cast(T*)pc) = value;
		pc += value.sizeof;
	}

	void putRexByteChecked(ArgType argType)(ubyte bits, bool forceRex = false) {
		static if (argType == ArgType.QWORD)
			sink_put!ubyte(REX_PREFIX | REX_W | bits);
		else
			if (bits || forceRex) sink_put!ubyte(REX_PREFIX | bits);
	}
	void putRexByte_RB(ArgType argType)(Register reg, Register rm) { // reg reg
		putRexByteChecked!argType(regTo_Rex_R(reg) | regTo_Rex_B(rm), regNeedsRexPrefix!argType(reg) || regNeedsRexPrefix!argType(rm)); }
	void putRexByte_regB(ArgType argType)(Register rm) { // R.R/M reg
		putRexByteChecked!argType(regTo_Rex_B(rm), regNeedsRexPrefix!argType(rm)); }
	void putRexByte_B(ArgType argType)(Register base) { // base
		putRexByteChecked!argType(regTo_Rex_B(base)); }
	void putRexByte_RXB(ArgType argType)(Register r, Register index, Register base) { // reg index base
		putRexByteChecked!argType(regTo_Rex_R(r) | regTo_Rex_X(index) | regTo_Rex_B(base), regNeedsRexPrefix!argType(r)); }
	void putRexByte_XB(ArgType argType)(Register index, Register base) { // index base
		putRexByteChecked!argType(regTo_Rex_X(index) | regTo_Rex_B(base)); }

	void putInstrBinaryRegReg(ArgType argType, O)(O opcode, Register dst_rm, Register src_reg) if (isAnyOpcode!O) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_RB!argType(src_reg, dst_rm);                                 // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), src_reg, dst_rm));          // ModR/r
	}
	// PUSH, POP, MOV, XCHG, BSWAP
	void putInstrBinaryRegImm1(ArgType argType, I)(OP1 opcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!argType(dst_rm);                                        // REX
		sink_put!ubyte(opcode.op0 | (dst_rm & 0b0111));                             // Opcode + reg
		sink_put(src_imm);                                                      // Imm8/16/32/64
	}
	void putInstrBinaryRegImm2(ArgType argType, I)(OP1 opcode, ubyte regOpcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!argType(dst_rm);                                        // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), cast(Register)regOpcode, dst_rm));  // ModO/R
		sink_put(src_imm);                                                      // Imm8/16/32/64
	}
	// if isReg == true then dst_r is register, otherwise it is extra opcode
	void putInstrBinaryRegMem(ArgType argType, bool isReg = true, O)(O opcode, Register reg_or_opcode, MemAddress src_mem) if (isAnyOpcode!O) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		static if (isReg) putRexByte_RXB!argType(reg_or_opcode, src_mem.indexReg, src_mem.baseReg); // REX
		else putRexByte_XB!argType(src_mem.indexReg, src_mem.baseReg);          // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(src_mem.modRmByte(reg_or_opcode));                             // ModR/M
		if (src_mem.hasSibByte)	   sink_put(src_mem.sibByte);                   // SIB
		if (src_mem.hasDisp32)     sink_put(src_mem.disp32);                    // disp32
		else if (src_mem.hasDisp8) sink_put(src_mem.disp8);                     // disp8
	}
	void putInstrBinaryMemImm(ArgType argType, I, O)(O opcode, ubyte regOpcode, MemAddress dst_mem, I src_imm) if (isAnyImm!I && isAnyOpcode!O) {
		putInstrBinaryRegMem!(argType, false)(opcode, cast(Register)regOpcode, dst_mem);
		sink_put(src_imm);                                                      // Imm8/16/32
	}

	void putInstrNullary(O)(O opcode) if(isAnyOpcode!O) {
		sink_put(opcode);                                                       // Opcode
	}
	void putInstrNullaryImm(O, I)(O opcode, I imm) if(isAnyOpcode!O && isAnyImm!I) {
		sink_put(opcode);                                                       // Opcode
		sink_put(imm);                                                          // Imm8/16/32/64
	}
	void putInstrUnaryReg1(ArgType argType, O)(O opcode, ubyte regOpcode, Register dst_rm) if (isAnyOpcode!O) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!argType(dst_rm);                                        // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), cast(Register)regOpcode, dst_rm));// ModO/R
	}
	void putInstrUnaryReg2(ArgType argType)(ubyte opcode, Register dst_rm) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!argType(dst_rm);                                        // REX
		sink_put!ubyte(opcode | (dst_rm & 0b0111));                             // Opcode
	}
	void putInstrUnaryMem(ArgType argType, O)(O opcode, ubyte regOpcode, MemAddress dst_mem) if (isAnyOpcode!O) {
		putInstrBinaryRegMem!(argType, false)(opcode, cast(Register)regOpcode, dst_mem);
	}
	void putInstrUnaryImm(ArgType argType, O, I)(O opcode, I imm) if (isAnyOpcode!O && isAnyImm!I) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		sink_put(opcode);                                                       // Opcode
		sink_put(imm);                                                          // Imm8/16/32
	}
}

struct Fixup
{
	private CodeGen_x86_64* codeGen;
	private PC fixupPC;

	template opDispatch(string member)
	{
		import std.traits : Parameters;
		static foreach(Over; __traits(getOverloads, CodeGen_x86_64, member))
		{
			auto opDispatch(Parameters!(Over) args) {
				auto tempPC = codeGen.encoder.pc;
				codeGen.encoder.pc = fixupPC;
				scope(exit)codeGen.encoder.pc = tempPC;
				mixin("return codeGen."~member~"(args);");
			}
		}
	}
}

struct Fixup32
{
	uint fixupOffset;
	uint extraOffset;
}

Imm32 jumpOffset(PC from, PC to) {
	assert(to - from == cast(int)(to - from), "offset is not representible as int");
	return Imm32(cast(int)(to - from));
}

// Sink defines put(T) for ubyte, ubyte[], Imm8, Imm16, Imm32, Imm64
struct CodeGen_x86_64
{
	Encoder encoder;

	Fixup fixupAt(PC at) { return Fixup(&this, at); }
	Fixup saveFixup() { return Fixup(&this, encoder.pc); }
	PC pc() { return encoder.pc; }
	alias stubPC = pc;

	/// Used for versions of instructions without argument size suffix.
	/// mov, add, sub, instead of movq, addb, subd.
	/// mov(Register.AX, Register.DI, ArgType.QWORD); instead of movq(Register.AX, Register.DI);
	void opDispatch(string s, Arg1, Arg2)(Arg1 dst, Arg2 src, ArgType argType) {
		switch(argType) {
			static if (__traits(hasMember, typeof(this), s~'b')) { case ArgType.BYTE:  mixin(s~"b(dst, src);"); break; }
			static if (__traits(hasMember, typeof(this), s~'w')) { case ArgType.WORD:  mixin(s~"w(dst, src);"); break; }
			static if (__traits(hasMember, typeof(this), s~'d')) { case ArgType.DWORD: mixin(s~"d(dst, src);"); break; }
			static if (__traits(hasMember, typeof(this), s~'q')) { case ArgType.QWORD: mixin(s~"q(dst, src);"); break; }
			default: assert(false, format("Cannot encode %s(%s, %s, ArgType.%s)", s, dst, src, argType));
		}
	}

	/// ditto
	void opDispatch(string s, Arg1)(Arg1 dst, ArgType argType) {
		switch(argType) {
			static if (__traits(hasMember, typeof(this), s~'b')) { case ArgType.BYTE:  mixin(s~"b(dst);"); break; }
			static if (__traits(hasMember, typeof(this), s~'w')) { case ArgType.WORD:  mixin(s~"w(dst);"); break; }
			static if (__traits(hasMember, typeof(this), s~'d')) { case ArgType.DWORD: mixin(s~"d(dst);"); break; }
			static if (__traits(hasMember, typeof(this), s~'q')) { case ArgType.QWORD: mixin(s~"q(dst);"); break; }
			default: assert(false, format("Cannot encode %s(%s, ArgType.%s)", s, dst, argType));
		}
	}

	void beginFunction() {
		// Copies parameters from registers to shadow space
		// Pushes registers to be preserved on stack
		// Allocates room on stack for local variables
		// Sets a frame pointer (so frame pointer is set AFTER local variables!) if needed
		pushq(Register.BP);
		movq(Register.BP, Register.SP);
		// Allocates space needed to store volatile registers that must be preserved in function calls
		// Allocates shadow space for called functions.
	}
	void endFunction() {
		popq(Register.BP);
		ret();
	}

	mixin binaryInstr_RMtoR_RtoRM!("add", [0x00, 0x01], [0x02, 0x03]);
	mixin binaryInstr_RM_Imm!("add", 0);

	mixin instrMOV!();
	mixin binaryInstr_RMtoR_RtoRM!("mov", [0x88, 0x89], [0x8A, 0x8B]);

	mixin binaryInstr_RMtoR_RtoRM!("sub", [0x28, 0x29], [0x2A, 0x2B]);
	mixin binaryInstr_RM_Imm!("sub", 5);

	mixin binaryInstr_RMtoR_RtoRM!("and", [0x20, 0x21], [0x22, 0x23]);
	mixin binaryInstr_RM_Imm!("and", 4);

	mixin binaryInstr_RMtoR_RtoRM!("or", [0x08, 0x09], [0x0A, 0x0B]);
	mixin binaryInstr_RM_Imm!("or", 1);

	mixin binaryInstr_RMtoR_RtoRM!("xor", [0x30, 0x31], [0x32, 0x33]);
	mixin binaryInstr_RM_Imm!("xor", 6);

	mixin binaryInstr_RMtoR_RtoRM!("cmp", [0x38, 0x39], [0x3A, 0x3B]);
	mixin binaryInstr_RM_Imm!("cmp", 7);

	void leaw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(0x8D), dst, src); }
	void lead(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(0x8D), dst, src); }
	void leaq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(0x8D), dst, src); }

	mixin unaryInstr_RM!("inc", [0xFE,0xFF], 0);
	mixin unaryInstr_RM!("dec", [0xFE,0xFF], 1);
	mixin unaryInstr_RM!("neg", [0xF6,0xF7], 3);
	mixin unaryInstr_RM!("mul", [0xF6,0xF7], 4);
	mixin unaryInstr_RM!("div", [0xF6,0xF7], 6);
	mixin unaryInstr_RM!("not", [0xF6,0xF7], 2);

	void nop() { encoder.putInstrNullary(OP1(0x90)); }

	/// relative call to target virtual address.
	void call(PC target) { encoder.putInstrNullaryImm(OP1(0xE8), jumpOffset(encoder.pc + 5, target)); } // relative to next instr
	void call(Register target) { encoder.putInstrUnaryReg1!(ArgType.QWORD)(OP1(0xFF), 2, target); } // absolute address
	void call(MemAddress target) { encoder.putInstrUnaryMem!(ArgType.DWORD)(OP1(0xFF), 2, target); } // absolute address, use DWORD to omit REX.W

	/// Generate fixup for last 32 bits of last instruction.
	Fixup32 getAddressFixup() { return Fixup32(encoder.pcOffset - 4, 4); }
	Fixup32 getDataFixup() { return Fixup32(encoder.pcOffset - 4, 0); }

	/// jump relative to next instr.
	void jmp(Imm8 offset ) { encoder.putInstrNullaryImm(OP1(0xEB), offset); }
	void jmp(Imm32 offset) { encoder.putInstrNullaryImm(OP1(0xE9), offset); }
	void jmpAbs(PC target) { encoder.putInstrNullaryImm(OP1(0xE9), jumpOffset(encoder.pc + 5, target) ); }

	/// jump relative to next instr.
	void jcc(Condition condition, Imm8  offset) { encoder.putInstrNullaryImm(OP1(0x70 | condition), offset); }
	void jcc(Condition condition, Imm32 offset) { encoder.putInstrNullaryImm(OP2(0x0F, 0x80 | condition), offset); }
	void jccAbs(Condition condition, PC target) { encoder.putInstrNullaryImm(OP2(0x0F, 0x80 | condition), jumpOffset(encoder.pc + 6, target) ); }

	void setcc(Condition condition, Register dst)   { encoder.putInstrUnaryReg1!(ArgType.BYTE)(OP2(0x0F, 0x90 | condition), 0, dst); }
	void setcc(Condition condition, MemAddress dst) { encoder.putInstrUnaryMem !(ArgType.BYTE)(OP2(0x0F, 0x90 | condition), 0, dst); }

	void testb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (OP1(0x84), dst, src); }
	void testw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP1(0x85), dst, src); }
	void testd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP1(0x85), dst, src); }
	void testq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP1(0x85), dst, src); }

	void movzx_btow(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP2(0x0F, 0xB6), dst, src); }
	void movzx_btod(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP2(0x0F, 0xB6), dst, src); }
	void movzx_btoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP2(0x0F, 0xB6), dst, src); }
	void movzx_wtod(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP2(0x0F, 0xB7), dst, src); }
	void movzx_wtoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP2(0x0F, 0xB7), dst, src); }

	void popw(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.WORD )(0x58, dst); }
	void popq(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.DWORD)(0x58, dst); } // use DWORD to omit REX.W
	void popw(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.WORD )(OP1(0x8F), 0, dst); }
	void popq(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.DWORD)(OP1(0x8F), 0, dst); } // use DWORD to omit REX.W

	void pushw(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.WORD )(0x50, dst); }
	void pushq(Register dst)   { encoder.putInstrUnaryReg2!(ArgType.DWORD)(0x50, dst); } // use DWORD to omit REX.W
	void pushw(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.WORD )(OP1(0xFF), 6, dst); }
	void pushq(MemAddress dst) { encoder.putInstrUnaryMem!( ArgType.DWORD)(OP1(0xFF), 6, dst); } // use DWORD to omit REX.W

	void pushb(Imm8  src) { encoder.putInstrUnaryImm!(ArgType.BYTE )(OP1(0x6A), src); }
	void pushw(Imm16 src) { encoder.putInstrUnaryImm!(ArgType.WORD )(OP1(0x68), src); }
	void pushd(Imm32 src) { encoder.putInstrUnaryImm!(ArgType.DWORD)(OP1(0x68), src); }

	void ret() { encoder.putInstrNullary(OP1(0xC3)); }
	void ret(Imm16 bytesToPop) { encoder.putInstrNullaryImm(OP1(0xC2), bytesToPop); }

	void int3() { encoder.putInstrNullary(OP1(0xCC)); }
}

mixin template instrMOV() {
	void movb(Register dst, Imm8  src){ encoder.putInstrBinaryRegImm1!(ArgType.BYTE) (OP1(0xB0), dst, src); }
	void movw(Register dst, Imm16 src){ encoder.putInstrBinaryRegImm1!(ArgType.WORD) (OP1(0xB8), dst, src); }
	void movd(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm1!(ArgType.DWORD)(OP1(0xB8), dst, src); }
	void movq(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(OP1(0xC7), 0, dst, src); }
	void movq(Register dst, Imm64 src){ encoder.putInstrBinaryRegImm1!(ArgType.QWORD)(OP1(0xB8), dst, src); }

	void movb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (OP1(0xC6), 0, dst, src); }
	void movw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0xC7), 0, dst, src); }
	void movd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0xC7), 0, dst, src); }
	void movq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0xC7), 0, dst, src); }
}

mixin template binaryInstr_RMtoR_RtoRM(string name, ubyte[2] rm_r, ubyte[2] r_rm) {
	mixin(format("void %sb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(%s), src, dst); }", name, rm_r[0]));
	mixin(format("void %sw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(%s), src, dst); }", name, rm_r[1]));
	mixin(format("void %sd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(%s), src, dst); }", name, rm_r[1]));
	mixin(format("void %sq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(%s), src, dst); }", name, rm_r[1]));

	mixin(format("void %sb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.BYTE) (OP1(%s), dst, src); }", name, rm_r[0]));
	mixin(format("void %sw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP1(%s), dst, src); }", name, rm_r[1]));
	mixin(format("void %sd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP1(%s), dst, src); }", name, rm_r[1]));
	mixin(format("void %sq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP1(%s), dst, src); }", name, rm_r[1]));

	mixin(format("void %sb(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(%s), dst, src); }", name, r_rm[0]));
	mixin(format("void %sw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(%s), dst, src); }", name, r_rm[1]));
	mixin(format("void %sd(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(%s), dst, src); }", name, r_rm[1]));
	mixin(format("void %sq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(%s), dst, src); }", name, r_rm[1]));
}

mixin template binaryInstr_RM_Imm(string name, ubyte extraOpcode) {
	mixin(format("void %sb(Register dst,   Imm8  src){ encoder.putInstrBinaryRegImm2!(ArgType.BYTE) (OP1(0x80), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sw(Register dst,   Imm16 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (OP1(0x81), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sd(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(OP1(0x81), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sq(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(OP1(0x81), %s, dst, src); }", name, extraOpcode));

	mixin(format("void %sw(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.WORD) (OP1(0x83), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sd(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.DWORD)(OP1(0x83), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sq(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(ArgType.QWORD)(OP1(0x83), %s, dst, src); }", name, extraOpcode));

	mixin(format("void %sb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(ArgType.BYTE) (OP1(0x80), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0x81), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0x81), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0x81), %s, dst, src); }", name, extraOpcode));

	mixin(format("void %sw(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.WORD) (OP1(0x83), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sd(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.DWORD)(OP1(0x83), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sq(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(ArgType.QWORD)(OP1(0x83), %s, dst, src); }", name, extraOpcode));
}

mixin template unaryInstr_RM(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(Register dst) { encoder.putInstrUnaryReg1!(ArgType.BYTE) (OP1(%s), %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(Register dst) { encoder.putInstrUnaryReg1!(ArgType.WORD) (OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(Register dst) { encoder.putInstrUnaryReg1!(ArgType.DWORD)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(Register dst) { encoder.putInstrUnaryReg1!(ArgType.QWORD)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));

	mixin(format("void %sb(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.BYTE) (OP1(%s), %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.WORD) (OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.DWORD)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(MemAddress dst) { encoder.putInstrUnaryMem!(ArgType.QWORD)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
}


//              #     #  #######   #####   #         #####
//              #     #     #        #     #        #     #
//              #     #     #        #     #        #
//              #     #     #        #     #         #####
//              #     #     #        #     #              #
//              #     #     #        #     #        #     #
//               #####      #      #####   ######    #####
// -----------------------------------------------------------------------------
import core.time : MonoTime, Duration, usecs, dur;
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

	auto absScale = scale * logSign;
	if (absScale < -24) absScale = 0;
	int clampedScale = clamp(absScale, -24, 24);

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

void printHex(ubyte[] buffer, size_t lineLength)
{
	size_t index = 0;
	if (lineLength) {
		while (index + lineLength <= buffer.length) {
			writefln("%(%02X %)", buffer[index..index+lineLength]);
			index += lineLength;
		}
	}
	if (index < buffer.length)
		writefln("%(%02X %)", buffer[index..buffer.length]);
}

enum size_t PAGE_SIZE = 4096;

version(Posix)
{
	ubyte[] allocate(size_t bytes, bool is_executable, MemType memoryType)
	{
		import core.sys.posix.sys.mman : mmap, MAP_ANON, PROT_READ,
			PROT_WRITE, PROT_EXEC, MAP_PRIVATE, MAP_FAILED;
		if (!bytes) return null;

		int protection = PROT_READ | PROT_WRITE;

		final switch(memoryType)
		{
			case MemType.RW:  protection |= 0; break;
			case MemType.RWX: protection |= PROT_EXEC; break;
		}

		auto p = mmap(null, bytes, protection, MAP_PRIVATE | MAP_ANON, -1, 0);
		if (p is MAP_FAILED) return null;
		return cast(ubyte[])p[0 .. bytes];
	}

	bool deallocate(ubyte[] b)
	{
		import core.sys.posix.sys.mman : munmap;
		if (b.ptr) munmap(b.ptr, b.length) == 0 || assert(0);
		return true;
	}
}
else version(Windows)
{
	import core.sys.windows.windows : GetLastError, VirtualAlloc, VirtualFree, VirtualProtect, MEM_COMMIT,
		PAGE_READWRITE, MEM_RELEASE, PAGE_EXECUTE_READWRITE, MEM_RESERVE;

	ubyte[] allocate(size_t bytes, void* location, MemType memoryType)
	{
		if (!bytes) return null;

		int protection;

		final switch(memoryType)
		{
			case MemType.RW:  protection = PAGE_READWRITE; break;
			case MemType.RWX: protection = PAGE_EXECUTE_READWRITE; break;
		}

		auto p = VirtualAlloc(location, bytes, MEM_COMMIT | MEM_RESERVE, protection);

		if (p == null)
		{
			import std.stdio;
			import std.windows.syserror;
			int errCode = GetLastError();
			writeln(sysErrorString(errCode));
			assert(false, "VirtualAlloc alloc failed");
			return null;
		}

		return cast(ubyte[])p[0 .. bytes];
	}

	bool deallocate(ubyte[] b)
	{
		return b.ptr is null || VirtualFree(b.ptr, 0, MEM_RELEASE) != 0;
	}

	void markAsRW(void* addr, size_t numPages)
	{
		uint val;
		VirtualProtect(addr, numPages*PAGE_SIZE, PAGE_READWRITE, &val);
	}
}

enum MemType
{
	RW,
	RWX
}

ubyte[] alloc_executable_memory(size_t bytes)
{
	return allocate(bytes, cast(void*)0x4000_0000UL, MemType.RWX);
}

bool free_executable_memory(ubyte[] bytes)
{
	return deallocate(bytes);
}
