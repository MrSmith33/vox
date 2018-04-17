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
import std.bitmanip : bitfields;
import std.algorithm : min, max, sort, swap;

// Grammar
version = print;
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
	//bench();
	test();
}

struct Test
{
	string testName;
	string source;
	string funcName;
	alias Tester = void function(void* funcPtr);
	void function(void* funcPtr) tester;
}

string input1 = q{
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

string input2 = q{void e() {
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
}};

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
string input5 = q{void f() {
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
}};

string input6 = q{void f() {
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
}};

string input7 = q{i32 fib(i32 number) {
	if (number < 1) return 0;
	if (number < 3) return 1;
	return fib(number-1) + fib(number-2);
}};

string input9 = q{i32 sign(i32 number) {
	i32 result;
	if (1 == 1)
	{
		result = 1;
	}
	else
	{
		result = 0;
	}
	return result;
}};

string input8 = q{i32 sign(i32 number) {
	i32 result;
	if (number < 0) result = 0-1;
	else if (number > 0) result = 1;
	else result = 0;
	return result;
}};
alias Func8 = extern(C) int function(int);
void tester8(Func8 fun) {
	int res1 = fun(10);
	int res2 = fun(0);
	int res3 = fun(-10);
	//writefln("sign(10) -> %s", res1);
	//writefln("sign(0) -> %s", res2);
	//writefln("sign(-10) -> %s", res3);
}

immutable input10 = q{i32 test(i32* array) {
	return array[0];
}};
alias Func10 = extern(C) int function(int*);
void tester10(Func10 fun) {
	int val = 42;
	int res = fun(&val);
	//writefln("test(&42) -> %s", res);
	assert(res == 42);
}
auto test10 = Test("Test 10", input10, "test", cast(Test.Tester)&tester10);

immutable input11 = q{i32 test(i32* array) {
	return array[1];
}};
alias Func11 = extern(C) int function(int*);
void tester11(Func11 fun) {
	int[2] val = [42, 56];
	int res = fun(val.ptr);
	//writefln("test([42, 56].ptr) -> %s", res);
	assert(res == 56);
}
auto test11 = Test("Test 11", input11, "test", cast(Test.Tester)&tester11);

immutable input12 = q{i32 test(i32* array, i32 index) {
	return array[index];
}};
alias Func12 = extern(C) int function(int*, int);
void tester12(Func12 fun) {
	int[2] val = [42, 56];
	int res = fun(val.ptr, 1);
	//writefln("test([42, 56].ptr, 1) -> %s", res);
	assert(res == 56);
}
auto test12 = Test("Test 12", input12, "test", cast(Test.Tester)&tester12);

immutable input13 = q{void test(i32* array, i32 index, i32 value) {
	array[index] = value;
}};
alias Func13 = extern(C) void function(int*, int, int);
void tester13(Func13 fun) {
	int[2] val = [42, 56];
	fun(val.ptr, 1, 20);
	//writefln("test([42, 56].ptr, 1, 20) -> %s", val);
	assert(val[1] == 20);
}
auto test13 = Test("Test 13", input13, "test", cast(Test.Tester)&tester13);

void test()
{
	Test curTest = test13;
	Driver driver;
	ubyte[] codeBuffer = alloc_executable_memory(PAGE_SIZE * 8);

	try
	{
		driver.initPasses();
		ModuleDeclNode* mod = driver.compileModule(curTest.source, codeBuffer);
		if (mod is null) return;

		version(print)
		{
			// Text dump
			auto astPrinter = AstPrinter(&driver.context, 2);
			astPrinter.printAst(cast(AstNode*)mod);

			writeln("// Source");
			writeln(driver.context.input);

			writeln("\n// IR");
			TextSink sink;
			mod.irModule.dump(sink, &driver.context);
			writeln(sink.text);

			writeln("\n// Amd64 code");
			printHex(mod.irModule.code, 16);
		}

		auto funDecl = mod.findFunction(curTest.funcName, &driver.context);
		if (funDecl != null && funDecl.irData.funcPtr != null)
		{
			curTest.tester(funDecl.irData.funcPtr);
		}
	}
	catch(CompilationException e) {
		writeln(driver.context.sink.text);
		if (e.isICE)
			writeln(e);
	}
	catch(Throwable t) {
		writeln(driver.context.sink.text);
		writeln(t);
	}
	//testNativeFun;
}

void bench()
{
	ModuleDeclNode* mod;
	string input =
	q{i32 sign(i32 number)
	{
		i32 result;
		if (number < 0) result = 0-1;
		else if (number > 0) result = 1;
		else result = 0;
		return result;
	}};

	ubyte[] codeBuffer = alloc_executable_memory(PAGE_SIZE * 8);

	Driver driver;
	driver.initPasses();

	enum iters = 100_000;
	auto times = PerPassTimeMeasurements(iters, driver.passes);

	foreach (iteration; 0..times.totalTimes.numIters)
	{
		auto time1 = currTime;
		mod = driver.compileModule(input, codeBuffer);
		auto time2 = currTime;

		times.onIteration(iteration, time2-time1);
	}

	times.print;
}

struct PerPassTimeMeasurements
{
	TimeMeasurements totalTimes;
	TimeMeasurements[] passTimes;
	CompilePass[] passes;

	this(size_t numIters, CompilePass[] passes)
	{
		this.passes = passes;
		totalTimes = TimeMeasurements(numIters);
		passTimes = new TimeMeasurements[passes.length];
		foreach (ref times; passTimes) times = TimeMeasurements(numIters);
	}

	void onIteration(size_t iterIndex, Duration iterTime)
	{
		totalTimes.onIteration(iterIndex, iterTime);
		foreach (passIndex, ref pass; passes)
			passTimes[passIndex].onIteration(iterIndex, pass.duration);
	}

	void print()
	{
		void printRow(string name, ref TimeMeasurements times)
		{
			writef("%- 20s", name);
			times.print;
			writeln;
		}

		writef("Iterations % 9s", scaledNumberFmt(totalTimes.numIters));
		totalTimes.printHeader; writeln;
		printRow("Total", totalTimes);
		foreach (passIndex, ref times; passTimes)
			printRow(passes[passIndex].name, times);
	}
}

struct TimeMeasurements
{
	size_t numIters;
	Duration[] iterTimes;
	Duration totalTime;
	Duration avgTime() { return totalTime/numIters; }
	Duration minTime = Duration.max;
	Duration maxTime = Duration.min;

	this(size_t numIters)
	{
		this.numIters = numIters;
		iterTimes = new Duration[numIters];
	}

	void onIteration(size_t iterIndex, Duration iterTime)
	{
		iterTimes[iterIndex] = iterTime;
		totalTime += iterTime;
		minTime = min(iterTime, minTime);
		maxTime = max(iterTime, maxTime);
	}

	enum showNumFirstIters = 3;

	void printHeader()
	{
		foreach (i; 0..min(numIters, showNumFirstIters))
			writef("  iter %s", i);
		write("   total     avg     min     max");
	}

	void print()
	{
		foreach (i; 0..min(numIters, showNumFirstIters))
			writef(" % 6ss", scaledNumberFmt(iterTimes[i]));
		writef(" % 6ss", scaledNumberFmt(totalTime));
		writef(" % 6ss", scaledNumberFmt(avgTime));
		writef(" % 6ss", scaledNumberFmt(minTime));
		writef(" % 6ss", scaledNumberFmt(maxTime));
	}
}

struct Driver
{
	CompilationContext context;
	CompilePass[] passes;

	void initPasses()
	{
		passes ~= CompilePass("Parse", &pass_parser);
		passes ~= CompilePass("Semantic insert", &pass_semantic_decl);
		passes ~= CompilePass("Semantic lookup", &pass_semantic_lookup);
		passes ~= CompilePass("Semantic types", &pass_semantic_type);
		passes ~= CompilePass("IR gen", &pass_ir_gen);
		passes ~= CompilePass("Live intervals", &pass_live_intervals);
		passes ~= CompilePass("Linear scan", &pass_linear_scan);
		passes ~= CompilePass("Stack layout", &pass_stack_layout);
		passes ~= CompilePass("Code gen", &pass_code_gen);
	}

	ModuleDeclNode* compileModule(string fileData, ubyte[] codeBuffer)
	{
		context = CompilationContext(fileData, codeBuffer);

		try foreach (ref pass; passes)
		{
			auto time1 = currTime;

			pass.run(context);

			auto time2 = currTime;
			pass.duration = time2-time1;

			context.throwOnErrors;
		}
		catch(CompilationException e)
		{
			writeln(context.sink.text);
			//if (e.isICE) // always show stacktrace
				writeln(e);
		}
		return context.mod;
	}
}

struct CompilePass
{
	string name;
	void function(ref CompilationContext context) run;

	Duration duration;
}

void testNativeFun()
{
	auto time0 = currTime;
	int res1;
	int res2;
	int res3;
	test_fun = &sign;
	foreach(_; 0..10_000)
	{
		res1 = test_fun(10);
		res2 = test_fun(0);
		res3 = test_fun(-10);
	}
	auto time1 = currTime;
	writefln("sign(10) -> %s", res1);
	writefln("sign(0) -> %s", res2);
	writefln("sign(-10) -> %s", res3);
	writefln("native fun run x10k %ss", scaledNumberFmt(time1-time0));
}

__gshared int function(int) test_fun;

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
	ubyte[] codeBuffer;
	ModuleDeclNode* mod;
	ScopeStack scopeStack;

	IdentifierMap idMap;
	bool hasErrors;
	TextSink sink;
	bool crashOnICE = true;
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

	void assertf(string file = __FILE__, int line = __LINE__, Args...)(bool cond, string fmt, lazy Args args)
	{
		if (cond) return;
		sink.putf("%s(%s): ICE: Assertion failure: ", file, line);
		sink.putfln(fmt, args);
		hasErrors = true;
		tryCrashOnICE;
		throw new CompilationException(true);
	}

	void assertf(string file = __FILE__, int line = __LINE__, Args...)(bool cond, SourceLocation loc, string fmt, lazy Args args)
	{
		if (cond) return;
		sink.putf("%s(%s): file(%s, %s): ICE: ", file, line, loc.line+1, loc.col+1);
		sink.putfln(fmt, args);
		hasErrors = true;
		tryCrashOnICE;
		throw new CompilationException(true);
	}

	void unreachable() {
		internal_error("Unreachable");
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
	VariableDeclNode*[] parameters;
	BlockStmtNode* block_stmt;
	Scope* _scope;
	IrFunction irData;
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
		visit(f.block_stmt); }
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
		pr_node(cast(AstNode*)f.block_stmt);
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
		return context.idMap.getOrReg(name);
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
			return cast(AstNode*)make!StructDeclNode(start, declarations, structId);
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
				return cast(AstNode*)make!VariableDeclNode(start, declarationId, type);
			}
			else if (tok.type == TokenType.LPAREN) // <func_declaration> ::= <type> <id> "(" <param_list> ")" <block_statement>
			{
				version(print_parse) auto s3 = scop("<func_declaration> %s", start);
				expectAndConsume(TokenType.LPAREN);
				VariableDeclNode*[] params;
				while (tok.type != TokenType.RPAREN)
				{
					TypeNode* paramType = parse_type_expected();
					Identifier paramId = expectIdentifier();
					VariableDeclNode* param = make!VariableDeclNode(start, paramId, paramType);
					param.varFlags |= VariableFlags.isParameter;
					param.paramIndex = cast(typeof(param.paramIndex))params.length;
					params ~= param;
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
			base = cast(TypeNode*)make!UserTypeNode(start, id);
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
				return makeExpr!CallExprNode(start, id, args);
			}
			if (tok.type == TokenType.LBRACKET) // <term> ::= <id> "[" <expr> "]"
			{
				expectAndConsume(TokenType.LBRACKET);
				ExpressionNode* index = expr();
				expectAndConsume(TokenType.RBRACKET);
				ExpressionNode* array = makeExpr!VariableExprNode(start, id);
				return makeExpr!IndexExprNode(start, array, index);
			}
			return makeExpr!VariableExprNode(start, id);
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
		visit(f.block_stmt);
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
		foreach (arg; c.args) _visit(arg);
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
	IrFunction* curFunc; // current function
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
		f.irData = IrFunction(f, &win64_call_conv, f.returnType.irType(context));

		// save previous function
		auto prevFunc = curFunc;

		// set new function as current
		curFunc = &f.irData;
		irModule.addFunction(&f.irData);

		// create Basic Block for function body
		// other code will use `last` Basic Block
		curFunc.addBasicBlock(IrName(startId));
		++curFunc.currentBB.refCount;
		curFunc.sealBlock(curFunc.currentBB);

		foreach (i, param; f.parameters)
		{
			visit(param);
		}

		visit(f.block_stmt);

		if (!curFunc.last.isFinished)
		{
			// Return must present in case of non-void function
			curFunc.last.exit = IrJump(IrJump.Type.ret0);
		}
		curFunc.sealBlock(curFunc.currentBB);
		curFunc.finishBlock();
		addUsers();

		// restore previous function
		curFunc = prevFunc;
	}

	void addUsers()
	{
		for (IrBasicBlock* block = curFunc.start; block; block = block.next)
		{
			switch(block.exit.type) with(IrJump.Type) {
				case ret1, branch: curFunc.addUser(block.lastInstrRef, block.exit.value); break;
				default: break;
			}
		}
	}

	void store(VariableDeclNode* v, IrRef value)
	{
		if (v.forceMemoryStorage)
		{
			curFunc.emitInstr2(IrOpcode.o_store, v.type.irType(context), v.stackSlotId, value);
		}
		else
		{
			curFunc.writeVariable(v.irVar, value);
		}
	}

	IrRef load(VariableDeclNode* v)
	{
		if (v.forceMemoryStorage)
		{
			return curFunc.emitInstr1(IrOpcode.o_load, v.type.irType(context), v.stackSlotId);
		}
		else
		{
			return curFunc.readVariable(v.irVar);
		}
	}

	void visit(VariableDeclNode* v)
	{
		IrRef irRef;
		v.irVar = IrVar(v.strId(context), curFunc.newIrVarId(), v.type.irType(context));

		if (context.buildDebug)
			v.varFlags |= VariableFlags.forceMemoryStorage;

		if (v.forceMemoryStorage)
		{
			v.stackSlotId = curFunc.stackLayout.addStackItem(v.type.size, v.type.alignment, v.isParameter, v.paramIndex);
		}

		if (v.isParameter)
		{
			++curFunc.numParameters;
			irRef = curFunc.emitInstr0(IrOpcode.o_param, v.type.irType(context));
			curFunc.writeVariable(v.irVar, irRef);
		}
		else
		{
			irRef = curFunc.put(0);
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
			if (curFunc.currentBB.isFinished) break;
		}
	}

	void tryInvertCondition(ExpressionNode* condition)
	{
		IrRef valueRef = condition.irRef;
		// invert condition, so that we jump to else on success
		if (valueRef.kind == IrValueKind.instr)
		{
			auto instr = &curFunc.instructions[valueRef.index];
			instr.condition = inverseIrCond[instr.condition];
		}
		else if (valueRef.isLiteral)
		{
			bool arg0 = curFunc.constants[valueRef.constIndex].i1;
			condition.irRef = curFunc.put(!arg0);
		}
		else context.internal_error(condition.loc, "Cannot invert condition");
	}

	IrCond getCondition(ExpressionNode* condition)
	{
		IrRef valueRef = condition.irRef;
		context.assertf(valueRef.kind == IrValueKind.instr, condition.loc, "Condition must be instruction");
		return curFunc.instructions[valueRef.index].condition;
	}

	void visit(IfStmtNode* i)
	{
		_visit(cast(AstNode*)i.condition);
		IrRef condRef = i.condition.irRef;

		// Compile single branch if condition is constant
		if (condRef.isLiteral)
		{
			bool condValue = curFunc.constants[condRef.constIndex].i1;
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
		IrBasicBlock* prevBB = curFunc.currentBB;
		prevBB.exit = IrJump(IrJump.Type.branch, condRef, cond);

		// create Basic Block for then statement. It links to afterBB
		IrBasicBlock* then_StartBB = curFunc.addBasicBlock(IrName(thenId, ++thenCounter));
		curFunc.addBlockTarget(prevBB, then_StartBB);
		curFunc.sealBlock(then_StartBB);

		_visit(i.thenStatement);
		IrBasicBlock* then_EndBB = curFunc.currentBB;
		curFunc.sealBlock(then_EndBB);

		IrBasicBlock* else_StartBB;
		IrBasicBlock* else_EndBB;
		if (i.elseStatement)
		{
			else_StartBB = curFunc.addBasicBlock(IrName(elseId, ++elseCounter));
			curFunc.sealBlock(else_StartBB);
			curFunc.addBlockTarget(prevBB, else_StartBB);
			_visit(i.elseStatement);
			else_EndBB = curFunc.currentBB;
			curFunc.sealBlock(else_EndBB);
		}

		IrBasicBlock* afterBB = curFunc.addBasicBlock(IrName(blkId, ++bbCounter));

		if (!then_EndBB.isFinished)
		{
			then_EndBB.exit = IrJump(IrJump.Type.jmp);
			curFunc.addBlockTarget(then_EndBB, afterBB);
		}

		if (i.elseStatement)
		{
			if (!else_EndBB.isFinished)
			{
				else_EndBB.exit = IrJump(IrJump.Type.jmp);
				curFunc.addBlockTarget(else_EndBB, afterBB);
			}
		}
		else
		{
			curFunc.addBlockTarget(prevBB, afterBB);
		}
		curFunc.sealBlock(afterBB);
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
			curFunc.currentBB.exit = IrJump(IrJump.Type.ret1, r.expression.irRef);
		}
		else curFunc.currentBB.exit = IrJump(IrJump.Type.ret0);
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
				curFunc.emitInstr2(IrOpcode.o_store, indexExpr.type.irType(context), indexExpr.irRef, a.right.irRef);
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
		c.irRef = curFunc.put(c.value);
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
			long arg0 = curFunc.constants[b.left.irRef.constIndex].i64;
			long arg1 = curFunc.constants[b.right.irRef.constIndex].i64;

			switch(b.op)
			{
				case BinOp.EQUAL_EQUAL: b.irRef = curFunc.put(arg0 == arg1); return;
				case BinOp.NOT_EQUAL: b.irRef = curFunc.put(arg0 != arg1); return;
				case BinOp.GREATER: b.irRef = curFunc.put(arg0 > arg1); return;
				case BinOp.GREATER_EQUAL: b.irRef = curFunc.put(arg0 >= arg1); return;
				case BinOp.LESS: b.irRef = curFunc.put(arg0 < arg1); return;
				case BinOp.LESS_EQUAL: b.irRef = curFunc.put(arg0 <= arg1); return;
				case BinOp.PLUS: b.irRef = curFunc.put(arg0 + arg1); return;
				case BinOp.MINUS: b.irRef = curFunc.put(arg0 - arg1); return;
				default:
					context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op);
					return;
			}
		}

		auto lRef = b.left.irRef;
		auto rRef = b.right.irRef;
		switch(b.op)
		{
			case BinOp.EQUAL_EQUAL: b.irRef = curFunc.emitInstrCmp(IrCond.eq, lRef, rRef); break;
			case BinOp.NOT_EQUAL: b.irRef = curFunc.emitInstrCmp(IrCond.ne, lRef, rRef); break;
			case BinOp.GREATER: b.irRef = curFunc.emitInstrCmp(IrCond.g, lRef, rRef); break;
			case BinOp.GREATER_EQUAL: b.irRef = curFunc.emitInstrCmp(IrCond.ge, lRef, rRef); break;
			case BinOp.LESS: b.irRef = curFunc.emitInstrCmp(IrCond.l, lRef, rRef); break;
			case BinOp.LESS_EQUAL: b.irRef = curFunc.emitInstrCmp(IrCond.le, lRef, rRef); break;

			case BinOp.PLUS: b.irRef = curFunc.emitInstr2(IrOpcode.o_add, lRef.type, lRef, rRef); break;
			case BinOp.MINUS: b.irRef = curFunc.emitInstr2(IrOpcode.o_sub, lRef.type, lRef, rRef); break;

			default: context.internal_error(b.loc, "Opcode `%s` is not implemented", b.op); break;
		}
	}
	void visit(CallExprNode* c) {
		context.internal_error(c.loc, "Call is not implemented");
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
			ulong index = curFunc.constants[i.index.irRef.constIndex].i64;
			if (index == 0)
				address = i.array.irRef;
			else
			{
				IrRef offset = curFunc.put(index * elemSize);
				address = curFunc.emitInstr2(IrOpcode.o_add, IrValueType.ptr, i.array.irRef, offset);
			}
		}
		else
		{
			IrRef scale = curFunc.put(i.type.size);
			writefln("index %s", RefPr(curFunc, i.index.irRef));
			IrRef offset = curFunc.emitInstr2(IrOpcode.o_mul, i.index.irRef.type, i.index.irRef, scale);
			address = curFunc.emitInstr2(IrOpcode.o_add, IrValueType.ptr, i.array.irRef, offset);
		}

		if (parentAssignSide == CurrentAssignSide.rightSide) {
			i.irRef = curFunc.emitInstr1(IrOpcode.o_load, i.type.irType(context), address);
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
			t.irRef = curFunc.emitInstr1(IrOpcode.o_conv, to, t.expr.irRef);
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
struct IrModule
{
	IrFunction*[] functions;

	ubyte[] codeBuffer;
	ubyte[] code;

	void addFunction(IrFunction* fun)
	{
		functions ~= fun;
	}

	void dump(ref TextSink sink, CompilationContext* context)
	{
		foreach (func; functions) dumpFunction(*func, sink, context);
	}
}

/// Linear SSA IR for function
struct IrFunction
{
	/// Ast node
	FunctionDeclNode* node;

	/// Calling convention info. Contains helpers for CC specific parts.
	CallConv* callingConvention;

	/// Function return type. Only valid if function has return value. Check AST node.
	IrValueType returnType;

	/// Position in buffer or in memory
	void* funcPtr;

	/// The first Basic Block of the function
	/// Also the first node in the linked list of Basic Blocks
	IrBasicBlock* start;

	/// This is the last Basic Block in the linked list of blocks of this function
	IrBasicBlock* last;

	/// Instructions are emitted to this block
	IrBasicBlock* currentBB;

	///
	uint numBasicBlocks;

	// All constants. All have unique value.
	IrConstant[] constants;

	/// This array begins with all parameters
	IrInstruction[] instructions;

	/// All phi-functions
	IrPhi[] phis;

	// Dense array of values produced by instructions or phi nodes
	IrOperand[] operands;

	/// Stores list of uses per IrOperand
	MultiLinkedList!IrRef opdUses;

	/// Stores current definition of variable per block during SSA-form IR construction.
	IrRef[BlockVarPair] blockVarDef;

	/// Parameters are saved as first `numParameters` instructions of start basic block.
	/// They immediately follow o_block instruction, and have indicies 1, 2, 3 ...
	uint numParameters;

	/// Returns a slice of all param instructions
	IrInstruction[] parameters(){
		return instructions[1..1+numParameters];
	}

	/// Info from buildIntervals pass
	FunctionLiveIntervals liveIntervals;

	IrVarId nextIrVarId;

	StackLayout stackLayout;

	/// Allocates new variable id for this function. It should be bound to a variable
	/// and used with writeVariable, readVariable functions
	IrVarId newIrVarId()
	{
		return IrVarId(nextIrVarId++);
	}

	/// Automatically sets `start`, sets last and links blocks together
	/// Sets currentBB to this block
	IrBasicBlock* addBasicBlock(IrName name) {
		finishBlock();
		auto newBlock = new IrBasicBlock(name, BlockId(numBasicBlocks));
		++numBasicBlocks;
		if (start is null) start = newBlock;
		else
		{
			newBlock.prev = last;
			last.next = newBlock;
		}
		currentBB = last = newBlock;
		newBlock.firstInstr = instructions.length;
		put(IrInstruction(IrOpcode.o_block));
		return newBlock;
	}

	/// Places ending instruction of current Basic Block
	void finishBlock() {
		if (currentBB) put(IrInstruction(IrOpcode.o_block_end));
	}

	/// Adds control-flow edge pointing from `block` to `target`.
	void addBlockTarget(IrBasicBlock* block, IrBasicBlock* target) {
		block.outs ~= target;
		++target.refCount;
		target.ins ~= block;
	}

	/// Disconnects Basic Block from it from predecessors/successors
	/// Does not remove its instructions/phis
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
		foreach(target; bb.outs) --target.refCount;
		bb.outs = null;
	}

	/// Adds a constant to a list of constants of this function.
	/// It tries to find existing constant with the same value and returns it if it does.
	IrRef put(long value) {
		foreach(uint i, con; constants)
			if (con.i64 == value)
				return IrRef(i, IrConstKind.literal, IrValueType.i64);
		uint index = cast(uint)constants.length;
		auto con = IrConstant(value);
		constants ~= con;
		if (con.numSignedBytes <= 2)
			return IrRef(index, IrConstKind.literal, IrValueType.i32);
		else
			return IrRef(index, IrConstKind.literal, IrValueType.i64);
	}

	///
	IrRef emitInstr0(IrOpcode op, IrValueType type) {
		return put(IrInstruction(op, type)); }
	///
	IrRef emitInstr1(IrOpcode op, IrValueType type, IrRef arg0) {
		auto i = IrInstruction(op, type); i.arg0 = arg0; return put(i); }
	///
	IrRef emitInstr2(IrOpcode op, IrValueType type, IrRef arg0, IrRef arg1) {
		auto i = IrInstruction(op, type); i.arg0 = arg0; i.arg1 = arg1; return put(i); }
	///
	IrRef emitInstrCmp(IrCond cond, IrRef arg0, IrRef arg1) {
		auto i = IrInstruction(IrOpcode.o_icmp, IrValueType.i1);
		i.condition = cond; i.arg0 = arg0; i.arg1 = arg1;
		auto irRef = put(i);
		//instructions[irRef.index].result.hint.hintReg = RegisterRef(0, RegClass.flags); // flag
		return irRef;
	}

	/// Append instruction to current block (`currentBB`)
	/// Adds user to instr arguments and creates operand for result.
	IrRef put(IrInstruction instr) {
		++currentBB.numInstrs;
		uint index = cast(uint)instructions.length;
		auto irRef = IrRef(index, IrValueKind.instr, instr.type);
		//assert(irRef.isDefined, format("irRef is undefined. i %s, instr %s", index, instr.op));
		if (instr.returnsValue)
		{
			instr.result = makeOperand(irRef);
		}
		instructions ~= instr;

		foreach(IrRef argRef; instr.args)
			addUser(irRef, argRef);

		return irRef;
	}

	/// Puts `user` into a list of users of `used` value
	void addUser(IrRef user, IrRef used)
	{
		//assert(user.isDefined, "user is undefined");
		//assert(used.isDefined, "used is undefined");
		void addOpdUser(IrOperandId opdId)
		{
			assert(!opdId.isNull, format("result of %s is null", RefPr(&this, used)));
			opdUses.putBack(operands[opdId].usesList, user);
			operands[opdId].addUser(user);
		}

		final switch (used.kind)
		{
			case IrValueKind.instr:
				auto opdId = instructions[used.index].result;
				addOpdUser(opdId); break;
			case IrValueKind.con:
				final switch(used.constKind) {
					case IrConstKind.literal: constants[used.constIndex].addUser(user); break;
					case IrConstKind.stackSlotId: stackLayout.slots[used.constIndex].addUser(user); break;
				}
				break;
			case IrValueKind.phi: auto opdId = phis[used.index].result; addOpdUser(opdId); break;
		}
	}

	/// Set hint for register allocator
	void setStorageHint(IrOperandId opdId, StorageHint storageHint) {
		operands[opdId].storageHint = storageHint;
	}

	/// ditto
	void setStorageHint(IrRef irRef, StorageHint storageHint) {
		final switch (irRef.kind) {
			case IrValueKind.con: break;
			case IrValueKind.instr:
				IrOperandId opdId = instructions[irRef.index].result;
				setStorageHint(opdId, storageHint);
				break;
			case IrValueKind.phi:
				auto phi = &phis[irRef.index];
				setStorageHint(phi.result, storageHint);
				foreach (IrPhiArg arg; phi.args)
					setStorageHint(arg.value, storageHint);
				break;
		}
	}

	// can return null (for constants)
	private IrOperandId getOperand(IrRef irRef)
	{
		final switch (irRef.kind)
		{
			case IrValueKind.instr: return instructions[irRef.index].result;
			case IrValueKind.con: return IrOperandId();
			case IrValueKind.phi: return phis[irRef.index].result;
		}
	}

	// Creates operand for result of phi/instruction
	private IrOperandId makeOperand(IrRef irRef)
	{
		uint index = cast(uint)operands.length;
		operands ~= IrOperand(irRef);
		return IrOperandId(index);
	}

	// ignores null opdId
	private void removeOperand(IrOperandId opdId)
	{
		if (opdId.isNull) return;
		size_t length = operands.length;

		if (opdId.index != length-1)
		{
			operands[opdId] = operands[length-1];
			IrRef irRef = operands[opdId].irRef;
			// Update reference from IrValue instr/phi to IrOperand
			// old .result points to length-1, new .result points to opdId
			final switch (irRef.kind)
			{
				case IrValueKind.instr: instructions[irRef.index].result = opdId; break;
				case IrValueKind.con: assert(false, "constants have no operand");
				case IrValueKind.phi: phis[irRef.index].result = opdId; break;
			}
		}
		operands = assumeSafeAppend(operands[0..length-1]);
	}

	// Adds phi function to specified block
	private IrRef addPhi(IrBasicBlock* block, IrValueType type) {
		uint index = cast(uint)phis.length;
		IrRef irRef = IrRef(index, IrValueKind.phi, type);
		IrOperandId operand = makeOperand(irRef);
		phis ~= IrPhi(block, type, operand);
		block.phis ~= irRef;
		return irRef;
	}

	// Algorithm 1: Implementation of local value numbering
	/// Redefines `variable` with `value`. Is used for assignment to variable
	void writeVariable(IrVar variable, IrRef value) { writeVariable(variable, currentBB, value); }
	void writeVariable(IrVar variable, IrBasicBlock* block, IrRef value) {
		//writefln("writeVariable %s:%s <- %s", variable.id, variable.name, RefPr(&this, value));
		blockVarDef[BlockVarPair(block.index, variable.id)] = value;
	}

	/// Returns the value that currently defines `variable`
	IrRef readVariable(IrVar variable) { return readVariable(variable, currentBB); }
	IrRef readVariable(IrVar variable, IrBasicBlock* block)
	{
		//writefln("readVariable %s:%s", variable.id, variable.name);
		if (auto irRef = BlockVarPair(block.index, variable.id) in blockVarDef)
			return *irRef;
		return readVariableRecursive(variable, block);
	}

	// Algorithm 2: Implementation of global value numbering
	private IrRef readVariableRecursive(IrVar variable, IrBasicBlock* block)
	{
		IrRef value;
		if (!block.isSealed) {
			// Incomplete CFG
			value = addPhi(block, variable.type);
			block.incompletePhis ~= IncompletePhi(variable, value);
		}
		else if (block.ins.length == 1) {
			// Optimize the common case of one predecessor: No phi needed
			value = readVariable(variable, block.ins[0]);
		}
		else
		{
			// Break potential cycles with operandless phi
			value = addPhi(block, variable.type);
			writeVariable(variable, block, value);
			value = addPhiOperands(variable, value, block);
		}
		writeVariable(variable, block, value);
		return value;
	}

	// ditto
	private IrRef addPhiOperands(IrVar variable, IrRef phiRef, IrBasicBlock* block)
	{
		// Determine operands from predecessors
		foreach (IrBasicBlock* pred; block.ins)
		{
			IrRef val = readVariable(variable, pred);
			// Phi should not be cached before loop, since readVariable can add phi to phis, reallocating the array
			phis[phiRef.index].addArg(val, pred.index);
			addUser(phiRef, val);
		}
		return tryRemoveTrivialPhi(phiRef);
	}

	// Algorithm 3: Detect and recursively remove a trivial φ function
	private IrRef tryRemoveTrivialPhi(IrRef phiRef)
	{
		IrPhiArg same = IrPhiArg();
		foreach (IrPhiArg arg; phis[phiRef.index].args) {
			if (arg == same || arg.value == phiRef) {
				continue; // Unique value or self−reference
			}
			if (same != IrPhiArg())
			{
				return phiRef; // The phi merges at least two values: not trivial
			}
			same = arg;
		}
		if (same == IrPhiArg()) {
			//same = new Undef(); // The phi is unreachable or in the start block
		}
		// Remember all users except the phi itself
		IrOperandId opdId = phis[phiRef.index].result;
		ListInfo usesList = operands[opdId].usesList;

		// Reroute all uses of phi to same and remove phi
		replaceBy(usesList.first, phiRef, same);
		phis[phiRef.index].block.phis.removeInPlace(phiRef);
		phis[phiRef.index] = IrPhi();

		// Try to recursively remove all phi users, which might have become trivial
		NodeIndex cur = usesList.first;
		while(!cur.isNull)
		{
			auto node = opdUses.nodes[cur];
			IrRef use = node.data;
			if (use.kind == IrValueKind.phi && use != phiRef)
				tryRemoveTrivialPhi(use);
			cur = node.nextIndex;
		}
		removeOperand(opdId);
		return same.value;
	}

	// ditto
	/// Rewrites all users of phi `phiRef` to point to `byWhat` instead.
	private void replaceBy(NodeIndex firstUser, IrRef phiRef, IrPhiArg byWhat)
	{
		for (NodeIndex cur = firstUser; !cur.isNull; cur = opdUses.nodes[cur].nextIndex)
		{
			auto node = opdUses.nodes[cur];
			IrRef userRef = opdUses.nodes[cur].data;
			final switch (userRef.kind) {
				case IrValueKind.con: assert(false);
				case IrValueKind.instr:
					auto instr = instructions[userRef.index];
					foreach (ref IrRef arg; instr.args)
						if (arg == phiRef) arg = byWhat.value;
					break;
				case IrValueKind.phi:
					auto otherPhi = &phis[userRef.index];
					foreach (ref IrPhiArg arg; otherPhi.args)
						if (arg.value == phiRef) arg = byWhat;
					break;
			}
		}
	}

	// Algorithm 4: Handling incomplete CFGs
	/// Called after
	/// Ignores already sealed blocks.
	void sealBlock(IrBasicBlock* block)
	{
		if (block.isSealed) return;
		foreach (pair; block.incompletePhis)
			addPhiOperands(pair.var, pair.phi, block);
		block.isSealed = true;
	}
}

//pragma(msg, IrFunction.sizeof);

void dumpFunction(ref IrFunction func, ref TextSink sink, CompilationContext* ctx)
{
	sink.putf("function %s $%s (", func.returnType, IrNameProxy(ctx, IrName(func.node.id)));
	//foreach (i, param; func.parameters)
	//{
	//	sink.putf("%s %%%s", param.type, IrNameProxy(ctx, func.variableNames[i]));
	//	if (i+1 < func.parameters.length) sink.put(", ");
	//}
	sink.putln(") {");

	bool printVars = false;
	bool printBlockIns =  true;
	bool printBlockRefs = false;
	bool printBlockInstrRange = false;
	bool printInstrIndex = true;
	bool printUses = false;

	void printOpdUses(IrOperandId opdId) {
		if (opdId.isNull) return;
		NodeIndex cur = func.operands[opdId].usesList.first;
		if (cur.isNull) return;
		sink.put(" uses [");
		auto nodes = func.opdUses.nodes.data;
		while(!cur.isNull)
		{
			IrRef use = nodes[cur].data;
			sink.putf(" %s", RefPr(&func, use));
			cur = nodes[cur].nextIndex;
		}
		sink.put("]");
	}

	if (printVars)
	{
		sink.putln("constants:");
		foreach(int i, con; func.constants)
			sink.putfln("  %%const_%s = %s", i, con.i64);
		sink.putln;
	}

	for (IrBasicBlock* block = func.start; block; block = block.next)
	{
		if (printInstrIndex) sink.put("   |");
		sink.putf("  @%s:%s", IrNameProxy(ctx, block.name), block.index);
		if (printBlockRefs) sink.putf(" %s refs", block.refCount);
		if (printBlockInstrRange) sink.putf(" instr[%s..%s]", block.firstInstr, block.lastInstr);
		if (printBlockIns && block.ins)
		{
			sink.put(" in[");
			foreach(i, pred; block.ins) {
				if (i > 0) sink.put(", ");
				sink.putf("@%s", IrNameProxy(ctx, pred.name));
			}
			sink.put("]");
		}
		sink.putln;
		foreach(phiRef; block.phis)
		{
			if (printInstrIndex) sink.putf("% 3s|", block.firstInstr);
			auto res = func.phis[phiRef.index].result;
			sink.putf("    %%%s = %s phi.%s(", res, phiRef.type, phiRef.index);
			foreach(phi_i, arg; func.phis[phiRef.index].args)
			{
				if (phi_i > 0) sink.put(", ");
				sink.putf("%s @%s", RefPr(&func, arg.value), arg.blockId);
			}
			sink.put(")");
			if (printUses) printOpdUses(res);
			sink.putln;
		}

		// skip block_op
		auto instrs = func.instructions[block.firstInstr+1..block.lastInstr+1];
		// print all instructions
		size_t i = block.firstInstr+1;
		foreach(ref instr; instrs)
		{
			if (printInstrIndex) sink.putf("% 3s|", i);
			switch(instr.op)
			{
				case IrOpcode.o_block: break;
				case IrOpcode.o_block_end: break;
				case IrOpcode.o_icmp:
					sink.putf("    %%%s = %- 3s %- 8s", instr.result, instr.type, instr.op);
					sink.putf(" %s %s, %s", instr.condition, RefPr(&func, instr.arg0), RefPr(&func, instr.arg1));
					if (printUses) printOpdUses(instr.result);
					sink.putln;
					break;

				default:
					if (instr.returnsValue) {
						sink.putf("    %%%s = %- 3s %- 8s", instr.result, instr.type, instr.op);
					}
					else  sink.putf("    %s", instr.op);

					switch (instr.numArgs) {
						case 1: sink.putf(" %s", RefPr(&func, instr.arg0)); break;
						case 2: sink.putf(" %s, %s", RefPr(&func, instr.arg0),
							RefPr(&func, instr.arg1)); break;
						default: break;
					}

					if (printUses) printOpdUses(instr.result);

					sink.putln;
					break;
			}
			++i;
		}

		// print jump/return at the end of block
		final switch(block.exit.type) with(IrJump.Type) {
			case none: sink.putln("  // block not sealed"); break;
			case ret0: sink.putln("    return"); break;
			case ret1: sink.putfln("    return %s", RefPr(&func, block.exit.value)); break;
			case jmp:  sink.putfln("    jmp @%s", IrNameProxy(ctx, block.outs[0].name)); break;
			case branch:  sink.putfln("    branch %s @%s, @%s",
				RefPr(&func, block.exit.value), IrNameProxy(ctx, block.outs[0].name), IrNameProxy(ctx, block.outs[1].name)); break;
		}
	}
	sink.putln("}");

	func.liveIntervals.print;
}

enum IrValueKind : ubyte
{
	instr,
	con,
	phi
}

enum IrConstKind : ubyte
{
	literal,
	stackSlotId
	// Later add function address, global address, table address
	// array data? (binary blobs)
}

enum IrValueType : ubyte
{
	i1, // bool
	i32,
	i64,
	//f32,
	//f64,

	ptr,
}

struct BlockVarPair
{
	BlockId blockId;
	IrVarId varId;
}

struct IrVarId { uint id; alias id this; }
struct IrVar { string name; IrVarId id; IrValueType type; }
struct BlockId { uint id; alias id this; }

// print helper for refs
struct RefPr
{
	IrFunction* fun;
	IrRef r;
	void toString(scope void delegate(const(char)[]) sink) {
		if (!r.isDefined) {
			sink("<undefined>");
			return;
		}
		final switch(r.kind) {
			case IrValueKind.con:
				final switch (r.constKind)
				{
					case IrConstKind.literal:
					final switch (r.type) {
						case IrValueType.i1:  sink.formattedWrite("i1  %s",  fun.constants[r.constIndex].i1);  break;
						case IrValueType.i32: sink.formattedWrite("i32 %s", fun.constants[r.constIndex].i32); break;
						case IrValueType.i64: sink.formattedWrite("i64 %s", fun.constants[r.constIndex].i64); break;
						case IrValueType.ptr: sink.formattedWrite("ptr 0x%x", fun.constants[r.constIndex].i64); break;
					} break;

					case IrConstKind.stackSlotId: sink.formattedWrite("slot(%s)", r.constIndex); break;
				}
				break;
			case IrValueKind.instr:
				auto res = fun.instructions[r.index].result;
				if (res.isNull) sink.formattedWrite("instr.%s", r.index);
				else sink.formattedWrite("%s %%%s", r.type, res);
				break;
			case IrValueKind.phi:  sink.formattedWrite("%s %%%s", r.type, fun.phis[r.index].result); break;
		}
	}
}

struct IrRef
{
	this(uint idx, IrValueKind k, IrValueType t) {
		index = idx; isDefined = true; kind = k; type = t;
	}
	this(StackSlotId idx) {
		constIndex = idx; isDefined = true; constKind = IrConstKind.stackSlotId; kind = IrValueKind.con; type = IrValueType.ptr;
	}
	this(uint idx, IrConstKind c, IrValueType t) {
		constIndex = idx; isDefined = true; constKind = c; kind = IrValueKind.con; type = t;
	}
	union
	{
		mixin(bitfields!(
			uint,        "index",     27, // instruction/phi index
			bool,        "isDefined",  1,
			IrValueKind, "kind",       2,
			IrValueType, "type",       2
		));
		mixin(bitfields!(
			uint,        "constIndex",     25,
			IrConstKind, "constKind",       2, // 2 bits are taken out of `index`
			uint,        "",                5
		));
	}
	static assert(IrConstKind.max <= 3, "2 bits are reserved");
	static assert(IrValueType.max <= 3, "2 bits are reserved");
	static assert(IrValueKind.max <= 3, "2 bits are reserved");
	bool isInstr() { return kind == IrValueKind.instr; }
	bool isCon() { return kind == IrValueKind.con; }
	bool isLiteral() { return kind == IrValueKind.con && constKind == IrConstKind.literal; }
}

/// result/argument of instruction
struct IrOperand
{
	// instruction or phi index
	IrRef irRef;
	StorageHint storageHint;
	/// Indicates number of uses. There can be multiple uses per instruction.
	/// 0 means unused, 1 means temporary, or more
	ushort numUses;
	void addUser(IrRef user) { ++numUses; }

	// info for the list of operand uses
	ListInfo usesList;
}

/// Id for IrOperand
struct IrOperandId
{
	this(size_t idx) { index = cast(uint)idx; }
	uint index = uint.max;
	alias index this;
	enum NULL = IrOperandId();
	bool isNull() { return index == uint.max; }
}

/// Stores numeric constant data
struct IrConstant
{
	this(long value) {
		this.i64 = value;

		if (cast(byte)(value & 0xFF) == value)
			numSignedBytes = 1;
		else if (cast(short)(value & 0xFFFF) == value)
			numSignedBytes = 2;
		else if (cast(int)(value & 0xFFFF_FFFF) == value)
			numSignedBytes = 4;
		else
			numSignedBytes = 8;

		if (cast(ubyte)(value & 0xFF) == value)
			numUnsignedBytes = 1;
		else if (cast(ushort)(value & 0xFFFF) == value)
			numUnsignedBytes = 2;
		else if (cast(uint)(value & 0xFFFF_FFFF) == value)
			numUnsignedBytes = 4;
		else
			numUnsignedBytes = 8;
	}
	ubyte numSignedBytes;
	ubyte numUnsignedBytes;
	ushort numUses;
	union {
		bool i1;
		byte i8;
		short i16;
		int i32;
		long i64;
	}

	void addUser(IrRef user) { ++numUses; }
}

/// Convenience struct for Id + num suffix
struct IrName
{
	Identifier id;
	uint suffix;
}

/// Helper for printing
struct IrNameProxy
{
	CompilationContext* ctx;
	IrName name;
	void toString(scope void delegate(const(char)[]) sink) {
		if (name.suffix == 0) sink.formattedWrite("%s", ctx.idString(name.id));
		else sink.formattedWrite("%s_%s", ctx.idString(name.id), name.suffix);
	}
}

struct IrBasicBlock
{
	IrName name;
	BlockId index;

	/// Address of this Basic Block in generated code
	PC startPC;

	/// BBs that jump to this block
	IrBasicBlock*[] ins;

	/// Number of jumps to this block. Starting block allways has additional ref.
	int refCount;

	/// BBs this block jumps or branches to. Null if ends with no jump/branch
	IrBasicBlock*[] outs;

	/// The jump or return that must be the last instruction is stored in `exit`
	size_t firstInstr;
	///
	size_t numInstrs;
	///
	size_t lastInstr() {
		if (numInstrs)
			return firstInstr + numInstrs - 1;
		else return firstInstr;
	}
	///
	IrRef lastInstrRef() {
		return IrRef(cast(uint)lastInstr, IrValueKind.instr, IrValueType.init);
	}

	/// Specifies the last instruction of this Basic Block
	/// Jumps use `outs` for targets
	IrJump exit;

	/// Next node in linked listed of Basic Blocks of the function
	IrBasicBlock* next;
	/// Prev node in linked listed of Basic Blocks of the function
	IrBasicBlock* prev;

	IncompletePhi[] incompletePhis;
	IrRef[] phis;

	mixin(bitfields!(
		bool, "isSealed",  1,
		uint, "",          7
	));

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
		branch   /// conditional jump
	}

	Type type;
	IrRef value; /// return value reference, or condition for branch

	/// Used for branch instruction fixup
	IrCond condition = IrCond.ne;

	/// Used by backend, for:
	///   - first `branch` instruction
	///   - `jmp` instruction
	///   - `ret0`, `ret1` jump instruction when jumping to the end of function
	PC fixup0;
	// Used by backend, for second target of `branch` instruction
	PC fixup1;

	/// Is set to true by cmp instruction if it precedes branch
	bool useFlagForCmp;
}

/// Per Basic Block info for unresolved Phi functions, when CFG is incomplete.
/// Finished IR contains no such values
struct IncompletePhi
{
	IrVar var;
	IrRef phi;
}


struct IrPhi
{
	IrBasicBlock* block;
	IrValueType type;
	IrOperandId result;
	IrPhiArg[] args;

	void addArg(IrRef arg, BlockId block)
	{
		args ~= IrPhiArg(arg, block);
	}
}

struct IrPhiArg
{
	IrRef value;
	BlockId blockId;
}

enum IrCond : ubyte {
	eq,
	ne,
	l,
	le,
	g,
	ge
}
IrCond[IrCond.max+1] inverseIrCond = [IrCond.ne,IrCond.eq,IrCond.ge,IrCond.g,IrCond.le,IrCond.l];

///
struct IrInstruction
{
	this(IrOpcode op, IrValueType type = IrValueType.init)
	{
		this.op = op;
		this.type = type;
		numArgs = numInstrArgs[op];
		returnsValue = hasInstrReturn[op];
	}
	IrOpcode op;
	IrValueType type;
	IrCond condition;
	mixin(bitfields!(
		// number of `args` fields used: 0, 1, or 2
		uint,        "numArgs",       2,
		// true if `result` field is used
		bool,        "returnsValue",  1,
		uint,        "",              5
	));
	IrOperandId result;
	union {
		struct {
			IrRef arg0, arg1;
		}
		IrRef[2] _args;
	}
	IrRef[] args() { return _args[0..numArgs]; }
}

enum IrOpcode : ubyte
{
	o_nop,
	o_param,
	o_block, // place of block's phi nodes
	o_block_end, // place of block exit instruction
	o_not,

	o_icmp,
	o_add,
	o_sub,
	o_mul,
	o_div,

	o_load, // arg0 - address
	o_store, // arg0 - address, arg1 - value

	o_conv, // instruction type if target type
}

immutable numInstrArgs =  cast(ubyte[IrOpcode.max+1])[0,0,0,0,1,2,2,2,2,2,1,2,1];
immutable hasInstrReturn = cast(bool[IrOpcode.max+1])[0,1,0,0,1,1,1,1,1,1,1,0,1];

immutable irOpcodeNames = cast(string[IrOpcode.max+1])
["nop", "param", "block", "block_end", "not", "icmp", "add", "sub", "mul", "div", "load", "store", "conv"];

immutable binOpToIrOpcode = cast(IrOpcode[BinOp.max+1])[
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,
	IrOpcode.o_icmp,

	IrOpcode.o_sub,
	IrOpcode.o_add,
	IrOpcode.o_div,
	IrOpcode.o_mul];


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

	foreach (ref fun; ctx.mod.irModule.functions)
	{
		size_t numValues = fun.operands.length;
		size_t numBucketsPerBlock = divCeil(numValues, size_t.sizeof * 8);
		allocSets(numBucketsPerBlock, fun.numBasicBlocks);
		fun.liveIntervals.initIntervals(numValues, numValues);

		void liveAdd(IrOperandId opdId)
		{
			if (opdId.isNull) return;
			live[opdId] = true;
		}

		void liveRemove(IrOperandId opdId) {
			if (opdId.isNull) return;
			live[opdId] = false;
		}

		size_t[] blockLiveIn(BlockId blockIndex)
		{
			size_t from = blockIndex * numBucketsPerBlock;
			size_t to = from + numBucketsPerBlock;
			return liveInBuckets[from..to];
		}

		// algorithm start
		// for each block b in reverse order do
		for (IrBasicBlock* block = fun.last; block; block = block.prev)
		{
			// live = union of successor.liveIn for each successor of block
			liveData[] = 0;
			foreach (IrBasicBlock* succ; block.outs)
			{
				foreach (size_t i, size_t bucket; blockLiveIn(succ.index))
					liveBuckets[i] |= bucket;
			}

			// for each phi function phi of successors of block do
			//     live.add(phi.inputOf(block))
			foreach (IrBasicBlock* succ; block.outs)
				foreach (ref IrRef phiRef; succ.phis)
					foreach (ref IrPhiArg arg; fun.phis[phiRef.index].args)
						if (arg.blockId == block.index)
							liveAdd(fun.getOperand(arg.value));

			//writef("in @%s live:", block.index);
			//foreach (size_t index; live.bitsSet)
			//	writef(" %s", index);
			//writeln;

			// for each opd in live do
			foreach (size_t index; live.bitsSet)
			{
				// intervals[opd].addRange(block.from, block.to)
				fun.liveIntervals.addRange(IrOperandId(cast(uint)index), cast(int)block.firstInstr, cast(int)block.lastInstr);
			}

			void eachArg(IrRef opd, size_t opId) {
				IrOperandId opdId = fun.getOperand(opd);
				if (opdId.isNull) return;

				// intervals[opd].addRange(block.from, op.id)
				fun.liveIntervals.addRange(opdId, cast(int)block.firstInstr, cast(int)opId);

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
		//fun.liveIntervals.print;
	}
}

///
struct FunctionLiveIntervals
{
	// invariant: all ranges of one interval are sorted by `from` and do not intersect
	Buffer!LiveRange ranges;
	LiveInterval[] intervals;

	bool intervalCoversPosition(NodeIndex cur, int position)
	{
		while (!cur.isNull)
		{
			auto r = &ranges[cur];
			if (position < r.from)
				return false;
			else if (position > r.to)
				cur = r.nextIndex;
			else // from >= position <= to
				return true;
		}
		return false;
	}

	ref LiveInterval getInterval(NodeIndex rangeId)
	{
		auto itId = ranges[rangeId].intervalId;
		return intervals[itId];
	}

	// returns rangeId pointing to range covering position or one to the right of pos.
	// returns -1 if no ranges left after pos.
	NodeIndex advanceRangeId(NodeIndex cur, int position)
	{
		while (!cur.isNull)
		{
			auto r = &ranges[cur];
			if (position < r.from)
				return cur;
			else if (position > r.to)
				cur = r.nextIndex;
			else // from >= position <= to
				return cur;
		}
		return cur;
	}

	void initIntervals(size_t numIntervals, size_t reserveRanges) {
		intervals.length = numIntervals;
		ranges.reserve(reserveRanges);
	}

	// bounds are from block start to block end of the same block
	// from is always == to block start
	// checks for interval being null
	void addRange(IrOperandId interval, int from, int to)
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
	void addDefinition(IrOperandId interval, IrValueType type, int from) {
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

	void appendRange(IrOperandId interval, LiveRange range)
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

	void print() {
		writeln("intervals:");
		//writeln(ranges.data);
		foreach (i, it; intervals) {
			NodeIndex cur = it.first;
			if (cur.isNull) {
				writefln("% 3s: null", i);
				continue;
			}
			if (it.reg.isNull)
				writef("% 3s [no reg]:", i);
			else
				writef("% 3s [%s %2s]:", i, it.reg.regClass, it.reg);

			while (!cur.isNull)
			{
				auto r = &ranges[cur];
				writef(" (%s %s)", r.from, r.to);
				cur = r.nextIndex;
			}
			writeln;
		}
	}
}

///
struct LiveInterval
{
	NodeIndex first;
	NodeIndex last;
	RegisterRef reg;
	RegClass regClass;
}

///
struct LiveRange
{
	int from;
	int to;
	IrOperandId intervalId;
	NodeIndex prevIndex = NodeIndex.NULL;
	NodeIndex nextIndex = NodeIndex.NULL;
	bool isLast() { return nextIndex == NodeIndex.NULL; }
	bool contains(int pos) {
		if (pos < from) return false;
		if (pos > to) return false;
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
		for (IrBasicBlock* block = fun.start; block; block = block.next)
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

		while unhandled != { } do
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

		FunctionLiveIntervals* live = &fun.liveIntervals;

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
		foreach (it; live.intervals)
			if (!it.first.isNull)
				unhandled.insert(it.first);
		//writefln("unhandled %s", unhandled);

		// while unhandled != { } do
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
				else if(!fun.liveIntervals.intervalCoversPosition(rangeId, position))
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
				else if(fun.liveIntervals.intervalCoversPosition(rangeId, position))
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
				allocateBlockedReg(fun, currentId);

			// if current has a register assigned then add current to active
			if (!live.getInterval(currentId).reg.isNull)
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

		IrOperandId intervalId = fun.liveIntervals.ranges[currentId].intervalId;
		LiveInterval* currentIt = &fun.liveIntervals.intervals[intervalId];
		int currentEnd = fun.liveIntervals.ranges[currentIt.last].to;

		// for each interval it in active do
		//     freeUntilPos[it.reg] = 0
		foreach (rangeId; active.data)
		{
			auto it = fun.liveIntervals.getInterval(rangeId);
			assert(!it.reg.isNull);
			physRegs[it.reg].freeUntilPos = 0;
		}

		// check inactive : TODO

		// reg = register with highest freeUntilPos
		int maxPos = 0;
		RegisterRef reg;

		// reg stored in hint
		RegisterRef hintReg = fun.operands[intervalId].storageHint.getRegHint;

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
	void allocateBlockedReg(ref IrFunction fun, NodeIndex currentRangeId)
	{
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
		foreach (ref reg; gpr) reg.freeUntilPos = int.max;
		foreach (ref reg; flags) reg.freeUntilPos = int.max;
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
	int numParams;
	int numLocals;
	StackSlot[] slots;

	/// paramIndex == -1 for non-params
	IrRef addStackItem(ulong size, ulong alignment, bool isParameter, ushort paramIndex)
	{
		assert(size > 0);
		assert(alignment > 0);

		auto id = StackSlotId(cast(uint)(slots.length));
		auto slot = StackSlot(size, alignment, isParameter, paramIndex);

		if (isParameter) ++numParams;
		else ++numLocals;

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
	void addUser(IrRef user) { ++numUses; }
}

struct StackSlotId
{
	uint id = uint.max;
	alias id this;
	bool isNull() { return id == uint.max; }
}

/// Info needed for calling convention implementation
struct CallConv
{
	RegisterRef[] paramsInRegs;
	RegisterRef returnReg;

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
		fun.setStorageHint(instr.result, hint);
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
	RegisterRef(0, Register.AX, RegClass.gpr)
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

//          ####     ###    #####    #######    ####   #######  #     #
//         #    #   #   #   #    #   #         #    #  #        ##    #
//        #        #     #  #     #  #        #        #        # #   #
//        #        #     #  #     #  #####    #   ###  #####    #  #  #
//        #        #     #  #     #  #        #     #  #        #   # #
//         #    #   #   #   #    #   #         #    #  #        #    ##
//          ####     ###    #####    #######    #####  #######  #     #
// -----------------------------------------------------------------------------

/// Generates amd64 machine code from IR
void pass_code_gen(ref CompilationContext ctx) {
	IrToAmd64 gen;
	gen.context = &ctx;
	ctx.assertf(ctx.codeBuffer.length > 0, "Code buffer is empty");
	ctx.mod.irModule.codeBuffer = ctx.codeBuffer;
	gen.visit(&ctx.mod.irModule);
}

struct IrToAmd64
{
	CompilationContext* context;

	enum STACK_ITEM_SIZE = 8; // x86_64
	enum USE_FRAME_POINTER = false;
	CodeGen_x86_64 gen;

	/// Those two store a state of variables and registers
	private IrFunction* curFunc;

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
		context.assertf(m.codeBuffer.length > 0, "No code buffer assigned");

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
		numLocals = 0;//cast(int)(curFunc.values.length - numParams);
		numVars = numLocals + numParams;

		// Copy parameters from registers to shadow space
		// parameter 5 RSP + 40
		if (context.buildDebug)
		{
			if (numParams > 3) gen.movq(memAddrBaseDisp8(Register.SP, 32), Register.R9); // save fourth parameter
			if (numParams > 2) gen.movq(memAddrBaseDisp8(Register.SP, 24), Register.R8); // save third parameter
			if (numParams > 1) gen.movq(memAddrBaseDisp8(Register.SP, 16), Register.DX); // save second parameter
			if (numParams > 0) gen.movq(memAddrBaseDisp8(Register.SP,  8), Register.CX); // save first parameter
		}
		// RSP + 0 points to RET


		// Establish frame pointer
		if (USE_FRAME_POINTER)
		{
			gen.pushq(Register.BP);
			gen.movq(Register.BP, Register.SP);
		}

		reservedBytes = curFunc.stackLayout.reservedBytes;
		if (reservedBytes) // Reserve space for locals
		{
			if (reservedBytes > byte.max) gen.subq(Register.SP, Imm32(reservedBytes));
			else gen.subq(Register.SP, Imm8(cast(byte)reservedBytes));
		}
	}

	/*
	Register loadValue(IrRef valueRef, Register hint)
	{
		assert(valueRef.isDefined);
		IrValue* value = &curFunc.deref(valueRef);
		if (value.reg.isDefined) return cast(Register)value.reg.nativeReg;

		RegisterRef regRef = machineState.gprRegs.tryAlloc(valueRef);
		if (!regRef.isDefined)
		{
			spillReg(hint);
			regRef.index = hint;
			//assert(false);
		}
		Register reg0 = cast(Register)(regRef.nativeReg);
		writefln("load %s:%s into %s", valueRef.kind, valueRef.index, reg0);

		final switch(valueRef.type)
		{
			case IrValueType.i1: break;
			case IrValueType.i32:
				if (valueRef.isVar) gen.movd(reg0, localVarMemAddress(valueRef));
				else gen.movd(reg0, Imm32(cast(uint)curFunc.constantData[valueRef.index].value));
				break;
			case IrValueType.i64:
				if (valueRef.isVar) gen.movq(reg0, localVarMemAddress(valueRef));
				else gen.movq(reg0, Imm64(curFunc.constantData[valueRef.index].value));
				break;
		}
		return reg0;
	}

	void onRegAssign(Register dst, IrRef src)
	{
		auto regState = &machineState.gprRegs.regStates[dst];
		regState.storedValue = src;
	}

	void storeValue(IrRef dest, Register reg)
	{
		if (!dest.isInstr) context.internal_error("Assignment into a constant");
		writefln("store %s into %s:%s", reg, dest.kind, dest.index);
		auto regState = &machineState.gprRegs.regStates[reg];
		regState.storedValue = dest;
		regState.isDirty = false;
		final switch(dest.type)
		{
			case IrValueType.i1: break;
			case IrValueType.i32: gen.movd(localVarMemAddress(dest), reg); break;
			case IrValueType.i64: gen.movq(localVarMemAddress(dest), reg); break;
		}
	}
	*/

	/// Generate move from src operand to dst operand. argType describes the size of operands.
	void genMove(OperandLocation dst, OperandLocation src, ArgType argType)
	{
		MoveType moveType = calcMoveType(dst.type, src.type);
		if (moveType != MoveType.invalid && dst == src) return;

		final switch(moveType)
		{
			case MoveType.invalid:
				context.internal_error("Invalid move to %s from %s",
					dst.type, src.type);
				context.unreachable;
				assert(false);

			case MoveType.const_to_reg:
				Register dstReg = cast(Register)(dst.reg.nativeReg);
				long con = curFunc.constants[src.irRef.constIndex].i64;
				//writefln("move.%s reg:%s, con:%s", argType, dstReg, con);
				gen.mov(dstReg, Imm32(cast(uint)con), argType);
				break;

			case MoveType.const_to_stack:
				long con = curFunc.constants[src.irRef.constIndex].i64;
				final switch(argType) {
					case ArgType.BYTE:
						gen.movb(localVarMemAddress(dst.irRef.constIndex), Imm8(cast(ubyte)con)); break;
					case ArgType.WORD:
						gen.movw(localVarMemAddress(dst.irRef.constIndex), Imm16(cast(ushort)con)); break;
					case ArgType.DWORD:
						gen.movd(localVarMemAddress(dst.irRef.constIndex), Imm32(cast(uint)con)); break;
					case ArgType.QWORD:
						gen.movq(localVarMemAddress(dst.irRef.constIndex), Imm32(cast(uint)con)); break;
				}
				break;

			case MoveType.reg_to_reg:
				Register dstReg = cast(Register)(dst.reg.nativeReg);
				Register srcReg = cast(Register)(src.reg.nativeReg);
				//writefln("move.%s reg:%s, reg:%s", argType, dstReg, srcReg);
				gen.mov(dstReg, srcReg, argType);
				break;

			case MoveType.reg_to_stack:
				Register srcReg = cast(Register)(src.reg.nativeReg);
				gen.mov(localVarMemAddress(dst.irRef.constIndex), srcReg, argType);
				break;

			case MoveType.stack_to_reg:
				Register dstReg = cast(Register)(dst.reg.nativeReg);
				gen.mov(dstReg, localVarMemAddress(src.irRef.constIndex), argType);
				break;

			case MoveType.const_to_mem:
				context.internal_error("const_to_mem is not implemented");
				break;

			case MoveType.reg_to_mem:
				context.internal_error("reg_to_mem is not implemented");
				break;

			case MoveType.mem_to_reg:
				context.internal_error("mem_to_reg is not implemented");
				break;
		}
	}

	/// Generate move from src operand to dst operand. argType describes the size of operands.
	void genLoad(OperandLocation dst, OperandLocation src, ArgType argType)
	{
		bool valid = dst.type == OpdLocType.physicalRegister;
		context.assertf(valid, "Invalid load %s -> %s", src.type, dst.type);

		switch(src.type)
		{
			case OpdLocType.physicalRegister:
				Register dstReg = cast(Register)(dst.reg.nativeReg);
				Register srcReg = cast(Register)(src.reg.nativeReg);
				writefln("mov %s, [%s]", dstReg, srcReg);
				gen.mov(dstReg, memAddrBase(srcReg), argType);

				//context.internal_error("physicalRegister is not implemented");
				break;

			case OpdLocType.constant:
				context.internal_error("constant is not implemented");
				break;

			default:
				context.internal_error("default is not implemented");
				break;
		}
	}

	void genStore(OperandLocation dst, OperandLocation src, ArgType argType)
	{
		//bool valid = dst.type == OpdLocType.physicalRegister;
		//context.assertf(valid, "Invalid load %s -> %s", src.type, dst.type);

		switch (dst.type)
		{
			case OpdLocType.physicalRegister:
				switch (src.type)
				{
					case OpdLocType.physicalRegister:
						Register dstReg = cast(Register)(dst.reg.nativeReg);
						Register srcReg = cast(Register)(src.reg.nativeReg);
						writefln("mov [%s], %s", dstReg, srcReg);
						gen.mov(memAddrBase(dstReg), srcReg, argType);
						break;
					default:
						context.internal_error("store from %s is not implemented", src.type);
						break;
				}
				break;

			default:
				context.internal_error("store %s <- %s is not implemented", dst.type, src.type);
				break;
		}
	}

	void genRegular(OperandLocation dst, OperandLocation src, AMD64OpRegular op, ArgType argType)
	{
		AsmArg argDst;
		AsmArg argSrc;
		AsmOpParam param;
		param.op = op;
		param.argType = argType;

		context.assertf(dst.type == OpdLocType.physicalRegister, "Destination must be register");
		Register reg0 = cast(Register)(dst.reg.nativeReg);
		argDst.reg = reg0;
		param.dstKind = AsmArgKind.REG;

		//writefln("%s.%s %s %s", op, argType, dst.type, src.type);

		final switch (src.type) with(OpdLocType)
		{
			case constant:
				IrConstant con = curFunc.constants[src.irRef.constIndex];
				if (con.numSignedBytes == 1) {
					param.immType = ArgType.BYTE;
					argSrc.imm8 = Imm8(con.i8);
				}
				else {
					param.immType = ArgType.DWORD;
					argSrc.imm32 = Imm32(con.i32);
				}
				param.srcKind = AsmArgKind.IMM;
				break;

			case virtualRegister: context.unreachable; assert(false);
			case memoryAddress: context.unreachable; assert(false);
			case physicalRegister:
				argSrc.reg = cast(Register)(src.reg.nativeReg);
				param.srcKind = AsmArgKind.REG;
				break;

			case stackSlot: context.unreachable; assert(false); // gen.mov(reg0, localVarMemAddress(valueRef), argType);
		}
		gen.encodeRegular(argDst, argSrc, param);
	}

	OperandLocation getLocation(IrOperandId opdId)
	{
		return OperandLocation(curFunc.liveIntervals.intervals[opdId].reg);
	}

	OperandLocation getLocation(IrRef irRef)
	{
		final switch (irRef.kind) with(IrValueKind)
		{
			case instr, phi: return getLocation(curFunc.getOperand(irRef));
			case con: return OperandLocation.makeConstant(irRef);
		}
	}

	/// At the start all values are in memory/regs according to call conv
	void compileFuncBody()
	{
		for (IrBasicBlock* block = curFunc.start; block; block = block.next)
		{
			block.startPC = gen.pc;
			foreach(ref instr; curFunc.instructions[block.firstInstr..block.lastInstr+1])
			{
				compileInstr(instr, *block);
			}
		}
	}

	void resolvePhis(ref IrBasicBlock block)
	{
		// TODO moves are not parallel here
		foreach (IrBasicBlock* succ; block.outs)
			foreach (ref IrRef phiRef; succ.phis)
				foreach (ref IrPhiArg arg; curFunc.phis[phiRef.index].args)
					if (arg.blockId == block.index)
						genMove(getLocation(phiRef), getLocation(arg.value), irTypeToArgType(phiRef.type));
	}

	void compileInstr(IrInstruction instr, ref IrBasicBlock block)
	{
		ArgType argType = irTypeToArgType(instr.type);

		switch (instr.op) with(IrOpcode)
		{
			case o_nop: break;
			case o_param: break;
			case o_block: break;

			case o_block_end:
				final switch(block.exit.type) with(IrJump.Type)
				{
					case none: context.internal_error("Compilation non-sealed basic block `%s`", block.name); break;
					case ret1:
						OperandLocation arg0 = getLocation(block.exit.value);
						ArgType retType = irTypeToArgType(curFunc.returnType);
						genMove(OperandLocation(curFunc.callingConvention.returnReg), arg0, retType);
						goto case;

					case ret0:
						// ignore jump in the last Basic Block
						if (block.next is null) break;
						block.exit.fixup0 = gen.pc;
						gen.jmp(Imm32(0));
						//writefln("jmp");
						break;

					case jmp:
						resolvePhis(block);
						if (block.next != block.outs[0]) {
							block.exit.fixup0 = gen.pc;
							gen.jmp(Imm32(0));
							//writefln("jmp");
						}
						break;

					case branch:
						block.exit.fixup0 = gen.pc;
						// TODO missing phi resolution
						gen.jcc(Condition.NZ, Imm32(0));
						//writefln("jcc");
						if (block.next != block.outs[1]) {
							block.exit.fixup1 = gen.pc;
							gen.jmp(Imm32(0));
							//writefln("jmp");
						}
						break;
				}
				break;

			case o_icmp:
				OperandLocation arg0 = getLocation(instr.arg0);
				OperandLocation arg1 = getLocation(instr.arg1);
				ArgType cmpArgType = irTypeToArgType(instr.arg0.type);
				genRegular(arg0, arg1, AMD64OpRegular.cmp, cmpArgType);
				break;

			case o_add, o_sub:
				OperandLocation res = getLocation(instr.result);
				OperandLocation arg0 = getLocation(instr.arg0);
				OperandLocation arg1 = getLocation(instr.arg1);
				genMove(res, arg0, irTypeToArgType(instr.type));
				switch(instr.op) with(IrOpcode) {
					case o_add: genRegular(res, arg1, AMD64OpRegular.add, argType); break;
					case o_sub: genRegular(res, arg1, AMD64OpRegular.sub, argType); break;
					default: context.internal_error("Unreachable");
				}
				break;

			case o_mul:
				OperandLocation res = getLocation(instr.result);
				OperandLocation arg0 = getLocation(instr.arg0);
				OperandLocation arg1 = getLocation(instr.arg1);
				if (arg0.isConstant) swap(arg0, arg1);
				if (!arg1.isConstant)
					context.assertf(instr.arg0.type == instr.arg1.type,
						"Type mismatch on imul %s != %s", instr.arg0.type, instr.arg1.type);
				context.assertf(arg0.type == OpdLocType.physicalRegister, "arg0 is %s != phys reg", arg0.type);
				Register regRes = cast(Register)(res.reg.nativeReg);
				Register reg0 = cast(Register)(arg0.reg.nativeReg);
				IrConstant con;
				if (arg1.isConstant) con = curFunc.constants[arg1.irRef.constIndex];
				switch (argType)
				{
					case ArgType.WORD:
						if (arg1.isConstant) {
							if (con.numSignedBytes == 1)
								gen.imulw(regRes, reg0, Imm8(con.i8));
							else if (con.numSignedBytes == 2)
								gen.imulw(regRes, reg0, Imm16(con.i16));
							else context.internal_error("Not implemented");
						}
						else context.internal_error("Not implemented");
						break;

					case ArgType.DWORD:
						if (arg1.isConstant) {
							if (con.numSignedBytes == 1)
								gen.imuld(regRes, reg0, Imm8(con.i8));
							else if (con.numSignedBytes >= 2 && con.numSignedBytes <= 4)
								gen.imuld(regRes, reg0, Imm32(con.i32));
							else context.internal_error("Not implemented");
						}
						else context.internal_error("Not implemented");
						break;

					case ArgType.QWORD:
						if (arg1.isConstant) {
							if (con.numSignedBytes == 1)
								gen.imulq(regRes, reg0, Imm8(con.i8));
							else if (con.numSignedBytes >= 2 && con.numSignedBytes <= 4)
								gen.imulq(regRes, reg0, Imm32(con.i32));
							else context.internal_error("Not implemented");
						}
						else context.internal_error("Not implemented");
						break;

					default: context.internal_error("Compilation of imul:%s is not implemented", argType); break;
				}
				break;

			case o_store:
				OperandLocation addr = getLocation(instr.arg0);
				OperandLocation value = getLocation(instr.arg1);
				genStore(addr, value, argType);
				break;

			case o_load:
				OperandLocation value = getLocation(instr.result);
				OperandLocation addr = getLocation(instr.arg0);
				genLoad(value, addr, argType);
				break;

			//case o_not:
			//	Register reg0;
			//	gen.not(reg0, argType);
			//	//writefln("not");
			//	break;

			default:
				context.internal_error("Compilation of `%s` is not implemented. In BB `%s`",
					instr.op, block.name);
				break;
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
					Condition cond = IrCond_to_Condition[block.exit.condition];
					fixup0.jccAbs(cond, block.outs[0].startPC);
					//writefln("fix jcc %s", cond);
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

	MemAddress localVarMemAddress(uint stackSlotId)
	{
		int displacement = curFunc.stackLayout.slots[stackSlotId].offset;
		Register baseReg = Register.SP;
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

enum AsmArgKind : ubyte { REG, IMM, MEM }
enum AsmArgKindProduct : ubyte {
//	REG      IMM      MEM         left
	REG_REG, IMM_REG, MEM_REG, // REG  right
	REG_IMM, IMM_IMM, MEM_IMM, // IMM
	REG_MEM, IMM_MEM, MEM_MEM, // MEM
}
AsmArgKindProduct asmArgKindProduct(AsmArgKind left, AsmArgKind right) {
	return cast(AsmArgKindProduct)(left + 3 * right);
}

union AsmArg
{
	Imm8 imm8;
	Imm16 imm16;
	Imm32 imm32;
	Imm64 imm64;
	Register reg;
	MemAddress memAddress;
}

enum ArgType : ubyte { BYTE, WORD, DWORD, QWORD }
ArgType irTypeToArgType(IrValueType t) {
	final switch(t) {
		case IrValueType.i1:  return ArgType.BYTE;
		case IrValueType.i32: return ArgType.DWORD;
		case IrValueType.i64: return ArgType.QWORD;
		case IrValueType.ptr: return ArgType.QWORD;
	}
}

// ensures REX prefix for ah ch dh bh
bool regNeedsRexPrefix(ArgType argType)(Register reg) {
	static if (argType == ArgType.BYTE) return reg >= 4;
	else return false;
}

enum AMD64OpRegular : ubyte {
	add,
	or,
	and,
	sub,
	xor,
	cmp
}

struct AsmOpParam
{
	AsmArgKind dstKind;
	AsmArgKind srcKind;
	AMD64OpRegular op;
	ArgType argType;
	ArgType immType;
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

Condition[IrCond.max+1] IrCond_to_Condition = [
	Condition.E,
	Condition.NE,
	Condition.L,
	Condition.LE,
	Condition.G,
	Condition.GE];
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
	void putInstrBinaryRegRegImm(ArgType argType, O, I)(O opcode, Register dst_rm, Register src_reg, I src_imm) if (isAnyOpcode!O && isAnyImm!I) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_RB!argType(src_reg, dst_rm);                                 // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), src_reg, dst_rm));          // ModR/r
		sink_put(src_imm);                                                      // Imm8/16/32/64
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
	void putInstrBinaryRegMemImm(ArgType argType, O, I)(O opcode, Register reg, MemAddress src_mem, I src_imm) if (isAnyOpcode!O && isAnyImm!I) {
		static if (argType == ArgType.WORD) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_XB!argType(src_mem.indexReg, src_mem.baseReg);               // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(src_mem.modRmByte(reg));                                       // ModR/M
		if (src_mem.hasSibByte)	   sink_put(src_mem.sibByte);                   // SIB
		if (src_mem.hasDisp32)     sink_put(src_mem.disp32);                    // disp32
		else if (src_mem.hasDisp8) sink_put(src_mem.disp8);                     // disp8
		sink_put(src_imm);                                                      // Imm8/16/32
	}
	void putInstrBinaryMemImm(ArgType argType, O, I)(O opcode, ubyte regOpcode, MemAddress dst_mem, I src_imm) if (isAnyOpcode!O && isAnyImm!I) {
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

	/// Used for versions of instructions without argument size suffix.
	/// mov, add, sub, instead of movq, addb, subd.
	/// mov(Register.AX, Register.DI, ArgType.QWORD); instead of movq(Register.AX, Register.DI);
	void opDispatch(string s, Arg1, Arg2)(Arg1 dst, Arg2 src, ArgType argType) {
		switch(argType) {
			static if (__traits(compiles, mixin(s~"b(dst, src)"))) { case ArgType.BYTE:  mixin(s~"b(dst, src);"); break; }
			static if (__traits(compiles, mixin(s~"w(dst, src)"))) { case ArgType.WORD:  mixin(s~"w(dst, src);"); break; }
			static if (__traits(compiles, mixin(s~"d(dst, src)"))) { case ArgType.DWORD: mixin(s~"d(dst, src);"); break; }
			static if (__traits(compiles, mixin(s~"q(dst, src)"))) { case ArgType.QWORD: mixin(s~"q(dst, src);"); break; }
			default: assert(false, format("Cannot encode %s(%s, %s, ArgType.%s)", s, dst, src, argType));
		}
	}

	/// ditto
	void opDispatch(string s, Arg1)(Arg1 dst, ArgType argType) {
		switch(argType) {
			static if (__traits(compiles, mixin(s~"b(dst)"))) { case ArgType.BYTE:  mixin(s~"b(dst);"); break; }
			static if (__traits(compiles, mixin(s~"w(dst)"))) { case ArgType.WORD:  mixin(s~"w(dst);"); break; }
			static if (__traits(compiles, mixin(s~"d(dst)"))) { case ArgType.DWORD: mixin(s~"d(dst);"); break; }
			static if (__traits(compiles, mixin(s~"q(dst)"))) { case ArgType.QWORD: mixin(s~"q(dst);"); break; }
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

	void encodeRegular(AsmArg dst, AsmArg src, AsmOpParam param)
	{
		static immutable ubyte[] op_tbl_bin = [
			0x00, // add,
			0x08, // or,
			0x20, // and,
			0x28, // sub,
			0x30, // xor,
			0x38, // cmp,
		];

		static immutable ubyte[] op_tbl_un = [
			0, // add,
			1, // or,
			4, // and,
			5, // sub,
			6, // xor,
			7, // cmp
		];

		AsmArgKindProduct prod = asmArgKindProduct(param.dstKind, param.srcKind);
		final switch(prod) with(AsmArgKindProduct)
		{
			case REG_REG:
				final switch(param.argType) {
					case ArgType.BYTE:  encoder.putInstrBinaryRegReg!(ArgType.BYTE) (OP1(cast(ubyte)(op_tbl_bin[param.op]+0)), dst.reg, src.reg); break;
					case ArgType.WORD:  encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), dst.reg, src.reg); break;
					case ArgType.DWORD: encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), dst.reg, src.reg); break;
					case ArgType.QWORD: encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), dst.reg, src.reg); break;
				} break;

			case MEM_REG:
				final switch(param.argType) {
					case ArgType.BYTE:  encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(cast(ubyte)(op_tbl_bin[param.op]+0)), src.reg, dst.memAddress); break;
					case ArgType.WORD:  encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), src.reg, dst.memAddress); break;
					case ArgType.DWORD: encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), src.reg, dst.memAddress); break;
					case ArgType.QWORD: encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), src.reg, dst.memAddress); break;
				} break;

			case REG_MEM:
				final switch(param.argType) {
					case ArgType.BYTE:  encoder.putInstrBinaryRegMem!(ArgType.BYTE) (OP1(cast(ubyte)(op_tbl_bin[param.op]+2)), dst.reg, src.memAddress); break;
					case ArgType.WORD:  encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP1(cast(ubyte)(op_tbl_bin[param.op]+3)), dst.reg, src.memAddress); break;
					case ArgType.DWORD: encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP1(cast(ubyte)(op_tbl_bin[param.op]+3)), dst.reg, src.memAddress); break;
					case ArgType.QWORD: encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP1(cast(ubyte)(op_tbl_bin[param.op]+3)), dst.reg, src.memAddress); break;
				} break;

			case REG_IMM:
				assert(param.argType == param.immType || param.immType == ArgType.BYTE);
				final switch(param.immType)
				{
					case ArgType.BYTE:
						final switch(param.argType) {
							case ArgType.BYTE:  encoder.putInstrBinaryRegImm2!(ArgType.BYTE)  (OP1(0x80), op_tbl_un[param.op], dst.reg, src.imm8); break;
							case ArgType.WORD:  encoder.putInstrBinaryRegImm2!(ArgType.WORD)  (OP1(0x83), op_tbl_un[param.op], dst.reg, src.imm8); break;
							case ArgType.DWORD: encoder.putInstrBinaryRegImm2!(ArgType.DWORD) (OP1(0x83), op_tbl_un[param.op], dst.reg, src.imm8); break;
							case ArgType.QWORD: encoder.putInstrBinaryRegImm2!(ArgType.QWORD) (OP1(0x83), op_tbl_un[param.op], dst.reg, src.imm8); break;
						} break;

					case ArgType.WORD:  encoder.putInstrBinaryRegImm2!(ArgType.WORD)  (OP1(0x81), op_tbl_un[param.op], dst.reg, src.imm16); break;
					case ArgType.DWORD: encoder.putInstrBinaryRegImm2!(ArgType.DWORD) (OP1(0x81), op_tbl_un[param.op], dst.reg, src.imm32); break;
					case ArgType.QWORD: encoder.putInstrBinaryRegImm2!(ArgType.QWORD) (OP1(0x81), op_tbl_un[param.op], dst.reg, src.imm32); break;
				} break;

			case MEM_IMM:
				assert(param.argType == param.immType || param.immType == ArgType.BYTE);
				final switch(param.immType)
				{
					case ArgType.BYTE:
						final switch(param.argType) {
							case ArgType.BYTE:  encoder.putInstrBinaryMemImm!(ArgType.BYTE)  (OP1(0x80), op_tbl_un[param.op], dst.memAddress, src.imm8); break;
							case ArgType.WORD:  encoder.putInstrBinaryMemImm!(ArgType.WORD)  (OP1(0x83), op_tbl_un[param.op], dst.memAddress, src.imm8); break;
							case ArgType.DWORD: encoder.putInstrBinaryMemImm!(ArgType.DWORD) (OP1(0x83), op_tbl_un[param.op], dst.memAddress, src.imm8); break;
							case ArgType.QWORD: encoder.putInstrBinaryMemImm!(ArgType.QWORD) (OP1(0x83), op_tbl_un[param.op], dst.memAddress, src.imm8); break;
						} break;

					case ArgType.WORD:  encoder.putInstrBinaryMemImm!(ArgType.WORD)  (OP1(0x81), op_tbl_un[param.op], dst.memAddress, src.imm16); break;
					case ArgType.DWORD: encoder.putInstrBinaryMemImm!(ArgType.DWORD) (OP1(0x81), op_tbl_un[param.op], dst.memAddress, src.imm32); break;
					case ArgType.QWORD: encoder.putInstrBinaryMemImm!(ArgType.QWORD) (OP1(0x81), op_tbl_un[param.op], dst.memAddress, src.imm32); break;
				} break;

			case IMM_REG, IMM_IMM, IMM_MEM, MEM_MEM:
				assert(false);
		}
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

	void imulw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.WORD) (OP2(0x0F, 0xAF), src, dst); }
	void imuld(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.DWORD)(OP2(0x0F, 0xAF), src, dst); }
	void imulq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(ArgType.QWORD)(OP2(0x0F, 0xAF), src, dst); }
	void imulw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.WORD) (OP2(0x0F, 0xAF), dst, src); }
	void imuld(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.DWORD)(OP2(0x0F, 0xAF), dst, src); }
	void imulq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(ArgType.QWORD)(OP2(0x0F, 0xAF), dst, src); }

	void imulw(Register dst, Register src1, Imm8 src2) { encoder.putInstrBinaryRegRegImm!(ArgType.WORD) (OP1(0x6B), src1, dst, src2); }
	void imuld(Register dst, Register src1, Imm8 src2) { encoder.putInstrBinaryRegRegImm!(ArgType.DWORD)(OP1(0x6B), src1, dst, src2); }
	void imulq(Register dst, Register src1, Imm8 src2) { encoder.putInstrBinaryRegRegImm!(ArgType.QWORD)(OP1(0x6B), src1, dst, src2); }
	void imulw(Register dst, Register src1, Imm16 src2){ encoder.putInstrBinaryRegRegImm!(ArgType.WORD) (OP1(0x69), src1, dst, src2); }
	void imuld(Register dst, Register src1, Imm32 src2){ encoder.putInstrBinaryRegRegImm!(ArgType.DWORD)(OP1(0x69), src1, dst, src2); }
	void imulq(Register dst, Register src1, Imm32 src2){ encoder.putInstrBinaryRegRegImm!(ArgType.QWORD)(OP1(0x69), src1, dst, src2); }

	void imulw(Register dst, MemAddress src1, Imm8 src2) { encoder.putInstrBinaryRegMemImm!(ArgType.WORD) (OP1(0x6B), dst, src1, src2); }
	void imuld(Register dst, MemAddress src1, Imm8 src2) { encoder.putInstrBinaryRegMemImm!(ArgType.DWORD)(OP1(0x6B), dst, src1, src2); }
	void imulq(Register dst, MemAddress src1, Imm8 src2) { encoder.putInstrBinaryRegMemImm!(ArgType.QWORD)(OP1(0x6B), dst, src1, src2); }
	void imulw(Register dst, MemAddress src1, Imm16 src2){ encoder.putInstrBinaryRegMemImm!(ArgType.WORD) (OP1(0x69), dst, src1, src2); }
	void imuld(Register dst, MemAddress src1, Imm32 src2){ encoder.putInstrBinaryRegMemImm!(ArgType.DWORD)(OP1(0x69), dst, src1, src2); }
	void imulq(Register dst, MemAddress src1, Imm32 src2){ encoder.putInstrBinaryRegMemImm!(ArgType.QWORD)(OP1(0x69), dst, src1, src2); }

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
	void toString(scope void delegate(const(char)[]) sink, FormatSpec!char fmt)
	{
		int scale = calcScale(value);
		auto scaledValue = scaled(value, -scale);
		int digits = numDigitsInNumber(scaledValue);
		import std.format : formattedWrite, formatValue;
		fmt.spec = 'f';
		string suf = scaleSuffixes[scaleToScaleIndex(scale)];
		fmt.width -= suf.length;
		fmt.precision = 3-digits;
		formatValue(sink, scaledValue, fmt);
		sink(suf);
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

	void removeInPlace(size_t index)
	{
		if (index+1 != length)
		{
			buf[index] = buf[length-1];
		}
		--length;
	}

	void unput(size_t numItems)
	{
		length -= numItems;
	}
}

struct NodeIndex {
	this(size_t id) { this.id = cast(uint)id; }
	enum NodeIndex NULL = NodeIndex(uint.max);
	uint id = uint.max;
	bool isNull() { return id == NULL; }
	alias id this;
}

struct ListInfo
{
	NodeIndex first;
	NodeIndex last;
}

struct MultiLinkedList(NodeData)
{
	struct Node
	{
		NodeData data;
		NodeIndex prevIndex;
		NodeIndex nextIndex;
	}

	Buffer!Node nodes;
	// head of free nodes linked list
	NodeIndex firstFreeNode;

	NodeIndex putFront(ref ListInfo listInfo, NodeData nodeData)
	{
		NodeIndex firstIndex = listInfo.first;
		NodeIndex index = allocNode(Node(nodeData));

		listInfo.first = index;
		if (firstIndex.isNull)
		{
			listInfo.last = index;
		}
		else
		{
			Node* node = &nodes[index];
			node.nextIndex = firstIndex;

			Node* first = &nodes[firstIndex];
			first.prevIndex = index;
		}

		return index;
	}

	NodeIndex putBack(ref ListInfo listInfo, NodeData nodeData)
	{
		NodeIndex lastIndex = listInfo.last;
		NodeIndex index = allocNode(Node(nodeData));

		listInfo.last = index;
		if (lastIndex.isNull)
		{
			listInfo.first = index;
		}
		else
		{
			Node* node = &nodes[index];
			node.prevIndex = lastIndex;

			Node* last = &nodes[lastIndex];
			last.nextIndex = index;
		}

		return index;
	}

	private NodeIndex allocNode(Node node)
	{
		if (firstFreeNode.isNull)
		{
			NodeIndex index = NodeIndex(nodes.length);
			nodes.put(node);
			return index;
		}
		else
		{
			NodeIndex index = firstFreeNode;
			firstFreeNode = nodes[firstFreeNode].nextIndex;
			nodes[index] = node;
			return index;
		}
	}

	// returns rangeId of the next range
	NodeIndex freeNode(ref ListInfo listInfo, NodeIndex nodeIndex)
	{
		auto node = &nodes[nodeIndex];

		if (nodeIndex == listInfo.first) listInfo.first = node.nextIndex;
		if (nodeIndex == listInfo.last) listInfo.last = node.prevIndex;

		NodeIndex nextIndex = node.nextIndex;

		if (node.prevIndex != NodeIndex.NULL)
			nodes[node.prevIndex].nextIndex = node.nextIndex;
		if (node.nextIndex != NodeIndex.NULL)
			nodes[node.nextIndex].prevIndex = node.prevIndex;

		// add to free list
		nodes[nodeIndex].nextIndex = firstFreeNode;
		firstFreeNode = nodeIndex;

		return nextIndex;
	}

	void print(R)(R lists) {
		import std.stdio;
		//write("nodes: ");
		//writeln(nodes.data);

		{
			write("free:");
			NodeIndex cur = firstFreeNode;
			while (!cur.isNull)
			{
				writef(" %s", cur);
				cur = nodes[cur].nextIndex;
			}
			writeln;
		}

		size_t i;
		foreach (list; lists) {
			scope(exit) ++i;
			NodeIndex cur = list.first;

			if (cur.isNull)
			{
				writefln("% 3s: empty", i);
				continue;
			}

			writef("% 3s:", i);

			while (!cur.isNull)
			{
				auto node = &nodes[cur];
				writef(" (%s)", node.data);
				cur = node.nextIndex;
			}
			writeln;
		}
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

T[] removeInPlace(T)(T[] array, T what)
{
	size_t i = 0;
	size_t length = array.length;
	while(i < length)
	{
		if (array[i] == what)
		{
			array[i] = array[length-1];
			--length;
		}
		++i;
	}
	return assumeSafeAppend(array[0..length]);
}

unittest
{
	assert(removeInPlace([], 1) == []);
	assert(removeInPlace([1], 1) == []);
	assert(removeInPlace([1], 2) == [1]);
	assert(removeInPlace([1, 2], 2) == [1]);
	assert(removeInPlace([2, 1], 2) == [1]);
}

T divCeil(T)(T a, T b)
{
	return a / b + (a % b > 0);
}
