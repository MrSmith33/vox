module driver;

import std.stdio : writeln, write, writef, writefln, stdout;
import compiler1;
import ir;
import utils;
import ast_to_ir;
import ir_to_lir_amd64;

unittest
{
	writefln("unittest");
}

void main()
{
	//bench();
	test();
}

version = print;

void test()
{
	Test curTest = test8;
	Driver driver;

	try
	{
		driver.initialize(compilerPasses);
		scope(exit) driver.releaseMemory;

		enum NUM_ITERS = 1;
		auto times = PerPassTimeMeasurements(NUM_ITERS, driver.passes);
		auto time1 = currTime;
		ModuleDeclNode* mod = driver.compileModule(curTest.source, curTest.externalSymbols);
		auto time2 = currTime;
		times.onIteration(0, time2-time1);

		if (mod is null) return;

		version(print)
		{
			// Text dump
			//auto astPrinter = AstPrinter(&driver.context, 2);
			//astPrinter.printAst(cast(AstNode*)mod);

			writeln("// Source");
			writeln(curTest.source);

			writeln("\n// IR");
			TextSink sink;
			mod.irModule.dump(sink, &driver.context);
			writeln(sink.text);

			writeln("\n// Amd64 code");
			printHex(mod.irModule.code, 16);

			times.print;
		}

		FunctionDeclNode* funDecl = mod.findFunction(curTest.funcName, &driver.context);
		if (funDecl != null && funDecl.funcPtr != null)
		{
			curTest.tester(funDecl.funcPtr);
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

	Driver driver;
	driver.initialize(compilerPasses);
	scope(exit) driver.releaseMemory;

	enum iters = 100_000;
	auto times = PerPassTimeMeasurements(iters, driver.passes);

	foreach (iteration; 0..times.totalTimes.numIters)
	{
		auto time1 = currTime;
		mod = driver.compileModule(input, null);
		auto time2 = currTime;

		times.onIteration(iteration, time2-time1);
	}

	times.print;
}

CompilePass[] compilerPasses = [
	CompilePass("Parse", &pass_parser),
	CompilePass("Semantic insert", &pass_semantic_decl),
	CompilePass("Semantic lookup", &pass_semantic_lookup),
	CompilePass("Semantic types", &pass_semantic_type),
	CompilePass("IR gen", &pass_new_ir_gen),
	CompilePass("IR to LIR AMD64", &pass_ir_to_lir_amd64),
	//// IR liveness
	//CompilePass("Live intervals", &pass_live_intervals),
	//// IR regalloc
	//CompilePass("Linear scan", &pass_linear_scan),
	//// Stack layout
	//CompilePass("Stack layout", &pass_stack_layout),
	//// LIR -> machine code
	//CompilePass("Code gen", &pass_code_gen),
];

struct Driver
{
	CompilationContext context;
	CompilePass[] passes;

	Win32Allocator allocator;
	ubyte[] codeBuffer;
	ubyte[] irBuffer;
	ubyte[] tempBuffer;

	void initialize(CompilePass[] passes_)
	{
		passes = passes_;

		// Try to allocate code buffer closer to host code pages,
		// so that 32-bit offset can be used for calls
		size_t thisAddr = cast(size_t)((&initialize).ptr); // take address of function in memory
		size_t step = 0x10_000_000;
		size_t aligned = alignValue(thisAddr, step) - step*20;
		//codeBuffer = allocate(PAGE_SIZE * 8, cast(void*)aligned, MemType.RWX);

		// IrIndex can address 2^28 * 4 bytes = 1GB
		size_t irMemSize = 1024UL*1024*1024*2;
		size_t arrSizes = 1024UL*1024*1024;
		bool success = allocator.reserve(irMemSize);
		context.assertf(success, "allocator failed");

		irBuffer = cast(ubyte[])allocator.allocate(arrSizes);
		tempBuffer = cast(ubyte[])allocator.allocate(arrSizes);
	}

	void releaseMemory()
	{
		allocator.releaseMemory();
	}

	ModuleDeclNode* compileModule(string moduleSource, ExternalSymbol[] externalSymbols)
	{
		context = CompilationContext(moduleSource, codeBuffer);
		context.irBuffer.setBuffer(cast(uint[])irBuffer);
		context.tempBuffer.setBuffer(cast(uint[])tempBuffer);
		foreach (ref extSym; externalSymbols)
			context.externalSymbols[context.idMap.getOrReg(extSym.name)] = extSym;

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

struct Test
{
	string testName;
	string source;
	string funcName;
	alias Tester = void function(void* funcPtr);
	void function(void* funcPtr) tester;
	ExternalSymbol[] externalSymbols;
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

immutable input8 = q{i32 sign(i32 number) {
	i32 result;
	if (number < 0) result = 0-1;
	else if (number > 0) result = 1;
	else result = 0;
	return result;
}};

immutable input8_1 = q{i32 sign(i32 number) {
	if (number < 0) return 0-1;
	else if (number > 0) return 1;
	else return 0;
}};
alias Func8 = extern(C) int function(int);
void tester8(Func8 sign) {
	int res1 = sign(10);
	int res2 = sign(0);
	int res3 = sign(-10);
	assert(res1 == 1);
	assert(res2 == 0);
	assert(res3 == -1);

	//writefln("sign(10) -> %s", res1);
	//writefln("sign(0) -> %s", res2);
	//writefln("sign(-10) -> %s", res3);
}
auto test8 = Test("Test 8", input8, "sign", cast(Test.Tester)&tester8);

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


immutable input14 = q{void test(i32* array, i32 index, i32 value, i32 value2, i32 value3) {
	array[index] = value + value2 + value3;
}};
alias Func14 = extern(C) void function(int*, int, int, int, int);
void tester14(Func14 fun) {
	int[2] val = [42, 56];
	fun(val.ptr, 1, 10, 6, 4);
	writefln("test([42, 56].ptr, 1, 10, 6, 4) -> %s", val);
	assert(val[1] == 20);
}
auto test14 = Test("Test 14", input14, "test", cast(Test.Tester)&tester14);


// Test 3 inputs no parameters pushed to the stack
immutable input15 = q{i32 test(i32 par) {
	return external(par, 10, 20);
}
i32 external(i32, i32, i32);
};
alias Func15 = extern(C) int function(int par);
extern(C) int test15_external_func(int par1, int par2, int par3) {
	return par1 + par2 + par3;
}
void tester15(Func15 funcPtr) {
	int result = funcPtr(10);
	writefln("fun(10) -> %s", result);
	assert(result == 40);
}
auto test15 = Test("Test 15", input15, "test", cast(Test.Tester)&tester15,
	[ExternalSymbol("external", cast(void*)&test15_external_func)]);


// Test more than 4 inputs (5-th parameter pushed to the stack, extra alignment needed)
immutable input16 = q{i32 test(i32 par) {
	return external(par, 10, 20, 30, 40);
}
i32 external(i32, i32, i32, i32, i32);
};
alias Func16 = extern(C) int function(int par);
extern(C) int test16_external_func(int par1, int par2, int par3, int par4, int par5) {
	return par1 + par2 + par3 + par4 + par5;
}
void tester16(Func16 funcPtr) {
	int result = funcPtr(10);
	writefln("fun(10) -> %s", result);
	assert(result == 110);
}
auto test16 = Test("Test 16", input16, "test", cast(Test.Tester)&tester16,
	[ExternalSymbol("external", cast(void*)&test16_external_func)]);

// Test 6 inputs (5-th and 6-th parameters pushed to the stack, no extra alignment needed)
immutable input17 = q{i32 test(i32 par) {
	return external(par, 10, 20, 30, 40, 50);
}
i32 external(i32, i32, i32, i32, i32, i32);
};
alias Func17 = extern(C) int function(int par);
extern(C) int test17_external_func(int par1, int par2, int par3, int par4, int par5, int par6) {
	return par1 + par2 + par3 + par4 + par5 + par6;
}
void tester17(Func17 funcPtr) {
	int result = funcPtr(10);
	writefln("fun(10) -> %s", result);
	assert(result == 160);
}
auto test17 = Test("Test 17", input17, "test", cast(Test.Tester)&tester17,
	[ExternalSymbol("external", cast(void*)&test17_external_func)]);

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