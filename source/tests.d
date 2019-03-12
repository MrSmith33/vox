/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module tests;

import std.stdio;
import std.format : formattedWrite, format;
import all;

void runDevTests()
{
	Driver driver;
	driver.initialize(jitPasses);
	driver.context.buildType = BuildType.jit;
	driver.context.validateIr = true;
	driver.context.printTraceOnError = true;
	driver.context.printTodos = true;
	scope(exit) driver.releaseMemory;

	FuncDumpSettings dumpSettings;
	dumpSettings.printBlockFlags = true;

	driver.context.printSource = true;
	//driver.context.printAstFresh = true;
	//driver.context.printAstSema = true;
	//driver.context.runTesters = false;

	driver.context.printIr = true;
	//driver.context.printIrOpt = true;
	//driver.context.printLir = true;
	//driver.context.printLirRA = true;
	//driver.context.printLiveIntervals = true;
	//driver.context.printStaticData = true;
	//driver.context.printCodeHex = true;
	//driver.context.printTimings = true;

	tryRunSingleTest(driver, dumpSettings, DumpTest.yes, test7);

	//driver.context.buildType = BuildType.exe;
	//driver.passes = exePasses;
	//driver.context.windowsSubsystem = WindowsSubsystem.WINDOWS_GUI;
	//tryRunSingleTest(driver, dumpSettings, DumpTest.yes, test30);
}

enum StopOnFirstFail : bool { no = false, yes = true }

void runAllTests(StopOnFirstFail stopOnFirstFail)
{
	auto startInitTime = currTime;
	Driver driver;
	driver.initialize(jitPasses);
	driver.context.buildType = BuildType.jit;
	driver.context.buildDebug = false;
	driver.context.validateIr = true;
	driver.context.printTraceOnError = true;
	auto endInitTime = currTime;

	FuncDumpSettings dumpSettings;
	dumpSettings.printBlockFlags = true;

	Test[] jitTests = [test7, test8, test8_1, test10, test9, test13, test18, test19,
		test20, test21, test21_2, test22, test23, test24, test25, test26, test27, test31, test32];

	Test[] exeTests = [test28, test29];

	size_t numTests = jitTests.length + exeTests.length;
	size_t numSuccessfulTests;
	writefln("Running %s tests", numTests);

	void runTests(size_t indexOffset, Test[] tests)
	{
		foreach(size_t i, ref Test test; tests)
		{
			TestResult res = tryRunSingleTest(driver, dumpSettings, DumpTest.no, test);

			if (res == TestResult.failure)
			{
				writefln("%s/%s %s %s", indexOffset+i+1, numTests, test.testName, res);

				if (stopOnFirstFail) {
					writeln("Stopping on first fail");
					break;
				}
			}
			else
			{
				//writefln("%s `%s` success", indexOffset+i+1, test.testName);
				++numSuccessfulTests;
			}
		}
	}

	auto time1 = currTime;

	runTests(0, jitTests);

	driver.context.buildType = BuildType.exe;
	driver.passes = exePasses;
	runTests(jitTests.length, exeTests);

	auto time2 = currTime;
	Duration duration = time2-time1;

	auto startReleaseTime = currTime;
	driver.releaseMemory;
	auto endReleaseTime = currTime;

	writefln("Done %s/%s successful in %ss, init %ss, release %ss",
		numSuccessfulTests,
		numTests,
		scaledNumberFmt(duration),
		scaledNumberFmt(endInitTime-startInitTime),
		scaledNumberFmt(endReleaseTime-startReleaseTime));
}

enum DumpTest : bool { no = false, yes = true }
enum TestResult { failure, success }

TestResult tryRunSingleTest(ref Driver driver, ref FuncDumpSettings dumpSettings, DumpTest dumpTest, Test curTest)
{
	try
	{
		runSingleTest(driver, dumpSettings, dumpTest, curTest);
	}
	catch(CompilationException e) {
		writeln(driver.context.sink.text);
		if (e.isICE)
			writeln(e);
		return TestResult.failure;
	}
	catch(Throwable t) {
		writeln(driver.context.sink.text);
		writeln(t);
		return TestResult.failure;
	}
	return TestResult.success;
}

void runSingleTest(ref Driver driver, ref FuncDumpSettings dumpSettings, DumpTest dumpTest, Test curTest)
{
	if (dumpTest)
	{
		// dump settings
		//driver.context.printSource = true;
		//driver.context.printAst = true;
		//driver.context.printIr = true;
		//driver.context.printLir = true;
		//driver.context.printLiveIntervals = true;
		//driver.context.printStaticData = true;
		//driver.context.printCodeHex = true;
		//driver.context.printTimings = true;
	}

	enum NUM_ITERS = 1;
	auto times = PerPassTimeMeasurements(NUM_ITERS, driver.passes);
	auto time1 = currTime;
		ModuleDeclNode* mod = driver.compileModule(
			SourceFileInfo("test", curTest.source),
			curTest.hostSymbols,
			curTest.dllModules);
		driver.markCodeAsExecutable();
	auto time2 = currTime;
	times.onIteration(0, time2-time1);

	if (dumpTest && driver.context.printTimings) times.print;

	if (!driver.context.runTesters) return;
	if (mod is null) return;

	final switch (driver.context.buildType)
	{
		case BuildType.jit:
			if (curTest.funcName is null) return;

			FunctionDeclNode* funDecl = mod.findFunction(curTest.funcName, &driver.context);

			if (funDecl != null && funDecl.backendData.funcPtr != null)
			{
				if (dumpTest) writefln("Running: %s %s()", curTest.testName, curTest.funcName);
				curTest.tester(funDecl.backendData.funcPtr);
			}
			break;

		case BuildType.exe:
			import std.process;
			import std.file : exists;
			import std.path;
			if(exists(driver.context.outputFilename))
			{
				if (dumpTest) writef("Running: %s", driver.context.outputFilename.absolutePath);
				auto result = execute(driver.context.outputFilename);
				if (dumpTest) writefln(", status %s, output '%s'", result.status, result.output);
			}
			else
			{
				writefln("No executable produced '%s'", driver.context.outputFilename);
			}
			break;
	}
}

struct Test
{
	string testName;
	string source;
	string funcName;
	alias Tester = void function(void* funcPtr);
	void function(void* funcPtr) tester;
	HostSymbol[] hostSymbols;
	DllModule[] dllModules;
}

TextSink testSink;

immutable input1 = q{
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

immutable input2 = q{void e() {
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

immutable input3 = q{
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

immutable input4 = q{
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
immutable input5 = q{void f() {
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

immutable input6 = q{void f() {
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

immutable input7 = q{i32 fib(i32 number) {
	if (number < 1) return 0;
	if (number < 3) return 1;
	return fib(number-1) + fib(number-2);
}};
alias Func7 = extern(C) int function(int);
void tester7(Func7 fib) {
	immutable int[] results = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233,
	377, 610, 987, 1597, 2584, 4181, 6765];
	foreach(size_t i, int expected; results)
	{
		int res = fib(cast(int)i+1);
		assert(res == expected, format("%s != %s", res, expected));
	}
}
auto test7 = Test("Test 7", input7, "fib", cast(Test.Tester)&tester7);

immutable input9 = q{i32 test(i32 number) {
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
auto test9 = Test("Test 9", input9);

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
	//writefln("sign(10) -> %s", res1);
	//writefln("sign(0) -> %s", res2);
	//writefln("sign(-10) -> %s", res3);
	assert(res1 == 1);
	assert(res2 == 0);
	assert(res3 == -1);
}
auto test8 = Test("Test 8", input8, "sign", cast(Test.Tester)&tester8);
auto test8_1 = Test("Test 8.1", input8_1, "sign", cast(Test.Tester)&tester8);

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
	int res0 = fun(val.ptr, 0);
	int res1 = fun(val.ptr, 1);
	//writefln("test([42, 56].ptr, 1) -> %s", res);
	assert(res0 == 42);
	assert(res1 == 56);
}
auto test12 = Test("Test 12", input12, "test", cast(Test.Tester)&tester12);

immutable input13 = q{void test(i32* array, i32 index, i32 value) {
	array[index] = value;
}};
alias Func13 = extern(C) void function(int*, int, int);
void tester13(Func13 fun) {
	int[4] val = [42, 56, 96, 102];
	int[4] expected = [42, 20, 96, 102];
	fun(val.ptr, 1, 20);
	//writefln("test([42, 56].ptr, 1, 20) -> %s", val);
	assert(val == expected, format("%s != %s", val, expected));
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
	[HostSymbol("external", cast(void*)&test15_external_func)]);


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
	[HostSymbol("external", cast(void*)&test16_external_func)]);

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
	[HostSymbol("external", cast(void*)&test17_external_func)]);

// test empty void function
immutable input18 = q{void test() {}};
alias Func18 = extern(C) void function();
void tester18(Func18 fun) { fun(); }
auto test18 = Test("Test 18", input18, "test", cast(Test.Tester)&tester18);

// test empty void function with return
immutable input19 = q{void test() { return; }};
alias Func19 = extern(C) void function();
void tester19(Func19 fun) { fun(); }
auto test19 = Test("Test 19", input19, "test", cast(Test.Tester)&tester19);

// test empty i32 function without return and with control flow
immutable input20 = q{void test(i32 i) { if(i){}else{} }};
alias Func20 = extern(C) void function(int);
void tester20(Func20 fun) { fun(1); }
auto test20 = Test("Test 20", input20, "test", cast(Test.Tester)&tester20);

// test fibonacci. while loop. func call
immutable input21 =
q{void print(i32); // external
void fibonacci() {
	i32 lo = 0;
	i32 hi = 1;
	while (hi < 10000) {
		hi = hi + lo;
		lo = hi - lo;
		print(lo);
	}
}};

// Causes other order of phi functions, which requires correct move sequence to resolve
// Tests phi resolution after register allocation
immutable input21_2 =
q{void print(i32); // external
void fibonacci() {
	i32 lo = 0;
	i32 hi = 1;
	while (hi < 10000) {
		i32 tmp = hi;
		hi = hi + lo;
		lo = tmp;
		print(lo);
	}
}};
alias Func21 = extern(C) void function();
void tester21(Func21 fibonacci) {
	fibonacci();
	assert(testSink.text == "1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765");
	testSink.clear;
}
extern(C) void test21_external_func(int par1) {
	formattedWrite(testSink, "%s ", par1);
}
auto test21 = Test("Test 21", input21, "fibonacci", cast(Test.Tester)&tester21,
	[HostSymbol("print", cast(void*)&test21_external_func)]);
auto test21_2 = Test("Test 21.2", input21_2, "fibonacci", cast(Test.Tester)&tester21,
	[HostSymbol("print", cast(void*)&test21_external_func)]);

// test phi resolution with critical edge and test break;
immutable input22 = q{
i32 test() {
	i32 counter = 10;
	i32 counter2 = 0;
	while (counter > 0)
	{
		counter = counter - 1;
		counter2 = counter2 + 1;
		if (counter2 == 5) break;
	}
	return counter;
}};
alias Func22 = extern(C) int function();
void tester22(Func22 fun) { int res = fun(); assert(res == 5); }
auto test22 = Test("Test 22", input22, "test", cast(Test.Tester)&tester22);

// test continue
immutable input23 = q{
i32 test() {
	i32 counter = 10;
	i32 counter2 = 2;
	while (counter > 0) {
		counter = counter - 1;
		if (counter < 5) continue;
		counter2 = counter2 + 1;
	}
	return counter2;
}};
alias Func23 = extern(C) int function();
void tester23(Func23 fun) { int res = fun(); assert(res == 7); }
auto test23 = Test("Test 23", input23, "test", cast(Test.Tester)&tester23);

// test string literal
immutable input24 = q{
	void print(u8*);
	void test(){ print("Hello"); }
};
extern(C) void test24_external_print(ubyte* param) {
	testSink.put(cast(char[])param[0..5]); // Hello
}
alias Func24 = extern(C) void function();
void tester24(Func24 fun) {
	fun();
	assert(testSink.text == "Hello");
	testSink.clear;
}
auto test24 = Test("String literal as u8* param", input24, "test", cast(Test.Tester)&tester24,
	[HostSymbol("print", cast(void*)&test24_external_print)]);

// test struct creation, member set, struct as func arg
immutable input25 = q{
	struct string { u64 length; u8* ptr; }
	void print(string);
	void test(){
		string str;
		str.ptr = "Hello";
		str.length = 5;
		print(str);
	}
};
struct Slice(T) {
	ulong length;
	T* ptr;
}
extern(C) void test25_external_print(Slice!char param) {
	char[] slice = *cast(char[]*)&param;
	testSink.put(slice); // Hello
}
alias Func25 = extern(C) void function();
void tester25(Func25 fun) {
	fun();
	//writefln("fun() == '%s'", testSink.text);
	assert(testSink.text == "Hello");
	testSink.clear;
}
auto test25 = Test("Stack struct as parameter", input25, "test", cast(Test.Tester)&tester25,
	[HostSymbol("print", cast(void*)&test25_external_print)]);

// test global parameter, assignment
immutable input26 = q{
	struct string { u64 length; u8* ptr; }
	void print(string);
	string str;
	void test(){
		str.ptr = "Hello";
		str.length = 5;
		print(str);
	}
};
auto test26 = Test("Global struct", input26, "test", cast(Test.Tester)&tester25,
	[HostSymbol("print", cast(void*)&test25_external_print)]);

// test slices
immutable input27 = q{
	void print(u8[]);
	void test() {
		u8[] array;
		array.length = 9;
		array.ptr = "AssignPtr";
		print(array);

		array = "AssignSlice";
		print(array);
	}
};
void tester27(Func25 fun) {
	fun();
	//writefln("fun() == '%s'", testSink.text);
	assert(testSink.text == "AssignPtrAssignSlice");
	testSink.clear;
}
auto test27 = Test("Assign string literal to slice/ptr", input27, "test", cast(Test.Tester)&tester27,
	[HostSymbol("print", cast(void*)&test25_external_print)]);

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

immutable inputX = q{
	#pragma(lib, "kernel32")
	u8 WriteConsoleA(
		void* hConsoleOutput,
		void* lpBuffer,
		u32 nNumberOfCharsToWrite,
		u32* lpNumberOfCharsWritten,
		void* lpReserved
	);
	#pragma(lib, "kernel32")
	void* GetStdHandle(u32 nStdHandle);
	enum : u32 {
		STD_INPUT_HANDLE  = 0xFFFFFFF6,
		STD_OUTPUT_HANDLE = 0xFFFFFFF5,
		STD_ERROR_HANDLE  = 0xFFFFFFF4
	}
	void main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		u8[] array = "Hello world";
		u32 numWritten;
		void* handle = GetStdHandle(STD_OUTPUT_HANDLE);
		WriteConsoleA(handle, array.ptr, array.length, &numWritten, null);
	}
};

immutable input28 = q{
	i32 main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		return 0;
	}
};
auto test28 = Test("exe no static data, no imports", input28);

immutable input29 = q{
	u8 WriteConsoleA(
		void* hConsoleOutput,
		u8* lpBuffer,
		u32 nNumberOfCharsToWrite,
		u32* lpNumberOfCharsWritten,
		u64 lpReserved
	);
	void* GetStdHandle(u32 nStdHandle);
	i32 main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		u8[] array = "Hello world";
		u32 numWritten;
		void* handle = GetStdHandle(0xFFFFFFF5); // STD_OUTPUT_HANDLE
		WriteConsoleA(handle, array.ptr, cast(u32)array.length, &numWritten, 0);
		return 0;
	}
};
auto test29 = Test("exe", input29, null, null, null,
	[DllModule("kernel32", ["WriteConsoleA", "GetStdHandle"])]);

immutable input30 = q{
	void SDL_SetMainReady();
	i32 SDL_Init(u32);
	void SDL_Quit();
	void* SDL_CreateWindow(u8* title, i32 x, i32 y, i32 w, i32 h, u32 flags);
	void* SDL_CreateRenderer(void* window, i32 index, u32 flags);
	void SDL_DestroyRenderer(void* renderer);
	void SDL_DestroyWindow(void* renderer);
	i32 SDL_PollEvent(SDL_Event* event);
	struct SDL_Event
	{
		u32 type;
	    u8[52] padding;
	}
	void ExitProcess(u32 uExitCode);

	i32 main(void* hInstance, void* hPrevInstance, u8* lpCmdLine, i32 nShowCmd) {
		SDL_SetMainReady();
		if(SDL_Init(0x00000020) < 0) return 1;
		void* window = SDL_CreateWindow("SDL test via tiny_jit", 0x1FFF0000, 0x1FFF0000, 300, 100, 4);
		void* renderer = SDL_CreateRenderer(window, 0xFFFF_FFFF, 2);
		SDL_Event e;
		while (1)
		{
			SDL_PollEvent(&e);
			if (e.type == 0x100) // SDL_QUIT
				break;
		}
		SDL_DestroyRenderer(renderer);
		SDL_DestroyWindow(window);
		SDL_Quit();
		ExitProcess(0);
		return 0;
	}
};
auto test30 = Test("exe SDL", input30, null, null, null, [
	DllModule("SDL2", ["SDL_SetMainReady", "SDL_Init", "SDL_Quit",
		"SDL_CreateWindow", "SDL_CreateRenderer", "SDL_PollEvent",
		"SDL_DestroyRenderer", "SDL_DestroyWindow"]),
	DllModule("kernel32", ["ExitProcess"])]
);

extern(C) void test31_external_print_num(long param) {
	testSink.putf("%s", param);
}

immutable input31 = q{
	//enum i32 e2; // manifest constant, invalid, need initializer
	enum e3 = 3; // manifest constant
	enum i32 e4 = 4; // manifest constant

	enum { e5 = 5 } // anon type
	enum : i32 { e6 = 6 } // anon type

	enum e1; // type
	enum e7 : i32 { e7 = 7 } // type
	enum e8 : i32; // type, body omitted
	enum e9 { e9 = 9 } // type

	void print_num(i64 val);
	void test() {
		print_num(e3);
		print_num(e4);
		print_num(e5);
		print_num(e6);
		print_num(e7.e7);
		print_num(e9.e9);
	}
};
void tester31(Func25 fun) {
	fun();
	//writefln("fun() == '%s'", testSink.text);
	assert(testSink.text == "345679");
	testSink.clear;
}
auto test31 = Test("enum", input31, "test", cast(Test.Tester)&tester31,
	[HostSymbol("print_num", cast(void*)&test31_external_print_num)]);

// Test reg alloc xchg generation
immutable input32 =
q{void print(i32); // external
void main() {
	i32 lo = 0;
	i32 hi = 1;
	while (hi < 10000) {
		i32 tmp = hi;
		hi = hi + lo;
		lo = tmp;
		print(lo);
	}
	while (hi < 10000) {
		i32 tmp = hi;
		hi = hi + lo;
		lo = tmp;
		print(lo);
	}
	while (hi < 10000) {
		i32 tmp = hi;
		hi = hi + lo;
		lo = tmp;
		print(lo);
	}
	while (hi < 10000) {
		i32 tmp = hi;
		hi = hi + lo;
		lo = tmp;
		print(lo);
	}
}};
alias Func32 = extern(C) void function();
void tester32(Func32 fibonacci) {
	fibonacci();
	assert(testSink.text == "1 1 2 3 5 8 13 32 34 55 89 144 233 377 610 987 1597 2584 4181 6765");
	testSink.clear;
}
extern(C) void test32_external_func(int par1) {
	formattedWrite(testSink, "%s ", par1);
}
auto test32 = Test("Test 32", input32, "fibonacci 4 times", cast(Test.Tester)&tester32,
	[HostSymbol("print", cast(void*)&test32_external_func)]);
