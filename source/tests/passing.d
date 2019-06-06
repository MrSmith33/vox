/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.passing;

import std.stdio;
import tester;

Test[] passingTests() { return collectTests!(tests.passing)(); }

extern(C) void external_print_i32_func(int par1) {
	formattedWrite(testSink, "%s ", par1);
}

@TestInfo(&tester7)
immutable test7 = q{--- test7
	i32 fib(i32 number) {
		if (number < 1) return 0;
		if (number < 3) return 1;
		return fib(number-1) + fib(number-2);
	}
};
void tester7(ref TestContext ctx) {
	auto fib = ctx.getFunctionPtr!(int, int)("fib");
	immutable int[] results = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233,
	377, 610, 987, 1597, 2584, 4181, 6765];
	foreach(size_t i, int expected; results)
	{
		int res = fib(cast(int)i+1);
		assert(res == expected, format("%s != %s", res, expected));
	}
}


@TestInfo()
immutable test9 = q{--- test9
	i32 test(i32 number) {
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
	}
};


@TestInfo(&tester8)
immutable test8 = q{--- test8
	i32 sign(i32 number) {
		i32 result;
		if (number < 0) result = 0-1;
		else if (number > 0) result = 1;
		else result = 0;
		return result;
	}
};


@TestInfo(&tester8)
immutable test8_1 = q{--- test8_1
	i32 sign(i32 number) {
		if (number < 0) return 0-1;
		else if (number > 0) return 1;
		else return 0;
	}
};

void tester8(ref TestContext ctx) {
	auto sign = ctx.getFunctionPtr!(int, int)("sign");
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


@TestInfo(&tester10)
immutable test10 = q{--- test10
	// Test reading pointer at zero index
	i32 run(i32* array) {
		return array[0];
	}
};
void tester10(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int, int*)("run");
	int val = 42;
	int res = run(&val);
	//writefln("test(&42) -> %s", res);
	assert(res == 42);
}


@TestInfo(&tester11)
immutable test11 = q{--- test11
	// Test reading pointer at constant non-zero index
	i32 run(i32* array) {
		return array[1];
	}
};
void tester11(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int, int*)("run");
	int[2] val = [42, 56];
	int res = run(val.ptr);
	//writefln("test([42, 56].ptr) -> %s", res);
	assert(res == 56);
}


@TestInfo(&tester12)
immutable test12 = q{--- test12
	// Test reading pointer at variable index
	i32 run(i32* array, i32 index) {
		return array[index];
	}
};
void tester12(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int, int*, int)("run");
	int[2] val = [42, 56];
	int res0 = run(val.ptr, 0);
	int res1 = run(val.ptr, 1);
	//writefln("test([42, 56].ptr, 1) -> %s", res);
	assert(res0 == 42);
	assert(res1 == 56);
}


@TestInfo(&tester13)
immutable test13 = q{--- test13
	// Test pointer index assign
	void run(i32* array, i32 index, i32 value) {
		array[index] = value;
	}
};
void tester13(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void, int*, int, int)("run");
	int[4] val = [42, 56, 96, 102];
	int[4] expected = [42, 20, 96, 102];
	run(val.ptr, 1, 20);
	//writefln("test([42, 56].ptr, 1, 20) -> %s", val);
	assert(val == expected, format("%s != %s", val, expected));
}


@TestInfo(&tester14)
immutable test14 = q{--- test14
	// Test pointer index assign
	void run(i32* array, i32 index, i32 value, i32 value2, i32 value3) {
		array[index] = value + value2 + value3;
	}
};
void tester14(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void, int*, int, int, int, int)("run");
	int[2] val = [42, 56];
	run(val.ptr, 1, 10, 6, 4);
	//writefln("test([42, 56].ptr, 1, 10, 6, 4) -> %s", val);
	assert(val[1] == 20);
}


@TestInfo(&tester15, [HostSymbol("external", cast(void*)&test15_external_func)])
immutable test15 = q{--- test15
	// Test 3 inputs no parameters pushed to the stack
	i32 run(i32 par) {
		return external(par, 10, 20);
	}
	i32 external(i32, i32, i32);
};
extern(C) int test15_external_func(int par1, int par2, int par3) {
	return par1 + par2 + par3;
}
void tester15(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int, int)("run");
	int result = run(10);
	//writefln("fun(10) -> %s", result);
	assert(result == 40);
}


@TestInfo(&tester16, [HostSymbol("external", cast(void*)&test16_external_func)])
immutable test16 = q{--- test16
	// Test more than 4 inputs (5-th parameter pushed to the stack, extra alignment needed)
	i32 run(i32 par) {
		return external(par, 10, 20, 30, 40);
	}
	i32 external(i32, i32, i32, i32, i32);
};
extern(C) int test16_external_func(int par1, int par2, int par3, int par4, int par5) {
	return par1 + par2 + par3 + par4 + par5;
}
void tester16(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int, int)("run");
	int result = run(10);
	//writefln("fun(10) -> %s", result);
	assert(result == 110);
}


@TestInfo(&tester17, [HostSymbol("external", cast(void*)&test17_external_func)])
immutable test17 = q{--- test17
	// Test 6 inputs (5-th and 6-th parameters pushed to the stack, no extra alignment needed)
	i32 run(i32 par) {
		return external(par, 10, 20, 30, 40, 50);
	}
	i32 external(i32, i32, i32, i32, i32, i32);
};
extern(C) int test17_external_func(int par1, int par2, int par3, int par4, int par5, int par6) {
	return par1 + par2 + par3 + par4 + par5 + par6;
}
void tester17(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int, int)("run");
	int result = run(10);
	//writefln("fun(10) -> %s", result);
	assert(result == 160);
}

void testerRunVoid(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
}


@TestInfo(&testerRunVoid)
immutable test18 = q{--- test18
	// test empty void function
	void run() {}
};


@TestInfo(&testerRunVoid)
immutable test19 = q{--- test19
	// test empty void function with return
	void run() { return; }
};


@TestInfo(&tester20)
immutable test20 = q{--- test20
	// test empty i32 function without return and with control flow
	void run(i32 i) { if(i){}else{} }
};
void tester20(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void, int)("run");
	run(1);
}


void tester21(ref TestContext ctx) {
	auto fibonacci = ctx.getFunctionPtr!(void)("fibonacci");
	fibonacci();
	assert(testSink.text == "1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765");
	testSink.clear;
}


@TestInfo(&tester21, [HostSymbol("print", cast(void*)&external_print_i32_func)])
immutable test21 = q{--- test21
	// test fibonacci. while loop. func call
	void print(i32); // external
	void fibonacci() {
		i32 lo = 0;
		i32 hi = 1;
		while (hi < 10000) {
			hi = hi + lo;
			lo = hi - lo;
			print(lo);
		}
	}
};


@TestInfo(&tester21, [HostSymbol("print", cast(void*)&external_print_i32_func)])
immutable test21_2 = q{--- test21_2
	// Causes other order of phi functions, which requires correct move sequence to resolve
	// Tests phi resolution after register allocation
	void print(i32); // external
	void fibonacci() {
		i32 lo = 0;
		i32 hi = 1;
		while (hi < 10000) {
			i32 tmp = hi;
			hi = hi + lo;
			lo = tmp;
			print(lo);
		}
	}
};


@TestInfo(&tester22)
immutable test22 = q{--- test22
	// test phi resolution with critical edge and test break;
	i32 run() {
		i32 counter = 10;
		i32 counter2 = 0;
		while (counter > 0)
		{
			counter = counter - 1;
			counter2 = counter2 + 1;
			if (counter2 == 5) break;
		}
		return counter;
	}
};
void tester22(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int)("run");
	int res = run();
	assert(res == 5);
}


@TestInfo(&tester23)
immutable test23 = q{--- test23
	// test continue
	i32 run() {
		i32 counter = 10;
		i32 counter2 = 2;
		while (counter > 0) {
			counter = counter - 1;
			if (counter < 5) continue;
			counter2 = counter2 + 1;
		}
		return counter2;
	}
};
void tester23(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int)("run");
	int res = run();
	assert(res == 7);
}


@TestInfo(&tester24, [HostSymbol("print", cast(void*)&test24_external_print)])
immutable test24 = q{--- test24
	// test string literal as u8* param
	void print(u8*);
	void run(){ print("Hello"); }
};
extern(C) void test24_external_print(ubyte* param) {
	testSink.put(cast(char[])param[0..5]); // Hello
}
void tester24(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	assert(testSink.text == "Hello");
	testSink.clear;
}


@TestInfo(&tester25, [HostSymbol("print", cast(void*)&test25_external_print)])
immutable test25 = q{--- test25
	// test struct creation, member set, stack struct as func argument
	struct string { u64 length; u8* ptr; }
	void print(string);
	void run(){
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
void tester25(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	//writefln("run() == '%s'", testSink.text);
	assert(testSink.text == "Hello");
	testSink.clear;
}


@TestInfo(&tester25, [HostSymbol("print", cast(void*)&test25_external_print)])
immutable test26 = q{--- test26
	// test global parameter, assignment
	struct string { u64 length; u8* ptr; }
	void print(string);
	string str;
	void run(){
		str.ptr = "Hello";
		str.length = 5;
		print(str);
	}
};


@TestInfo(&tester27, [HostSymbol("print", cast(void*)&test25_external_print)])
immutable test27 = q{--- test27
	// test slices
	void print(u8[]);
	void run() {
		u8[] array;
		array.length = 9;
		// Assign string literal to ptr
		array.ptr = "AssignPtr";
		print(array);

		// Assign string literal to slice
		array = "AssignSlice";
		print(array);
	}
};
void tester27(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	//writefln("run() == '%s'", testSink.text);
	assert(testSink.text == "AssignPtrAssignSlice");
	testSink.clear;
}


@TestInfo(&tester31, [HostSymbol("print_num", cast(void*)&test31_external_print_num)])
immutable test31 = q{--- test31
	// test enums
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
	void run() {
		print_num(e3);
		print_num(e4);
		print_num(e5);
		print_num(e6);
		print_num(e7.e7);
		print_num(e9.e9);
	}
};
extern(C) void test31_external_print_num(long param) {
	testSink.putf("%s", param);
}
void tester31(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	//writefln("run() == '%s'", testSink.text);
	assert(testSink.text == "345679");
	testSink.clear;
}


@TestInfo(&tester21, [HostSymbol("print", cast(void*)&external_print_i32_func)])
immutable test32 = q{--- test32
	// Test reg alloc xchg generation
	void print(i32); // external
	void fibonacci() {
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
	}
};


@TestInfo(&tester21, [HostSymbol("print", cast(void*)&external_print_i32_func)])
immutable test33 = q{
--- test33_1
	// test multifile compilation
	void print(i32); // external
--- test33_2
	import test33_1;
	void fibonacci() {
		i32 lo = 0;
		i32 hi = 1;
		while (hi < 10000) {
			hi = hi + lo;
			lo = hi - lo;
			print(lo);
		}
	}
};

@TestInfo(&tester34)
immutable test34 = q{--- test34
	i32 getElement(i32[] items, i32 index) { return items[index]; }
};
void tester34(ref TestContext ctx) {
	auto getElement = ctx.getFunctionPtr!(int, Slice!int, int)("getElement");
	int[2] val = [42, 56];
	Slice!int slice = {val.length, val.ptr};
	int res0 = getElement(slice, 0);
	int res1 = getElement(slice, 1);
	assert(res0 == 42);
	assert(res1 == 56);
}


@TestInfo(&tester35)
immutable test35 = q{--- test35
	void setElement(i32[] items, i32 index, i32 value) { items[index] = value; }
};
void tester35(ref TestContext ctx) {
	auto setElement = ctx.getFunctionPtr!(void, Slice!int, int, int)("setElement");
	int[2] val = [42, 56];
	Slice!int slice = {val.length, val.ptr};
	setElement(slice, 0, 88);
	assert(val == [88, 56]);
	setElement(slice, 1, 96);
	assert(val == [88, 96]);
}


@TestInfo()
immutable test36 = q{--- test36
	// Test null literal implicit conversion to pointer types
	void test() {
		callee1(null);
		callee2(null);
	}
	void callee1(void*) {}
	void callee2(u8*) {}
};


@TestInfo()
immutable test37 = q{--- test37
	// Test negative int literal
	void test() {
		callee1(-1);
	}
	void callee1(i32) {}
};


@TestInfo()
immutable test38 = q{--- test38
	// Test empty struct
	struct A {}
};


@TestInfo(&tester39)
immutable test39 = q{--- test39
	// Test left shift
	i32 shl(i32 a, i32 b) {
		return a << b;
	}
};
void tester39(ref TestContext ctx) {
	auto shl = ctx.getFunctionPtr!(int, int, int)("shl");
	foreach(i; 0..33) {
		int res = shl(1, i);
		assert(res == 1 << i);
		//writefln("1 << %s == %b", i, res);
	}
}


@TestInfo(&tester40)
immutable test40 = q{--- test40
	// Test left shift by a constant
	i32 shl(i32 a) {
		i32 res1 = a << 1;
		i32 res2 = 4 << a;
		i32 res3 = a << 3;
		i32 res4 = a << 31;
		i32 res5 = 1 << 31;
		return res1 + res2 + res3 + res4 + res5;
	}
};
void tester40(ref TestContext ctx) {
	auto shl = ctx.getFunctionPtr!(int, int)("shl");
	int res = shl(1);
	//writefln("%b", res);
	assert(res == (1 << 1) + (4 << 1) + (1 << 3) + (1 << 31) + (1 << 31));
}
