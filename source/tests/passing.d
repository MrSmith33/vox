/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.passing;

import core.stdc.stdlib : malloc, free;
import std.stdio;
import tester;

Test[] passingTests() { return collectTests!(tests.passing)(); }

/// Used to force correct type of float literal, like `42.54f.force`
/// Without it, 42.54f will be store as dobule by the compiler
T force(T)(T t) { return t; }

extern(C) void external_print_i32_func(int par1) {
	formattedWrite(testSink, "%s ", par1);
}
extern(C) void external_print_i64_func(long par1) {
	formattedWrite(testSink, "%s ", par1);
}
extern(C) void external_print_string(Slice!char param) {
	char[] slice = *cast(char[]*)&param;
	testSink.put(slice);
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
	@extern(module, "host")
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
	@extern(module, "host")
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
	@extern(module, "host")
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
}


@TestInfo(&tester21, [HostSymbol("print", cast(void*)&external_print_i32_func)])
immutable test21 = q{--- test21
	// test fibonacci. while loop. func call
	@extern(module, "host")
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
	@extern(module, "host")
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
	@extern(module, "host")
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
}


@TestInfo(&tester25, [HostSymbol("print", cast(void*)&external_print_string)])
immutable test25 = q{--- test25
	// test struct creation, member set, stack struct as func argument
	struct string { u64 length; u8* ptr; }
	@extern(module, "host")
	void print(string);
	void run(){
		string str;
		str.ptr = "Hello";
		str.length = 5;
		print(str);
	}
};
void tester25(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	//writefln("run() == '%s'", testSink.text);
	assert(testSink.text == "Hello");
}


@TestInfo(&tester25, [HostSymbol("print", cast(void*)&external_print_string)])
immutable test26 = q{--- test26
	// test global parameter, assignment
	struct string { u64 length; u8* ptr; }
	@extern(module, "host")
	void print(string);
	string str;
	void run(){
		str.ptr = "Hello";
		str.length = 5;
		print(str);
	}
};


@TestInfo(&tester27, [HostSymbol("print", cast(void*)&external_print_string)])
immutable test27 = q{--- test27
	// test slices
	@extern(module, "host")
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

	@extern(module, "host")
	void print_num(i64 val);
	e9 ret_enum(){ return e9.e9; }
	void accept_enum(e9 e){print_num(e);}
	void run() {
		print_num(e3);
		print_num(e4);
		print_num(e5);
		print_num(e6);
		// enum implicitly casts to base
		print_num(e7.e7);
		print_num(e9.e9);
		accept_enum(e9.e9);
		// explicit cast of int to enum
		accept_enum(cast(e9)42);
		// makes sure that e9.e9 is of type e9 and that e9 is implicitly castable to i32
		print_num(ret_enum());
	}
};
extern(C) void test31_external_print_num(long param) {
	testSink.putf("%s", param);
}
void tester31(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	//writefln("run() == '%s'", testSink.text);
	assert(testSink.text == "3456799429");
}


@TestInfo(&tester21, [HostSymbol("print", cast(void*)&external_print_i32_func)])
immutable test32 = q{--- test32
	// Test reg alloc xchg generation
	@extern(module, "host")
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
	@extern(module, "host")
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
	Slice!int slice = Slice!int(val);
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
	Slice!int slice = Slice!int(val);
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
	// Test shifts
	i32 shl(i32 a, i32 b) {
		return a << b;
	}
	i32 shr(i32 a, i32 b) {
		return a >> b;
	}
	i32 sar(i32 a, i32 b) {
		return a >>> b;
	}
};
void tester39(ref TestContext ctx) {
	auto shl = ctx.getFunctionPtr!(int, int, int)("shl");
	auto shr = ctx.getFunctionPtr!(int, int, int)("shr");
	auto sar = ctx.getFunctionPtr!(int, int, int)("sar");
	foreach(i; 0..33) {
		int res = shl(1, i);
		assert(res == 1 << i);
		//writefln("1 << %s == %b", i, res);

		enum HIGH_BIT = 1 << 31;
		int res2 = shr(HIGH_BIT, i);
		//writefln("%032b >> %s == %032b", HIGH_BIT, i, res2);
		assert(res2 == HIGH_BIT >> i);

		int res3 = sar(HIGH_BIT, i);
		assert(res3 == HIGH_BIT >>> i);
		//writefln("%032b >>> %s == %032b", HIGH_BIT, i, res3);
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


@TestInfo(&tester41)
immutable test41 = q{--- test41
	// Test division, multiplication, remainder
	i32 idiv(i32 a, i32 b) {
		return a / b;
	}
	u32 div(u32 a, u32 b) {
		return a / b;
	}
	i32 imul(i32 a, i32 b) {
		return a * b;
	}
	u32 mul(u32 a, u32 b) {
		return a * b;
	}
	i32 irem(i32 a, i32 b) {
		return a % b;
	}
	u32 rem(u32 a, u32 b) {
		return a % b;
	}
};
void tester41(ref TestContext ctx) {
	auto div = ctx.getFunctionPtr!(int, int, int)("div");
	auto idiv = ctx.getFunctionPtr!(int, int, int)("idiv");
	auto rem = ctx.getFunctionPtr!(int, int, int)("rem");
	auto irem = ctx.getFunctionPtr!(int, int, int)("irem");
	auto mul = ctx.getFunctionPtr!(int, int, int)("mul");
	auto imul = ctx.getFunctionPtr!(int, int, int)("imul");
	int resDiv = div(10, 5);
	//writefln("10 / 5 == %s", resDiv);
	assert(resDiv == 10 / 5);
	int resIdiv = idiv(-10, 5);
	//writefln("-10 / 5 == %s %s", resIdiv, -10 / 5);
	assert(resIdiv == -10 / 5);
	int resRem = rem(10, 4);
	//writefln("10 %% 4 == %s", resRem);
	assert(resRem == 10 % 4);
	int resIrem = irem(-10, 4);
	//writefln("-10 %% 4 == %s %s", resIrem, -10 % 4);
	assert(resIrem == -10 % 4);
	int resMul = mul(-10, 4);
	//writefln("-10 * 4 == %s %s", resMul, -10 * 4);
	assert(resMul == -10 * 4);
	int resImul = imul(-10, 4);
	//writefln("-10 * 4 == %s %s", resImul, -10 * 4);
	assert(resImul == -10 * 4);
}


@TestInfo(&tester42)
immutable test42 = q{--- test42
	// Test bit-wise negation, unary minus, xor, or, and operators
	i32 not(i32 a) {
		return ~a;
	}
	i32 neg(i32 a) {
		return -a;
	}
	i32 xor(i32 a, i32 b) {
		return a ^ b;
	}
	i32 or(i32 a, i32 b) {
		return a | b;
	}
	i32 and(i32 a, i32 b) {
		return a & b;
	}
};
void tester42(ref TestContext ctx) {
	auto not = ctx.getFunctionPtr!(int, int)("not");
	auto neg = ctx.getFunctionPtr!(int, int)("neg");
	auto xor = ctx.getFunctionPtr!(int, int, int)("xor");
	auto or  = ctx.getFunctionPtr!(int, int, int)("or");
	auto and = ctx.getFunctionPtr!(int, int, int)("and");

	int res_not = not(0b01);
	//writefln("~0b01 == %02b", res_not);
	assert(res_not == ~0b01);

	int res_neg = neg(1);
	//writefln("-1 == %s", res_neg);
	assert(res_neg == -1);

	int res_xor = xor(0b0011, 0b0101);
	//writefln("0b0011 ^ 0b0101 == %04b", res_xor);
	assert(res_xor == (0b0011 ^ 0b0101));

	int res_or = or(0b0011, 0b0101);
	//writefln("0b0011 | 0b0101 == %04b", res_or);
	assert(res_or == (0b0011 | 0b0101));

	int res_and = and(0b0011, 0b0101);
	//writefln("0b0011 & 0b0101 == %04b", res_and);
	assert(res_and == (0b0011 & 0b0101));
}


@TestInfo(&tester43)
immutable test43 = q{--- test43
	// Test op=
	i32 add(i32 a, i32 b) { a += b; return a; }
	i32 sub(i32 a, i32 b) { a -= b; return a; }
	i32 mul(i32 a, i32 b) { a *= b; return a; }
	i32 div(i32 a, i32 b) { a /= b; return a; }
	i32 rem(i32 a, i32 b) { a %= b; return a; }
	i32 shl(i32 a, i32 b) { a <<= b; return a; }
	i32 shr(i32 a, i32 b) { a >>>= b; return a; }
	i32 sar(i32 a, i32 b) { a >>= b; return a; }
	i32 or (i32 a, i32 b) { a |= b; return a; }
	i32 xor(i32 a, i32 b) { a ^= b; return a; }
	i32 and(i32 a, i32 b) { a &= b; return a; }
};
void tester43(ref TestContext ctx) {
	auto add = ctx.getFunctionPtr!(int, int, int)("add");
	auto sub = ctx.getFunctionPtr!(int, int, int)("sub");
	auto mul = ctx.getFunctionPtr!(int, int, int)("mul");
	auto div = ctx.getFunctionPtr!(int, int, int)("div");
	auto rem = ctx.getFunctionPtr!(int, int, int)("rem");
	auto shl = ctx.getFunctionPtr!(int, int, int)("shl");
	auto shr = ctx.getFunctionPtr!(int, int, int)("shr");
	auto sar = ctx.getFunctionPtr!(int, int, int)("sar");
	auto or  = ctx.getFunctionPtr!(int, int, int)("or");
	auto xor = ctx.getFunctionPtr!(int, int, int)("xor");
	auto and = ctx.getFunctionPtr!(int, int, int)("and");

	assert(add(1, 3) == 4);
	assert(sub(3, 1) == 2);
	assert(mul(3, 2) == 6);
	assert(div(7, 2) == 3);
	assert(rem(7, 2) == 1);
	assert(shl(1, 2) == 4);
	assert(shr(int.min, 2) == (int.min >>> 2));
	assert(sar(int.min, 2) == (int.min >> 2));
	assert(or(0b0011, 0b0101) == 0b0111);
	assert(xor(0b0011, 0b0101) == 0b0110);
	assert(and(0b0011, 0b0101) == 0b0001);
}


@TestInfo(&tester44)
immutable test44 = q{--- test44
	// Test --a, ++a
	i32 preInc(i32 a) { return ++a + ++a; }
	i32 postInc(i32 a) { return a++ + a++; }
	i32 preDec(i32 a) { return --a + --a; }
	i32 postDec(i32 a) { return a-- + a--; }

	i32* preIncPtr(i32* a) { return ++a; }
	i32* postIncPtr(i32* a) { return a++; }
	i32* preDecPtr(i32* a) { return --a; }
	i32* postDecPtr(i32* a) { return a--; }
};
void tester44(ref TestContext ctx) {
	auto preInc = ctx.getFunctionPtr!(int, int)("preInc");
	auto postInc = ctx.getFunctionPtr!(int, int)("postInc");
	auto preDec = ctx.getFunctionPtr!(int, int)("preDec");
	auto postDec = ctx.getFunctionPtr!(int, int)("postDec");

	assert(preInc(10) == 23);
	assert(postInc(10) == 21);
	assert(preDec(3) == 3);
	assert(postDec(3) == 5);

	auto preIncPtr = ctx.getFunctionPtr!(int*, int*)("preIncPtr");
	auto postIncPtr = ctx.getFunctionPtr!(int*, int*)("postIncPtr");
	auto preDecPtr = ctx.getFunctionPtr!(int*, int*)("preDecPtr");
	auto postDecPtr = ctx.getFunctionPtr!(int*, int*)("postDecPtr");

	int[4] arr;
	assert(preIncPtr(&arr[1]) == &arr[2]);
	assert(postIncPtr(&arr[1]) == &arr[1]);
	assert(preDecPtr(&arr[1]) == &arr[0]);
	assert(postDecPtr(&arr[1]) == &arr[1]);
}


@TestInfo(&tester45)
immutable test45 = q{--- test45
	void incArray(i32* arr, i32 length) {
		i32 i = 0;
		while(i < length) {
			++arr[i++];
		}
	}
};
void tester45(ref TestContext ctx) {
	int[4] arr = [1, 2, 3, 4];
	auto incArray = ctx.getFunctionPtr!(void, int*, int)("incArray");
	incArray(arr.ptr, arr.length);
	assert(arr == [2, 3, 4, 5]);
}


@TestInfo(&tester46)
immutable test46 = q{--- test46
	void incArray(i32* begin, i32* end) {
		while(begin < end) {
			++(*begin++);
		}
	}
};
void tester46(ref TestContext ctx) {
	int[4] arr = [1, 2, 3, 4];
	auto incArray = ctx.getFunctionPtr!(void, int*, int*)("incArray");
	incArray(arr.ptr, arr.ptr + arr.length);
	assert(arr == [2, 3, 4, 5]);
}


@TestInfo(&tester47)
immutable test47 = q{--- test47
	// test logical not, or, and
	i32 selectNot(i32 selector, i32 a, i32 b) {
		if (!selector)
			return a;
		else return b;
	}
	i32 selectOr(i32 selectorA, i32 selectorB, i32 a, i32 b) {
		if (selectorA || selectorB)
			return a;
		else return b;
	}
	i32 selectAnd(i32 selectorA, i32 selectorB, i32 a, i32 b) {
		if (selectorA && selectorB)
			return a;
		else return b;
	}
};
void tester47(ref TestContext ctx) {
	auto selectNot = ctx.getFunctionPtr!(int, int, int, int)("selectNot");
	assert(selectNot(0, 1, 2) == 1);
	assert(selectNot(1, 1, 2) == 2);
	auto selectOr = ctx.getFunctionPtr!(int, int, int, int, int)("selectOr");
	assert(selectOr(0, 0, 1, 0) == 0);
	assert(selectOr(0, 1, 1, 0) == 1);
	assert(selectOr(1, 0, 1, 0) == 1);
	assert(selectOr(1, 1, 1, 0) == 1);
	auto selectAnd = ctx.getFunctionPtr!(int, int, int, int, int)("selectAnd");
	assert(selectAnd(0, 0, 1, 0) == 0);
	assert(selectAnd(0, 1, 1, 0) == 0);
	assert(selectAnd(1, 0, 1, 0) == 0);
	assert(selectAnd(1, 1, 1, 0) == 1);
}


@TestInfo(&tester48)
immutable test48 = q{--- test48
	// bool values
	bool getFalse() { return false; }
	bool getTrue() { return true; }
	bool getBool(i32 selectorA) {
		return selectorA;
	}
	bool getNot(i32 selectorA) {
		return !selectorA;
	}
	bool getOr(i32 selectorA, i32 selectorB) {
		return selectorA || selectorB;
	}
	bool getAnd(i32 selectorA, i32 selectorB) {
		return selectorA && selectorB;
	}
};
void tester48(ref TestContext ctx) {
	auto getFalse = ctx.getFunctionPtr!(bool)("getFalse");
	assert(getFalse() == false);
	auto getTrue = ctx.getFunctionPtr!(bool)("getTrue");
	assert(getTrue() == true);
	auto getBool = ctx.getFunctionPtr!(bool, int)("getBool");
	assert(getBool(0) == false);
	assert(getBool(1) == true);
	auto getNot = ctx.getFunctionPtr!(bool, int)("getNot");
	assert(getNot(0) == true);
	assert(getNot(1) == false);
	auto getOr = ctx.getFunctionPtr!(bool, int, int)("getOr");
	assert(getOr(0, 0) == false);
	assert(getOr(0, 1) == true);
	assert(getOr(1, 0) == true);
	assert(getOr(1, 1) == true);
	auto getAnd = ctx.getFunctionPtr!(bool, int, int)("getAnd");
	assert(getAnd(0, 0) == false);
	assert(getAnd(0, 1) == false);
	assert(getAnd(1, 0) == false);
	assert(getAnd(1, 1) == true);
}


@TestInfo(&tester49)
immutable test49 = q{--- test49
	// Test full suite of pointer arithmetic operations
	void preIncrement(i32* arr, i32 length) {
		(++arr)[-1] = 10; // arr[0] = 10, negative index
		*arr        = 15; // arr[1] = 15, deref pointer
	}
	void preDecrement(i32* arr, i32 length) {
		(--arr)[1] = 20; // arr[0] = 20
		arr[2]  = 25; // arr[1] = 25
	}
	void postIncrement(i32* arr, i32 length) {
		(arr++)[0] = 30; // arr[0] = 30
		arr[0]  = 35; // arr[1] = 35
	}
	void postDecrement(i32* arr, i32 length) {
		(arr--)[0] = 40; // arr[0] = 40
		arr[2]  = 45; // arr[1] = 45
	}
	void addInt(i32* arr, i32 length) {
		*(arr + 3) = 50; // arr[3] = 50
	}
	void subInt(i32* arr, i32 length) {
		*(arr - 3) = 60; // arr[-4] = 60
	}
	void diff(i32* arr, i32 length) {
		i32* last = arr + length; // last = &arr[4];
		// sub two pointers
		i64 diff = last - arr;
		arr[length - 3] = cast(i32)diff; // ar[1] = 4
	}
	void plusEqual(i32* arr, i32 length) {
		arr += 3;
		arr[0] = 90; // arr[3] = 90
	}
	void minusEqual(i32* arr, i32 length) {
		arr -= 3;
		arr[3] = 100; // arr[0] = 100
	}
};
void tester49(ref TestContext ctx) {
	int[2] arr;
	auto preIncrement = ctx.getFunctionPtr!(void, int*, int)("preIncrement");
	preIncrement(arr.ptr, arr.length);
	assert(arr == [10, 15]);

	auto preDecrement = ctx.getFunctionPtr!(void, int*, int)("preDecrement");
	preDecrement(arr.ptr, arr.length);
	assert(arr == [20, 25]);

	auto postIncrement = ctx.getFunctionPtr!(void, int*, int)("postIncrement");
	postIncrement(arr.ptr, arr.length);
	assert(arr == [30, 35]);

	auto postDecrement = ctx.getFunctionPtr!(void, int*, int)("postDecrement");
	postDecrement(arr.ptr, arr.length);
	assert(arr == [40, 45]);

	int[4] arr2;

	auto addInt = ctx.getFunctionPtr!(void, int*, int)("addInt");
	addInt(arr2.ptr, arr2.length);
	assert(arr2 == [0, 0, 0, 50]);

	auto subInt = ctx.getFunctionPtr!(void, int*, int)("subInt");
	subInt(arr2.ptr+3, arr2.length);
	assert(arr2 == [60, 0, 0, 50]);

	auto diff = ctx.getFunctionPtr!(void, int*, int)("diff");
	diff(arr2.ptr, arr2.length);
	assert(arr2 == [60, 4, 0, 50]);

	auto plusEqual = ctx.getFunctionPtr!(void, int*, int)("plusEqual");
	plusEqual(arr2.ptr, arr2.length);
	assert(arr2 == [60, 4, 0, 90]);

	auto minusEqual = ctx.getFunctionPtr!(void, int*, int)("minusEqual");
	minusEqual(arr2.ptr, arr2.length);
	assert(arr2 == [100, 4, 0, 90]);
}

@TestInfo(&tester50)
immutable test50 = q{--- test50
	// test ptr cmp with null
	// test unary branch on byte value
	u64 cstrlen(u8* str) {
		if (str == null) return 0;

		u8* start = str;
		while(*str)
		{
			++str;
		}
		return cast(u64)(str - start);
	}
};
void tester50(ref TestContext ctx) {
	auto cstrlen = ctx.getFunctionPtr!(ulong, const(char)*)("cstrlen");
	string str = "test";
	ulong length = cstrlen(str.ptr);
	assert(length == str.length);
}

@TestInfo(&tester51)
immutable test51 = q{--- test51
	// test null assign, compare
	u8* assignNull() {
		u8* ptr = null;
		return ptr;
	}
	bool testNull(u8* ptr) {
		return ptr == null;
	}
};
void tester51(ref TestContext ctx) {
	auto assignNull = ctx.getFunctionPtr!(ubyte*)("assignNull");
	ubyte* val = assignNull();
	assert(val == null);

	auto testNull = ctx.getFunctionPtr!(bool, ubyte*)("testNull");
	bool resNull = testNull(val);
	assert(resNull == true);

	ubyte value;
	bool resNotNull = testNull(&value);
	assert(resNotNull == false);
}

@TestInfo(&tester52)
immutable test52 = q{--- test52
	// test integer comparison
	bool cmp8(i8 a, i8 b) { return a == b; }
	bool cmp16(i16 a, i16 b) { return a == b; }
	bool cmp32(i32 a, i32 b) { return a == b; }
	bool cmp64(i64 a, i64 b) { return a == b; }

	bool br8(i8 a, i8 b) { bool result; if (a == b) result = true; else result = false; return result; }
	bool br16(i16 a, i16 b) { bool result; if (a == b) result = true; else result = false; return result; }
	bool br32(i32 a, i32 b) { bool result; if (a == b) result = true; else result = false; return result; }
	bool br64(i64 a, i64 b) { bool result; if (a == b) result = true; else result = false; return result; }
};
void tester52(ref TestContext ctx) {
	// pass 64bit values, to check that only lower bits are compared
	void test(string funcName, ulong a, ulong b)
	{
		auto cmp = ctx.getFunctionPtr!(bool, ulong, ulong)(funcName);
		bool res = cmp(a, b);
		assert(res == true);
	}
	test("cmp8", 0xF0_F0_F0_F0_F0_F0_F0_FF, 0x0F_0F_0F_0F_0F_0F_0F_FF);
	test("br8", 0xF0_F0_F0_F0_F0_F0_F0_FF, 0x0F_0F_0F_0F_0F_0F_0F_FF);

	test("cmp16", 0xF0_F0_F0_F0_F0_F0_FF_FF, 0x0F_0F_0F_0F_0F_0F_FF_FF);
	test("br16", 0xF0_F0_F0_F0_F0_F0_FF_FF, 0x0F_0F_0F_0F_0F_0F_FF_FF);

	test("cmp32", 0xF0_F0_F0_F0_FF_FF_FF_FF, 0x0F_0F_0F_0F_FF_FF_FF_FF);
	test("br32", 0xF0_F0_F0_F0_FF_FF_FF_FF, 0x0F_0F_0F_0F_FF_FF_FF_FF);

	test("cmp64", 0xFF_FF_FF_FF_FF_FF_FF_FF, 0xFF_FF_FF_FF_FF_FF_FF_FF);
	test("br64", 0xFF_FF_FF_FF_FF_FF_FF_FF, 0xFF_FF_FF_FF_FF_FF_FF_FF);
}

@TestInfo(&tester53)
immutable test53 = q{--- test53
	// Test constant folding
	i32 add() { return 1 + 3; }
	i32 sub() { return 3 - 1; }
	i32 mul() { return 3 * 2; }
	i32 div() { return 7 / 2; }
	i32 rem() { return 7 % 2; }
	i32 shl() { return 1 << 2; }
	i32 shr() { return i32.min >>> 2; }
	i32 sar() { return i32.min >> 2; }
	i32 or () { return 0b0011 | 0b0101; }
	i32 xor() { return 0b0011 ^ 0b0101; }
	i32 and() { return 0b0011 & 0b0101; }
};
void tester53(ref TestContext ctx) {
	auto add = ctx.getFunctionPtr!(int)("add");
	auto sub = ctx.getFunctionPtr!(int)("sub");
	auto mul = ctx.getFunctionPtr!(int)("mul");
	auto div = ctx.getFunctionPtr!(int)("div");
	auto rem = ctx.getFunctionPtr!(int)("rem");
	auto shl = ctx.getFunctionPtr!(int)("shl");
	auto shr = ctx.getFunctionPtr!(int)("shr");
	auto sar = ctx.getFunctionPtr!(int)("sar");
	auto or  = ctx.getFunctionPtr!(int)("or");
	auto xor = ctx.getFunctionPtr!(int)("xor");
	auto and = ctx.getFunctionPtr!(int)("and");

	assert(add() == 4);
	assert(sub() == 2);
	assert(mul() == 6);
	assert(div() == 3);
	assert(rem() == 1);
	assert(shl() == 4);
	assert(shr() == (int.min >>> 2));
	assert(sar() == (int.min >> 2));
	assert(or() == 0b0111);
	assert(xor() == 0b0110);
	assert(and() == 0b0001);
}

@TestInfo()
immutable test54 = q{--- test54
	// Test bool literal branching
	void run() {
		while(true) {
			break;
		}
		while(false) {

		}
		while(true) {

		}
		if (true){}
		if (false){}
	}
};


@TestInfo(&tester55, [HostSymbol("print", cast(void*)&external_print_string)])
immutable test55 = q{--- test55
	// test slice forwarding
	@extern(module, "host")
	void print(u8[] str);
	void forward(u8[] str) {
		print(str);
	}
};
void tester55(ref TestContext ctx) {
	auto forward = ctx.getFunctionPtr!(void, Slice!(immutable(char)))("forward");
	forward(Slice!(immutable(char))("testString"));
	assert(testSink.text == "testString");
}


@TestInfo()
immutable test56 = q{--- test56
	// test using bool variable for branching
	void branch() {
		bool run = true;
		while (run) {}
	}
};

@TestInfo()
immutable test57 = q{--- test57
	// test using bool variable for branching
	void use(bool){}
	void branch() {
		bool run = true;
		while (run) {}
		use(run);
	}
};

@TestInfo()
immutable test58 = q{--- test58
	// Test declaration statement that declares pointer var
	struct SDL_Event {
		u32 type;
	}
	void run() {
		SDL_Event* key;
	}
};

@TestInfo()
immutable test59 = q{--- test59
	// Test dot operator on dereference and member expressions
	struct SDL_KeyboardEvent {
		u32 type;
		u32 timestamp;
		SDL_Keysym keysym;
	}
	struct SDL_Keysym {
		u32 scancode;
		u32 sym;
	}
	void run(SDL_KeyboardEvent* key) {
		if ((*key).keysym.scancode == 1) {
			(*key).keysym.sym = 42;
		}
	}
};

@TestInfo()
immutable test60 = q{--- test60
	// Test dot operator on pointer type
	struct SDL_KeyboardEvent {
		u32 type;
		u32 timestamp;
		SDL_Keysym keysym;
	}
	struct SDL_Keysym {
		u32 scancode;
		u32 sym;
	}
	void run(SDL_KeyboardEvent* key) {
		if (key.keysym.scancode == 1) {
			key.keysym.sym = 42;
		}
	}
};

@TestInfo()
immutable test61 = q{--- test61
	// Test pointer to pointer cast
	struct SDL_Event {
		u32 type;
		u8[52] padding;
	}
	struct SDL_KeyboardEvent {
		u32 type;
		u32 timestamp;
		SDL_Keysym keysym;
	}
	struct SDL_Keysym {
		u32 scancode;
		u32 sym;
	}
	void run(SDL_KeyboardEvent* key) {
		SDL_Event e;
		SDL_KeyboardEvent* key = cast(SDL_KeyboardEvent*)&e;
	}
};

@TestInfo()
immutable test62 = q{--- test62
	// Bug. Infinite loop in trivial phi removal
	struct SDL_Event {
		u32 type;
		u8[52] padding;
	}
	struct SDL_KeyboardEvent {
		u32 type;
		u32 timestamp;
		u32 keysym;
	}
	i32 SDL_PollEvent(SDL_Event*){return 0;}
	i32 player_x;
	void run(SDL_KeyboardEvent* key) {
		bool run = true;
		SDL_Event e;

		while (run)
		{
			if (e.type == 1)
			{
				SDL_KeyboardEvent* key = cast(SDL_KeyboardEvent*)&e;
				if ((*key).keysym == 1)
				{
					++player_x;
				}
			}
		}
	}
};

@TestInfo()
immutable test63 = q{--- test63
	// Store const into stack
	void usePtru8(u8*){}
	void usePtru16(u16*){}
	void usePtru32(u32*){}
	void usePtru64(u64*){}
	void run() {
		u8 num8 = 10;
		u16 num16 = 10;
		u32 num32 = 10;
		u64 num64 = 10;
		usePtru8(&num8);
		usePtru16(&num16);
		usePtru32(&num32);
		usePtru64(&num64);
	}
};


@TestInfo(&tester65, [HostSymbol("print", cast(void*)&external_print_string)])
immutable test65 = q{--- test65
	// Test string literals
	@extern(module, "host")
	void print(u8[]); // external
	u8[] returnStringLiteral() {
		return "testString";
	}
	void passStringLiteralArgument() {
		print("testString");
	}
};
void tester65(ref TestContext ctx) {
	auto returnStringLiteral = ctx.getFunctionPtr!(Slice!char)("returnStringLiteral");
	assert(returnStringLiteral() == "testString");

	auto passStringLiteralArgument = ctx.getFunctionPtr!(void)("passStringLiteralArgument");
	passStringLiteralArgument();
	assert(testSink.text == "testString");
}

@TestInfo(&tester66)
immutable test66 = q{--- test66
	// Test char literals
	u8 getChar() { return '\n'; }
};
void tester66(ref TestContext ctx) {
	auto getChar = ctx.getFunctionPtr!(char)("getChar");
	assert(getChar() == '\n');
}

@TestInfo(&tester67)
immutable test67 = q{--- test67
	// Test static arrays
	u8* getPtr(u8[21]* arrPtr) { return (*arrPtr).ptr; }
	u64 getLength(u8[21]* arrPtr) { return (*arrPtr).length; }
	u8 getElement(u8[21]* arrPtr, i64 index) { return (*arrPtr)[index]; }
	void arrayToSlice(u8[21]* arrPtr) { receiveSlice(*arrPtr); }
	void receiveSlice(u8[] arr) {}
	struct Color {
		u8 r;
		u8 g;
		u8 b;
		u8 a;
	}
	Color[2] colors;
	void fun() {
		Color[2] colors_static_local;
		Color[] colors_local;
	}
};
void tester67(ref TestContext ctx) {
	ubyte[21] array;
	foreach(i, ref elem; array) elem = cast(ubyte)i;

	auto getPtr = ctx.getFunctionPtr!(ubyte*, ubyte[21]*)("getPtr");
	assert(getPtr(&array) == array.ptr);

	auto getLength = ctx.getFunctionPtr!(ulong, ubyte[21]*)("getLength");
	assert(getLength(&array) == array.length);

	auto getElement = ctx.getFunctionPtr!(ubyte, ubyte[21]*, ulong)("getElement");
	foreach(i, elem; array)
		assert(getElement(&array, i) == elem);

	auto arrayToSlice = ctx.getFunctionPtr!(void, ubyte[21]*)("arrayToSlice");
	arrayToSlice(&array);
}

@TestInfo()
immutable test68 = q{--- test68
	// Test enum inside struct
	struct Struct {
		u8 a;
		enum someVal = 42;
	}
	void run() {
		i64 var = Struct.someVal;
		Struct s;
		i64 var2 = s.someVal;
	}
};

@TestInfo()
immutable test69 = q{--- test69
	// Bug. Multiple users of trivial phis do not get
	// all users added when replacing phi result by singular phi argument
	struct GameMap {
		bool blocked;
		bool block_sight;
	}
	void initialize_tiles(GameMap* map)
	{
		i32 x = 0;
		while(x < 20) {
			map.blocked = false;
			map.block_sight = false;
		}
	}
};

@TestInfo()
immutable test70 = q{--- test70
	// usage of member expr in condition
	struct GameMap {
		bool blocked;
		bool block_sight;
	}
	void initialize_tiles(GameMap* map)
	{
		if (map.blocked) {
			map.block_sight = true;
		}
	}
};

@TestInfo()
immutable test71 = q{--- test71
	// Take address of array index
	u8* getFirst(u8[2]* array)
	{
		return &(*array)[0];
	}
};

@TestInfo()
immutable test72 = q{--- test72
	// Use call result as condition
	i32 run()
	{
		i32 val;
		if (getBool()) {
			val = 42;
		} else {
			val = 21;
		}
		return val;
	}
	bool getBool() { return true; }
};

@TestInfo()
immutable test73 = q{--- test73
	// General instruction const handling
	void run()
	{
		i8 val8;
		i16 val16;
		i32 val32;
		i64 val64;
		val8 += 8;
		val16 += 8;
		val16 += 500;
		val32 += 8;
		val32 += 500;
		val32 += 70000;
		val64 += 8;
		val64 += 500;
		val64 += 70000;
		val64 += 0x8_0000_0000;
	}
};


@TestInfo(&tester74)
immutable test74 = q{--- test74
	// Test nested call expression
	struct GameMap;
	GameMap* pass_ptr_and_struct(GameMap* map) {
		return create_room(map, Rect(1,1,5,5));
	}

	struct Rect {
		i32 x1;
		i32 y1;
		i32 x2;
		i32 y2;
	}

	GameMap* create_room(GameMap* map, Rect room) {
		return map;
	}
};
void tester74(ref TestContext ctx) {
	ubyte b;
	auto pass_ptr_and_struct = ctx.getFunctionPtr!(ubyte*, ubyte*)("pass_ptr_and_struct");
	assert(pass_ptr_and_struct(&b) == &b);
}


@TestInfo(&tester75)
immutable test75 = q{--- test75
	// Test for loop
	i32 sum(i32[] numbers) {
		i32 sum = 0;
		for (u32 i = 0; i < numbers.length; ++i)
		{
			sum += numbers[i];
		}
		return sum;
	}
	void loops1() {
		i32 outer1;
		i32 outer2;
		for (;;) break;
		for (i32 i;;) break;
		for (i32 i, i32 j, i32 k = 8;;) break;
		for (;outer1 == 0;) break;
		for (;;outer1 = 0, outer2 = 10) break;
	}
};
void tester75(ref TestContext ctx) {
	int[10] nums = [1,2,3,4,5,6,7,8,9,10];
	auto sum = ctx.getFunctionPtr!(int, Slice!int)("sum");
	assert(sum(Slice!int(nums[])) == 55);
}


@TestInfo(&tester76)
immutable test76 = q{--- test76
	// Test for loop break avoiding increments block
	i32 test_for() {
		i32 outer;
		for (;;outer = 10) break;
		return outer; // must return 0, not 10
	}
};
void tester76(ref TestContext ctx) {
	auto test_for = ctx.getFunctionPtr!(int)("test_for");
	assert(test_for() == 0);
}

@TestInfo()
immutable test77 = q{--- test77
	//
	struct Tile {
		bool blocked;
		bool block_sight;
	}

	struct GameMap {
		enum map_width = 40;
		enum map_height = 40;
		Tile[40][40] tiles;
	}

	void initialize_tiles(GameMap* map) {
		for (i32 y = 0; y < map.map_height; ++y) {
			for (i32 x = 0; x < map.map_width; ++x) {
				map.tiles[y][x].blocked = true;
				map.tiles[y][x].block_sight = true;
			}
		}
	}
};

@TestInfo(&tester78)
immutable test78 = q{--- test78
	// Bug: division by small constant generated BYTE idiv, instead of DWORD
	i64 test_div(i64 i, u8[] res) {
		res[0] = cast(u8)(i % 10);
		i /= 10;
		res[1] = cast(u8)(i % 10);
		return i;
	}
};
void tester78(ref TestContext ctx) {
	auto test_div = ctx.getFunctionPtr!(long, long, Slice!ubyte)("test_div");
	ubyte[2] buf;
	assert(test_div(97, Slice!ubyte(buf[])) == 9);
	assert(buf[] == [7, 9]);
}

@TestInfo(&tester79)
immutable test79 = q{--- test79
	// test null to slice conversion
	void receive(u8[]) {}
	u8[] make() {
		receive(null);
		return null;
	}
};
void tester79(ref TestContext ctx) {
	auto make = ctx.getFunctionPtr!(Slice!ubyte)("make");
	assert(make() == null);
}

@TestInfo(&tester80)
immutable test80 = q{--- test80
	// integer types .min, .max properties
	u8 u8min() {  return u8.min; }
	u16 u16min() { return u16.min; }
	u32 u32min() { return u32.min; }
	u64 u64min() { return u64.min; }
	u8 u8max() {  return u8.max; }
	u16 u16max() { return u16.max; }
	u32 u32max() { return u32.max; }
	u64 u64max() { return u64.max; }
	i8 i8min() {  return i8.min; }
	i16 i16min() { return i16.min; }
	i32 i32min() { return i32.min; }
	i64 i64min() { return i64.min; }
	i8 i8max() {  return i8.max; }
	i16 i16max() { return i16.max; }
	i32 i32max() { return i32.max; }
	i64 i64max() { return i64.max; }
};
void tester80(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ubyte)("u8min")()  == ubyte.min);
	assert(ctx.getFunctionPtr!(ushort)("u16min")() == ushort.min);
	assert(ctx.getFunctionPtr!(uint)("u32min")() == uint.min);
	assert(ctx.getFunctionPtr!(ulong)("u64min")() == ulong.min);
	assert(ctx.getFunctionPtr!(ubyte)("u8max")()  == ubyte.max);
	assert(ctx.getFunctionPtr!(ushort)("u16max")() == ushort.max);
	assert(ctx.getFunctionPtr!(uint)("u32max")() == uint.max);
	assert(ctx.getFunctionPtr!(ulong)("u64max")() == ulong.max);
	assert(ctx.getFunctionPtr!(byte)("i8min")()  == byte.min);
	assert(ctx.getFunctionPtr!(short)("i16min")() == short.min);
	assert(ctx.getFunctionPtr!(int)("i32min")() == int.min);
	assert(ctx.getFunctionPtr!(long)("i64min")() == long.min);
	assert(ctx.getFunctionPtr!(byte)("i8max")()  == byte.max);
	assert(ctx.getFunctionPtr!(short)("i16max")() == short.max);
	assert(ctx.getFunctionPtr!(int)("i32max")() == int.max);
	assert(ctx.getFunctionPtr!(long)("i64max")() == long.max);
}


@TestInfo(&tester81)
immutable test81 = q{--- test81
	// Test mov of 64bit constant into memory
	void run(i64* ptr) { *ptr = i64.max; }
};
void tester81(ref TestContext ctx) {
	long val = 0;
	ctx.getFunctionPtr!(void, long*)("run")(&val);
	assert(val == long.max);
}


@TestInfo()
immutable test82 = q{--- test82
	// Bug
	void test()
	{
		for (u32 roomIndex = 0; roomIndex < 20; ++roomIndex)
		{
			for (u32 otherRoom = 0; otherRoom < roomIndex; ++otherRoom)
			{
				if (otherRoom < roomIndex) {
					break;
				}
			}
		}
	}
};

@TestInfo(&tester83)
immutable test83 = q{--- test83
	// using enum as array size
	// constant folding in static expressions
	// test forward references too

	// use static array length in constant folded expression
	enum combined_enums = cast(u64)arr_size3 + arrBackref.length;
	i32[combined_enums] arrBackrefX;
	u64 combinedEnums() { return arrBackrefX.length; } // 16

	enum arr_size = 4;
	i32[arr_size] arrBackref;
	u64 getSize1() { return arrBackref.length; } // 4

	u64 testBackref() {
		i32[arr_size] local_arr;
		return local_arr.length; // 4
	}

	i32[arr_size2] arrForwardref;
	u64 getSize2() { return arrForwardref.length; } // 7

	u64 testForwardref() {
		i32[arr_size2] local_arr;
		return local_arr.length; // 7
	}

	enum arr_size2 = 7;

	i32[arr_size3 + 4] arrForwardref2;
	u64 getSize3() { return arrForwardref2.length; } // 16

	u64 testForwardref2() {
		i32[arr_size3 + 10] local_arr;
		return local_arr.length; // 22
	}

	enum arr_size3 = 12;
};
void tester83(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!ulong("combinedEnums")() == 16);
	assert(ctx.getFunctionPtr!ulong("getSize1")() == 4);
	assert(ctx.getFunctionPtr!ulong("testBackref")() == 4);
	assert(ctx.getFunctionPtr!ulong("getSize2")() == 7);
	assert(ctx.getFunctionPtr!ulong("testForwardref")() == 7);
	assert(ctx.getFunctionPtr!ulong("getSize3")() == 16);
	assert(ctx.getFunctionPtr!ulong("testForwardref2")() == 22);
}

@TestInfo(&tester84)
immutable test84 = q{--- test84
	// check nested array length is set correctly
	i32[40][30] arr;
	u64 nestedArrLen() { return arr[0].length; } // 40
};
void tester84(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!ulong("nestedArrLen")() == 40);
}

@TestInfo(&tester85)
immutable test85 = q{--- test85
	// UFCS without parenthesis
	i32 static_func0() { return 42; }
	void static_func_void() {}
	i32 static_func1(i32 num) { return num; }
	i32 static_func2(i32 num, i32 num2) { return num + num2; }
	i32 test0() {
		static_func_void;
		return static_func0;
	}
	i32 test1(i32 num) {
		return num.static_func1;
	}
	i32 test2(i32 num, i32 num2) {
		return num.static_func2(num2);
	}
};
void tester85(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("test0")() == 42);
	assert(ctx.getFunctionPtr!(int, int)("test1")(42) == 42);
	assert(ctx.getFunctionPtr!(int, int, int)("test2")(42, 24) == 66);
}

@TestInfo()
immutable test86 = q{--- test86
	// alias type
	alias T = i32;
	T num = 2;
	T test(T t) { return t; }

	struct S { i32 member; }
	alias U = S;
	void test2() {
		U s;
		s.member = 10;
	}

	alias funcAlias = test; // function alias
	T test3() { return funcAlias(30); }

	alias varAlias = num; // variable alias
	T test4() { return varAlias; }

	alias aliasOfAlias = varAlias; // alias of alias
	T test5() { return aliasOfAlias; }

	enum enumVal = 42;
	alias enumAlias = enumVal; // enum alias
	T test6() { return enumAlias; }

	alias Ptr = i32*;
	alias HANDLE = void*;
	alias alias_bool = bool;
	enum ptrsize = i32*.sizeof;
	enum u8min = u8.max;
	u8[u8min] arr;
	u8[ptrsize] arr2;
};

@TestInfo(&tester87)
immutable test87 = q{--- test87
	// pointer type expr parsing
	enum ptrsize = i32*.sizeof;
	enum ptrsize2 = i32**.sizeof;
	enum arrsize = i32[4].sizeof;
	struct S { i64 var; } // 8
	enum S_ptrsize = S*.sizeof;
	enum S_ptrsize2 = S**.sizeof;
	enum S_arrsize = S[4].sizeof; // 32
	S[S_arrsize] arr; // 256
	u64 S_sizeof() { return S.sizeof; } // 8
	u64 arr_sizeof() { return arr.sizeof; } // 256
};

void tester87(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("S_sizeof")() == 8);
	assert(ctx.getFunctionPtr!(ulong)("arr_sizeof")() == 256);
}

@TestInfo(&tester88)
immutable test88 = q{--- test88
	// function pointer
	alias i32_funType = i32 function();
	alias i32ptr_funType = i32* function();
	enum i32_funType funPtrEnum = &i32Fun;
	enum i32 function() inlinefunPtrEnum = &i32Fun;
	// i32_funType funPtrGlobal = &i32Fun; // TODO: globals are uninitialized now
	// i32 function() inlinefunPtrGlobal = &i32Fun; // TODO: globals are uninitialized now
	i32 i32Fun() {
		return 42;
	}
	i32_funType retFuncPtr() {
		return &i32Fun;
	}
	i32 test() {
		i32_funType funPtr = retFuncPtr(); // call by ptr (may be optimized to direct call later)
		i32 function() inlinefunPtr = &i32Fun; // direct call
		return funPtr() + inlinefunPtr();
	}
	i32 test2() {
		return funPtrEnum() + inlinefunPtrEnum(); // direct call is performed
	}
	i32 sum(i32 a, i32 b) { return a + b; }
	// pass ptr to function with parameters
	i32 callFunc(i32 function(i32, i32) func, i32 a, i32 b) {
		return func(a, b);
	}
	i32 test3() {
		return callFunc(&sum, 10, 40);
	}
	i32 callFunc2(i32 a1, i32 a2, i32 a3, i32 a4, i32 a5, i32 a6, i32 function(i32, i32) func) {
		return func(a1, a2);
	}
	// test push function address
	i32 test4() {
		return callFunc2(1, 2, 3, 4, 5, 6, &sum);
	}
};
void tester88(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("test")() == 84);
	assert(ctx.getFunctionPtr!(int)("test2")() == 84);
	assert(ctx.getFunctionPtr!(int)("test3")() == 50);
	assert(ctx.getFunctionPtr!(int)("test4")() == 3);
}

@TestInfo()
immutable test89 = q{--- test89
	// function pointer as parameter
	void tran_thong(i32 xstart, i32 ystart, i32 xend, i32 yend, void function(void*, i32, i32) callback)
	{}
};

@TestInfo(&tester90)
immutable test90 = q{--- test90
	void setRangeu8(u8* slice, u64 from, u64 to, u8 value) {
		while(from < to) {
			slice[from] = value;
			++from;
		}
	}
	void formatInt(i64 i, u8[21]* output, u32 minSize, u8[]* result)
	{
		u32 numDigits = 0;
		if (i == 0)
		{
			if (minSize == 0)
				minSize = 1;
			setRangeu8((*output).ptr, 21 - minSize, 21, '0');
			(*output)[20] = '0';
			numDigits = minSize;
		}
		else
		{
			bool neg = i < 0;
			if (neg) {
				i = -i;
			}
			bool overflow = i < 0;
			if (overflow)
				i = i64.max;

			while (i)
			{
				u8 c = cast(u8)('0' + i % 10);
				(*output)[21 - ++numDigits] = c;
				i /= 10;
			}

			while (numDigits < minSize) {
				(*output)[21 - ++numDigits] = '0';
			}

			if (neg) {
				(*output)[21 - ++numDigits] = '-';
			}
			if (overflow) {
				++(*output)[20];
			}
		}
		(*result).ptr = (*output).ptr + (21 - numDigits);
		(*result).length = numDigits;
	}
};
void tester90(ref TestContext ctx) {
	auto setRangeu8 = ctx.getFunctionPtr!(void, char*, ulong, ulong, char)("setRangeu8");

	char[23] buf;
	buf[] = 'z';

	setRangeu8(&buf[1], 0, 21, 'x');
	setRangeu8(&buf[1], 0, 1, '0');
	setRangeu8(&buf[1], 20, 21, '1');
	assert(buf == "z0xxxxxxxxxxxxxxxxxxx1z");

	auto formatInt = ctx.getFunctionPtr!(void, long, char*, uint, Slice!char*)("formatInt");

	void testFormatInt(long num, string expectedBuf, string expectedResult) {
		buf[] = 'x';
		Slice!char result;
		formatInt(num, &buf[1], 0, &result);
		//writefln("buf ptr %s len %s ptr %s %s", buf.ptr, result.length, result.ptr, buf);
		assert(buf == expectedBuf);
		assert(result.slice == expectedResult);
	}

	testFormatInt(  0,      "xxxxxxxxxxxxxxxxxxxxx0x", "0");
	testFormatInt(  1,      "xxxxxxxxxxxxxxxxxxxxx1x", "1");
	testFormatInt( 10,      "xxxxxxxxxxxxxxxxxxxx10x", "10");
	testFormatInt(-10,      "xxxxxxxxxxxxxxxxxxx-10x", "-10");
	testFormatInt(long.min, "xx-9223372036854775808x", "-9223372036854775808");
	testFormatInt(long.max, "xxx9223372036854775807x", "9223372036854775807");
}


@TestInfo(&tester91)
immutable test91 = q{--- test91
	// splitting and spilling
	void tran_thong(i32 xstart, i32 ystart, i32 xend, i32 yend, void function(void*, i32, i32) callback, void* userData)
	{
		i32 x = xstart;
		i32 y = ystart;

		i32 deltax;
		i32 signdx;
		if (xend >= xstart) {
			deltax = xend - xstart;
			signdx = 1;
		} else {
			deltax = xstart - xend;
			signdx = -1;
		}

		i32 deltay;
		i32 signdy;
		if (yend >= ystart) {
			deltay = yend - ystart;
			signdy = 1;
		} else {
			deltay = ystart - yend;
			signdy = -1;
		}

		callback(userData, x, y);

		i32 test;
		if (signdy == -1)
			test = -1;
		else
			test = 0;

		if (deltax >= deltay) {
			test = (deltax + test) >> 1;
			for (i32 i = 1; i < deltax; ++i) {
				test -= deltay;
				x += signdx;
				if (test < 0) {
					y += signdy;
					test += deltax;
				}
				callback(userData, x, y);
			}
		} else {
			test = (deltay + test) >> 1;
			for (i32 i = 1; i < deltay; ++i) {
				test -= deltax;
				y += signdy;
				if (test < 0) {
					x += signdx;
					test += deltay;
				}
				callback(userData, x, y);
			}
		}
		callback(userData, xend, yend);
	}
};
void tester91(ref TestContext ctx) {
	static struct test91_user_data {
		string text;
		size_t i;
	}
	static extern(C) void external_print_coords_func(void* userData, int x, int y) {
		auto data = cast(test91_user_data*)userData;
		if (data.i > 0) testSink.put(", ");
		formattedWrite(testSink, "(%s %s %s)", data.text, x, y);
		++data.i;
	}
	alias func_T = extern(C) void function(void*, int, int);
	auto tran_thong = ctx.getFunctionPtr!(void, int, int, int, int, func_T, void*)("tran_thong");

	test91_user_data data = {
		text : "hi"
	};

	tran_thong(0, 0, 2, 2, &external_print_coords_func, &data);

	//writefln("%s", testSink.text);
	assert(testSink.text == "(hi 0 0), (hi 1 1), (hi 2 2)");
}

@TestInfo(&tester92)
immutable test92 = q{--- test92
	// struct methods
	struct Struct {
		i32 member;
		void set42() {
			this.member += 42;
			member = 42;
		}
		void set(i32 par) {
			member = par;
			this.member = par;
		}
		void set2() {
			set42(); // pass this
		}
		void set3() {
			set42; // pass this
		}
		void set4() {
			this.set42; // explicit this
		}
		void set5() {
			this.set42(); // explicit this
		}
		void set6() {
			set(42); // pass this
		}
	}
	i32 testMethod1() {
		Struct s;
		s.set42(); // take address
		return s.member;
	}
	i32 testMethod2(i32 val) {
		Struct s;
		s.set(val); // take address
		return s.member;
	}
	i32 testMethod3() {
		Struct s;
		s.set42; // take address
		return s.member;
	}
	i32 testMethod4() {
		Struct s;
		Struct* sptr = &s;
		sptr.set42; // call method on pointer
		return sptr.member;
	}
	i32 testMethod5() {
		Struct[1] s;
		s[0].set42; // call method on index expr
		s[0].set42();
		s[0].set(42);
		return s[0].member;
	}
	i32 testMethodToMethod2() {
		Struct s = Struct(10); // reset stack value for future tests
		s.set2;
		return s.member;
	}
	i32 testMethodToMethod3() {
		Struct s = Struct(10); // reset stack value for future tests
		s.set3;
		return s.member;
	}
	i32 testMethodToMethod4() {
		Struct s = Struct(10); // reset stack value for future tests
		s.set4;
		return s.member;
	}
	i32 testMethodToMethod5() {
		Struct s = Struct(10); // reset stack value for future tests
		s.set5;
		return s.member;
	}
	i32 testMethodToMethod6() {
		Struct s = Struct(10); // reset stack value for future tests
		s.set6;
		return s.member;
	}
};
void tester92(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("testMethod1")() == 42);
	assert(ctx.getFunctionPtr!(int, int)("testMethod2")(60) == 60);
	assert(ctx.getFunctionPtr!(int)("testMethod3")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethod4")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethod5")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethodToMethod2")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethodToMethod3")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethodToMethod4")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethodToMethod5")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethodToMethod6")() == 42);
}

@TestInfo()
immutable test93 = q{--- test93
	// empty string literal
	void print(u8[]){}
	void run(){
		print("");
	}
};

@TestInfo(&tester94)
immutable test94 = q{--- test94
	// return small struct
	struct Small {
		i32 a;
		i32 b;
	}
	Small glue(i32 a, i32 b)
	{
		return Small(a, b);
	}
};
void tester94(ref TestContext ctx) {
	static struct Small {
		int a;
		int b;
	}
	assert(ctx.getFunctionPtr!(Small, int, int)("glue")(42, 100) == Small(42, 100));
}


@TestInfo(&tester95)
immutable test95 = q{--- test95
	// if callback returns true the traversal stops
	void tran_thong(i32 xstart, i32 ystart, i32 xend, i32 yend, bool function(void*, i32, i32) callback, void* userData)
	{
		i32 x = xstart;
		i32 y = ystart;

		if (callback(userData, x, y)) return;

		i32 deltax;
		i32 signdx;
		if (xend >= xstart) {
			deltax = xend - xstart;
			signdx = 1;
		} else {
			deltax = xstart - xend;
			signdx = -1;
		}

		i32 deltay;
		i32 signdy;
		if (yend >= ystart) {
			deltay = yend - ystart;
			signdy = 1;
		} else {
			deltay = ystart - yend;
			signdy = -1;
		}

		i32 test;
		if (signdy == -1)
			test = -1;
		else
			test = 0;

		if (deltax >= deltay) {
			test = (deltax + test) >> 1;
			for (i32 i = 1; i < deltax; ++i) {
				test -= deltay;
				x += signdx;
				if (test < 0) {
					y += signdy;
					test += deltax;
				}
				if (callback(userData, x, y)) return;
			}
		} else {
			test = (deltay + test) >> 1;
			for (i32 i = 1; i < deltay; ++i) {
				test -= deltax;
				y += signdy;
				if (test < 0) {
					x += signdx;
					test += deltay;
				}
				if (callback(userData, x, y)) return;
			}
		}
		if (callback(userData, xend, yend)) return;
	}
};
void tester95(ref TestContext ctx) {
	static struct test95_user_data {
		string text;
		size_t i;
		size_t breakIndex;
	}
	static extern(C) bool external_print_coords_func(void* userData, int x, int y) {
		auto data = cast(test95_user_data*)userData;
		if (data.i > 0) testSink.put(", ");
		formattedWrite(testSink, "(%s %s %s)", data.text, x, y);
		if (data.breakIndex == data.i) return true;
		++data.i;
		return false;
	}

	alias func_T = extern(C) bool function(void*, int, int);
	auto tran_thong = ctx.getFunctionPtr!(void, int, int, int, int, func_T, void*)("tran_thong");

	void runTest(size_t breakIndex, string expectedString)
	{
		test95_user_data data;
		data = test95_user_data("hi", 0, breakIndex);
		tran_thong(0, 0, 3, 3, &external_print_coords_func, &data);
		//writefln("%s", testSink.text);
		assert(testSink.text == expectedString);
		testSink.clear;
	}

	runTest(0, "(hi 0 0)");
	runTest(1, "(hi 0 0), (hi 1 1)");
	runTest(2, "(hi 0 0), (hi 1 1), (hi 2 2)");
	runTest(3, "(hi 0 0), (hi 1 1), (hi 2 2), (hi 3 3)");
	runTest(4, "(hi 0 0), (hi 1 1), (hi 2 2), (hi 3 3)");
}

@TestInfo(&tester96)
immutable test96 = q{--- test96
	// splitting and spilling
	void tran_thong(i32 xstart, i32 ystart, i32 xend, i32 yend, void function(void*, i32, i32) callback, void* userData)
	{
		i32 x = xstart;
		i32 y = ystart;

		i32 deltax;
		i32 signdx;
		if (xend >= xstart) {
			deltax = xend - xstart;
			signdx = 1;
		} else {
			deltax = xstart - xend;
			signdx = -1;
		}

		i32 deltay;
		i32 signdy;
		if (yend >= ystart) {
			deltay = yend - ystart;
			signdy = 1;
		} else {
			deltay = ystart - yend;
			signdy = -1;
		}

		i32 test;
		if (signdy == -1)
			test = -1;
		else
			test = 0;

		if (deltax >= deltay) {
			test = (deltax + test) >> 1;
			for (i32 i = 1; i < deltax; ++i) {
				test -= deltay;
				x += signdx;
				if (test < 0) {
					y += signdy;
					test += deltax;
				}
				callback(userData, x, y);
			}
		} else {
			test = (deltay + test) >> 1;
			for (i32 i = 1; i < deltay; ++i) {
				test -= deltax;
				y += signdy;
				if (test < 0) {
					x += signdx;
					test += deltay;
				}
				callback(userData, x, y);
			}
		}
	}
};
void tester96(ref TestContext ctx) {
	static struct test96_user_data {
		string text;
		size_t i;
	}
	static extern(C) void external_print_coords_func(void* userData, int x, int y) {
		auto data = cast(test96_user_data*)userData;
		if (data.i > 0) testSink.put(", ");
		formattedWrite(testSink, "(%s %s %s)", data.text, x, y);
		++data.i;
	}
	alias func_T = extern(C) void function(void*, int, int);
	auto tran_thong = ctx.getFunctionPtr!(void, int, int, int, int, func_T, void*)("tran_thong");

	test96_user_data data = {
		text : "hi"
	};

	tran_thong(0, 0, 2, 2, &external_print_coords_func, &data);

	//writefln("%s", testSink.text);
	assert(testSink.text == "(hi 1 1)");
}

@TestInfo(&tester97)
immutable test97 = q{--- test97
	// test proper GEP lowering for stack allocated data
	// stack argument of add instruction should be detected and lea instruction must be produced
	i32 index_array(i32 index)
	{
		i32[10] array;
		array[index] = 42;
		return array[index];
	}
};
void tester97(ref TestContext ctx) {
	auto index_array = ctx.getFunctionPtr!(int, int)("index_array");

	assert(index_array(0) == 42);
	assert(index_array(9) == 42);
}

@TestInfo()
immutable test98 = q{--- folder/test98.vx
	import dep98;
	// test module name creation. Folders must be stripped
	void test()
	{
		libfunc();
	}
--- folder/dep98.vx
	void libfunc(){}
};

@TestInfo(&tester99)
immutable test99 = `--- test99
	// test #if
	enum valTrue = true;
	enum valFalse = false;

	// global scope
	#if(valTrue)
		i32 foo() { return 42; } // no else

	#if(valFalse)
		i32 foo() { return 42; } // no items to insert

	#if(valTrue){} // empty

	// curly braces
	#if(valFalse)
	{
		i32 bar() { return 42; }
	}
	else // insert more than 1 item
	{
		i32 bar() { return 100; }
		i32 bar2() { return 200; }
	}

	// function scope
	i32 funcTrue() {
		#if(valTrue)
			return 42;
		else
			return 100;
	}

	i32 funcFalse() {
		#if(valFalse)
			return 42;
		else
			return 100;
	}
`;
void tester99(ref TestContext ctx) {
	auto foo = ctx.getFunctionPtr!(int)("foo");
	auto bar = ctx.getFunctionPtr!(int)("bar");
	auto bar2 = ctx.getFunctionPtr!(int)("bar2");
	auto funcTrue = ctx.getFunctionPtr!(int)("funcTrue");
	auto funcFalse = ctx.getFunctionPtr!(int)("funcFalse");

	assert(foo() == 42);
	assert(bar() == 100);
	assert(bar2() == 200);
	assert(funcTrue() == 42);
	assert(funcFalse() == 100);
}

@TestInfo()
immutable test100 = `--- test100
	enum a = true;
	#if (a)
		enum b = true;

	void main()
	{
		#if (b) enum c = 42; // requires #if (a) to be evalauated first
	}
`;

@TestInfo()
immutable test101 = `--- test101
	enum a = true;
	enum b = true;
	#if (a)
	{
		#if (b) enum c = true;
	}

	void main()
	{
		#if (c) enum d = 42; // requires #if (b) to be evalauated first
	}
`;

@TestInfo()
immutable test102 = q{--- test102
	// test taking address of members
	struct S {
		u64 member;
		void method() {
			func(&member);
		}
	}
	void func(u64* ptr) {}
};

@TestInfo()
immutable test103 = q{--- test103
	// test loading of pointers in member chains
	struct A {
		u64 member;
	}
	struct S {
		A* a;
	}
	void func() {
		S s;
		s.a.member = 42;
	}
};

@TestInfo(&tester104)
immutable test104 = q{--- test104
	// cast int to int
	i16 fold__i8_to_i16_sext() { return cast(i16)cast( i8)-1; } // sext   i8 -> i16
	i32 fold__i8_to_i32_sext() { return cast(i32)cast( i8)-1; } // sext   i8 -> i32
	i32 fold_i16_to_i32_sext() { return cast(i32)cast(i16)-1; } // sext  i16 -> i32
	i64 fold__i8_to_i64_sext() { return cast(i64)cast( i8)-1; } // sext   i8 -> i64
	i64 fold_i16_to_i64_sext() { return cast(i64)cast(i16)-1; } // sext  i16 -> i64
	i64 fold_i32_to_i64_sext() { return cast(i64)cast(i32)-1; } // sext  i32 -> i64

	i16 fold__u8_to_i16_zext() { return cast(i16)cast( u8)u8.max;  } // zext   u8 -> i16
	i32 fold__u8_to_i32_zext() { return cast(i32)cast( u8)u8.max;  } // zext   u8 -> i32
	i32 fold_u16_to_i32_zext() { return cast(i32)cast(u16)u16.max; } // zext  u16 -> i32
	i64 fold__u8_to_i64_zext() { return cast(i64)cast( u8)u8.max;  } // zext   u8 -> i64
	i64 fold_u16_to_i64_zext() { return cast(i64)cast(u16)u16.max; } // zext  u16 -> i64
	i64 fold_u32_to_i64_zext() { return cast(i64)cast(u32)u32.max; } // zext  u32 -> i64

	u16 fold__u8_to_u16_zext() { return cast(u16)cast( u8)u8.max;  } // zext   u8 -> u16
	u32 fold__u8_to_u32_zext() { return cast(u32)cast( u8)u8.max;  } // zext   u8 -> u32
	u32 fold_u16_to_u32_zext() { return cast(u32)cast(u16)u16.max; } // zext  u16 -> u32
	u64 fold__u8_to_u64_zext() { return cast(u64)cast( u8)u8.max;  } // zext   u8 -> u64
	u64 fold_u16_to_u64_zext() { return cast(u64)cast(u16)u16.max; } // zext  u16 -> u64
	u64 fold_u32_to_u64_zext() { return cast(u64)cast(u32)u32.max; } // zext  u32 -> u64

	u16 fold__i8_to_u16_zext() { return cast(u16)i8.min;  } // zext   i8 -> u16 TODO: these should error without cast
	u32 fold__i8_to_u32_zext() { return cast(u32)i8.min;  } // zext   i8 -> u32 TODO: these should error without cast
	u32 fold_i16_to_u32_zext() { return cast(u32)i16.min; } // zext  i16 -> u32 TODO: these should error without cast
	u64 fold__i8_to_u64_zext() { return cast(u64)i8.min;  } // zext   i8 -> u64 TODO: these should error without cast
	u64 fold_i16_to_u64_zext() { return cast(u64)i16.min; } // zext  i16 -> u64 TODO: these should error without cast
	u64 fold_i32_to_u64_zext() { return cast(u64)i32.min; } // zext  i32 -> u64 TODO: these should error without cast


	i16 func__i8_to_i16( i8 a) { return a; } // sext   i8 -> i16
	i32 func__i8_to_i32( i8 a) { return a; } // sext   i8 -> i32
	i32 func_i16_to_i32(i16 a) { return a; } // sext  i16 -> i32
	i64 func__i8_to_i64( i8 a) { return a; } // sext   i8 -> i64
	i64 func_i16_to_i64(i16 a) { return a; } // sext  i16 -> i64
	i64 func_i32_to_i64(i32 a) { return a; } // sext  i32 -> i64

	i16 func__u8_to_i16( u8 a) { return a; } // zext   u8 -> i16
	i32 func__u8_to_i32( u8 a) { return a; } // zext   u8 -> i32
	i32 func_u16_to_i32(u16 a) { return a; } // zext  u16 -> i32
	i64 func__u8_to_i64( u8 a) { return a; } // zext   u8 -> i64
	i64 func_u16_to_i64(u16 a) { return a; } // zext  u16 -> i64
	i64 func_u32_to_i64(u32 a) { return a; } // zext  u32 -> i64

	u16 func__u8_to_u16( u8 a) { return a; } // zext   u8 -> u16
	u32 func__u8_to_u32( u8 a) { return a; } // zext   u8 -> u32
	u32 func_u16_to_u32(u16 a) { return a; } // zext  u16 -> u32
	u64 func__u8_to_u64( u8 a) { return a; } // zext   u8 -> u64
	u64 func_u16_to_u64(u16 a) { return a; } // zext  u16 -> u64
	u64 func_u32_to_u64(u32 a) { return a; } // zext  u32 -> u64

	u16 func__i8_to_u16( i8 a) { return cast(u16)a; } // zext   i8 -> u16 TODO: these should error without cast
	u32 func__i8_to_u32( i8 a) { return cast(u32)a; } // zext   i8 -> u32 TODO: these should error without cast
	u32 func_i16_to_u32(i16 a) { return cast(u32)a; } // zext  i16 -> u32 TODO: these should error without cast
	u64 func__i8_to_u64( i8 a) { return cast(u64)a; } // zext   i8 -> u64 TODO: these should error without cast
	u64 func_i16_to_u64(i16 a) { return cast(u64)a; } // zext  i16 -> u64 TODO: these should error without cast
	u64 func_i32_to_u64(i32 a) { return cast(u64)a; } // zext  i32 -> u64 TODO: these should error without cast
};
void tester104(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(short)("fold__i8_to_i16_sext")() == short(-1));
	assert(ctx.getFunctionPtr!(  int)("fold__i8_to_i32_sext")() ==   int(-1));
	assert(ctx.getFunctionPtr!(  int)("fold_i16_to_i32_sext")() ==   int(-1));
	assert(ctx.getFunctionPtr!( long)("fold__i8_to_i64_sext")() ==  long(-1));
	assert(ctx.getFunctionPtr!( long)("fold_i16_to_i64_sext")() ==  long(-1));
	assert(ctx.getFunctionPtr!( long)("fold_i32_to_i64_sext")() ==  long(-1));

	assert(ctx.getFunctionPtr!(short)("fold__u8_to_i16_zext")() ==  ubyte.max);
	assert(ctx.getFunctionPtr!(  int)("fold__u8_to_i32_zext")() ==  ubyte.max);
	assert(ctx.getFunctionPtr!(  int)("fold_u16_to_i32_zext")() == ushort.max);
	assert(ctx.getFunctionPtr!( long)("fold__u8_to_i64_zext")() ==  ubyte.max);
	assert(ctx.getFunctionPtr!( long)("fold_u16_to_i64_zext")() == ushort.max);
	assert(ctx.getFunctionPtr!( long)("fold_u32_to_i64_zext")() ==   uint.max);

	assert(ctx.getFunctionPtr!(ushort)("fold__u8_to_u16_zext")() ==  ubyte.max);
	assert(ctx.getFunctionPtr!(  uint)("fold__u8_to_u32_zext")() ==  ubyte.max);
	assert(ctx.getFunctionPtr!(  uint)("fold_u16_to_u32_zext")() == ushort.max);
	assert(ctx.getFunctionPtr!( ulong)("fold__u8_to_u64_zext")() ==  ubyte.max);
	assert(ctx.getFunctionPtr!( ulong)("fold_u16_to_u64_zext")() == ushort.max);
	assert(ctx.getFunctionPtr!( ulong)("fold_u32_to_u64_zext")() ==   uint.max);

	//writefln("%08X", ctx.getFunctionPtr!(ushort)("fold__i8_to_u16_zext")());
	assert(ctx.getFunctionPtr!(ushort)("fold__i8_to_u16_zext")() ==  ubyte(0x0000_0080));
	assert(ctx.getFunctionPtr!(  uint)("fold__i8_to_u32_zext")() ==  ubyte(0x0000_0080));
	assert(ctx.getFunctionPtr!(  uint)("fold_i16_to_u32_zext")() == ushort(0x0000_8000));
	assert(ctx.getFunctionPtr!( ulong)("fold__i8_to_u64_zext")() ==  ubyte(0x0000_0080));
	assert(ctx.getFunctionPtr!( ulong)("fold_i16_to_u64_zext")() == ushort(0x0000_8000));
	assert(ctx.getFunctionPtr!( ulong)("fold_i32_to_u64_zext")() ==   uint(0x8000_0000));


	assert(ctx.getFunctionPtr!(short,   byte)("func__i8_to_i16")(  byte(-1)) == short(-1));
	assert(ctx.getFunctionPtr!(  int,   byte)("func__i8_to_i32")(  byte(-1)) ==   int(-1));
	assert(ctx.getFunctionPtr!(  int,  short)("func_i16_to_i32")( short(-1)) ==   int(-1));
	assert(ctx.getFunctionPtr!( long,   byte)("func__i8_to_i64")(  byte(-1)) ==  long(-1));
	assert(ctx.getFunctionPtr!( long,  short)("func_i16_to_i64")( short(-1)) ==  long(-1));
	assert(ctx.getFunctionPtr!( long,    int)("func_i32_to_i64")(   int(-1)) ==  long(-1));

	assert(ctx.getFunctionPtr!(short,  ubyte)("func__u8_to_i16")( ubyte.max) ==  ubyte.max);
	assert(ctx.getFunctionPtr!(  int,  ubyte)("func__u8_to_i32")( ubyte.max) ==  ubyte.max);
	assert(ctx.getFunctionPtr!(  int, ushort)("func_u16_to_i32")(ushort.max) == ushort.max);
	assert(ctx.getFunctionPtr!( long,  ubyte)("func__u8_to_i64")( ubyte.max) ==  ubyte.max);
	assert(ctx.getFunctionPtr!( long, ushort)("func_u16_to_i64")(ushort.max) == ushort.max);
	assert(ctx.getFunctionPtr!( long,   uint)("func_u32_to_i64")(  uint.max) ==   uint.max);

	assert(ctx.getFunctionPtr!(ushort,  ubyte)("func__u8_to_u16")( ubyte.max) ==  ubyte.max);
	assert(ctx.getFunctionPtr!(  uint,  ubyte)("func__u8_to_u32")( ubyte.max) ==  ubyte.max);
	assert(ctx.getFunctionPtr!(  uint, ushort)("func_u16_to_u32")(ushort.max) == ushort.max);
	assert(ctx.getFunctionPtr!( ulong,  ubyte)("func__u8_to_u64")( ubyte.max) ==  ubyte.max);
	assert(ctx.getFunctionPtr!( ulong, ushort)("func_u16_to_u64")(ushort.max) == ushort.max);
	assert(ctx.getFunctionPtr!( ulong,   uint)("func_u32_to_u64")(  uint.max) ==   uint.max);

	//writefln("%08X", ctx.getFunctionPtr!(ushort,   byte)("func__i8_to_u16")(  byte.min));
	assert(ctx.getFunctionPtr!(ushort,   byte)("func__i8_to_u16")(  byte.min) ==  ubyte(0x0000_0080));
	assert(ctx.getFunctionPtr!(  uint,   byte)("func__i8_to_u32")(  byte.min) ==  ubyte(0x0000_0080));
	assert(ctx.getFunctionPtr!(  uint,  short)("func_i16_to_u32")( short.min) == ushort(0x0000_8000));
	assert(ctx.getFunctionPtr!( ulong,   byte)("func__i8_to_u64")(  byte.min) ==  ubyte(0x0000_0080));
	assert(ctx.getFunctionPtr!( ulong,  short)("func_i16_to_u64")( short.min) == ushort(0x0000_8000));
	assert(ctx.getFunctionPtr!( ulong,    int)("func_i32_to_u64")(   int.min) ==   uint(0x8000_0000));
}

@TestInfo(&tester105)
immutable test105 = q{--- test105
	// test struct constructor with default initialization
	struct Struct
	{
		i32 var = 42;
		void fun(){}
		i32 var2;
	}
	Struct get_struct()
	{
		Struct s = Struct();
		return s;
	}
	// test struct constructor with nested struct initialization
	struct Struct2
	{
		u8[] str;
	}
	Struct2 get_struct2(u8[] str)
	{
		return Struct2(str);
	}
};
void tester105(ref TestContext ctx) {
	struct Struct { int a; int b; }
	auto get_struct = ctx.getFunctionPtr!(Struct)("get_struct");
	assert(get_struct() == Struct(42, 0));

	struct Struct2 { const(char)[] str; }
	auto get_struct2 = ctx.getFunctionPtr!(Struct2, SliceString)("get_struct2");
	assert(get_struct2(SliceString("test")) == Struct2("test"));
}


@TestInfo(&tester106, [HostSymbol("fun2", cast(void*)&external_tester106)])
immutable test106 = q{--- test106
	// Test structs passed as pointer on the stack (win64 abi) (name parameter)
	// big struct on stack (sysv64)
	void fun(i32, i32, i32, u8, u8, u8, u8[] name, bool){
		fun2(name);
	}
	@extern(module, "host")
	void fun2(u8[]);
	void run()
	{
		fun(50, 50, 32, 0, 255, 0, "Player", true);
	}
};
extern(C) void external_tester106(Slice!char str) {
	assert(str == "Player");
}
void tester106(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
}


@TestInfo(&tester107)
immutable test107 = q{--- test107
	// Test slicing
	i32[] slice_ptr1(i32* ptr)
	{
		return ptr[0..10];
	}
	i32[] slice_ptr2(i32* ptr, u64 from, u64 to)
	{
		return ptr[from..to];
	}
	i32[] slice_array1(i32[10]* ptr)
	{
		return (*ptr)[0..10];
	}
	i32[] slice_array2(i32[10]* ptr, u64 from, u64 to)
	{
		return (*ptr)[from..to];
	}
	i32[] slice_slice1(i32[] ptr)
	{
		return ptr[0..10];
	}
	i32[] slice_slice2(i32[] ptr, u64 from, u64 to)
	{
		return ptr[from..to];
	}
	void test()
	{
		u8[2] array;
		u8[] slice = array[0..1];
		u8_user(array[0..1]);
	}
	void u8_user(u8[]){}
};
void tester107(ref TestContext ctx) {
	int[10] array = [0,1,2,3,4,5,6,7,8,9];

	auto slice_ptr1 = ctx.getFunctionPtr!(Slice!int, int*)("slice_ptr1");
	assert(slice_ptr1(array.ptr) == array[]);
	auto slice_ptr2 = ctx.getFunctionPtr!(Slice!int, int*, ulong, ulong)("slice_ptr2");
	assert(slice_ptr2(array.ptr, 0, 10) == array[]);
	assert(slice_ptr2(array.ptr, 5, 7) == array[5..7]);

	auto slice_array1 = ctx.getFunctionPtr!(Slice!int, int[10]*)("slice_array1");
	assert(slice_array1(&array) == array[]);
	auto slice_array2 = ctx.getFunctionPtr!(Slice!int, int[10]*, ulong, ulong)("slice_array2");
	assert(slice_array2(&array, 0, 10) == array[]);
	assert(slice_array2(&array, 5, 7) == array[5..7]);

	auto slice_slice1 = ctx.getFunctionPtr!(Slice!int, Slice!int)("slice_slice1");
	assert(slice_slice1(Slice!int(array[])) == array[]);
	auto slice_slice2 = ctx.getFunctionPtr!(Slice!int, Slice!int, ulong, ulong)("slice_slice2");
	assert(slice_slice2(Slice!int(array[]), 0, 10) == array[]);
	assert(slice_slice2(Slice!int(array[]), 5, 7) == array[5..7]);
}


@TestInfo(&tester108)
immutable test108 = q{--- test108
	// Test function templates
	T min[T](T a, T b) {
		if (a < b) return a;
		return b;
	}
	i8 test_i8(i8 a, i8 b) {
		return min[i8](a, b) + min[i8](a, b); // test double instantiation
	}
	i16 test_i16(i16 a, i16 b) {
		return min[i16](a, b) + min[i16](a, b); // test multiple instances
	}
	i32 test_i32(i32 a, i32 b) {
		return min[i32](a, b) + min[i32](a, b);
	}
	i64 test_i64(i64 a, i64 b) {
		return min[i64](a, b) + min[i64](a, b);
	}
};
void tester108(ref TestContext ctx) {
	auto test_i8 = ctx.getFunctionPtr!(byte, byte, byte)("test_i8");
	auto test_i16 = ctx.getFunctionPtr!(short, short, short)("test_i16");
	auto test_i32 = ctx.getFunctionPtr!(int, int, int)("test_i32");
	auto test_i64 = ctx.getFunctionPtr!(long, long, long)("test_i64");
	assert(test_i8(42, 120) == 42 * 2);
	assert(test_i16(420, 1200) == 420 * 2);
	assert(test_i32(-10_000, 10_000) == -10_000 * 2);
	assert(test_i64(-10_000, 10_000) == -10_000 * 2);
}

@TestInfo()
immutable test109 = q{--- test109
	// Test all type kinds
	struct S {
		i32 n;
	}
	T pass[T](T a) {
		return a;
	}
	i8 test_basic(i8 a) {
		return pass[i8](a);
	}
	u8* test_ptr(u8* ptr) {
		return pass[u8*](ptr);
	}
	S test_struct(S s) {
		return pass[S](s);
	}
	u8[] test_i64(u8[] slice) {
		return pass[u8[]](slice);
	}
};

@TestInfo(&tester110)
immutable test110 = q{--- test110
	// Test struct templates
	struct Array[T] {
		T item;
	}
	T fun[T](T arg) { return arg; }
	Array[i32] test(Array[i32] array) {
		return fun[Array[i32]](array);
	}
};
void tester110(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int, int)("test")(42) == 42);
}


@TestInfo(&tester111)
immutable test111 = q{--- test111
	// Test string escapes
	u8[] test() {
		return "\'\"\?\\\0\a\b\f\n\r\t\v\xAA\uAAAA\U0000AAAA";
	}
};
void tester111(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(Slice!char)("test")() == "\'\"\?\\\0\a\b\f\n\r\t\v\xAA\uAAAA\U0000AAAA");
}

@TestInfo(&tester112)
immutable test112 = q{--- test112
	// Test int <--> ptr conversions
	u64 ptr_to_int(u8* ptr) {
		return cast(u64)ptr;
	}
	u8* int_to_ptr(u64 i) {
		return cast(u8*)i;
	}
};
void tester112(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong, ubyte*)("ptr_to_int")(cast(ubyte*)0xFFFF_FFFF_FFFF_FFFF) == 0xFFFF_FFFF_FFFF_FFFF);
	assert(ctx.getFunctionPtr!(ubyte*, ulong)("int_to_ptr")(0xFFFF_FFFF_FFFF_FFFF) == cast(ubyte*)0xFFFF_FFFF_FFFF_FFFF);
}


@TestInfo(&tester113)
immutable test113 = q{--- test113
	// default initialization of variables
	struct S
	{
		i32 integer;
		u8[] slice;
	}
	void fun()
	{
		S s;
		i32 integer;
		u8[] slice;
	}
};
void tester113(ref TestContext ctx) {
}


@TestInfo(&tester114)
immutable test114 = q{--- test114
	// test ptr slice correctenss
	struct Array
	{
		i32* bufPtr;
		u32 length;
		u32 capacity;

		i32[] data()
		{
			return bufPtr[0..length];
		}
	}

	i32[] test(Array* array)
	{
		return array.data;
	}
};
void tester114(ref TestContext ctx) {
	static struct Array {
		int* ptr;
		uint length;
		uint capacity;
	}
	int[2] buffer = [42, 0];
	Array array = Array(buffer.ptr, 1, 2);
	int[] data = ctx.getFunctionPtr!(Slice!int, Array*)("test")(&array).slice;
	assert(data.ptr == buffer.ptr);
	assert(data.length == 1);
}


@TestInfo(&tester115)
immutable test115 = q{--- test115
	// Test recursive templates
	T test[T](T arg) {
		if (arg == 0)
			return 42;
		return test[T](arg-1);
	}
	i32 test_i32() {
		return test[i32](42);
	}
};
void tester115(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("test_i32")() == 42);
}


@TestInfo(&tester116)
immutable test116 = q{--- test116
	// Test default arguments
	struct S { i32 a; i32 b; }
	i32 func_i32(i32 arg = 42) { return arg; }
	u8[] func_string(u8[] arg = "Hello") { return arg; }
	//S func_struct(S arg = S(1, 2)) { return arg; } // TODO

	i32 test_i32() { return func_i32; }
	u8[] test_string() { return func_string; }
	//S test_struct() { return func_struct; }
};
void tester116(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("test_i32")() == 42);
	assert(ctx.getFunctionPtr!SliceString("test_string")().slice == "Hello");
}


@TestInfo(&tester117)
immutable test117 = q{--- test117
	// BUG: passing small constant (u8) as big (u32) argument causes u8 <- u8 mov instead of u32 <- u32
	u32 pass(u32 param) {
		return param;
	}
	u64 test() {
		u64 val = pass(0xFFFF_FFFF); // fill register with garbage
		val += pass(0); // BUG: sets only the lowest byte, passing 0xFFFF_FF00 as an argument
		return val;
	}
};
void tester117(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("test")() == 0xFFFF_FFFF);
}


@TestInfo(&tester118)
immutable test118 = q{--- test118
	// Test global ptr deref assign
	u32* glob_ptr;
	u32** get_ptr() { return &glob_ptr; }
	void set_value(u32 val) { *glob_ptr = val; }
};
void tester118(ref TestContext ctx) {
	auto get_ptr = ctx.getFunctionPtr!(uint**)("get_ptr");
	auto set_value = ctx.getFunctionPtr!(void, uint)("set_value");
	uint val;
	(*get_ptr()) = &val;
	set_value(100);
	assert(val == 100);
}


@TestInfo(&tester119)
immutable test119 = q{--- test119
	// Test global scalar initialization
	u32 glob = 42;
	u32 read_global() { return glob; }
	void set_global(u32 val) { glob = val; }
};
void tester119(ref TestContext ctx) {
	auto read_global = ctx.getFunctionPtr!(uint)("read_global");
	auto set_global = ctx.getFunctionPtr!(void, uint)("set_global");
	assert(read_global() == 42);
	set_global(100);
	assert(read_global() == 100);
}


@TestInfo(&tester120)
immutable test120 = q{--- test120
	// Test global struct initialization
	struct S {
		u32 a;
		u32 b;
		u32 c;
		u32 d;
	}
	S glob = S(1, 2, 3, 4);
	S read_global() { return glob; }
	void set_global(S val) { glob = val; }
};
void tester120(ref TestContext ctx) {
	struct S {
		uint a;
		uint b;
		uint c;
		uint d;
	}
	auto read_global = ctx.getFunctionPtr!(S)("read_global");
	auto set_global = ctx.getFunctionPtr!(void, S)("set_global");
	assert(read_global() == S(1, 2, 3, 4));
	set_global(S(10, 20, 30, 40));
	assert(read_global() == S(10, 20, 30, 40));
}


@TestInfo(&tester121)
immutable test121 = q{--- test121
	// Test global struct initialization with pointers
	u32 a = 1;
	u32 b = 2;
	u32 c = 3;
	u32 d = 4;
	struct S {
		u32* a;
		u32* b;
		u32* c;
		u32* d;
	}
	S glob = S(&a, &b, &c, &d);
	S read_global() { return glob; }
};
void tester121(ref TestContext ctx) {
	struct S {
		uint* a;
		uint* b;
		uint* c;
		uint* d;
	}
	auto read_global = ctx.getFunctionPtr!(S)("read_global");
	//import utils : printHex;
	//printHex(ctx.driver.context.staticDataBuffer.data, 16);
	S s = read_global();
	assert(*s.a == 1);
	assert(*s.b == 2);
	assert(*s.c == 3);
	assert(*s.d == 4);
}


@TestInfo(&tester122)
immutable test122 = q{--- test122
	// Test global pointer initialization
	u32 glob = 42;
	u32* glob_ptr = &glob;
	u32* get_ptr() { return glob_ptr; }
	u32 read_global() { return *glob_ptr; }
	void set_global(u32 val) { *glob_ptr = val; }
};
void tester122(ref TestContext ctx) {
	auto get_ptr = ctx.getFunctionPtr!(uint*)("get_ptr");
	auto read_global = ctx.getFunctionPtr!(uint)("read_global");
	auto set_global = ctx.getFunctionPtr!(void, uint)("set_global");
	//writefln("%X %s", get_ptr(), *get_ptr());
	//writefln("%s", read_global());
	assert(read_global() == 42);
	set_global(100);
	//writefln("%X %s", get_ptr(), *get_ptr());
	//writefln("%s", read_global());
	assert(read_global() == 100);
}


@TestInfo(&tester123)
immutable test123 = q{--- test123
	// Test switch
	i32 fun(i32 val) {
		switch (val) {
			0 { return 40; }
			1 { return 10; }
			2 { return 15; }
			else { return 0; }
		}
	}
};
void tester123(ref TestContext ctx) {
	auto fun = ctx.getFunctionPtr!(int, int)("fun");
	assert(fun(0) == 40);
	assert(fun(1) == 10);
	assert(fun(2) == 15);
	assert(fun(100) == 0);
	assert(fun(200) == 0);
	assert(fun(-200) == 0);
}

@TestInfo(&tester124)
immutable test124 = q{--- test124
	// Test global in separate module
	import file2;
	i64 read_global() { return noop(glob); }
	i64 noop(i64 val) { return val; }
--- file2
	i64 glob = 42;
};
void tester124(ref TestContext ctx) {
	auto read_global = ctx.getFunctionPtr!(uint)("read_global");
	assert(read_global() == 42);
}


@TestInfo()
immutable test125 = q{--- test125
	// Index expression over template should copy isType of template body
	void freeArray[T](T[] array) {}

	struct Array[T]
	{
		T* bufPtr;
		u32 length;
		u32 capacity;

		void free()
		{
			freeArray[T](bufPtr[0..length]);
		}
	}
	alias int_array = Array[i32];
};

@TestInfo()
immutable test126 = q{--- test126
	// Template bug. .ptr PtrTypeNode was not marked as type checked
	void run() {
		i32[] slice;
		freeArray[i32](slice);
	}

	void freeArray[T](T[] array)
	{
		if (array.ptr == null) return;
	}
};

@TestInfo(&tester127)
immutable test127 = q{--- test127
	// Small struct store
	void put(Point* bufPtr, Point item) {
		bufPtr[0] = item;
	}
	struct Point { i32 x; i32 y; }
};
void tester127(ref TestContext ctx) {
	static struct Point { int x; int y; }
	auto put = ctx.getFunctionPtr!(void, Point*, Point)("put");
	Point point;
	put(&point, Point(42, 90));
	assert(point == Point(42, 90));
}


@TestInfo()
immutable test128 = q{--- test128
	// Test member access on null constant
	struct Tile {
		u16 a;
		u16 b;
	}
	void run() {
		Tile* t;
		t.b = 42; // member of null pointer
	}
};


@TestInfo()
immutable test129 = `--- test129
	// Test inlining a recursive function inside non-recursive caller
	i32 caller(i32 param) {
		return callee(param) + 10;
	}

	i32 callee(i32 param) #inline {
		if (param == 0) return 42;
		return callee(param - 1);
	}
`;

@TestInfo()
immutable test130 = `--- test130
	// Test inlining with phi function
	// Test inlining with array used
	i32 caller(i32 param) {
		return sign(param);
	}

	i32 sign(i32 number) #inline {
		if (number < 0) return 0-1;
		else if (number > 0) return 1;
		else return 0;
	}
`;


@TestInfo(null, [HostSymbol("log", cast(void*)&external_print_i64_func)])
immutable test131 = q{--- test131
	i64 fac(i64 x) {
		if (x < 2) {
			return 1;
		} else {
			return fac(x - 1) * x;
		}
	}
	@extern(module, "host")
	void log(i64);
	void run() {
		log(fac(5));
	}
};


@TestInfo()
immutable test132 = q{--- test132
	#assert(true, "Assert test");
};


@TestInfo()
immutable test133 = q{--- test133
	#assert(false, "Assert test");
--- <error>
test133:1:2: Error: #assert: "Assert test"
};


@TestInfo()
immutable test134 = q{--- test134
	$alias func() {
		return func; // return alias to itself
	}
};

@TestInfo()
immutable test135 = q{--- test135
	$alias func() {
		return func; // return alias to itself
	}
};

@TestInfo(&tester136)
immutable test136 = q{--- test136
	// Various meta types tests
	i32 func() { return 42; }
	$alias metaFun1() { return func; } // return $alias of func
	$alias metaFun2() { return u8; } // return $alias of built-in
	$type  metaFun3() { return u8; } // return $type
	enum bool res1 = metaFun1() == u8; // false
	enum bool res2 = metaFun2() == u8; // true
	enum bool res3 = metaFun3() == u8; // true

	enum $alias a = func;
	//enum $type t = i32;
	i32 run() { return a(); }
	bool run1() { return res1; }
	bool run2() { return res2; }
	bool run3() { return res3; }

	bool isInteger($type type) {
		return type == u8
			|| type == i8
			|| type == u16
			|| type == i16
			|| type == u32
			|| type == i32
			|| type == u64
			|| type == i64;
	}
	enum bool res4 =
		isInteger(u8) &&
		isInteger(i8) &&
		isInteger(u16) &&
		isInteger(i16) &&
		isInteger(u32) &&
		isInteger(i32) &&
		isInteger(u64) &&
		isInteger(i64);
	bool run4() { return res4; }
};
void tester136(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(int)("run");
	auto run1 = ctx.getFunctionPtr!(bool)("run1");
	auto run2 = ctx.getFunctionPtr!(bool)("run2");
	auto run3 = ctx.getFunctionPtr!(bool)("run3");
	auto run4 = ctx.getFunctionPtr!(bool)("run4");
	assert(run() == 42);
	assert(run1() == false);
	assert(run2() == true);
	assert(run3() == true);
	assert(run4() == true);
}


@TestInfo()
immutable test137 = q{--- test137
	// ctfe only structs
	struct S1 {
		$alias ctfeVar;
	}
	struct S2 {
		$alias ctfeFun(){ return S1; }
	}
};


@TestInfo(null, [HostSymbol("crash", cast(void*)&external_noop)])
immutable test138 = q{--- test138
	// noreturn
	@extern(module, "host")
	noreturn crash();
	void run1() {
		crash();
	}
	noreturn run2() {
		crash();
	}
	// should not complain about missing return
	i32 run3() {
		crash();
	}
};


@TestInfo()
immutable test139 = q{--- test139
	// builtin functions
	bool run() {
		return val;
	}
	enum val = $compileError("CTFE error");
--- <error>
test139:5:26: Error: CTFE error
};


@TestInfo()
immutable test140 = q{--- test140
	// CTFE in type check pass
	#assert(callee1, "test");
	bool callee1() {
		return callee2();
	}
	bool callee2() {
		return false;
	}
--- <error>
test140:2:2: Error: #assert: "test"
};


// TODO: no file/line number in the error
@TestInfo()
immutable test141 = q{--- test141
	// builtin functions
	#assert(callee1, "test");
	bool callee1() {
		return callee2();
	}

	bool callee2() {
		$compileError("CTFE error");
	}
--- <error>
: Error: CTFE error
};


@TestInfo(&tester142)
immutable test142 = q{--- test142
	// Store $alias and $type in alias
	alias type1 = getType1();
	$alias getType1() { return u8; }
	type1 run1() {
		 return 42;
	}
	alias type2 = getType2();
	$type getType2() { return u8; }
	type2 run2() {
		 return 42;
	}
};
void tester142(ref TestContext ctx) {
	auto run1 = ctx.getFunctionPtr!(ubyte)("run1");
	auto run2 = ctx.getFunctionPtr!(ubyte)("run2");
	assert(run1() == 42);
	assert(run2() == 42);
}


@TestInfo(&tester143)
immutable test143 = q{--- test143
	// bool $isSlice($type type)
	bool run1() {
		enum bool val = $isSlice(u8[]);
		return val; // true
	}
	bool run2() {
		enum bool val = $isSlice(u8);
		return val; // false
	}
};
void tester143(ref TestContext ctx) {
	auto run1 = ctx.getFunctionPtr!(bool)("run1");
	auto run2 = ctx.getFunctionPtr!(bool)("run2");
	assert(run1() == true);
	assert(run2() == false);
}


@TestInfo(&tester144, [
	HostSymbol("printStr", cast(void*)&external_print_string),
	HostSymbol("printInt", cast(void*)&external_print_i64_func)])
immutable test144 = q{--- test144
	// select function
	@extern(module, "host")
	void printStr(u8[]);
	@extern(module, "host")
	void printInt(i64 i);
	bool isInteger($type type) {
		return type == u8
			|| type == i8
			|| type == u16
			|| type == i16
			|| type == u32
			|| type == i32
			|| type == u64
			|| type == i64;
	}
	$alias selectPrintFunc($type T) {
		if (isInteger(T))
			return printInt;
		if ($isSlice(T))
			return printStr;
		$compileError("Invalid type");
	}
	void run() {
		alias func1 = selectPrintFunc(u8[]);
		func1("Hello");
		alias func2 = selectPrintFunc(i32);
		func2(42);
	}
};
void tester144(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	assert(testSink.text == "Hello42");
}


@TestInfo()
immutable test145 = q{--- test145
	// Test implicit function template instantiation (0 args)
	void fun[]() {}
	void run(){ fun(); }
};


@TestInfo()
immutable test146 = q{--- test146
	// Test implicit function template instantiation (1 arg)
	void fun[T](T param) {}
	void run(){ fun(42); }
};


@TestInfo()
immutable test147 = q{--- test147
	// Test implicit function template instantiation (2 equivalent types on single arg)
	void fun[T](T param1, T param2) {}
	void run(){ fun(42, 42); }
};


@TestInfo(&tester148)
immutable test148 = q{--- test148
	// Test implicit function template instantiation
	T min[T](T a, T b) {
		if (a < b) return a;
		return b;
	}
	i8 test_i8(i8 a, i8 b) {
		return min(a, b) + min(a, b);
	}
	i16 test_i16(i16 a, i16 b) {
		return min(a, b) + min(a, b);
	}
	i32 test_i32(i32 a, i32 b) {
		return min(a, b) + min(a, b);
	}
	i64 test_i64(i64 a, i64 b) {
		return min(a, b) + min(a, b);
	}
};
void tester148(ref TestContext ctx) {
	auto test_i8 = ctx.getFunctionPtr!(byte, byte, byte)("test_i8");
	auto test_i16 = ctx.getFunctionPtr!(short, short, short)("test_i16");
	auto test_i32 = ctx.getFunctionPtr!(int, int, int)("test_i32");
	auto test_i64 = ctx.getFunctionPtr!(long, long, long)("test_i64");
	assert(test_i8(42, 120) == 42 * 2);
	assert(test_i16(420, 1200) == 420 * 2);
	assert(test_i32(-10_000, 10_000) == -10_000 * 2);
	assert(test_i64(-10_000, 10_000) == -10_000 * 2);
}


@TestInfo()
immutable test149 = q{--- test149
	// Test implicit function template instantiation (2 different types that have common type)
	void fun[T](T param1, T param2) {}
	void run(){ fun(42, 500); }
};


@TestInfo(&tester150, [
	HostSymbol("printStr", cast(void*)&external_print_string),
	HostSymbol("printInt", cast(void*)&external_print_i64_func)])
immutable test150 = q{--- test150
	// $isInteger + IFTI
	@extern(module, "host")
	void printStr(u8[]);
	@extern(module, "host")
	void printInt(i64 i);
	$alias selectPrintFunc($type T) {
		if ($isInteger(T))
			return printInt;
		if ($isSlice(T))
			return printStr;
		$compileError("Invalid type");
	}
	void write[T](T val) {
		alias func = selectPrintFunc(T);
		func(val);
	}
	void run() {
		write("Hello");
		write(42);
	}
};
void tester150(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	assert(testSink.text == "Hello42");
}


@TestInfo()
immutable test151 = q{--- test151
	// Variadic template arg (0 uses, 0 args)
	void fun[Args...]() {
		#assert(Args.length == 0);
	}
	void run(){ fun(); }
};


@TestInfo()
immutable test152 = q{
--- test152
	// Variadic template arg (0 uses, 0 args, 2 variadics)
	void fun[Args1..., Args2...]() {}
	void run(){ fun(); }
--- <error>
test152:2:21: Error: Only single variadic template parameter allowed
};


@TestInfo()
immutable test153 = q{
--- test153
	// Variadic template arg (0 uses, 0 args, 1 variadic, 1 param after variadic)
	void fun[Args..., T]() {}
	void run(){ fun(); }
--- <error>
test153:2:20: Error: Cannot have template parameters after variadic parameter (WIP)
};


@TestInfo()
immutable test154 = q{--- test154
	// Variadic template arg (0 args)
	void fun[Args...](Args... args) {
		#assert(Args.length == 0);
	}
	void run(){ fun(); }
};


@TestInfo()
immutable test155 = q{--- test155
	// Variadic template arg (1 arg)
	void fun[Args...](Args... args) {
		#assert(Args.length == 1);
	}
	void run(){ fun(42); }
};


@TestInfo()
immutable test156 = q{--- test156
	// Variadic template arg (2 args)
	void fun[Args...](Args... args) {
		#assert(Args.length == 2);
	}
	void run(){ fun(42, 4096); }
};


@TestInfo()
immutable test157 = q{--- test157
	// Variadic template arg (3 args)
	void fun[Args...](Args... args) {
		#assert(Args.length == 3);
	}
	void run(){ fun(42, 4096, 200000); }
};


@TestInfo()
immutable test158 = q{--- test158
	// Variadic template arg, same instance (2 args)
	void fun[Args...](Args... args) {
		#assert(Args.length == 3);
	}
	void run(){
		fun(42, 4096, 200000);
		fun(50, 3000, 300000);
	}
};


@TestInfo()
immutable test159 = q{--- test159
	// Variadic template arg, 2 different instances (2 args)
	void fun[Args...](Args... args) {}
	void run(){
		fun(42, 4096);
		fun(50, 3000, 300000);
	}
};


@TestInfo()
immutable test160 = q{--- test160
	// Non-variadic + Variadic template arg
	void fun[Args...](u8 par, Args... args) {
		#assert(Args.length == 1);
	}
	void run(){ fun(42, 4096); }
};


@TestInfo()
immutable test161 = q{--- test161
	// Non-variadic + 0 Variadic template args
	void fun[Args...](u8 par, Args... args) {
		#assert(Args.length == 0);
	}
	void run(){ fun(42); }
};


@TestInfo()
immutable test162 = q{--- test162
	// Non-variadic + 0 Variadic template args
	void fun[Args...](u8 par) {
		#assert(Args.length == 0);
	}
	void run(){ fun(42); }
};


@TestInfo()
immutable test163 = q{--- test163
	// Non-variadic + 0 Variadic template args + default RT params
	void fun[Args...](u8 par1, Args... args, u8 par2 = 50) {
		#assert(Args.length == 0);
	}
	void run(){ fun(42); }
};


@TestInfo()
immutable test164 = q{--- test164
	// Non-variadic + 0 Variadic template args + non-default RT param
	void fun[Args...](u8 par1, Args... args, u8 par2) {
		#assert(Args.length == 0);
	}
	void run(){ fun(42, 50); }
};


@TestInfo()
immutable test165 = q{--- test165
	// Non-variadic + 1 Variadic template args + non-default RT param
	void fun[Args...](u8 par1, Args... args, u8 par2) {
		#assert(Args.length == 1);
	}
	void run(){ fun(42, 50, 60); }
};


@TestInfo()
immutable test166 = q{--- test166
	// Access variadic variable
	i32 fun[Args...](Args... args) {
		return args[0];
	}
	#assert(fun(42) == 42);
};


@TestInfo()
immutable test167 = q{--- test167
	// Access variadic variable
	i32 fun[Args...](Args... args) {
		alias arg0 = args[0];
		alias T0 = Args[0];
		T0 result = arg0;
		return result;
	}
	#assert(fun(42) == 42);
};


@TestInfo()
immutable test168 = q{--- test168
	// #foreach
	i64 fun[Args...](Args... args) {
		i64 sum = 0;
		#foreach(i, arg; args) {
			sum += arg;
		}
		return sum;
	}
	#assert(fun(1, 2, 3) == 6);
};


@TestInfo(&tester169, [
	HostSymbol("printStr", cast(void*)&external_print_string),
	HostSymbol("printInt", cast(void*)&external_print_i64_func)])
immutable test169 = q{--- test169
	// #foreach with multiple statements
	@extern(module, "host")
	void printStr(u8[]);
	@extern(module, "host")
	void printInt(i64 i);
	$alias selectPrintFunc($type T) {
		if ($isInteger(T))
			return printInt;
		if ($isSlice(T))
			return printStr;
		$compileError("Invalid type");
	}
	void write[Args...](Args... args) {
		#foreach(i, arg; args) {
			alias func = selectPrintFunc(Args[i]);
			func(arg);
		}
	}
	void run() {
		write("Hello", 42);
	}
};
void tester169(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(void)("run");
	run();
	assert(testSink.text == "Hello42");
}


@TestInfo()
immutable test170 = q{
--- test170_1
	// Use name from other module in func signature
	import test170_2;
	void fun(Struct s){}
--- test170_2
	struct Struct {}
};


@TestInfo()
immutable test171 = q{
--- test171
	// Variadic function parameter (2 expanded parameters)
	void fun[Args...](Args... args, Args... args2) {
		#assert(Args.length == 1);
	}
	void run(){ fun(42); }
--- <error>
test171:2:38: Error: Cannot have two expanded parameters
};


//@TestInfo()
//immutable test172 = q{--- test172
//	// No parenths variadic call
//	i64 fun[Args...](Args... args) {
//		return 0;
//	}
//	#assert(fun == 0);
//};


@TestInfo()
@(TargetOs.linux)
immutable test173 = q{--- test173
	// Extern attribute
	@extern(syscall, 60)
	void exit();
	void run() {
		exit();
	}
};


@TestInfo()
@(TargetOs.linux)
immutable test174 = q{--- test174
	// 2 Extern attributes
	@extern(syscall, 60)
	void exit();

	// var attributes
	@extern(syscall, 42)
	u8 var;

	// struct attribute
	@extern(syscall, 1)
	struct A {
		// member var attribute
		@extern(syscall, 2)
		u8 var;

		// member func attribute
		@extern(syscall, 3)
		void foo();
	}
};


@TestInfo(&tester_float_1)
immutable test_float_1 = q{--- test_float_1.vx
	// floats negation
	f32 neg_f32(f32 n) { return -n; }
	f64 neg_f64(f64 n) { return -n; }
};
void tester_float_1(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(float, float)("neg_f32")(2) == -2);
	assert(ctx.getFunctionPtr!(double, double)("neg_f64")(2) == -2);
}


@TestInfo(&tester_float_2)
immutable test_float_2 = q{--- test_float_2
	// floats: arithmetics and comparisons
	f32 f32_add(f32 a, f32 b) { return a + b; }
	f64 f64_add(f64 a, f64 b) { return a + b; }
	f32 f32_sub(f32 a, f32 b) { return a - b; }
	f64 f64_sub(f64 a, f64 b) { return a - b; }
	f32 f32_mul(f32 a, f32 b) { return a * b; }
	f64 f64_mul(f64 a, f64 b) { return a * b; }
	f32 f32_div(f32 a, f32 b) { return a / b; }
	f64 f64_div(f64 a, f64 b) { return a / b; }
	bool f32_lt(f32 a, f32 b) { return a < b; }
	bool f64_lt(f64 a, f64 b) { return a < b; }
	bool f32_le(f32 a, f32 b) { return a <= b; }
	bool f64_le(f64 a, f64 b) { return a <= b; }
	bool f32_eq(f32 a, f32 b) { return a == b; }
	bool f64_eq(f64 a, f64 b) { return a == b; }
	bool f32_ne(f32 a, f32 b) { return a != b; }
	bool f64_ne(f64 a, f64 b) { return a != b; }
	bool f32_gt(f32 a, f32 b) { return a > b; }
	bool f64_gt(f64 a, f64 b) { return a > b; }
	bool f32_ge(f32 a, f32 b) { return a >= b; }
	bool f64_ge(f64 a, f64 b) { return a >= b; }
};
void tester_float_2(ref TestContext ctx) {
	auto f32_add = ctx.getFunctionPtr!(float, float, float)("f32_add");
	assert(f32_add(42, 100) == 42f + 100f);
	auto f64_add = ctx.getFunctionPtr!(double, double, double)("f64_add");
	assert(f64_add(42, 100) == 42.0 + 100.0);

	auto f32_sub = ctx.getFunctionPtr!(float, float, float)("f32_sub");
	assert(f32_sub(42, 100) == 42f - 100f);
	auto f64_sub = ctx.getFunctionPtr!(double, double, double)("f64_sub");
	assert(f64_sub(42, 100) == 42.0 - 100.0);

	auto f32_mul = ctx.getFunctionPtr!(float, float, float)("f32_mul");
	assert(f32_mul(42, 100) == 42f * 100f);
	auto f64_mul = ctx.getFunctionPtr!(double, double, double)("f64_mul");
	assert(f64_mul(42, 100) == 42.0 * 100.0);

	auto f32_div = ctx.getFunctionPtr!(float, float, float)("f32_div");
	assert(f32_div(42, 100) == 42f / 100f);
	auto f64_div = ctx.getFunctionPtr!(double, double, double)("f64_div");
	assert(f64_div(42, 100) == 42.0 / 100.0);

	auto f32_lt = ctx.getFunctionPtr!(bool, float, float)("f32_lt");
	assert(f32_lt(42, 42) == (42f < 42f));
	assert(f32_lt(42, 100) == (42f < 100f));
	assert(f32_lt(100, 42) == (100f < 42f));
	assert(f32_lt(-100, -42) == (-100f < -42f));
	auto f64_lt = ctx.getFunctionPtr!(bool, double, double)("f64_lt");
	assert(f64_lt(42, 42) == (42.0 < 42.0));
	assert(f64_lt(42, 100) == (42.0 < 100.0));
	assert(f64_lt(100, 42) == (100.0 < 42.0));
	assert(f64_lt(-100, -42) == (-100.0 < -42.0));

	auto f32_le = ctx.getFunctionPtr!(bool, float, float)("f32_le");
	assert(f32_le(42, 42) == (42f <= 42f));
	assert(f32_le(42, 100) == (42f <= 100f));
	assert(f32_le(100, 42) == (100f <= 42f));
	assert(f32_le(-100, -42) == (-100f <= -42f));
	auto f64_le = ctx.getFunctionPtr!(bool, double, double)("f64_le");
	assert(f64_le(42, 42) == (42.0 <= 42.0));
	assert(f64_le(42, 100) == (42.0 <= 100.0));
	assert(f64_le(100, 42) == (100.0 <= 42.0));
	assert(f64_le(-100, -42) == (-100.0 <= -42.0));

	auto f32_eq = ctx.getFunctionPtr!(bool, float, float)("f32_eq");
	assert(f32_eq(42, 42) == (42f == 42f));
	assert(f32_eq(42, 100) == (42f == 100f));
	assert(f32_eq(100, 42) == (100f == 42f));
	assert(f32_eq(-100, -42) == (-100f == -42f));
	auto f64_eq = ctx.getFunctionPtr!(bool, double, double)("f64_eq");
	assert(f64_eq(42, 42) == (42.0 == 42.0));
	assert(f64_eq(42, 100) == (42.0 == 100.0));
	assert(f64_eq(100, 42) == (100.0 == 42.0));
	assert(f64_eq(-100, -42) == (-100.0 == -42.0));

	auto f32_ne = ctx.getFunctionPtr!(bool, float, float)("f32_ne");
	assert(f32_ne(42, 42) == (42f != 42f));
	assert(f32_ne(42, 100) == (42f != 100f));
	assert(f32_ne(100, 42) == (100f != 42f));
	assert(f32_ne(-100, -42) == (-100f != -42f));
	auto f64_ne = ctx.getFunctionPtr!(bool, double, double)("f64_ne");
	assert(f64_ne(42, 42) == (42.0 != 42.0));
	assert(f64_ne(42, 100) == (42.0 != 100.0));
	assert(f64_ne(100, 42) == (100.0 != 42.0));
	assert(f64_ne(-100, -42) == (-100.0 != -42.0));

	auto f32_gt = ctx.getFunctionPtr!(bool, float, float)("f32_gt");
	assert(f32_gt(42, 42) == (42f > 42f));
	assert(f32_gt(42, 100) == (42f > 100f));
	assert(f32_gt(100, 42) == (100f > 42f));
	assert(f32_gt(-100, -42) == (-100f > -42f));
	auto f64_gt = ctx.getFunctionPtr!(bool, double, double)("f64_gt");
	assert(f64_gt(42, 42) == (42.0 > 42.0));
	assert(f64_gt(42, 100) == (42.0 > 100.0));
	assert(f64_gt(100, 42) == (100.0 > 42.0));
	assert(f64_gt(-100, -42) == (-100.0 > -42.0));

	auto f32_ge = ctx.getFunctionPtr!(bool, float, float)("f32_ge");
	assert(f32_ge(42, 42) == (42f >= 42f));
	assert(f32_ge(42, 100) == (42f >= 100f));
	assert(f32_ge(100, 42) == (100f >= 42f));
	assert(f32_ge(-100, -42) == (-100f >= -42f));
	auto f64_ge = ctx.getFunctionPtr!(bool, double, double)("f64_ge");
	assert(f64_ge(42, 42) == (42.0 >= 42.0));
	assert(f64_ge(42, 100) == (42.0 >= 100.0));
	assert(f64_ge(100, 42) == (100.0 >= 42.0));
	assert(f64_ge(-100, -42) == (-100.0 >= -42.0));
}

/*
@TestInfo(&tester_float_3)
immutable test_float_3 = q{--- test_float_3.vx
	// floats compare with literal
	bool f32_lt_l(f32 b) { return 0  < b; }
	bool f64_lt_l(f64 b) { return 0  < b; }
	bool f32_le_l(f32 b) { return 0 <= b; }
	bool f64_le_l(f64 b) { return 0 <= b; }
	bool f32_eq_l(f32 b) { return 0 == b; }
	bool f64_eq_l(f64 b) { return 0 == b; }
	bool f32_ne_l(f32 b) { return 0 != b; }
	bool f64_ne_l(f64 b) { return 0 != b; }
	bool f32_gt_l(f32 b) { return 0  > b; }
	bool f64_gt_l(f64 b) { return 0  > b; }
	bool f32_ge_l(f32 b) { return 0 >= b; }
	bool f64_ge_l(f64 b) { return 0 >= b; }

	bool f32_lt_r(f32 a) { return a  < 0; }
	bool f64_lt_r(f64 a) { return a  < 0; }
	bool f32_le_r(f32 a) { return a <= 0; }
	bool f64_le_r(f64 a) { return a <= 0; }
	bool f32_eq_r(f32 a) { return a == 0; }
	bool f64_eq_r(f64 a) { return a == 0; }
	bool f32_ne_r(f32 a) { return a != 0; }
	bool f64_ne_r(f64 a) { return a != 0; }
	bool f32_gt_r(f32 a) { return a  > 0; }
	bool f64_gt_r(f64 a) { return a  > 0; }
	bool f32_ge_r(f32 a) { return a >= 0; }
	bool f64_ge_r(f64 a) { return a >= 0; }
};
void tester_float_3(ref TestContext ctx) {

}*/


@TestInfo(&tester_float_4)
immutable test_float_4 = q{--- test_float_4
	// floats: argument passing via stack and registers
	f32 f32_add(f32 p1, f32 p2, f32 p3, f32 p4, f32 p5, f32 p6, f32 p7, f32 p8, f32 p9) {
		return p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9;
	}
	f64 f64_add(f64 p1, f64 p2, f64 p3, f64 p4, f64 p5, f64 p6, f64 p7, f64 p8, f64 p9) {
		return p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9;
	}
	f32 f32_call(f32 p1, f32 p2, f32 p3, f32 p4, f32 p5, f32 p6, f32 p7, f32 p8, f32 p9) {
		return f32_add(p1, p2, p3, p4, p5, p6, p7, p8, p9);
	}
	f64 f64_call(f64 p1, f64 p2, f64 p3, f64 p4, f64 p5, f64 p6, f64 p7, f64 p8, f64 p9) {
		return f64_add(p1, p2, p3, p4, p5, p6, p7, p8, p9);
	}
};
void tester_float_4(ref TestContext ctx) {
	auto f32_add = ctx.getFunctionPtr!(float, float, float, float, float, float, float, float, float, float)("f32_add");
	assert(f32_add(1, 2, 3, 4, 5, 6, 7, 8, 9) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
	auto f64_add = ctx.getFunctionPtr!(double, double, double, double, double, double, double, double, double, double)("f64_add");
	assert(f64_add(1, 2, 3, 4, 5, 6, 7, 8, 9) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
	auto f32_call = ctx.getFunctionPtr!(float, float, float, float, float, float, float, float, float, float)("f32_call");
	assert(f32_call(1, 2, 3, 4, 5, 6, 7, 8, 9) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
	auto f64_call = ctx.getFunctionPtr!(double, double, double, double, double, double, double, double, double, double)("f64_call");
	assert(f64_call(1, 2, 3, 4, 5, 6, 7, 8, 9) == 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9);
}


@TestInfo(&tester177)
immutable test177 = q{--- test177
	f64 func_f32_to_f64(f32 a) { return a; } // f32 -> f64
	f32 func_f64_to_f32(f64 a) { return cast(f32)a; } // f64 -> f32

	f32 func_f32_zero1() { return 0; }
	f64 func_f64_zero1() { return 0; }
	f32 func_f32_zero2() { return 0.0; }
	f64 func_f64_zero2() { return 0.0; }
	f32 func_f32_const() { return 0.5; }
	f64 func_f64_const() { return 0.5; }

	f32 func_f32_const_mult(f32 a) { return a * cast(f32)0.5; }
	f64 func_f64_const_mult(f64 a) { return a * 0.5; }

	f32 func_const_f32_mult(f32 a) { return cast(f32)0.5 * a; }
	f64 func_const_f64_mult(f64 a) { return 0.5 * a; }

	 i8 func_f32_to__i8(f32 a) { return a; } // f32 ->  i8
	i16 func_f32_to_i16(f32 a) { return a; } // f32 -> i16
	i32 func_f32_to_i32(f32 a) { return a; } // f32 -> i32
	i64 func_f32_to_i64(f32 a) { return a; } // f32 -> i64

	 u8 func_f32_to__u8(f32 a) { return a; } // f32 ->  u8
	u16 func_f32_to_u16(f32 a) { return a; } // f32 -> u16
	u32 func_f32_to_u32(f32 a) { return a; } // f32 -> u32
	u64 func_f32_to_u64(f32 a) { return a; } // f32 -> u64 // TODO: more precise conversion

	 i8 func_f64_to__i8(f64 a) { return a; } // f64 ->  i8
	i16 func_f64_to_i16(f64 a) { return a; } // f64 -> i16
	i32 func_f64_to_i32(f64 a) { return a; } // f64 -> i32
	i64 func_f64_to_i64(f64 a) { return a; } // f64 -> i64

	 u8 func_f64_to__u8(f64 a) { return a; } // f64 ->  u8
	u16 func_f64_to_u16(f64 a) { return a; } // f64 -> u16
	u32 func_f64_to_u32(f64 a) { return a; } // f64 -> u32
	u64 func_f64_to_u64(f64 a) { return a; } // f64 -> u64 // TODO: more precise conversion

	f32 func__i8_to_f32( i8 a) { return a; } //  i8 -> f32
	f32 func_i16_to_f32(i16 a) { return a; } // i16 -> f32
	f32 func_i32_to_f32(i32 a) { return a; } // i32 -> f32
	f32 func_i64_to_f32(i64 a) { return a; } // i64 -> f32

	f32 func__u8_to_f32( u8 a) { return a; } //  u8 -> f32
	f32 func_u16_to_f32(u16 a) { return a; } // u16 -> f32
	f32 func_u32_to_f32(u32 a) { return a; } // u32 -> f32
	f32 func_u64_to_f32(u64 a) { return a; } // u64 -> f32 // TODO: more precise conversion

	f64 func__i8_to_f64( i8 a) { return a; } //  i8 -> f64
	f64 func_i16_to_f64(i16 a) { return a; } // i16 -> f64
	f64 func_i32_to_f64(i32 a) { return a; } // i32 -> f64
	f64 func_i64_to_f64(i64 a) { return a; } // i64 -> f64

	f64 func__u8_to_f64( u8 a) { return a; } //  u8 -> f64
	f64 func_u16_to_f64(u16 a) { return a; } // u16 -> f64
	f64 func_u32_to_f64(u32 a) { return a; } // u32 -> f64
	f64 func_u64_to_f64(u64 a) { return a; } // u64 -> f64 // TODO: more precise conversion
};
void tester177(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(double,  float)("func_f32_to_f64")(42.54f) == 42.54f.force);
	assert(ctx.getFunctionPtr!(float,  double)("func_f64_to_f32")(42.54) == 42.54f.force);

	assert(ctx.getFunctionPtr!(float)("func_f32_zero1")() == 0f.force);
	assert(ctx.getFunctionPtr!(double)("func_f64_zero1")() == 0);
	assert(ctx.getFunctionPtr!(float)("func_f32_zero2")() == 0f.force);
	assert(ctx.getFunctionPtr!(double)("func_f64_zero2")() == 0);
	assert(ctx.getFunctionPtr!(float)("func_f32_const")() == 0.5f.force);
	assert(ctx.getFunctionPtr!(double)("func_f64_const")() == 0.5);

	assert(ctx.getFunctionPtr!(float, float)("func_f32_const_mult")(1) == 0.5f.force);
	assert(ctx.getFunctionPtr!(double, double)("func_f64_const_mult")(1) == 0.5);

	assert(ctx.getFunctionPtr!(float, float)("func_const_f32_mult")(1) == 0.5f.force);
	assert(ctx.getFunctionPtr!(double, double)("func_const_f64_mult")(1) == 0.5);

	assert(ctx.getFunctionPtr!(  byte,  float)("func_f32_to__i8")(-127.6f.force) == -127);
	assert(ctx.getFunctionPtr!( short,  float)("func_f32_to_i16")(-127.6f.force) == -127);
	assert(ctx.getFunctionPtr!(   int,  float)("func_f32_to_i32")(-127.6f.force) == -127);
	assert(ctx.getFunctionPtr!(  long,  float)("func_f32_to_i64")(-127.6f.force) == -127);
	assert(ctx.getFunctionPtr!( ubyte,  float)("func_f32_to__u8")(255.6f.force) == 255);
	assert(ctx.getFunctionPtr!(ushort,  float)("func_f32_to_u16")(255.6f.force) == 255);
	assert(ctx.getFunctionPtr!(  uint,  float)("func_f32_to_u32")(255.6f.force) == 255);
	assert(ctx.getFunctionPtr!( ulong,  float)("func_f32_to_u64")(255.6f.force) == 255);

	assert(ctx.getFunctionPtr!(  byte, double)("func_f64_to__i8")(-127.6.force) == -127);
	assert(ctx.getFunctionPtr!( short, double)("func_f64_to_i16")(-127.6.force) == -127);
	assert(ctx.getFunctionPtr!(   int, double)("func_f64_to_i32")(-127.6.force) == -127);
	assert(ctx.getFunctionPtr!(  long, double)("func_f64_to_i64")(-127.6.force) == -127);
	assert(ctx.getFunctionPtr!( ubyte, double)("func_f64_to__u8")(255.6.force) == 255);
	assert(ctx.getFunctionPtr!(ushort, double)("func_f64_to_u16")(255.6.force) == 255);
	assert(ctx.getFunctionPtr!(  uint, double)("func_f64_to_u32")(255.6.force) == 255);
	assert(ctx.getFunctionPtr!( ulong, double)("func_f64_to_u64")(255.6.force) == 255);

	assert(ctx.getFunctionPtr!( float,   byte)("func__i8_to_f32")(-127) == -127.0f.force);
	assert(ctx.getFunctionPtr!( float,  short)("func_i16_to_f32")(-127) == -127.0f.force);
	assert(ctx.getFunctionPtr!( float,    int)("func_i32_to_f32")(-127) == -127.0f.force);
	assert(ctx.getFunctionPtr!( float,   long)("func_i64_to_f32")(-127) == -127.0f.force);
	assert(ctx.getFunctionPtr!( float,  ubyte)("func__u8_to_f32")(127) == 127.0f.force);
	assert(ctx.getFunctionPtr!( float, ushort)("func_u16_to_f32")(127) == 127.0f.force);
	assert(ctx.getFunctionPtr!( float,   uint)("func_u32_to_f32")(127) == 127.0f.force);
	assert(ctx.getFunctionPtr!( float,  ulong)("func_u64_to_f32")(127) == 127.0f.force);

	assert(ctx.getFunctionPtr!(double,   byte)("func__i8_to_f64")(-127) == -127.0);
	assert(ctx.getFunctionPtr!(double,  short)("func_i16_to_f64")(-127) == -127.0);
	assert(ctx.getFunctionPtr!(double,    int)("func_i32_to_f64")(-127) == -127.0);
	assert(ctx.getFunctionPtr!(double,   long)("func_i64_to_f64")(-127) == -127.0);
	assert(ctx.getFunctionPtr!(double,  ubyte)("func__u8_to_f64")(127) == 127.0);
	assert(ctx.getFunctionPtr!(double, ushort)("func_u16_to_f64")(127) == 127.0);
	assert(ctx.getFunctionPtr!(double,   uint)("func_u32_to_f64")(127) == 127.0);
	assert(ctx.getFunctionPtr!(double,  ulong)("func_u64_to_f64")(127) == 127.0);
}


@TestInfo(&tester178)
immutable test178 = q{--- test178
	struct Result { i32 sum; i32 maxflips; }
	// https://benchmarksgame-team.pages.debian.net/benchmarksgame/description/fannkuchredux.html
	Result fannkuch(i32 n) {
		i32 signx;
		i32 maxflips;
		i32 sum;
		i32 i;
		i32 j;
		i32 k;
		i32 q1;
		i32 flips;
		i32 qq;
		i32 t;
		i32 sx;
		i32 tt;
		i32[100] p;
		i32[100] q;
		i32[100] s;

		signx = 1;
		maxflips = 0;
		sum = 0;
		for (i=1; i<=n; ++i) {
			p[i-1] = i;
			q[i-1] = i;
			s[i-1] = i;
		}
		while (true) {
			q1 = p[1-1];
			if (q1 != 1) {
				for (i=2; i<=n; ++i) {
					q[i-1] = p[i-1];
				}
				flips = 1;
				while (true) {
					qq = q[q1-1];
					if (qq == 1) {
						sum += signx*flips;
						if (flips > maxflips) {
							maxflips = flips;
						}
						break;
					}
					q[q1-1] = q1;
					if (q1 >= 4) {
						i = 2;
						j = q1-1;
						while (true) {
							{
								i32 temp = q[i-1];
								q[i-1] = q[j-1];
								q[j-1] = temp;
							}
							++i;
							--j;
							if (i >= j) break;
						}
					}
					q1 = qq;
					++flips;
				}
			}
			if (signx == 1) {
				{
					i32 temp = p[2-1];
					p[2-1] = p[1-1];
					p[1-1] = temp;
				}
				signx = -1;
			}
			else {
				{
					i32 temp = p[2-1];
					p[2-1] = p[3-1];
					p[3-1] = temp;
				}
				signx = 1;
				for (i=3; i<=n; ++i) {
					sx = s[i-1];
					if (sx != 1) {
						s[i-1] = sx-1;
						break;
					}
					if (i == n) {
						return Result(sum, maxflips);
					}
					s[i-1] = i;
					tt = p[1-1];
					for (j=1; j<=i; ++j) {
						p[j-1] = p[j+1-1];
					}
					p[i+1-1] = tt;
				}
			}
		}
	}
};
void tester178(ref TestContext ctx) {
	static struct Result { int sum; int maxflips; }
	auto fannkuch = ctx.getFunctionPtr!(Result, int)("fannkuch");
	assert(fannkuch(3) == Result(2, 2));
	//assert(fannkuch(11) == Result(556355, 51)); // slow: ~2s
}

/*
// aggregate lowering bug
@TestInfo(&tester179)
immutable test179 = q{--- test179
	void use(i64, i64){}
	// floats: argument passing via stack and registers
	u8[] decompress(i32 res, u8[] data, u8[] outBuffer)
	{
		//i32 res = LZ4_decompress_safe(data.ptr, outBuffer.ptr, cast(i32)data.length, cast(i32)outBuffer.length);
		if (res < 0)
		{
			use(cast(i64)data.length, cast(i64)outBuffer.length);
			return null;
		}
		return outBuffer[0..res];
	}
};
void tester179(ref TestContext ctx) {
}
*/

@TestInfo()
// empty source
immutable test180 = q{--- test180};


@TestInfo()
immutable test181 = q{--- test181
	// alias as enum type
	alias ENetSocketType = i32;
	enum : ENetSocketType
	{
		ENET_SOCKET_TYPE_STREAM   = 1,
		ENET_SOCKET_TYPE_DATAGRAM = 2,
	}
};


@TestInfo()
immutable test182 = q{--- test182
	// store {{}*, i8}* s0, {{}*, i8} {zeroinit, 1}
	// isSameType gets to compare pointers to different structs, with different number of members
	Struct run1() {
		return Struct();
	}
	void run2() {
		Struct s;
	}
	struct GLFWwindow;
	struct Struct {
		GLFWwindow* window;
		bool isRunning = true;
	}
};


@TestInfo(&tester183)
immutable test183 = q{--- test183
	// operations on enum types
	enum F : u32 {
		f1 = 0b01,
		f2 = 0b10,
	}
	u32 run1() {
		return F.f1 | F.f2;
	}
	F run2() {
		return F.f1 | F.f2;
	}
	F run3(F a, F b) {
		return a | b;
	}
	F run4(F a) {
		return ~a;
	}
};
void tester183(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(uint)("run1")() == 3);
	assert(ctx.getFunctionPtr!(uint)("run2")() == 3);
	assert(ctx.getFunctionPtr!(uint, uint, uint)("run3")(1, 2) == 3);
	assert(ctx.getFunctionPtr!(uint, uint)("run4")(1) == ~1);
}


// packages
@TestInfo()
immutable test184 = q{--- test184/mod1.vx
	module test184.mod1;
--- test184/mod2.vx
	module test184.mod2;
};


@TestInfo()
immutable test185 = q{--- test185/mod1.vx
	module test185.mod1;
--- test185/mod2.vx
	module mod2;
};


@TestInfo(&tester186)
immutable test186 = q{--- test186/mod1.vx
	module test186.mod1;
	i32 test() { return 42; }
--- test186/mod2.vx
	module test186.mod2;
	import test186.mod1;
	i32 run() { return test(); }
};
void tester186(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(uint)("run")() == 42);
}

@TestInfo()
immutable test187 = q{--- test187
	// lexer bug with leading underscore
	i32 _import;
	i32 _module;
};


@TestInfo()
immutable test188 = q{--- test188
	// Union
	union VkClearColorValue {
		f32[4]  float32;
		i32[4]  int32;
		u32[4]  uint32;
	}
	VkClearColorValue getf32(f32[4] val) {
		VkClearColorValue result;
		result.float32 = val;
		return result;
	}
	VkClearColorValue geti32(i32[4] val) {
		VkClearColorValue result;
		result.int32 = val;
		return result;
	}
	VkClearColorValue getu32(u32[4] val) {
		VkClearColorValue result;
		result.uint32 = val;
		return result;
	}
};


@TestInfo(&tester189)
immutable test189 = q{--- test189
	// Call function pointer in member variable
	i32 get42(){ return 42; }
	struct DispatchDevice {
		i32 DestroyDevice() { return vkDestroyDevice(); }
		i32 function() vkDestroyDevice = &get42;
	}
	i32 run() {
		DispatchDevice dev;
		return dev.DestroyDevice();
	}
};
void tester189(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("run")() == 42);
}


@TestInfo()
immutable test190 = q{--- test190
	// Null enum initializer
	enum VK_NULL_HANDLE = null;
	void* run() { return VK_NULL_HANDLE; }
};


@TestInfo()
immutable test191 = q{--- test191
	// Unary op eval
	enum VK_ATTACHMENT_UNUSED = (~0);
};


@TestInfo(&tester192)
immutable test192 = q{--- test192
	// Default value of enum
	enum VkStructureType {
		VK_STRUCTURE_TYPE_APPLICATION_INFO     = 0,
		VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = 1,
	}
	struct VkBaseInStructure {
		VkStructureType     sType; // enum default value is needed
		VkBaseInStructure*  pNext; // recursive type
	}
	VkStructureType run() {
		VkBaseInStructure s;
		return s.sType;
	}
};
void tester192(ref TestContext ctx) {
	enum VkStructureType {
		VK_STRUCTURE_TYPE_APPLICATION_INFO     = 0,
		VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = 1,
	}
	assert(ctx.getFunctionPtr!(VkStructureType)("run")() == VkStructureType.VK_STRUCTURE_TYPE_APPLICATION_INFO);
}

@TestInfo()
immutable test193 = q{--- test193
	// circular dependency on enum type
	VkStructureType sType = VkStructureType.VK_STRUCTURE_TYPE_APPLICATION_INFO;
	enum VkStructureType {
		VK_STRUCTURE_TYPE_APPLICATION_INFO = 0,
	}
};


@TestInfo(&tester194)
immutable test194 = q{--- test194
	// #version
	#version(windows) {
		i32 run() { return 1; }
	}
	else #version(linux) {
		i32 run() { return 2; }
	}
	else #version(macos) {
		i32 run() { return 3; }
	}
	else #assert(false, "Unsupported OS");
};
void tester194(ref TestContext ctx) {
	     version(Windows) int result = 1;
	else version(linux)   int result = 2;
	else version(OSX)     int result = 3;
	assert(ctx.getFunctionPtr!int("run")() == result);
}


@TestInfo()
immutable test195 = q{--- test195
	// Circular deps on enum type
	bool run() {
		VkResult result;
		return result != VkResult.VK_SUCCESS;
	}
	enum VkResult {
		VK_SUCCESS = 0
	}
};


@TestInfo()
immutable test196 = q{--- test196
	u8*[] var;
};


@TestInfo()
immutable test197 = q{--- test197
	void run() {
		u8* ptr;
		foo(ptr);
	}
	void foo(void*){}
};

/*
@TestInfo()
immutable test198 = q{--- test198
	// TODO: Need to either deduplicate types, or special code for == on types, or $sameType
	bool isU8Ptr($type type) {
		return type == u8*;
	}
	#assert(isU8Ptr(u8*));
};
*/

@TestInfo(&tester199)
immutable test199 = q{--- test199
	alias i32_funType = i32 function();
	enum i32_funType funPtrEnum = &i32Fun;
	i32 i32Fun() { return 42; }
	i32 test2() {
		return funPtrEnum();
	}
};
void tester199(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("test2")() == 42);
}


@TestInfo()
immutable test200 = q{--- test200
	// Common type between enum and u32
	enum MDBX_env_flags_t : u32 {
		MDBX_SAFE_NOSYNC = 0x10000,
		MDBX_MAPASYNC = MDBX_SAFE_NOSYNC,
		MDBX_UTTERLY_NOSYNC = MDBX_SAFE_NOSYNC | 0x100000,
	}
};


@TestInfo()
immutable test201 = q{--- test201
	// Enum of structs
	enum Colors : Color {
		UNEXPLORED = Color(0, 0, 0),
		DARK_WALL = Color(0, 0, 100),
	}
	struct Color {
		u8 r;
		u8 g;
		u8 b;
	}
	// Cast from enum to base struct
	Color color = Colors.DARK_WALL;
};


@TestInfo()
immutable test202 = q{--- test202
	// aggregate lowering with phi function (two paths that return a big struct) and phi has constant argument
	u8[] fromStringz(u8* cString, u64 len) {
		if (cString == null) return null;
		return cString[0..len];
	}
};


@TestInfo()
immutable test203 = q{--- test203
	// String literal to u8* in ctfe
	u8* ptr = "hello";
	struct TracyLoc {
		u8* name;
		u8* func;
		u8* file;
		u32 line;
		u32 color;
	}
	TracyLoc zone_loc = TracyLoc("Zonename", "update", "main.vx", 81, 0xFF00FF);
};


@TestInfo()
immutable test204 = q{--- test204
	// aggregate lowering
	struct A {
		i32 val1;
		i32 val2;
	}
	A run(A res, bool cond, i32 val) {
		if (cond) res.val1 = val;
		return res;
	}
};


@TestInfo(&tester205)
immutable test205 = q{--- test205
	// aggregate lowering
	struct Struct {
		i32 member;
		void set42() {
			this.member += 42;
			member = 42;
		}
	}
	i32 testMethod5() {
		Struct[1] s;
		s[0].set42;
		return s[0].member;
	}
};
void tester205(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("testMethod5")() == 42);
}


@TestInfo()
immutable test206 = q{--- test206
	//
	struct Tile {
		bool blocked;
		bool block_sight;
	}

	struct GameMap {
		enum map_width = 40;
		enum map_height = 40;
		Tile[40] tiles;
	}

	void initialize_tiles(Tile[40]* map, i32 x) {
		(*map)[x].block_sight = true;
	}
	void initialize_tiles2(i32 x) {
		Tile[40] map;
		map[x].block_sight = true;
	}
};


@TestInfo(&tester207)
immutable test207 = q{--- test207
	// aggregate lowering
	u8[] fromStringz(u8* cString) {
		if (cString == null) return null;
		u8* cursor = cString;
		while(*cursor) ++cursor;
		u64 length = cast(u64)(cursor - cString);
		return cString[0..length];
	}
};
void tester207(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(Slice!(const(char)), const(char)*)("fromStringz")("test") == "test");
}


@TestInfo(&tester208, [HostSymbol("print", cast(void*)&test208_print)])
immutable test208 = q{--- test208
	// bug. Could not encode 32-bit offset because static data sections were too big.
	void run() {
		print("0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000");
		print("1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111");
		print("2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222");
		print("3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333");
		print("4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444");
		print("5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555");
		print("6666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666");
		print("7777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777");
		print("8888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888");
		print("9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999");
		print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
		print("BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB");
		print("CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC");
		print("DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD");
		print("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE");
		print("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
	}
	@extern(module, "host")
	void print(u8[]);
};
extern(C) void test208_print(SliceString str) {
	testSink.put(str.slice);
}
void tester208(ref TestContext ctx) {
	ctx.getFunctionPtr!(void)("run")();
	assert(testSink.text ==
		"0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"~
		"1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"~
		"2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222"~
		"3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333"~
		"4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444"~
		"5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555"~
		"6666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666"~
		"7777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777"~
		"8888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888"~
		"9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"~
		"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"~
		"BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"~
		"CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"~
		"DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"~
		"EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE"~
		"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
}


@TestInfo()
immutable test209 = q{--- test209
	//
	$alias selectPrintFunc($type T) {
		return printInt;
	}
	void printInt() {
		print("res");
	}
	void print[Args...](Args... args) {
		alias func = selectPrintFunc(u8[]);
		func();
	}
};


@TestInfo(&tester210)
immutable test210 = q{--- test210
	// struct size with forward reference
	u64 run() { return VkExtensionProperties.sizeof; } // 260

	VkExtensionProperties[11] extensions;
	struct VkExtensionProperties {
		u8[256] extensionName;
		u32 specVersion;
	}
};
void tester210(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("run")() == 260);
}


@TestInfo(&tester211)
immutable test211 = q{--- test211
	// code gen of `add reg global`
	VkExtensionProperties[11] extensions;
	struct VkExtensionProperties {
		u8[256] extensionName;
	}
	u8* get(i32 i) {
		return extensions[i].extensionName.ptr;
	}
};
void tester211(ref TestContext ctx) {
	auto get = ctx.getFunctionPtr!(ubyte*, int)("get");
	void* arenaPtr = ctx.driver.context.staticDataBuffer.bufPtr;
	assert(get(1) - get(0) == 256);
	assert(get(0) == arenaPtr);
	assert(get(1) == arenaPtr + 256);
}


@TestInfo()
immutable test212 = q{--- test212
	// Forward reference for variable init value -> enum init value -> enum member init value
	VkQueryType queryType;
	enum VkQueryType {
		VK_QUERY_TYPE_OCCLUSION = 0,
	}
};


/*@TestInfo()
immutable test213 = q{--- test213
	// Recursive type
	struct Qq
	{
		i32[Qq.sizeof]* a;
	}
};*/


@TestInfo(&tester214)
immutable test214 = q{--- test214
	// code gen of add rax, imm32 generated add eax, imm32
	VkExtensionProperties[11] extensions;
	struct VkExtensionProperties {
		u8[256] extensionName;
		u32 specVersion;
	}
	void* get(i32 i) {
		return &extensions[i].specVersion;
	}
};
void tester214(ref TestContext ctx) {
	auto get = ctx.getFunctionPtr!(void*, int)("get");
	void* arenaPtr = ctx.driver.context.staticDataBuffer.bufPtr;
	void* ptr = get(0);

	assert(ptr == arenaPtr + 256);
	assert(get(1) - get(0) == 260);
}


@TestInfo()
immutable test215 = q{--- test215
	// UFCS for ptr type
	VkExtensionProperties[11] extensions;
	struct VkExtensionProperties {
		u8[256] extensionName;
		u32 specVersion;
	}
	void run() {
		extensions[0].extensionName.ptr.fromStringz;
	}
	void fromStringz(u8*){}
};


@TestInfo()
immutable test216 = q{--- test216
	// $baseOf
	#assert($baseOf(u8[]) == u8);
	#assert($baseOf(u8*) == u8);
	#assert($baseOf(u8[10]) == u8);
	#assert($baseOf(u16[10]) != u8);
};


@TestInfo()
immutable test217 = q{--- test217
	// doIfti missing ref on AstNodes, causing UFCS call that replaced node to use stale array
	struct VkLayerProperties {
		u8[1] description;
	}
	u8[] fromStringz(u8* cString) { return null; }
	void println[Args...](Args... args) {}
	void run() {
		VkLayerProperties layer;
		println(layer.description.ptr.fromStringz);
		println("", layer.description.ptr.fromStringz);
	}
};

@TestInfo(&tester218)
immutable test218 = q{--- test218
	// indexing of call
	struct App {
		u8*[1] validationLayers;
		void init() {
			validationLayers[0] = "VK_LAYER_KHRONOS_validation";
		}
		u8** get() {
			return validationLayers.ptr;
		}
	}
	u8* run() {
		App app;
		app.init;
		return app.get()[0];
	}
};
void tester218(ref TestContext ctx) {
	import std.string : fromStringz;
	auto run = ctx.getFunctionPtr!(char*)("run");
	assert(run().fromStringz == "VK_LAYER_KHRONOS_validation");
}

@TestInfo(&tester219)
immutable test219 = q{--- test219
	// Assigning struct member array
	struct S {
		u8** ptr;
	}
	struct App {
		u8*[1] validationLayers;
		void init() {
			validationLayers[0] = "VK_LAYER_KHRONOS_validation"; // here
		}
		S get() {
			return S(validationLayers.ptr); // also test access via .ptr
		}
	}
	S run() {
		App app;
		app.init;
		return app.get();
	}
};
void tester219(ref TestContext ctx) {
	import std.string : fromStringz;
	auto run = ctx.getFunctionPtr!(char**)("run");
	assert((*run()).fromStringz == "VK_LAYER_KHRONOS_validation");
}


@TestInfo(&tester220)
immutable test220 = q{--- test220
	// slicing of static array bug
	struct App {
		u8*[1] validationLayers;
		void init() {
			validationLayers[0] = "VK_LAYER_KHRONOS_validation";
		}
		u8* get() {
			return accessor(validationLayers); // here
		}
	}
	u8* accessor(u8*[] layers) { return layers[0]; }
	u8* run() {
		App app;
		app.init;
		return app.get();
	}
};
void tester220(ref TestContext ctx) {
	import std.string : fromStringz;
	auto run = ctx.getFunctionPtr!(char*)("run");
	assert(run().fromStringz == "VK_LAYER_KHRONOS_validation");
}


@TestInfo()
immutable test221 = q{--- test221
	// store function pointer into struct member
	// `debugCallback` IR type was not generated because it is located after `run`
	struct S {
		void function() funcPtr;
	}
	void run() {
		S createInfo;
		createInfo.funcPtr = &debugCallback;
	}
	void debugCallback() {}
};


@TestInfo(null, [HostSymbol("ExitProcess", cast(void*)&external_noop)])
immutable test222 = q{--- test222
	/// @extern(module)
	@extern(module, "host")
	noreturn ExitProcess(u32 uExitCode);
};


@TestInfo(null, [HostSymbol("ExitProcess", cast(void*)&external_noop)])
immutable test223 = q{
	/// Issue #16. `TypeCheckState.curFunc` was used in func signature, while it pointed to the caller function.
	void main() {
		ExitProcess(0);
	}
	@extern(module, "host")
	noreturn ExitProcess(u32 uExitCode);
};


@TestInfo()
immutable test224 = q{--- test224
	/// Bug with circular dependency
	void main() {
		Client client;
		client.run(); // 1 call
	}
	struct Client { // 4 Client
		void run() {} // 2 func signature, 3 Client* this, 5 run function, 6 func signature
	}
};


@TestInfo()
immutable test225 = q{--- test225
	/// Feature: `@attr:`
	/// Should not give an error if function with the body has broadcasted @extern attribute
	@extern(module, "modA"): // both are broadcasted
	@extern(module, "modB"): // each attribute with its own :
	void withAttrib(){}
};


@TestInfo()
immutable test226 = q{--- test226
	/// Feature: `@attr:`
	@extern(module, "modA")  // both are broadcasted
	@extern(module, "modB"): // with single :
	void withAttrib(){}
};


@TestInfo(null, [HostSymbol("withAttribA", cast(void*)&external_noop), HostSymbol("withAttribB", cast(void*)&external_noop)])
immutable test227 = q{--- test227
	/// Feature: `@attr:`
	/// Multiple functions with the same attribute
	@extern(module, "host"):
	void withAttribA();
	void withAttribB();
};


@TestInfo()
immutable test227_1 = q{--- test227_1
	/// No definitions after @:
	@extern(module, "modB"):
};


/* // TODO:
immutable
	/// Feature: `@attr:`
	/// Overriding with later attribute
	@extern(module, "host1"):
	void withAttribA();
	@extern(module, "host2"):
	void withAttribB();
};
immutable
	/// Feature: `@attr:`
	/// Overriding with later attribute
	@extern(module, "host1"):
	void withAttribA();
	@extern(module, "host2")
	void withAttribB();
};
*/


@TestInfo()
immutable test228 = q{--- test228
	/// Take alias of global var
	u32 hey;
	$alias getHey() {
		return hey;
	}

	void run() {
		alias u = getHey();
		foo(u);
	}

	void foo(u32 a){}
};


@TestInfo()
immutable test229 = q{--- test229
	/// Attribute scope
	@extern(module, "host") {
		void foo(u32 a){}
	}
	// shouldn't get the attribute. TODO: need ability to assert that.
	void bar(u32 a){}
};


@TestInfo()
immutable test230 = q{--- test230
	/// All call variants
	struct S {
		void methodCaller() {
			method;
			method();

			methodT;
			methodT();

			methodT[];
			methodT[]();
		}
		void method() {}
		void methodT[]() {}
	}

	void funcCaller() {
		S s;

		s.method;
		s.method();

		s.methodT;
		s.methodT();

		s.methodT[];
		s.methodT[]();

		func;
		func();

		funcT;
		funcT();

		funcT[];
		funcT[]();

		// s.funcT; // TODO
		// s.funcT(); // TODO
		// s.funcT[](); // TODO
	}

	void func() {}
	void funcT[]() {}

	// TODO:
	// UFCS
	// call func ptr variable
	// call through alias
	// call member functions through pointer
};


@TestInfo(&tester231)
immutable test231 = q{--- test231
	/// sign extension of 64-bit constants
	i64 run() {
		return -1;
	}
};
void tester231(ref TestContext ctx) {
	auto run = ctx.getFunctionPtr!(long)("run");
	assert(run() == -1);
}


/*@TestInfo()
immutable test232 = q{--- test232
	/// TODO
	/// Semantic ordering bug. Trying to access NUM's type before it was computed
	void run() {
		for(u32 i = 0; i < S.NUM; ++i) {
		}
	}
	struct S {
		enum NUM = 2;
	}
};*/


/* @TestInfo()
immutable test233 = q{--- test233
	/// TODO
	/// Semantic ordering bug
	void run() {
		A a;
		a.b.foo();
	}
	struct A {
		B[] b;
	}
	struct B[] {
		void foo(){}
	}
};*/


@TestInfo()
immutable test234 = q{--- test234
	/// IR gen bug. Address of `arr` must be taken and passed as first parameter of `data`. Instead it is passed by value.
	struct Array
	{
		u64* ptr;
		u64 length;
		u64[] data() { return ptr[0..length]; }
	}
	u64 get(Array arr) {
		return arr.data[0];
	}
};


@TestInfo()
immutable test235 = q{--- test235
	/// Address of this parameter is taken, which forces it to be allocated on the stack
	/// Then member assignment doesn't load the stored pointer
	/// lowerToMember missed `memberNode.flags |= MemberExprFlags.needsDeref;` line
	/// Without it no deref node was generated
	struct S {
		bool b;
		void method() {
			b = false;
			receive(&this);
		}
	}
	void receive(S**){}
};


@TestInfo()
immutable test236 = q{--- test236
	// Bug #27. be.reg_alloc.move_solver(82): ICE: Assertion failure: Second write to rax detected

	// First DCE removes some instructions. This leads to some phi functions not having any users.
	// This makes liveness analysis emit empty range for those vregs ([102; 102) and [144; 144) for example).
	// Then register allocator allocates two intervals to the same register, because
	// overlap of [144; 144) and [144; 148) is not detected and they are both allocated to eax.
	// Then move solver sees 2 writes to the same register and reports an error.

	f64 to_f64(u8* s) {
		f64 a = 0.0;
		i32 c;
		i32 e = 0;
		c = *s++; // This expression will give use the "illegal hardware instruction" issue if I un-comment it
		while (c != '\0' && is_digit(c)) {
			a = a * 10.0 + (c - '0');
		}

		if (c == '.') {
			c = *s++; // However, the same expression doesn't give the error here
			while (c != '\0' && is_digit(c)) {
				a = a * 10.0 + (c - '0');
				e = e - 1;
				c = *s++; // And here too!
			}
		}

		if (c == 'e' || c == 'E') {
			i32 sign = 1;
			i32 i = 0;
			c = *s++;
			if (c == '+') c = *s++;
			else if (c == '-') {
				c = *s++;
				sign = -1;
			}

			while (is_digit(c)) {
				i = i * 10 + (c - '0');
				c = *s++;
			}
			e += i*sign;
		}
		return a;
	}

	bool is_digit(i32 c) {
		return c >= '0' && c <= '9';
	}
};


@TestInfo()
immutable test237 = q{--- test237
	// Bug #27. be.emit_mc_amd64(257): ICE: Assertion failure: reg size mismatch 2 != 3
	f64 to_f64(u8* s) {
		f64 a = 0.0;
		i32 c;
		i32 e = 0;

		c = *s++; // This expression will give use the "illegal hardware instruction" issue if I un-comment it
		while (c != '\0' && is_digit(c)) {
			a = a * 10.0 + (c - '0');
		}

		if (c == '.') {
			c = *s++; // However, the same expression doesn't give the error here
			while (c != '\0' && is_digit(c)) {
				a = a * 10.0 + (c - '0');
				e = e - 1;
				c = *s++; // And here too!
			}
		}

		if (c == 'e' || c == 'E') {
			i32 sign = 1;
			i32 i = 0;
			c = *s++;
			if (c == '+') c = *s++;
			else if (c == '-') {
				c = *s++;
				sign = -1;
			}

			while (is_digit(c)) {
			  i = i * 10 + (c - '0');
			  c = *s++;
			}
			e += i*sign;
		}

		while (e > 0) {
			a *= 10.0;
			e--;
		}

		while (e < 0) {
			a *= 0.1;
			e++;
		}

		return a;
	}

	bool is_digit(i32 c) {
		return c >= '0' && c <= '9';
	}
};


@TestInfo()
immutable test238 = q{--- test238
	/// Bug #28. Missing handling of globals, functions in move solver
	u8* retGlobal(i32 num) {
		if (num == 0) return "0";
		return "1";
	}
	void function() regFunc(i32 num) {
		if (num == 0) return &fun1;
		return &fun2;
	}
	void fun1(){}
	void fun2(){}
};


@TestInfo()
immutable test239 = q{--- test239
	// circular dependency on enum type
	VkSharingMode mode = VkSharingMode.VK_SHARING_MODE_CONCURRENT;
	enum VkSharingMode {
		VK_SHARING_MODE_CONCURRENT = 1,
		VK_SHARING_MODE_END_RANGE  = VK_SHARING_MODE_CONCURRENT,
	}
};


@TestInfo()
immutable test240 = q{--- test240
	// Recursive type
	struct S {
		S[] nested1;
		S* nested2;
	}
};


@TestInfo(&tester241)
immutable test241 = q{--- test241
	// Compile-time int <-> float
	f32  i8_to_f32() { return cast(f32)cast( i8)-100; }
	f32 i16_to_f32() { return cast(f32)cast(i16)-100; }
	f32 i32_to_f32() { return cast(f32)cast(i32)-100; }
	f32 i64_to_f32() { return cast(f32)cast(i64)-100; }

	f32  u8_to_f32() { return cast(f32)cast( u8)100; }
	f32 u16_to_f32() { return cast(f32)cast(u16)100; }
	f32 u32_to_f32() { return cast(f32)cast(u32)100; }
	f32 u64_to_f32() { return cast(f32)cast(u64)100; }

	f64  i8_to_f64() { return cast(f64)cast( i8)-100; }
	f64 i16_to_f64() { return cast(f64)cast(i16)-100; }
	f64 i32_to_f64() { return cast(f64)cast(i32)-100; }
	f64 i64_to_f64() { return cast(f64)cast(i64)-100; }

	f64  u8_to_f64() { return cast(f64)cast( u8)100; }
	f64 u16_to_f64() { return cast(f64)cast(u16)100; }
	f64 u32_to_f64() { return cast(f64)cast(u32)100; }
	f64 u64_to_f64() { return cast(f64)cast(u64)100; }


	 i8 f32_to__i8() { return cast( i8)cast(f32)-100.5; }
	i16 f32_to_i16() { return cast(i16)cast(f32)-100.5; }
	i32 f32_to_i32() { return cast(i32)cast(f32)-100.5; }
	i64 f32_to_i64() { return cast(i64)cast(f32)-100.5; }

	 u8 f32_to__u8() { return cast( u8)cast(f32)100.5; }
	u16 f32_to_u16() { return cast(u16)cast(f32)100.5; }
	u32 f32_to_u32() { return cast(u32)cast(f32)100.5; }
	u64 f32_to_u64() { return cast(u64)cast(f32)100.5; }

	 i8 f64_to__i8() { return cast( i8)cast(f64)-100.5; }
	i16 f64_to_i16() { return cast(i16)cast(f64)-100.5; }
	i32 f64_to_i32() { return cast(i32)cast(f64)-100.5; }
	i64 f64_to_i64() { return cast(i64)cast(f64)-100.5; }

	 u8 f64_to__u8() { return cast( u8)cast(f64)100.5; }
	u16 f64_to_u16() { return cast(u16)cast(f64)100.5; }
	u32 f64_to_u32() { return cast(u32)cast(f64)100.5; }
	u64 f64_to_u64() { return cast(u64)cast(f64)100.5; }
};
void tester241(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!( float)( "i8_to_f32")() == -100);
	assert(ctx.getFunctionPtr!( float)("i16_to_f32")() == -100);
	assert(ctx.getFunctionPtr!( float)("i32_to_f32")() == -100);
	assert(ctx.getFunctionPtr!( float)("i64_to_f32")() == -100);

	assert(ctx.getFunctionPtr!( float)( "u8_to_f32")() == 100);
	assert(ctx.getFunctionPtr!( float)("u16_to_f32")() == 100);
	assert(ctx.getFunctionPtr!( float)("u32_to_f32")() == 100);
	assert(ctx.getFunctionPtr!( float)("u64_to_f32")() == 100);

	assert(ctx.getFunctionPtr!(double)( "i8_to_f64")() == -100);
	assert(ctx.getFunctionPtr!(double)("i16_to_f64")() == -100);
	assert(ctx.getFunctionPtr!(double)("i32_to_f64")() == -100);
	assert(ctx.getFunctionPtr!(double)("i64_to_f64")() == -100);

	assert(ctx.getFunctionPtr!(double)( "u8_to_f64")() == 100);
	assert(ctx.getFunctionPtr!(double)("u16_to_f64")() == 100);
	assert(ctx.getFunctionPtr!(double)("u32_to_f64")() == 100);
	assert(ctx.getFunctionPtr!(double)("u64_to_f64")() == 100);

	assert(ctx.getFunctionPtr!(  byte)("f32_to__i8")() == -100);
	assert(ctx.getFunctionPtr!( short)("f32_to_i16")() == -100);
	assert(ctx.getFunctionPtr!(   int)("f32_to_i32")() == -100);
	assert(ctx.getFunctionPtr!(  long)("f32_to_i64")() == -100);

	assert(ctx.getFunctionPtr!( ubyte)("f32_to__u8")() == 100);
	assert(ctx.getFunctionPtr!(ushort)("f32_to_u16")() == 100);
	assert(ctx.getFunctionPtr!(  uint)("f32_to_u32")() == 100);
	assert(ctx.getFunctionPtr!( ulong)("f32_to_u64")() == 100);

	assert(ctx.getFunctionPtr!(  byte)("f64_to__i8")() == -100);
	assert(ctx.getFunctionPtr!( short)("f64_to_i16")() == -100);
	assert(ctx.getFunctionPtr!(   int)("f64_to_i32")() == -100);
	assert(ctx.getFunctionPtr!(  long)("f64_to_i64")() == -100);

	assert(ctx.getFunctionPtr!( ubyte)("f64_to__u8")() == 100);
	assert(ctx.getFunctionPtr!(ushort)("f64_to_u16")() == 100);
	assert(ctx.getFunctionPtr!(  uint)("f64_to_u32")() == 100);
	assert(ctx.getFunctionPtr!( ulong)("f64_to_u64")() == 100);
}


@TestInfo(&tester242)
immutable test242 = q{--- test242
	// Compile-time binary float ops
	bool f32_gt()  { return cast(f32)100.0 >  cast(f32)50.0; }
	bool f32_ge()  { return cast(f32)100.0 >= cast(f32)50.0; }
	bool f32_lt()  { return cast(f32)100.0 <  cast(f32)50.0; }
	bool f32_le()  { return cast(f32)100.0 <= cast(f32)50.0; }

	f32 f32_add() { return cast(f32)100.0 + cast(f32)100.0; }
	f32 f32_sub() { return cast(f32)100.0 - cast(f32)100.0; }
	f32 f32_mul() { return cast(f32)100.0 * cast(f32)100.0; }
	f32 f32_div() { return cast(f32)100.0 / cast(f32)100.0; }

	bool f64_gt()  { return cast(f64)100.0 >  cast(f64)50.0; }
	bool f64_ge()  { return cast(f64)100.0 >= cast(f64)50.0; }
	bool f64_lt()  { return cast(f64)100.0 <  cast(f64)50.0; }
	bool f64_le()  { return cast(f64)100.0 <= cast(f64)50.0; }

	f64 f64_add() { return cast(f64)100.0 + cast(f64)100.0; }
	f64 f64_sub() { return cast(f64)100.0 - cast(f64)100.0; }
	f64 f64_mul() { return cast(f64)100.0 * cast(f64)100.0; }
	f64 f64_div() { return cast(f64)100.0 / cast(f64)100.0; }
};
void tester242(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(bool)("f32_gt")() == true);
	assert(ctx.getFunctionPtr!(bool)("f32_ge")() == true);
	assert(ctx.getFunctionPtr!(bool)("f32_lt")() == false);
	assert(ctx.getFunctionPtr!(bool)("f32_le")() == false);

	assert(ctx.getFunctionPtr!(float)("f32_add")() == 100 + 100);
	assert(ctx.getFunctionPtr!(float)("f32_sub")() == 100 - 100);
	assert(ctx.getFunctionPtr!(float)("f32_mul")() == 100 * 100);
	assert(ctx.getFunctionPtr!(float)("f32_div")() == 100 / 100);

	assert(ctx.getFunctionPtr!(bool)("f64_gt")() == true);
	assert(ctx.getFunctionPtr!(bool)("f64_ge")() == true);
	assert(ctx.getFunctionPtr!(bool)("f64_lt")() == false);
	assert(ctx.getFunctionPtr!(bool)("f64_le")() == false);

	assert(ctx.getFunctionPtr!(double)("f64_add")() == 100 + 100);
	assert(ctx.getFunctionPtr!(double)("f64_sub")() == 100 - 100);
	assert(ctx.getFunctionPtr!(double)("f64_mul")() == 100 * 100);
	assert(ctx.getFunctionPtr!(double)("f64_div")() == 100 / 100);
}


@TestInfo(&tester243)
immutable test243 = q{--- test243
	// remainder
	i8  rem_i8__fold() { return cast(i8) -32 % 10; }
	i16 rem_i16_fold() { return cast(i16)-32 % 10; }
	i32 rem_i32_fold() { return cast(i32)-32 % 10; }
	i64 rem_i64_fold() { return cast(i64)-32 % 10; }

	u8  rem_u8__fold() { return cast(u8) 32 % 10; }
	u16 rem_u16_fold() { return cast(u16)32 % 10; }
	u32 rem_u32_fold() { return cast(u32)32 % 10; }
	u64 rem_u64_fold() { return cast(u64)32 % 10; }

	i8  rem_i8__con(i8  a) { return a % 10; }
	i16 rem_i16_con(i16 a) { return a % 10; }
	i32 rem_i32_con(i32 a) { return a % 10; }
	i64 rem_i64_con(i64 a) { return a % 10; }

	u8  rem_u8__con(u8  a) { return a % 10; }
	u16 rem_u16_con(u16 a) { return a % 10; }
	u32 rem_u32_con(u32 a) { return a % 10; }
	u64 rem_u64_con(u64 a) { return a % 10; }

	i8  rem_i8__var(i8  a, i8  b) { return a % b; }
	i16 rem_i16_var(i16 a, i16 b) { return a % b; }
	i32 rem_i32_var(i32 a, i32 b) { return a % b; }
	i64 rem_i64_var(i64 a, i64 b) { return a % b; }

	u8  rem_u8__var(u8  a, u8  b) { return a % b; }
	u16 rem_u16_var(u16 a, u16 b) { return a % b; }
	u32 rem_u32_var(u32 a, u32 b) { return a % b; }
	u64 rem_u64_var(u64 a, u64 b) { return a % b; }
};
void tester243(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(  byte)("rem_i8__fold")() == -32 % 10);
	assert(ctx.getFunctionPtr!( short)("rem_i16_fold")() == -32 % 10);
	assert(ctx.getFunctionPtr!(   int)("rem_i32_fold")() == -32 % 10);
	assert(ctx.getFunctionPtr!(  long)("rem_i64_fold")() == -32 % 10);

	assert(ctx.getFunctionPtr!( ubyte)("rem_u8__fold")() == 32 % 10);
	assert(ctx.getFunctionPtr!(ushort)("rem_u16_fold")() == 32 % 10);
	assert(ctx.getFunctionPtr!(  uint)("rem_u32_fold")() == 32 % 10);
	assert(ctx.getFunctionPtr!( ulong)("rem_u64_fold")() == 32 % 10);

	assert(ctx.getFunctionPtr!(  byte,  byte)("rem_i8__con")(-32) == -32 % 10);
	assert(ctx.getFunctionPtr!( short, short)("rem_i16_con")(-32) == -32 % 10);
	assert(ctx.getFunctionPtr!(   int,   int)("rem_i32_con")(-32) == -32 % 10);
	assert(ctx.getFunctionPtr!(  long,  long)("rem_i64_con")(-32) == -32 % 10);

	assert(ctx.getFunctionPtr!( ubyte, ubyte)("rem_u8__con")(32) == 32 % 10);
	assert(ctx.getFunctionPtr!(ushort,ushort)("rem_u16_con")(32) == 32 % 10);
	assert(ctx.getFunctionPtr!(  uint,  uint)("rem_u32_con")(32) == 32 % 10);
	assert(ctx.getFunctionPtr!( ulong, ulong)("rem_u64_con")(32) == 32 % 10);

	assert(ctx.getFunctionPtr!(  byte,  byte,  byte)("rem_i8__var")(-32, 10) == -32 % 10);
	assert(ctx.getFunctionPtr!( short, short, short)("rem_i16_var")(-32, 10) == -32 % 10);
	assert(ctx.getFunctionPtr!(   int,   int,   int)("rem_i32_var")(-32, 10) == -32 % 10);
	assert(ctx.getFunctionPtr!(  long,  long,  long)("rem_i64_var")(-32, 10) == -32 % 10);

	assert(ctx.getFunctionPtr!( ubyte, ubyte, ubyte)("rem_u8__var")(32, 10) == 32 % 10);
	assert(ctx.getFunctionPtr!(ushort,ushort,ushort)("rem_u16_var")(32, 10) == 32 % 10);
	assert(ctx.getFunctionPtr!(  uint,  uint,  uint)("rem_u32_var")(32, 10) == 32 % 10);
	assert(ctx.getFunctionPtr!( ulong, ulong, ulong)("rem_u64_var")(32, 10) == 32 % 10);
}


@TestInfo()
immutable test244 = q{--- test244
	// small int literals are implicitly i32
	// but they should auto-cast if necessary

	i8  lit_i8()  { return 1; }
	i16 lit_i16() { return 1; }
	i32 lit_i32() { return 1; }
	i64 lit_i64() { return 1; }

	u8  lit_u8()  { return 1; }
	u16 lit_u16() { return 1; }
	u32 lit_u32() { return 1; }
	u64 lit_u64() { return 1; }
};


@TestInfo()
immutable test245 = r"--- test245
	// literal type suffix

	f32 lit_f32a() { return 1f32; }
	f64 lit_f64a() { return 1f64; }
	f32 lit_f32b() { return 1.0f32; }
	f64 lit_f64b() { return 1.0f64; }

	i8  lit_i8_() { return 1i8; }
	i16 lit_i16() { return 1i16; }
	i32 lit_i32() { return 1i32; }
	i64 lit_i64() { return 1i64; }

	u8  lit_u8_() { return 1u8; }
	u16 lit_u16() { return 1u16; }
	u32 lit_u32() { return 1u32; }
	u64 lit_u64() { return 1u64; }

	i8  lit_i8_b() { return 0b1i8; }
	i16 lit_i16b() { return 0b1i16; }
	i32 lit_i32b() { return 0b1i32; }
	i64 lit_i64b() { return 0b1i64; }

	u8  lit_u8_b() { return 0b1u8; }
	u16 lit_u16b() { return 0b1u16; }
	u32 lit_u32b() { return 0b1u32; }
	u64 lit_u64b() { return 0b1u64; }

	i8  lit_i8_x() { return 0x1i8; }
	i16 lit_i16x() { return 0x1i16; }
	i32 lit_i32x() { return 0x1i32; }
	i64 lit_i64x() { return 0x1i64; }

	u8  lit_u8_x() { return 0x1u8; }
	u16 lit_u16x() { return 0x1u16; }
	u32 lit_u32x() { return 0x1u32; }
	u64 lit_u64x() { return 0x1u64; }
";


@TestInfo(&tester246)
immutable test246 = q{--- test246
	struct MonoTime {
		i64 ticks;
		Duration sub(MonoTime other) {
			return Duration(ticks - other.ticks);
		}
	}
	struct Duration {
		i64 ticks;
	}
	i64 run(i64 a, i64 b) {
		MonoTime ma = MonoTime(a);
		MonoTime mb = MonoTime(b);
		return ma.sub(mb).ticks;
	}
};
void tester246(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(long, long, long)("run")(100, 40) == 60);
}

/*
@TestInfo(&tester247)
immutable test247 = q{--- test247
	struct MonoTime {
		i64 ticks;
		Duration sub(MonoTime other) {
			return Duration(ticks - other.ticks);
		}
	}
	struct Duration {
		i64 ticks;
	}
	i64 run(i64 a, i64 b) {
		return MonoTime(a).sub(MonoTime(b)).ticks;
	}
};
void tester247(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(long, long, long)("run")(100, 40) == 60);
}*/


@TestInfo(&tester248)
immutable test248 = q{--- test248.vx
	// special keywords
	u64 getLine() { return __LINE__; }
	u8[] getFile() { return __FILE__; }
	u8[] getFunctionName() { return __FUNCTION_NAME__; }
	u8[] getModuleName() { return __MODULE_NAME__; }
};
void tester248(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("getLine")() == 2);
	assert(ctx.getFunctionPtr!(SliceString)("getFile")() == "test248.vx");
	assert(ctx.getFunctionPtr!(SliceString)("getFunctionName")() == "getFunctionName");
	assert(ctx.getFunctionPtr!(SliceString)("getModuleName")() == "test248");
}


@TestInfo(&tester249)
immutable test249 = q{
--- test249_1.vx
	// special keywords as a default argument
	u64  funLine(u64 line = __LINE__) { return line; }
	u8[] funFile(u8[] file = __FILE__) { return file; }
	u8[] funFunctionName(u8[] func = __FUNCTION_NAME__) { return func; }
	u8[] funModuleName(u8[] mod = __MODULE_NAME__) { return mod; }
--- test249_2.vx
	import test249_1;

	u64 getLine() { return funLine; }
	u8[] getFile() { return funFile; }
	u8[] getFunctionName() { return funFunctionName; }
	u8[] getModuleName() { return funModuleName; }
};
void tester249(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("getLine")() == 3);
	assert(ctx.getFunctionPtr!(SliceString)("getFile")() == "test249_2.vx");
	assert(ctx.getFunctionPtr!(SliceString)("getFunctionName")() == "getFunctionName");
	assert(ctx.getFunctionPtr!(SliceString)("getModuleName")() == "test249_2");
}


@TestInfo(&tester250)
immutable test250 = q{--- test250.vx
	// @static variables inside functions and structs
	u64 incAndReturn1() {
		@static u64 var = 42;
		++var;
		return var;
	}
	// broadcasted variant
	u64 incAndReturn2() {
		@static:
		u64 var1 = 10;
		u64 var2 = 42;
		++var1;
		++var2;
		return var1 + var2;
	}

	struct S {
		@static u64 structVar = 50;
		u64 incAndReturn1() {
			++structVar;
			return structVar;
		}
		u64 incAndReturn2() {
			++this.structVar;
			return this.structVar;
		}
	}
	// @static struct member
	u64 incAndReturn3() {
		++S.structVar;
		return S.structVar;
	}
	// through variable
	u64 incAndReturn4() {
		S s;
		++s.structVar;
		return s.structVar;
	}
	// through method of instance
	u64 incAndReturn5() {
		S s;
		return s.incAndReturn1();
	}
	// through method of instance through this
	u64 incAndReturn6() {
		S s;
		return s.incAndReturn2();
	}
};
void tester250(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn1")() == 43);
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn1")() == 44);

	assert(ctx.getFunctionPtr!(ulong)("incAndReturn2")() == 11 + 43);
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn2")() == 12 + 44);

	assert(ctx.getFunctionPtr!(ulong)("incAndReturn3")() == 51);
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn3")() == 52);

	assert(ctx.getFunctionPtr!(ulong)("incAndReturn4")() == 53);
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn4")() == 54);

	assert(ctx.getFunctionPtr!(ulong)("incAndReturn5")() == 55);
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn5")() == 56);

	assert(ctx.getFunctionPtr!(ulong)("incAndReturn6")() == 57);
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn6")() == 58);
}


@TestInfo()
immutable test251 = q{--- test251.vx
	// Forbid @static on parameters
	void fun(@static u64 param) {}
--- <error>
test251.vx:2:23: Error: Parameters cannot be @static
};


@TestInfo(&tester252)
immutable test252 = q{--- test252.vx
	// @static methods
	struct S {
		@static u64 structVar = 50;
		@static u64 incAndReturnStatic() {
			++structVar;
			return structVar;
		}
	}
	// through static method
	u64 incAndReturn1() {
		return S.incAndReturnStatic();
	}
	// through static method via instance
	u64 incAndReturn2() {
		S s;
		return s.incAndReturnStatic();
	}
	// check that access to non-static members from static method is forbidden
};
void tester252(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn1")() == 51);
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn1")() == 52);

	assert(ctx.getFunctionPtr!(ulong)("incAndReturn2")() == 53);
	assert(ctx.getFunctionPtr!(ulong)("incAndReturn2")() == 54);
}


@TestInfo()
immutable test253 = q{--- test253.vx
	// Forbid access to non-@static member from @static method
	// @static methods
	struct S {
		u64 structVar = 50;
		@static void useNonstatic() {
			++structVar;
		}
	}
--- <error>
test253.vx:6:6: Error: undefined identifier `this`
};


@TestInfo()
immutable test254 = q{--- test254.vx
	alias PFN_vkAllocationFunction = void* function(void* pUserData);
};


@TestInfo()
immutable test255 = q{--- test255.vx
	// Issue 40
	// Incorrect aggregate lowering
	// Was checking for isConstant, which ignored zero constants
	struct Color { u8 r; u8 g; u8 b; u8 a; }

	void draw() {
		for(u64 i = 0; i < 4; i++){
			u8 a = cast(u8) i;
			Color c = Color(255, 0, 0, a);
		}
	}
};


@TestInfo()
immutable test256 = `--- test256.vx
	// Logical not CTFE
	enum VAL = true;

	#if(!VAL) struct A {}
`;

@TestInfo(&tester257)
immutable test257 = q{--- test257.vx
	// offsetof
	struct Color { u8 r; u8 g; u8 b; u8 a; }

	u64 offsetof_r() { return Color.r.offsetof; }
	u64 offsetof_g() { return Color.g.offsetof; }
	u64 offsetof_b() { return Color.b.offsetof; }
	u64 offsetof_a() { return Color.a.offsetof; }
};
void tester257(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("offsetof_r")() == 0);
	assert(ctx.getFunctionPtr!(ulong)("offsetof_g")() == 1);
	assert(ctx.getFunctionPtr!(ulong)("offsetof_b")() == 2);
	assert(ctx.getFunctionPtr!(ulong)("offsetof_a")() == 3);
}


@TestInfo()
immutable test258 = q{--- test258.vx
	// offsetof only applies to `type.member`, not `instance.member`
	struct Color { u8 r; u8 g; u8 b; u8 a; }

	u64 offsetof_a(Color c) { return c.offsetof; }
--- <error>
test258.vx:4:36: Error: `Color` has no member `offsetof`
};


@TestInfo()
immutable test259 = q{--- test259.vx
	// offsetof should not work on non-struct types
	enum a;
	enum b {m}
	enum test_1 = a.offsetof;
	enum test_2 = u8.offsetof;
	enum test_3 = b.m.offsetof;
--- <error>
test259.vx:4:17: Error: `a` has no member `offsetof`
test259.vx:5:18: Error: `u8` has no member `offsetof`
test259.vx:6:19: Error: `b` has no member `offsetof`
};


@TestInfo(&tester260)
immutable test260 = q{--- test260.vx
	// offsetof with non-static members
	struct Color {
		u8 r;
		enum e;
		u8 g;
		alias e1 = e;
		u8 b;
		u8 a;
	}

	u64 offsetof_r() { return Color.r.offsetof; }
	u64 offsetof_g() { return Color.g.offsetof; }
	u64 offsetof_b() { return Color.b.offsetof; }
	u64 offsetof_a() { return Color.a.offsetof; }
	//u64 offsetof_e() { return Color.e.offsetof; }
};
void tester260(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(ulong)("offsetof_r")() == 0);
	assert(ctx.getFunctionPtr!(ulong)("offsetof_g")() == 1);
	assert(ctx.getFunctionPtr!(ulong)("offsetof_b")() == 2);
	assert(ctx.getFunctionPtr!(ulong)("offsetof_a")() == 3);
}


@TestInfo()
immutable test261 = q{--- test261.vx
	// offsetof only applies to non-static memebers
	struct Color { enum e; alias e1 = e; @static u8 g; }

	u64 offsetof_e() { return Color.e.offsetof; }
	u64 offsetof_e1() { return Color.e1.offsetof; }
	u64 offsetof_g() { return Color.g.offsetof; }
--- <error>
test261.vx:4:35: Error: `e` has no member `offsetof`
test261.vx:5:37: Error: `e` has no member `offsetof`
test261.vx:6:35: Error: `u8` has no member `offsetof`
};


@TestInfo()
immutable test262 = q{--- test262.vx
	// bug Type [2 x {i32, i32, i32, i32}] of size 32 cannot be stored in a register
	// caused by generating load instead of load_aggregate for arrays
	struct Color { i32 r; i32 g; i32 b; i32 a; }
	Color[2] get() { Color[2] c; return c; }
};


@TestInfo(&tester266)
immutable test266 = q{--- test266.vx
	// using int literals to initialize floats
	f32 get_f32() {
		f32 val = 1;
		return val;
	}
	f64 get_f64() {
		f64 val = 1;
		return val;
	}
};
void tester266(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(float)("get_f32")() == 1.0f);
	assert(ctx.getFunctionPtr!(double)("get_f64")() == 1.0);
}


@TestInfo(&tester267)
immutable test267 = q{--- test267.vx
	// implicit conversion of pointer to bool
	bool null_to_bool() {
		return null;
	}
	bool ptr_to_bool(void* ptr) {
		return ptr;
	}
	bool u8_ptr_to_bool(u8* ptr) {
		return ptr;
	}
	struct Handle_dummy; alias Handle = Handle_dummy*;
	bool struct_ptr_to_bool(Handle ptr) {
		return ptr;
	}
	u8 null_branch() {
		if(null) return 42;
		return 12;
	}
	u8 ptr_branch(void* ptr) {
		if(ptr) return 42;
		return 12;
	}
	u8 u8_ptr_branch(u8* ptr) {
		if(ptr) return 42;
		return 12;
	}
	u8 struct_ptr_branch(Handle ptr) {
		if(ptr) return 42;
		return 12;
	}
};
void tester267(ref TestContext ctx) {
	ubyte var;
	assert(ctx.getFunctionPtr!(bool)("null_to_bool")() == false);
	assert(ctx.getFunctionPtr!(bool, void*)("ptr_to_bool")(null) == false);
	assert(ctx.getFunctionPtr!(bool, void*)("ptr_to_bool")(&var) == true);
	assert(ctx.getFunctionPtr!(bool, void*)("u8_ptr_to_bool")(null) == false);
	assert(ctx.getFunctionPtr!(bool, void*)("u8_ptr_to_bool")(&var) == true);
	assert(ctx.getFunctionPtr!(bool, void*)("struct_ptr_to_bool")(null) == false);
	assert(ctx.getFunctionPtr!(bool, void*)("struct_ptr_to_bool")(&var) == true);
	assert(ctx.getFunctionPtr!(ubyte)("null_branch")() == 12);
	assert(ctx.getFunctionPtr!(ubyte, void*)("ptr_branch")(null) == 12);
	assert(ctx.getFunctionPtr!(ubyte, void*)("ptr_branch")(&var) == 42);
	assert(ctx.getFunctionPtr!(ubyte, void*)("u8_ptr_branch")(null) == 12);
	assert(ctx.getFunctionPtr!(ubyte, void*)("u8_ptr_branch")(&var) == 42);
	assert(ctx.getFunctionPtr!(ubyte, void*)("struct_ptr_branch")(null) == 12);
	assert(ctx.getFunctionPtr!(ubyte, void*)("struct_ptr_branch")(&var) == 42);
}


@TestInfo()
immutable test268 = `--- test268.vx
	// auto for variable declarations in function body
	struct S {}
	enum E { e = 1 }
	void fun() {
		auto val_i8  = 1_i8;
		auto val_i16 = 1_i16;
		auto val_i32 = 1_i32;
		auto val_i64 = 1_i64;
		auto val_u8  = 1_u8;
		auto val_u16 = 1_u16;
		auto val_u32 = 1_u32;
		auto val_u64 = 1_u64;
		auto val_f32 = 1_f32;
		auto val_f64 = 1_f64;
		auto val_bool = true;
		auto val_null = null;
		auto val_S = S();
		auto val_E = E.e;
		auto val_type = u8;
	}
	auto val_i8  = 1_i8;
	auto val_i16 = 1_i16;
	auto val_i32 = 1_i32;
	auto val_i64 = 1_i64;
	auto val_u8  = 1_u8;
	auto val_u16 = 1_u16;
	auto val_u32 = 1_u32;
	auto val_u64 = 1_u64;
	auto val_f32 = 1_f32;
	auto val_f64 = 1_f64;
	auto val_bool = true;
	auto val_null = null;
	auto val_S = S();
	auto val_E = E.e;
	auto val_type = u8;
`;


@TestInfo()
immutable test269 = q{--- test269.vx
	// functions returning auto
	auto fun() {}
--- <error>
test269.vx:2:7: Error: functions cannot return `auto`
};


@TestInfo()
immutable test270 = q{--- test270.vx
	// functions returning auto
	auto fun[T]() {}
--- <error>
test270.vx:2:7: Error: functions cannot return `auto`
};


@TestInfo()
immutable test271 = q{--- test271.vx
	// auto variables requiring initializer
	auto var;
--- <error>
test271.vx:2:7: Error: variables declared as `auto` must have an initializer
};


@TestInfo(&tester272)
immutable test272 = q{--- test272.vx
	// named arguments
	i32 func(i32 param1, i32 param2, i32 param3) { return param1 * 40 + param2 * 20 + param3; }
	i32 call_0() { return func(1, 2, 3); }
	i32 call_1() { return func(param1: 1, 2, 3); }
	i32 call_2() { return func(1, param2: 2, 3); }
	i32 call_3() { return func(1, 2, param3: 3); }
	i32 call_4() { return func(param1: 1, param2: 2, 3); }
	i32 call_5() { return func(param2: 2, 3, param1: 1, ); }
	i32 call_6() { return func(param1: 1, 2, param3: 3); }
	i32 call_7() { return func(param3: 3, param1: 1, 2); }
	i32 call_8() { return func(1, param2: 2, param3: 3); }
	i32 call_9() { return func(1, param3: 3, param2: 2); }
	i32 call10() { return func(param1: 1, param2: 2, param3: 3); }
	i32 call11() { return func(param1: 1, param3: 3, param2: 2); }
	i32 call12() { return func(param2: 2, param1: 1, param3: 3); }
	i32 call13() { return func(param2: 2, param3: 3, param1: 1); }
	i32 call14() { return func(param3: 3, param1: 1, param2: 2); }
	i32 call15() { return func(param3: 3, param2: 2, param1: 1); }
};
void tester272(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("call_0")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_1")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_2")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_3")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_4")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_5")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_6")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_7")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_8")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_9")() == 83);
	assert(ctx.getFunctionPtr!(int)("call10")() == 83);
	assert(ctx.getFunctionPtr!(int)("call11")() == 83);
	assert(ctx.getFunctionPtr!(int)("call12")() == 83);
	assert(ctx.getFunctionPtr!(int)("call13")() == 83);
	assert(ctx.getFunctionPtr!(int)("call14")() == 83);
}

@TestInfo()
immutable test273 = q{--- test273.vx
	// named arguments, error cases
	i32 func(i32 param1, i32 param2, i32 param3) { return param1 * 40 + param2 * 20 + param3; }
	i32 call_1() { return func(param4: 4, 1, 2); } // nonexisting named parameter
	i32 call_2() { return func(param2: 2, 1, 3); } // setting parameter 4, param1 not set
	i32 call_3() { return func(param3: 3, 1, 2); } // setting parameter 4 and 5, params 1 and 2 not set
	i32 call_4() { return func(param1: 1, param1: 1, 2); } // setting parameter 1 twice
--- <error>
test273.vx:3:29: Error: Function `func` has no parameter named `param4`
test273.vx:3:28: Error: Missing argument for parameter 1: `param1`
test273.vx:3:28: Error: Missing argument for parameter 2: `param2`
test273.vx:3:28: Error: Missing argument for parameter 3: `param3`
test273.vx:4:43: Error: Trying to provide parameter 4, while `func` has 3 parameters
test273.vx:4:28: Error: Missing argument for parameter 1: `param1`
test273.vx:5:40: Error: Trying to provide parameter 4, while `func` has 3 parameters
test273.vx:5:43: Error: Trying to provide parameter 5, while `func` has 3 parameters
test273.vx:5:28: Error: Missing argument for parameter 1: `param1`
test273.vx:5:28: Error: Missing argument for parameter 2: `param2`
test273.vx:6:40: Error: Parameter `param1` provided several times
test273.vx:6:28: Error: Missing argument for parameter 3: `param3`
};


@TestInfo(&tester274)
immutable test274 = q{--- test274.vx
	// named arguments with default arguments
	i32 func(i32 param1, i32 param2, i32 param3 = 3) { return param1 * 40 + param2 * 20 + param3; }
	i32 call_0() { return func(1, 2); }
	i32 call_1() { return func(1, 2, 3); }
	i32 call_2() { return func(param1: 1, 2); }
	i32 call_3() { return func(1, param2: 2); }
	i32 call_4() { return func(param1: 1, param2: 2); }
	i32 call_5() { return func(param2: 2, param1: 1); }
};
void tester274(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("call_0")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_1")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_2")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_3")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_4")() == 83);
	assert(ctx.getFunctionPtr!(int)("call_5")() == 83);
}


@TestInfo()
immutable test275 = q{--- test275.vx
	// named arguments, anonymous parameters
	void func(i32) {}
	void call() { return func(__param_0: 1); }
--- <error>
test275.vx:3:28: Error: Function `func` has no parameter named `__param_0`
test275.vx:3:27: Error: Missing argument for anonymous parameter 1
};


@TestInfo(&tester276)
immutable test276 = q{--- test276.vx
	// return void typed expression from void function
	void func() {}
	void call() { return func(); }
};
void tester276(ref TestContext ctx) {
	ctx.getFunctionPtr!(void)("call")();
}


@TestInfo(&tester277)
immutable test277 = q{--- test277.vx
	// named arguments for structs (named cannot be mixed with positional)
	struct Color { u8 r = 1; u8 g = 2; u8 b = 3; u8 a; }
	Color make_0() { return Color(r: 1, g: 2, b: 3, a: 4); }
	Color make_1() { return Color(r: 1); }
	Color make_2() { return Color(r: 1, a: 4); }
	Color make_3() { return Color(a: 4, b: 3, g: 2, r: 1); }
	//union Union { u32 u; bool b; f32 f; }
	//Union make_4() { return Union(u: 100); }
	//Union make_5() { return Union(b: true); }
	//Union make_6() { return Union(f: 10); }
};
void tester277(ref TestContext ctx) {
	static struct Color { ubyte r; ubyte g; ubyte b; ubyte a; }
	static struct Union {
		this(uint u){ this.u = u; }
		this(bool b){ this.b = b; }
		this(float f){ this.f = f; }
		uint u;
		bool b;
		float f;
	}
	assert(ctx.getFunctionPtr!(Color)("make_0")() == Color(1, 2, 3, 4));
	assert(ctx.getFunctionPtr!(Color)("make_1")() == Color(1, 2, 3, 0));
	assert(ctx.getFunctionPtr!(Color)("make_2")() == Color(1, 2, 3, 4));
	assert(ctx.getFunctionPtr!(Color)("make_3")() == Color(1, 2, 3, 4));
	//assert(ctx.getFunctionPtr!(Union)("make_4")() == Union(100));
}


@TestInfo()
immutable test278 = q{--- test278.vx
	// nnamed arguments for structs, errors
	struct Color { u8 r = 1; u8 g = 2; u8 b = 3; u8 a; }
	struct ColorStatic { @static u8 r = 1; u8 g = 2; u8 b = 3; u8 a; }
	union U { u8 b; f32 f; }
	Color make_1() { return Color(r: 1, g: 2, b: 3, w: 4); }
	ColorStatic make_2() { return ColorStatic(r: 1); }
	Color make_3() { return Color(1, 2, 3, 4, 5); }
	Color make_4() { return Color(r: 1, 2, 3, 4, 5); }
	U make_5() { return U(b: 1, f: 1.0); }
--- <error>
test278.vx:5:50: Error: struct `Color` has no member named `w`
test278.vx:6:44: Error: cannot initialize variable `r` of struct `ColorStatic`
test278.vx:7:31: Error: cannot initialize struct `Color` with 5 arguments, it has 4 members
test278.vx:8:31: Error: cannot initialize struct `Color` with 5 arguments, it has 4 members
test278.vx:8:38: Error: named and positional arguments cannot be mixed in struct constructor
test278.vx:9:23: Error: union constructor must have a single argument, not 2
};


@TestInfo()
immutable test279 = q{--- test279.vx
	// issue 42. Call expression of index expression didn't account for different template kinds and always assumed function template
	struct Vector[T] {
		T* ptr;
		u64 capacity;
		u64 length;
	}

	void main() {
		Vector[i32] numbers = Vector[i32](null, 10, 0);
	}
};


@TestInfo()
immutable test280 = q{--- test280.vx
	// issue 43. Missing check for expand operator on non-templated function
	void printa(u8[]... data) {}
--- <error>
test280.vx:2:22: Error: Variadic arrays are not yet implemented
};


@TestInfo()
immutable test281 = q{--- test281.vx
	// issue 44. Missing check for expand operator on templated function without variadic parameter
	void print[Args](Args... data) {}
	void run() {
		print("Hello", ", ", "world!\n");
	}
--- <error>
test281.vx:2:27: Error: Variadic parameters are not implemented
};


@TestInfo()
immutable test282 = q{--- test282.vx
	// issue 44
	void print[Args](Args data) {}
	void run() {
		print("Hello", ", ", "world!\n");
	}
--- <error>
test282.vx:4:8: Error: Cannot infer template parameters. Number of runtime arguments (3) does not match number of function parameters (1)
};


@TestInfo()
immutable test283 = q{--- test283.vx
	// Templated method with IFTI
	struct S {
		void methodCaller() {
			methodT(42);
			methodT[](42);
		}
		void method(i32 arg) {}
		void methodT[](i32 arg) {}
	}
};


@TestInfo()
immutable test284 = q{--- test284.vx
	// Check for expand on incorrect template parameter
	void print[T, Args...](T... data) {}
	void run() {
		print("Hello", ", ", "world!\n");
	}
--- <error>
test284.vx:2:30: Error: Should be `Args... data`
test284.vx:2:30: Error: Cannot expand non-variadic template parameter T
};


@TestInfo()
immutable test285 = q{--- test285.vx
	// Use of non-expanded variadic type
	void print[Args...](Args data) {}
	void run() {
		print("Hello", ", ", "world!\n");
	}
--- <error>
test285.vx:2:22: Error: Should be `Args... data`
test285.vx:4:8: Error: Cannot infer template parameters. Number of runtime arguments (3) does not match number of function parameters (1)
};



@TestInfo()
immutable test286 = q{--- test286.vx
	// Report error on out-of-bound access to array
	void sys_write(void* ptr){}
	void printa[Args...](Args... data) {
		sys_write(data[3].ptr);
	}
	void main() {
		printa("Hello", ", ", "world!\n");
	}
--- <error>
test286.vx:4:17: Error: Accessing index 3 of array of length 3
};


@TestInfo()
immutable test287 = q{--- test287.vx
	// Forward reference bug. Type of `b` was not type checked
	struct Array[T] {}
	struct Ctx {
		S a;
	}
	struct S {
		Array[i32] b;
	}
};


@TestInfo(&tester288)
immutable test288 = q{--- test288.vx
	// Aggregate lowering bug with constant aggregate nested inside non-constant one
	struct vec3 { f32 x; f32 y; f32 z; }
	struct Vertex {
		f32 pos;
		vec3 color;
	}
	Vertex putQuadAt(f32 pos) {
		return Vertex(pos, vec3(1, 0, 0));
	}
};
void tester288(ref TestContext ctx) {
	static struct vec3 { float x; float y; float z; }
	static struct Vertex { float pos; vec3 color; }
	assert(ctx.getFunctionPtr!(Vertex, float)("putQuadAt")(56.0f) == Vertex(56.0f, vec3(1, 0, 0)));
}


/*
TODO
@TestInfo(&tester289)
immutable test289 = q{--- test289.vx
	// Aggregate of float type
	struct vec2 { f32 x; f32 y; }
	vec2 make(f32 x, f32 y) {
		return vec2(x, y);
	}
};
void tester289(ref TestContext ctx) {
	static struct vec2 { float x; float y; }
	assert(ctx.getFunctionPtr!(vec2, float, float)("make")(56.0f, 30.0f) == vec2(56.0f, 30.0f));
}*/


@TestInfo(&tester290, [HostSymbol("noop", cast(void*)&external_noop)])
immutable test290 = q{--- test290.vx
	// Taking address of the external symbol
	@extern(module, "host") void noop();
	void* run() {
		return &noop;
	}
};
void tester290(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(void*)("run")() == &external_noop);
}


@TestInfo()
immutable test291 = q{--- test291.vx
	// bug in error report of invalid return type
	i32 fun(i32 size) {
		return;
	}
--- <error>
test291.vx:3:3: Error: Cannot return void from non-void function
};


@TestInfo(&tester292)
immutable test292 = q{--- test292.vx
	// Eval function address during global init
	i32 function(i32 param) fptr = &callback;
	i32 callback(i32 param){ return param; }
	i32 run(i32 val) { return fptr(val); }
};
void tester292(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int, int)("run")(42) == 42);
}


@TestInfo()
immutable test293 = q{--- test293.vx
	// Named arguments of function pointer
	void function(i32 param) fptr = &callback;
	void callback(i32){}
	void fun() {
		fptr(param : 1);
	}
};


@TestInfo()
immutable test294 = q{--- test294.vx
	// Make sure function type parameters are not visible outside
	struct S
	{
		void* param;
		void function(void* param) fun;
	}
};
