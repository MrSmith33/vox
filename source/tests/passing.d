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
	T[] slice() { return ptr[0..length]; }
	alias slice this;
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
	enum i32 i32_min = -2147483648;
	i32 add() { return 1 + 3; }
	i32 sub() { return 3 - 1; }
	i32 mul() { return 3 * 2; }
	i32 div() { return 7 / 2; }
	i32 rem() { return 7 % 2; }
	i32 shl() { return 1 << 2; }
	i32 shr() { return i32_min >>> 2; }
	i32 sar() { return i32_min >> 2; }
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
