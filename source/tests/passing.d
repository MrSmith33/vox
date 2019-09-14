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


@TestInfo(&tester25, [HostSymbol("print", cast(void*)&external_print_string)])
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
	this(T[] data) {
		ptr = data.ptr;
		length = data.length;
	}
	ulong length;
	T* ptr;
	T[] slice() { return ptr[0..length]; }
	alias slice this;
}
extern(C) void external_print_string(Slice!char param) {
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


@TestInfo(&tester25, [HostSymbol("print", cast(void*)&external_print_string)])
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


@TestInfo(&tester27, [HostSymbol("print", cast(void*)&external_print_string)])
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


@TestInfo(&tester55, [HostSymbol("print", cast(void*)&external_print_string)])
immutable test55 = q{--- test55
	// test slice forwarding
	void print(u8[] str);
	void forward(u8[] str) {
		print(str);
	}
};
void tester55(ref TestContext ctx) {
	auto forward = ctx.getFunctionPtr!(void, Slice!(immutable(char)))("forward");
	forward(Slice!(immutable(char))("testString"));
	assert(testSink.text == "testString");
	testSink.clear;
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

@TestInfo(&tester64)
immutable test64 = q{--- test64
	// Test structs
	struct Test64 {
		i64 a;
		i64 b;
	}
	// constructor is a function (expression) that returns struct type
	// can compile it into create_aggregate instruction
	// - default initialization of members
	// + return result (by ptr)
	// return aggregate by storing into hidden first parameter
	Test64 returnBigStruct() {
		return Test64(10, 42);
	}
	Test64 returnBigStruct2() {
		Test64 res = Test64(10, 42);
		return res;
	}
	// - pass as arg (fits in register)
	// - pass as arg (by ptr)
	void passArgBigStruct() {
		receiveArgBigStruct(Test64(10, 42));
	}
	// - pass as arg (by ptr, pushed)
	void passArgBigStructPush() {
		receiveArgBigStructPush(1,2,3,4,Test64(10, 42));
	}
	// - receive parameter (fits in register)
	// - receive parameter (by ptr)
	void receiveArgBigStruct(Test64 arg) {}
	void receiveArgBigStructPush(i32,i32,i32,i32,Test64 arg) {}
	// - pass member as arg (by ptr)
	// - pass member as arg (fits in register)
	// - receive result (fits in register)
	// - receive result (by ptr)
	// - return result (fits in register)
	// - store in memory
	// - load from memory
	// - set member
	// - get member
	// - get member ptr
	// - get ptr
};
struct Test64 {
	long a;
	long b;
}
void tester64(ref TestContext ctx) {
	auto returnBigStruct = ctx.getFunctionPtr!(Test64)("returnBigStruct");
	assert(returnBigStruct() == Test64(10, 42));

	auto returnBigStruct2 = ctx.getFunctionPtr!(Test64)("returnBigStruct2");
	assert(returnBigStruct2() == Test64(10, 42));

	auto passArgBigStruct = ctx.getFunctionPtr!(void)("passArgBigStruct");
	passArgBigStruct();

	auto passArgBigStructPush = ctx.getFunctionPtr!(void)("passArgBigStructPush");
	passArgBigStructPush();
}


@TestInfo(&tester65, [HostSymbol("print", cast(void*)&external_print_string)])
immutable test65 = q{--- test65
	// Test string literals
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
	testSink.clear;
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
	i32 test_div(i64 i, u8[] res) {
		res[0] = cast(u8)(i % 10);
		i /= 10;
		res[1] = cast(u8)(i % 10);
		return i;
	}
};
void tester78(ref TestContext ctx) {
	auto test_div = ctx.getFunctionPtr!(int, long, Slice!ubyte)("test_div");
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
	u64 combinedEnums() { return arrBackrefX.length; } // 160

	enum arr_size = 40;
	i32[arr_size] arrBackref;
	u64 getSize1() { return arrBackref.length; } // 40

	u64 testBackref() {
		i32[arr_size] local_arr;
		return local_arr.length; // 40
	}

	i32[arr_size2] arrForwardref;
	u64 getSize2() { return arrForwardref.length; } // 100

	u64 testForwardref() {
		i32[arr_size2] local_arr;
		return local_arr.length; // 100
	}

	enum arr_size2 = 100;

	i32[arr_size3 + 4] arrForwardref2;
	u64 getSize3() { return arrForwardref2.length; } // 124

	u64 testForwardref2() {
		i32[arr_size3 + 10] local_arr;
		return local_arr.length; // 130
	}

	enum arr_size3 = 120;
};
void tester83(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!ulong("combinedEnums")() == 160);
	assert(ctx.getFunctionPtr!ulong("getSize1")() == 40);
	assert(ctx.getFunctionPtr!ulong("testBackref")() == 40);
	assert(ctx.getFunctionPtr!ulong("getSize2")() == 100);
	assert(ctx.getFunctionPtr!ulong("testForwardref")() == 100);
	assert(ctx.getFunctionPtr!ulong("getSize3")() == 124);
	assert(ctx.getFunctionPtr!ulong("testForwardref2")() == 130);
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
};
void tester88(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("test")() == 84);
	assert(ctx.getFunctionPtr!(int)("test2")() == 84);
	assert(ctx.getFunctionPtr!(int)("test3")() == 50);
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

	assert(testSink.text == "(hi 0 0), (hi 1 1), (hi 2 2)");
	testSink.clear;
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
			set42; // pass this
			this.set42; // explicit this
			this.set42(); // explicit this
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
};
void tester92(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("testMethod1")() == 42);
	assert(ctx.getFunctionPtr!(int, int)("testMethod2")(60) == 60);
	assert(ctx.getFunctionPtr!(int)("testMethod3")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethod4")() == 42);
	assert(ctx.getFunctionPtr!(int)("testMethod5")() == 42);
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
