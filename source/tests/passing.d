/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.passing;

import std.stdio;
import tester;

Test[] passingTests() { return [
	test7, test8, test8_1, test10, test9, test13, test14, test15, test16, test17,
	test18, test19, test20, test21, test21_2, test22, test23, test24, test25, test26,
	test27, test31, test32, test33, test34, test35, test36, test37, test38, test39];
}

extern(C) void external_print_i32_func(int par1) {
	formattedWrite(testSink, "%s ", par1);
}

immutable input7 = q{--- test7
	i32 fib(i32 number) {
		if (number < 1) return 0;
		if (number < 3) return 1;
		return fib(number-1) + fib(number-2);
	}
};
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

immutable input9 = q{--- test9
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
auto test9 = Test("Test 9", input9);

immutable input8 = q{--- test8
	i32 sign(i32 number) {
		i32 result;
		if (number < 0) result = 0-1;
		else if (number > 0) result = 1;
		else result = 0;
		return result;
	}
};

immutable input8_1 = q{--- test8_1
	i32 sign(i32 number) {
		if (number < 0) return 0-1;
		else if (number > 0) return 1;
		else return 0;
	}
};
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

immutable input10 = q{--- test10
	i32 test(i32* array) {
		return array[0];
	}
};
alias Func10 = extern(C) int function(int*);
void tester10(Func10 fun) {
	int val = 42;
	int res = fun(&val);
	//writefln("test(&42) -> %s", res);
	assert(res == 42);
}
auto test10 = Test("Test 10", input10, "test", cast(Test.Tester)&tester10);

immutable input11 = q{--- test11
	i32 test(i32* array) {
		return array[1];
	}
};
alias Func11 = extern(C) int function(int*);
void tester11(Func11 fun) {
	int[2] val = [42, 56];
	int res = fun(val.ptr);
	//writefln("test([42, 56].ptr) -> %s", res);
	assert(res == 56);
}
auto test11 = Test("Test 11", input11, "test", cast(Test.Tester)&tester11);

immutable input12 = q{--- test12
	i32 test(i32* array, i32 index) {
		return array[index];
	}
};
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

immutable input13 = q{--- test13
	void test(i32* array, i32 index, i32 value) {
		array[index] = value;
	}
};
alias Func13 = extern(C) void function(int*, int, int);
void tester13(Func13 fun) {
	int[4] val = [42, 56, 96, 102];
	int[4] expected = [42, 20, 96, 102];
	fun(val.ptr, 1, 20);
	//writefln("test([42, 56].ptr, 1, 20) -> %s", val);
	assert(val == expected, format("%s != %s", val, expected));
}
auto test13 = Test("Test 13", input13, "test", cast(Test.Tester)&tester13);


immutable input14 = q{--- test14
	void test(i32* array, i32 index, i32 value, i32 value2, i32 value3) {
		array[index] = value + value2 + value3;
	}
};
alias Func14 = extern(C) void function(int*, int, int, int, int);
void tester14(Func14 fun) {
	int[2] val = [42, 56];
	fun(val.ptr, 1, 10, 6, 4);
	//writefln("test([42, 56].ptr, 1, 10, 6, 4) -> %s", val);
	assert(val[1] == 20);
}
auto test14 = Test("Test 14", input14, "test", cast(Test.Tester)&tester14);


immutable input15 = q{--- test15
	// Test 3 inputs no parameters pushed to the stack
	i32 test(i32 par) {
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
	//writefln("fun(10) -> %s", result);
	assert(result == 40);
}
auto test15 = Test("Test 15", input15, "test", cast(Test.Tester)&tester15,
	[HostSymbol("external", cast(void*)&test15_external_func)]);


immutable input16 = q{--- test16
	// Test more than 4 inputs (5-th parameter pushed to the stack, extra alignment needed)
	i32 test(i32 par) {
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
	//writefln("fun(10) -> %s", result);
	assert(result == 110);
}
auto test16 = Test("Test 16", input16, "test", cast(Test.Tester)&tester16,
	[HostSymbol("external", cast(void*)&test16_external_func)]);

// Test 6 inputs (5-th and 6-th parameters pushed to the stack, no extra alignment needed)
immutable input17 = q{--- test17
	i32 test(i32 par) {
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
	//writefln("fun(10) -> %s", result);
	assert(result == 160);
}
auto test17 = Test("Test 17", input17, "test", cast(Test.Tester)&tester17,
	[HostSymbol("external", cast(void*)&test17_external_func)]);

immutable input18 = q{--- test18
	// test empty void function
	void test() {}
};
alias Func18 = extern(C) void function();
void tester18(Func18 fun) { fun(); }
auto test18 = Test("Test 18", input18, "test", cast(Test.Tester)&tester18);

immutable input19 = q{--- test19
	// test empty void function with return
	void test() { return; }
};
alias Func19 = extern(C) void function();
void tester19(Func19 fun) { fun(); }
auto test19 = Test("Test 19", input19, "test", cast(Test.Tester)&tester19);

immutable input20 = q{--- test20
	// test empty i32 function without return and with control flow
	void test(i32 i) { if(i){}else{} }
};
alias Func20 = extern(C) void function(int);
void tester20(Func20 fun) { fun(1); }
auto test20 = Test("Test 20", input20, "test", cast(Test.Tester)&tester20);

immutable input21 = q{--- test21
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

immutable input21_2 = q{--- test21_2
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
alias Func21 = extern(C) void function();
void tester21(Func21 fibonacci) {
	fibonacci();
	assert(testSink.text == "1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765");
	testSink.clear;
}
auto test21 = Test("Test 21", input21, "fibonacci", cast(Test.Tester)&tester21,
	[HostSymbol("print", cast(void*)&external_print_i32_func)]);
auto test21_2 = Test("Test 21.2", input21_2, "fibonacci", cast(Test.Tester)&tester21,
	[HostSymbol("print", cast(void*)&external_print_i32_func)]);

immutable input22 = q{--- test22
	// test phi resolution with critical edge and test break;
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
	}
};
alias Func22 = extern(C) int function();
void tester22(Func22 fun) { int res = fun(); assert(res == 5); }
auto test22 = Test("Test 22", input22, "test", cast(Test.Tester)&tester22);

immutable input23 = q{--- test23
	// test continue
	i32 test() {
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
alias Func23 = extern(C) int function();
void tester23(Func23 fun) { int res = fun(); assert(res == 7); }
auto test23 = Test("Test 23", input23, "test", cast(Test.Tester)&tester23);

immutable input24 = q{--- test24
	// test string literal
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

immutable input25 = q{--- test25
	// test struct creation, member set, struct as func arg
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

immutable input26 = q{--- test26
	// test global parameter, assignment
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

immutable input27 = q{--- test27
	// test slices
	void print(u8[]);
	void test() {
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
void tester27(Func25 fun) {
	fun();
	//writefln("fun() == '%s'", testSink.text);
	assert(testSink.text == "AssignPtrAssignSlice");
	testSink.clear;
}
auto test27 = Test("Test 27", input27, "test", cast(Test.Tester)&tester27,
	[HostSymbol("print", cast(void*)&test25_external_print)]);


immutable input31 = q{--- test31
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
	void test() {
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
void tester31(Func25 fun) {
	fun();
	//writefln("fun() == '%s'", testSink.text);
	assert(testSink.text == "345679");
	testSink.clear;
}
auto test31 = Test("enum", input31, "test", cast(Test.Tester)&tester31,
	[HostSymbol("print_num", cast(void*)&test31_external_print_num)]);

immutable input32 = q{--- test32
	// Test reg alloc xchg generation
	void print(i32); // external
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
	}
};
alias Func32 = extern(C) void function();
auto test32 = Test("Test 32 - fibonacci 4 times", input32, "main", cast(Test.Tester)&tester21,
	[HostSymbol("print", cast(void*)&external_print_i32_func)]);

immutable input33 = q{
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
auto test33 = Test("Test 33", input33, "fibonacci", cast(Test.Tester)&tester21,
	[HostSymbol("print", cast(void*)&external_print_i32_func)]);

alias Func34 = extern(C) int function(Slice!int, int);
immutable input34 = q{--- test34
	i32 getElement(i32[] items, i32 index) { return items[index]; }
};
void tester34(Func34 fun) {
	int[2] val = [42, 56];
	Slice!int slice = {val.length, val.ptr};
	int res0 = fun(slice, 0);
	int res1 = fun(slice, 1);
	assert(res0 == 42);
	assert(res1 == 56);
}
auto test34 = Test("Test 34", input34, "getElement", cast(Test.Tester)&tester34);

alias Func35 = extern(C) void function(Slice!int, int, int);
immutable input35 = q{--- test35
	void setElement(i32[] items, i32 index, i32 value) { items[index] = value; }
};
void tester35(Func35 fun) {
	int[2] val = [42, 56];
	Slice!int slice = {val.length, val.ptr};
	fun(slice, 0, 88);
	assert(val == [88, 56]);
	fun(slice, 1, 96);
	assert(val == [88, 96]);
}
auto test35 = Test("Test 35", input35, "setElement", cast(Test.Tester)&tester35);

immutable input36 = q{--- test36
	// Test null literal implicit conversion to pointer types
	void test() {
		callee1(null);
		callee2(null);
	}
	void callee1(void*) {}
	void callee2(u8*) {}
};
auto test36 = Test("Test 36", input36);

immutable input37 = q{--- test37
	// Test negative int literal
	void test() {
		callee1(-1);
	}
	void callee1(i32) {}
};
auto test37 = Test("Test 37", input37);

immutable input38 = q{--- test38
	// Test empty struct
	struct A {}
};
auto test38 = Test("Test 38", input38);

immutable input39 = q{--- test39
	// Test left shift
	i32 shl(i32 a, i32 b) {
		return a << b;
	}
};
alias Func39 = extern(C) int function(int, int);
void tester39(Func39 fun) {
	foreach(i; 0..33) {
		int res = fun(1, i);
		assert(res == 1 << i);
		//writefln("1 << %s == %b", i, res);
	}
}
auto test39 = Test("Test 39", input39, "shl", cast(Test.Tester)&tester39);
