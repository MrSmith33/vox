/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.failing;

import tester;

Test[] failingTests() { return collectTests!(tests.failing)(); }

@TestInfo()
immutable fail1 = `
--- fail1
	"dddd
--- <error>
fail1(1, 2): Error: Unexpected end of input inside string literal
`;


@TestInfo()
immutable fail2 = `
--- fail2
	/*
--- <error>
fail2(1, 2): Error: Unterminated multiline comment
`;


@TestInfo()
immutable fail3 = r"
--- fail3
	struct
--- <error>
fail3(2, 1): Error: Expected `IDENTIFIER` token, while got `EOI` token 'end of input'
";


@TestInfo()
immutable fail4 = r"
--- fail4
	int a = ;
--- <error>
fail4(1, 10): Error: SEMICOLON is not an expression
";


@TestInfo()
immutable fail5 = r"
--- fail5
	int[ a;
--- <error>
fail5(1, 8): Error: Expected `RBRACKET` token, while got `SEMICOLON` token ';'
";


@TestInfo()
immutable fail6 = r"
--- fail6
	int[] ;
--- <error>
fail6(1, 8): Error: Expected `IDENTIFIER` token, while got `SEMICOLON` token ';'
";


@TestInfo()
immutable fail7 = r"
--- fail7
	int[8 ;
--- <error>
fail7(1, 8): Error: Expected `RBRACKET` token, while got `SEMICOLON` token ';'
";


@TestInfo()
immutable fail8 = r"
--- fail8
	struct test
--- <error>
fail8(2, 1): Error: Expected `LCURLY` token, while got `EOI` token 'end of input'
";


@TestInfo()
immutable fail9 = r"
--- fail9
	struct test {
--- <error>
fail9(2, 1): Error: Expected `RCURLY` token, while got `EOI` token 'end of input'
";


@TestInfo()
immutable fail10 = r"
--- fail10
	i32 fun(
--- <error>
fail10(2, 1): Error: Expected `RPAREN` token, while got `EOI` token 'end of input'
";


@TestInfo()
immutable fail11 = r"
--- fail11
	i32 fun(){
--- <error>
fail11(2, 1): Error: Expected `RCURLY` token, while got `EOI` token 'end of input'
";


@TestInfo()
immutable fail12 = r"
--- fail12
	i32 fun(){
		a = b +
--- <error>
fail12(3, 1): Error: Unexpected end of input
";


@TestInfo()
immutable fail13 = r"
--- fail13
	i32 fun(){
		a = b + c
--- <error>
fail13(3, 1): Error: Expected `SEMICOLON` token, while got `EOI` token 'end of input'
";

@TestInfo()
immutable fail14 = r"
--- fail14
	i32 fun(){
	/*
	*/
		bar();
	}
--- <error>
fail14(4, 3): Error: undefined identifier `bar`
";

@TestInfo()
immutable fail15 = r"
--- fail15
	void run() {
		i32 val1 = 0;
		i32 flags = 1;
		if (val1 & flags != flags) {}
	}
--- <error>
fail15(4, 12): Error: Cannot perform `i32` & `bool` operation
";

@TestInfo()
immutable fail16 = q{
--- fail16
	// Test argument checking for pointers
	void usePtru8(u8*){}
	void usePtru16(u16*){}
	void usePtru32(u32*){}
	void usePtru64(u64*){}
	void run() {
		u8 num8 = 10;
		usePtru8(&num8);
		usePtru16(&num8);
		usePtru32(&num8);
		usePtru64(&num8);
	}
--- <error>
fail16(9, 13): Error: Argument 1, must have type u16*, not u8*
fail16(10, 13): Error: Argument 1, must have type u32*, not u8*
fail16(11, 13): Error: Argument 1, must have type u64*, not u8*
};

@TestInfo()
immutable fail17 = q{
--- fail17
	// Test instantiation of opaque structs
	struct Opaque;
	Opaque op_global;
	Opaque op_return(Opaque op_param) {
		Opaque op_local;
		return op_local;
	}
--- <error>
fail17(3, 2): Error: cannot declare variable `op_global` of opaque type `Opaque`
fail17(4, 2): Error: function cannot return opaque type `Opaque`
fail17(4, 19): Error: cannot declare parameter of opaque type `Opaque`
fail17(5, 3): Error: cannot declare variable `op_local` of opaque type `Opaque`
};

@TestInfo()
immutable fail18 = q{
--- fail18
	// Test for loop. Init declaration must not escape
	i32 test() {
		for (i32 i = 0; i < 10; ++i) {}
		return i;
	}
--- <error>
fail18(4, 10): Error: undefined identifier `i`
};

@TestInfo()
immutable fail19 = q{
--- fail19
	enum enumT : u32 {
	}
	// Test access to non-existing enum member
	u32 test() {
		return enumT.ESCAPE;
	}
--- <error>
fail19(5, 15): Error: `enumT` has no member `ESCAPE`
};

@TestInfo()
immutable fail20 = q{
--- fail20
	enum enumT : u32 {
	}
	// Test access to non-existing enum member that is used in expression
	void test() {
		if (enumT.ESCAPE == 0) {
		}
	}
--- <error>
fail20(5, 12): Error: `enumT` has no member `ESCAPE`
};

@TestInfo()
immutable fail21 = q{
--- fail21
	struct S {
		u16 num;
	}
	// Test wrong constructor argument type
	void test() {
		S s = S("string");
	}
--- <error>
fail21(6, 11): Error: Argument 1, must have type u16, not u8[]
};

@TestInfo()
immutable fail22 = q{
--- fail22
	// Test return null when int is expected
	u32 test() {
		return null;
	}
--- <error>
fail22(3, 3): Error: Cannot implicitly convert expression of type `typeof(null)` to `u32`
};

@TestInfo()
immutable fail23 = q{
--- fail23
	// Test default argument is expected, unnamed parameter
	void test(u32 arg1 = 42, u32) {}
--- <error>
fail23(2, 27): Error: Default argument expected for __param_1
};

@TestInfo()
immutable fail24 = q{
--- fail24
	// Test default argument is expected
	void test(u32 arg1 = 42, u32 arg2) {}
--- <error>
fail24(2, 27): Error: Default argument expected for arg2
};

@TestInfo()
immutable fail25 = q{
--- fail25
	// Test insufficient args with default args
	void func(u32 arg1, u32 arg2 = 42) {}
	void test() { func(); }
--- <error>
fail25(3, 20): Error: Insufficient arguments to `func`, got 0, expected 1-2
};

@TestInfo()
immutable fail26 = q{
--- fail26
	// Test excessive arguments
	void func(u32 arg1 = 42) {}
	void test() { func(1, 2); }
--- <error>
fail26(3, 20): Error: Too much arguments to `func`, got 2, expected 0-1
};

@TestInfo()
immutable fail27 = q{
--- fail27
	// Test excessive arguments
	void func(u32 arg1) {}
	void test() { func(1, 2); }
--- <error>
fail27(3, 20): Error: Too much arguments to `func`, got 2, expected 1
};

@TestInfo()
immutable fail28 = q{
--- fail28
	// Test ; as empty statement
	void test() { if(true); }
--- <error>
fail28(2, 24): Error: Cannot use `;` as an empty statement. Use `{}` instead
};

@TestInfo()
immutable fail29 = q{
--- fail29
	// Unknown identifiers
	Array[Point] array;
--- <error>
fail29(2, 2): Error: undefined identifier `Array`
fail29(2, 8): Error: undefined identifier `Point`
};


@TestInfo()
immutable fail30 = q{
--- fail30
	// Call used as a type. Example
	// a()  <-- missing semicolon
	// b ...
	void run() {
		a() b;
	}
--- <error>
fail30(5, 7): Error: Invalid expression. Missing `;` before `b`
};


@TestInfo()
immutable fail31 = q{
--- fail31
	#assert 42
--- <error>
fail31(1, 10): Error: Expected `(` after #assert, while got `42`
};

@TestInfo()
immutable fail32 = q{
--- fail32
	#assert(42
--- <error>
fail32(2, 1): Error: Expected `,` after condition of #assert, while got `end of input`
};

@TestInfo()
immutable fail33 = q{
--- fail33
	#assert(42, )
--- <error>
fail33(1, 14): Error: RPAREN is not an expression
};

@TestInfo()
immutable fail34 = q{
--- fail34
	#assert(42, "message")
--- <error>
fail34(2, 1): Error: Expected `;` after #assert, while got `end of input`
};

@TestInfo()
immutable fail35 = q{
--- fail35
	#assert(42, 42);
--- <error>
fail35(1, 2): Error: Error: #assert only supports string literal as a message
};


@TestInfo()
immutable fail36 = q{
--- fail36
	// Should not return from noreturn
	noreturn run() {
		return foo;
	}
	i32 foo(){ return 42; }
--- <error>
fail36(3, 3): Error: Cannot implicitly convert expression of type `i32` to `noreturn`
};


@TestInfo()
immutable fail37 = q{
--- fail37
	// Non-type used as return type
	alias type1 = getType1;
	$alias getType1() { return u8; }
	type1 run1() {
		 return 42;
	}
--- <error>
fail37(2, 16): Error: function is not a type
};


@TestInfo()
immutable fail38 = q{
--- fail38
	// Non-type used as var type
	$alias getType1() { return u8; }
	alias type1 = getType1;
	type1 var;
--- <error>
fail38(3, 16): Error: function is not a type
};


version(linux)
@TestInfo()
immutable fail39 = q{--- fail39
	// 2 Extern syscall attributes
	@extern(syscall, 60)
	@extern(syscall, 100)
	void exit();
--- <error>
fail39(3, 2): Error: Duplicate @extern attribute
};


version(Windows)
@TestInfo()
immutable fail40 = q{--- fail40
	// Should fail on Windows target
	@extern(syscall, 60)
	void exit();
--- <error>
fail40(2, 2): Error: @extern(syscall) attribute is only implemented on linux
};


@TestInfo()
immutable fail41 = q{--- fail41
	import unknown;
--- <error>
fail41(1, 2): Error: Cannot find module `unknown`
};


@TestInfo()
immutable fail42 = q{--- fail42
	enum e7 : i32 { e7 = 7 } // type
	enum e9 { e9 = 9 } // type
	void accept_e9(e9){}
	void run() {
		accept_e9(e7.e7);
	}
--- <error>
fail42(5, 15): Error: Argument 1, must have type e9, not e7
};


@TestInfo()
immutable fail43 = q{--- fail43
	enum e9 { e9 = 9 } // type
	void accept_e9(e9){}
	void run() {
		accept_e9(42);
	}
--- <error>
fail43(4, 13): Error: Argument 1, must have type e9, not u8
};

// packages
@TestInfo()
immutable fail44 = q{--- fail44/mod1.vx
	module fail44.mod1;
	module fail44.mod2;
--- <error>
mod1(2, 2): Error: Module declaration can only occur as first declaration of the module
};

@TestInfo()
immutable fail45 = q{--- fail45/mod1.vx
	module mod1;
--- fail45/mod2.vx
	module mod1;
--- <error>
mod1(1, 2): Error: Module `mod1` in file fail45/mod2.vx conflicts with another module `mod1` in file fail45/mod1.vx
};

@TestInfo()
immutable fail46 = q{--- fail46/mod1.vx
	module fail46.mod1;
--- fail46/mod2.vx
	module fail46.mod1;
--- <error>
mod1(1, 2): Error: Module `fail46.mod1` in file fail46/mod2.vx conflicts with another module `fail46.mod1` in file fail46/mod1.vx
};

@TestInfo()
immutable fail47 = q{--- fail47/mod1.vx
	module fail47.mod1;
--- fail47/mod2.vx
	module fail47;
--- <error>
fail47(1, 2): Error: Module `fail47` in file fail47/mod2.vx conflicts with package `fail47` in files fail47/mod1.vx
};

@TestInfo()
immutable fail48 = q{--- fail48/mod1.vx
	module fail48;
--- fail48/mod2.vx
	module fail48.mod2;
--- <error>
mod2(1, 2): Error: Module `fail48.mod2` in file fail48/mod2.vx conflicts with another module `fail48` in file fail48/mod1.vx
};

@TestInfo()
immutable fail49 = q{--- fail49/mod1.vx
	module fail49.sub.mod1;
--- fail49/mod2.vx
	module fail49.sub;
--- <error>
sub(1, 2): Error: Module `fail49.sub` in file fail49/mod2.vx conflicts with package `fail49.sub` in files fail49/mod1.vx
};

@TestInfo()
immutable fail50 = q{--- fail50/mod1.vx
	module fail50.sub;
--- fail50/mod2.vx
	module fail50.sub.mod2;
--- <error>
mod2(1, 2): Error: Module `fail50.sub.mod2` in file fail50/mod2.vx conflicts with another module `fail50.sub` in file fail50/mod1.vx
};

@TestInfo()
immutable fail51 = q{--- fail51/mod1.vx
	module fail51.sub.mod1;
--- fail51/mod2.vx
	module fail51;
--- <error>
fail51(1, 2): Error: Module `fail51` in file fail51/mod2.vx conflicts with package `fail51` in files fail51/mod1.vx
};

@TestInfo()
immutable fail52 = q{--- fail52/mod1.vx
	module fail52;
--- fail52/mod2.vx
	module fail52.sub.mod2;
--- <error>
mod2(1, 2): Error: Module `fail52.sub.mod2` in file fail52/mod2.vx conflicts with another module `fail52` in file fail52/mod1.vx
};

@TestInfo()
immutable fail53 = q{--- fail53/mod1.vx
	module fail53.sub.mod1;
--- fail53/mod2.vx
	module fail53.sub.mod2;
--- fail53/mod3.vx
	module fail53.sub.mod3;
--- fail53/mod4.vx
	module fail53;
--- <error>
fail53(1, 2): Error: Module `fail53` in file fail53/mod4.vx conflicts with package `fail53` in files fail53/mod3.vx, fail53/mod1.vx and 1 more
};

/*
@TestInfo()
immutable fail54 = q{--- fail54/mod1.vx
	module fail54.sub.mod1;
--- fail54/mod2.vx
	module fail54.sub.mod2;
--- fail54/mod3.vx
	module fail54.sub.mod3;
--- fail54/mod4.vx
	module fail54.sub.mod4;
--- fail54/mod5.vx
	module fail54;
--- <error>
fail54(1, 2): Error: Module `fail54` in file fail54/mod5.vx conflicts with package `fail54` in files fail54/mod4.vx, fail54/mod1.vx and 2 more
};*/


// package imports
@TestInfo()
immutable fail55 = q{--- fail55/mod1.vx
	module fail55.mod1;
--- fail55/mod2.vx
	module fail55.mod2;
	import fail55;
--- <error>
mod2(2, 2): Error: Cannot import package `fail55`
};

@TestInfo()
immutable fail56 = q{--- fail56/mod1.vx
	module fail56.mod1;
--- fail56/mod2.vx
	module fail56.mod2;
	import fail56.mod1.mod1;
--- <error>
mod2(2, 2): Error: Cannot find module `fail56.mod1.mod1`. But there is module with name `fail56.mod1`
};
