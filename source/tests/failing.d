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
fail3(2, 1): Error: Expected `IDENTIFIER` token, while got `EOI` token ''
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
fail8(2, 1): Error: Expected `LCURLY` token, while got `EOI` token ''
";


@TestInfo()
immutable fail9 = r"
--- fail9
	struct test {
--- <error>
fail9(2, 1): Error: Expected `RCURLY` token, while got `EOI` token ''
";


@TestInfo()
immutable fail10 = r"
--- fail10
	i32 fun(
--- <error>
fail10(2, 1): Error: Expected `RPAREN` token, while got `EOI` token ''
";


@TestInfo()
immutable fail11 = r"
--- fail11
	i32 fun(){
--- <error>
fail11(2, 1): Error: Expected `RCURLY` token, while got `EOI` token ''
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
fail13(3, 1): Error: Expected `SEMICOLON` token, while got `EOI` token ''
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
fail22(3, 3): Error: Cannot implicitly convert expression of type `null` to `u32`
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
