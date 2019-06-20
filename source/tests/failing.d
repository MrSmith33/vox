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
fail1(1, 2): Error: Unterminated string literal
`;


@TestInfo()
immutable fail2 = `
--- fail2
	/*
--- <error>
fail2(1, 2): Error: Unterminated comment
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
fail5(1, 7): Error: Expected int constant, while got 'a'
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
