/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.failing;

import tester;

Test[] failingTests() { return [
	fail1, fail2, fail3, fail4, fail5, fail6, fail7, fail8, fail9, fail10,
	fail11, fail12, fail13];
}

immutable input1 = `
--- fail1
	"dddd
--- <error>
fail1(1, 2): Error: Unterminated string literal
`;
auto fail1 = Test("Fail 1", input1);

immutable input2 = `
--- fail2
	/*
--- <error>
fail2(1, 2): Error: Unterminated comment
`;
auto fail2 = Test("Fail 2", input2);

immutable input3 = r"
--- fail3
	struct
--- <error>
fail3(2, 1): Error: Expected `IDENTIFIER` token, while got `EOI` token ''
";
auto fail3 = Test("Fail 3", input3);

immutable input4 = r"
--- fail4
	int a = ;
--- <error>
fail4(1, 10): Error: SEMICOLON is not an expression
";
auto fail4 = Test("Fail 4", input4);

immutable input5 = r"
--- fail5
	int[ a;
--- <error>
fail5(1, 7): Error: Expected int constant, while got 'a'
";
auto fail5 = Test("Fail 5", input5);

immutable input6 = r"
--- fail6
	int[] ;
--- <error>
fail6(1, 8): Error: Expected `IDENTIFIER` token, while got `SEMICOLON` token ';'
";
auto fail6 = Test("Fail 6", input6);

immutable input7 = r"
--- fail7
	int[8 ;
--- <error>
fail7(1, 8): Error: Expected `RBRACKET` token, while got `SEMICOLON` token ';'
";
auto fail7 = Test("Fail 7", input7);

immutable input8 = r"
--- fail8
	struct test
--- <error>
fail8(2, 1): Error: Expected `LCURLY` token, while got `EOI` token ''
";
auto fail8 = Test("Fail 8", input8);

immutable input9 = r"
--- fail9
	struct test {
--- <error>
fail9(2, 1): Error: Expected `RCURLY` token, while got `EOI` token ''
";
auto fail9 = Test("Fail 9", input9);

immutable input10 = r"
--- fail10
	i32 fun(
--- <error>
fail10(2, 1): Error: Expected `RPAREN` token, while got `EOI` token ''
";
auto fail10 = Test("Fail 10", input10);

immutable input11 = r"
--- fail11
	i32 fun(){
--- <error>
fail11(2, 1): Error: Expected `RCURLY` token, while got `EOI` token ''
";
auto fail11 = Test("Fail 11", input11);

immutable input12 = r"
--- fail12
	i32 fun(){
		a = b +
--- <error>
fail12(3, 1): Error: Unexpected end of input
";
auto fail12 = Test("Fail 12", input12);

immutable input13 = r"
--- fail13
	i32 fun(){
		a = b + c
--- <error>
fail13(3, 1): Error: Expected `SEMICOLON` token, while got `EOI` token ''
";
auto fail13 = Test("Fail 13", input13);
