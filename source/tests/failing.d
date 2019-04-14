/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.failing;

import tester;

Test[] failingTests() { return [
	fail1, fail2];
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
