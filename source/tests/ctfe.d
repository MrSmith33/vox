/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.ctfe;

import tester;

Test[] ctfeTests() { return collectTests!(tests.ctfe)(); }


@TestInfo(&tester1)
immutable ctfe1 = q{--- ctfe1
	// Test CTFE
	i32 func() { return 42; }
	enum i32 val = func(); // CTFE is performed at IR gen of `run`, `func` already has IR
	i32 run() {
		return val;
	}
};
void tester1(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("run")() == 42);
}
