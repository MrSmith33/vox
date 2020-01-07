/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.ctfe;

import tester;

Test[] ctfeTests() { return collectTests!(tests.ctfe)(); }


@TestInfo(&tester1)
immutable ctfe1 = q{--- ctfe1
	// Simplest case
	i32 func() { return 42; }
	enum i32 val = func(); // CTFE is performed at IR gen of `run`, `func` already has IR
	i32 run() {
		return val;
	}
};
void tester1(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("run")() == 42);
}


@TestInfo(&tester2)
immutable ctfe2 = q{--- ctfe2
	// Parameter and vreg
	i32 func(i32 param) { return param; }
	enum i32 val = func(42); // CTFE
	i32 run() {
		return val;
	}
};
void tester2(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("run")() == 42);
}
