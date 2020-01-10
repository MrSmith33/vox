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
		return val; // returns 42
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
		return val; // returns 42
	}
};
void tester2(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("run")() == 42);
}


@TestInfo(&tester3)
immutable ctfe3 = q{--- ctfe3
	// add
	i32 func(i32 param) { return param + param; }
	enum i32 val = func(42); // CTFE
	i32 run() {
		return val; // returns 84
	}
};
void tester3(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("run")() == 84);
}


@TestInfo(&tester4)
immutable ctfe4 = q{--- ctfe4
	// control flow
	i32 sign(i32 number) {
		i32 result;
		if (number < 0) result = -1;
		else if (number > 0) result = 1;
		else result = 0;
		return result;
	}
	enum i32 val0 = sign(-1); // CTFE
	enum i32 val1 = sign( 0); // CTFE
	enum i32 val2 = sign( 1); // CTFE
	i32 get_val0() { return val0; } // -1
	i32 get_val1() { return val1; } //  0
	i32 get_val2() { return val2; } //  1
};
void tester4(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("get_val0")() == -1);
	assert(ctx.getFunctionPtr!(int)("get_val1")() ==  0);
	assert(ctx.getFunctionPtr!(int)("get_val2")() ==  1);
}


@TestInfo(&tester5)
immutable ctfe5 = q{--- ctfe5
	// call
	i32 fib(i32 number) {
		if (number < 1) return 0;
		if (number < 3) return 1;
		return fib(number-1) + fib(number-2);
	}
	enum i32 val = fib(6); // CTFE
	i32 get() { return val; } // 8
};
void tester5(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int)("get")() == 8);
}
