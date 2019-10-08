/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tests.reg_alloc;

import std.stdio;
import tester;

Test[] regAllocTests() { return collectTests!(tests.reg_alloc)(); }

extern(C) void external_void_func(){}

@TestInfo(&tester1, [HostSymbol("callee", cast(void*)&external_void_func)])
immutable reg_alloc1 = q{--- reg_alloc1
	// test register allocation
	void callee(); // uses all registers causing spilling
	i32 run(i32 a)
	{
		callee;
		return a;
	}
};
void tester1(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int, int)("run")(1) == 1);
}

@TestInfo(&tester2, [HostSymbol("callee", cast(void*)&external_void_func)])
immutable reg_alloc2 = q{--- reg_alloc2
	// test register allocation
	void callee();
	i32 run(i32 a, i32 b, i32 c, i32 d)
	{
		i32 e = b * c;
		callee;
		return a + b + c + d + e;
	}
};
void tester2(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int, int, int, int, int)("run")(1, 1, 1, 1) == 5);
}
