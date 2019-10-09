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

@TestInfo(&tester3, [HostSymbol("callee", cast(void*)&external_void_func)])
immutable reg_alloc3 = q{--- reg_alloc3
	// test register allocation
	void callee();
	i32 run(i32 a, i32 b, i32 c, i32 d)
	{
		i32 e = b * c;
		return a + b + c + d + e;
	}
};
void tester3(ref TestContext ctx) {
	assert(ctx.getFunctionPtr!(int, int, int, int, int)("run")(1, 1, 1, 1) == 5);
}

@TestInfo()
immutable reg_alloc4 = q{--- reg_alloc4
	void setRangeu8(u8* slice, u64 from, u64 to, u8 value) {}
	void formatInt(i64 i, u8[21]* output, u32 minSize, u8[]* result)
	{
		u32 numDigits = 0;
		if (i == 0)
		{
			if (minSize == 0)
				minSize = 1;
			setRangeu8((*output).ptr, 21 - minSize, 21, '0');
			(*output)[20] = '0';
			numDigits = minSize;
		}
		else
		{
			bool neg = i < 0;
			if (neg) {
				i = -i;
			}
			bool overflow = i < 0;
			if (overflow)
				i = i64.max;

			while (i)
			{
				u8 c = cast(u8)('0' + i % 10);
				(*output)[21 - ++numDigits] = c;
				i /= 10;
			}

			while (numDigits < minSize) {
				(*output)[21 - ++numDigits] = '0';
			}

			if (neg) {
				(*output)[21 - ++numDigits] = '-';
			}
			if (overflow) {
				++(*output)[20];
			}
		}
		(*result).ptr = (*output).ptr + (21 - numDigits);
		(*result).length = numDigits;
	}
};
