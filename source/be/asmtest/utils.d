/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.asmtest.utils;

public import be.amd64asm;
import utils;

struct CodegenTester
{
	CodeGen_x86_64 gen;
	alias gen this;

	void assertHexAndReset(string file = __MODULE__, size_t line = __LINE__)(string expected) {
		assertEqual!(file, line)(expected, toHexString(gen.encoder.code));
		gen.encoder.resetPC();
	}
}

void assertEqual(string file = __MODULE__, size_t line = __LINE__, A, B)(A expected, B generated)
{
	if (expected != generated)
	{
		import std.stdio;
		writefln("%s expected", expected);
		writefln("%s generated", generated);
		stdout.flush();
		writefln("at %s:%s", file, line);

		assert(false);
	}
}

private string toHexString(ubyte[] arr)
{
	import std.string : format;
	return format("%(%02X%)", arr);
}
