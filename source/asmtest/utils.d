/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module asmtest.utils;

//version(unittest)

public import amd64asm;
import utils;
import std.stdio;
import std.string : format;
CodeGen_x86_64 testCodeGen;

void assertHexAndReset(string file = __MODULE__, size_t line = __LINE__)(string expected) {
	assertEqual!(file, line)(expected, toHexString(testCodeGen.encoder.code));
	testCodeGen.encoder.resetPC();
}

private string toHexString(ubyte[] arr)
{
	return format("%(%02X%)", arr);
}

void assertEqual(string file = __MODULE__, size_t line = __LINE__, A, B)(A expected, B generated)
{
	if (expected != generated)
	{
		writefln("%s expected", expected);
		writefln("%s generated", generated);
		stdout.flush();
		writefln("at %s:%s", file, line);

		assert(false);
	}
}

