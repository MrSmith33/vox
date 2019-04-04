/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils;

public import core.time : MonoTime, Duration, usecs, dur;
public import std.algorithm : min, max;
public import std.string : format;
public import std.conv : to;
public import std.stdio;
public import core.bitop : bsr;

public import utils.arena;
public import utils.arenapool;
public import utils.array;
public import utils.buffer;
public import utils.fixedbuffer;
public import utils.mem;
public import utils.numfmt;
public import utils.textsink;

enum PAGE_SIZE = 4096;
enum ulong GiB = 1024UL*1024*1024;

void printHex(ubyte[] buffer, size_t lineLength)
{
	import std.stdio;

	size_t index = 0;

	if (lineLength) {
		while (index + lineLength <= buffer.length) {
			writefln("%(%02X %)", buffer[index..index+lineLength]);
			index += lineLength;
		}
	}

	if (index < buffer.length)
		writefln("%(%02X %)", buffer[index..buffer.length]);
}

T divCeil(T)(T a, T b)
{
	return a / b + (a % b > 0);
}

T nextPOT(T)(T x)
{
	--x;
	x |= x >> 1;  // handle  2 bit numbers
	x |= x >> 2;  // handle  4 bit numbers
	x |= x >> 4;  // handle  8 bit numbers
	static if (T.sizeof >= 2) x |= x >> 8;  // handle 16 bit numbers
	static if (T.sizeof >= 4) x |= x >> 16; // handle 32 bit numbers
	static if (T.sizeof >= 8) x |= x >> 32; // handle 64 bit numbers
	++x;

	return x;
}

T isPowerOfTwo(T)(T x)
{
	return (x != 0) && ((x & (~x + 1)) == x);
}

/// alignment is POT
T alignValue(T)(T value, T alignment) pure
{
	assert(isPowerOfTwo(alignment), format("alignment is not power of two (%s)", alignment));
	return cast(T)((value + (alignment-1)) & ~(alignment-1));
}

/// multiple can be NPOT
T roundUp(T)(T value, T multiple) pure
{
	assert(multiple != 0, "multiple must not be zero");
	return cast(T)(((value + multiple - 1) / multiple) * multiple);
}

/// alignment is POT
T paddingSize(T)(T address, T alignment)
{
	return cast(T)(alignValue(address, alignment) - address);
}

MonoTime currTime() { return MonoTime.currTime(); }

T[] removeInPlace(T)(T[] array, T what)
{
	size_t i = 0;
	size_t length = array.length;
	while(i < length)
	{
		if (array[i] == what)
		{
			array[i] = array[length-1];
			--length;
		}
		++i;
	}
	return assumeSafeAppend(array[0..length]);
}

unittest
{
	assert(removeInPlace([], 1) == []);
	assert(removeInPlace([1], 1) == []);
	assert(removeInPlace([1], 2) == [1]);
	assert(removeInPlace([1, 2], 2) == [1]);
	assert(removeInPlace([2, 1], 2) == [1]);
}


