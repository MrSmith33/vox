/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils;

public import core.bitop : bsr;
public import core.time : MonoTime, Duration, usecs, dur;
public import std.algorithm : min, max, swap, map;
public import std.conv : to;
public import std.exception : enforce;
public import std.format : formattedWrite;
public import std.stdio : stdout, write, writef, writeln, writefln;
public import std.string : format;

public import utils.arena;
public import utils.arenapool;
public import utils.array;
public import utils.arrayarena;
public import utils.buffer;
public import utils.har : parseHar;
public import utils.hash;
public import utils.mem;
public import utils.numfmt;
public import utils.textsink;

enum PAGE_SIZE = 4096;
enum ulong GiB = 1024UL*1024*1024;

string[] gatherEnumStrings(E)()
{
	string[] res = new string[__traits(allMembers, E).length];
	foreach (i, m; __traits(allMembers, E))
	{
		res[i] = __traits(getAttributes, __traits(getMember, E, m))[0];
	}
	return res;
}

enum PrintAscii { no = false, yes = true }
void printHex(ubyte[] buffer, size_t lineLength, PrintAscii printAscii = PrintAscii.no)
{
	import std.stdio;

	size_t index = 0;

	static char toAscii(ubyte b) {
		if (b < 32) return '.';
		if (b > 126) return '.';
		return cast(char)b;
	}

	if (lineLength) {
		while (index + lineLength <= buffer.length) {
			writef("%(%02X %)", buffer[index..index+lineLength]);
			if (printAscii) {
				write(' ');
				buffer[index..index+lineLength].map!toAscii.write;
			}
			writeln;
			index += lineLength;
		}
	}

	if (index < buffer.length)
	{
		writef("%(%02X %)", buffer[index..buffer.length]);
		if (printAscii) {
			foreach(_; 0..(index + lineLength - buffer.length)*3 + 1) write(' ');
			buffer[index..buffer.length].map!toAscii.write;
		}
		writeln;
	}
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

struct FileDataSlicer
{
	ubyte[] fileData;
	size_t fileCursor = 0;

	// Returns array of Ts of length 'length' stating from fileCursor offset in fileData
	T[] getArrayOf(T)(size_t length)
	{
		enforce(fileData.length >= fileCursor + T.sizeof * length);
		auto res = (cast(T*)(fileData.ptr + fileCursor))[0..length];
		fileCursor += T.sizeof * length;
		return res;
	}

	T* getPtrTo(T)() { return getArrayOf!T(1).ptr; }
	T parseBigEndian(T)() {
		ubyte[T.sizeof] buf = getArrayOf!ubyte(T.sizeof);
		return bigEndianToNative!T(buf);
	}

	void advanceToAlignment(size_t alignment) { fileCursor += paddingSize(fileCursor, alignment); }
}

import core.bitop : bsf, bt, bts, btr;
bool setBitAt(T)(T[] bitmap, size_t at) { return bts(cast(size_t*)bitmap.ptr, at) != 0; }
bool resetBitAt(T)(T[] bitmap, size_t at) { return btr(cast(size_t*)bitmap.ptr, at) != 0; }
bool getBitAt(T)(T[] bitmap, size_t at) { return bt(cast(size_t*)bitmap.ptr, at) != 0; }

// Most efficient with ulong
// Iterates all set bits in increasing order
BitsSet!T bitsSet(T)(T[] bitmap) { return BitsSet!T(bitmap); }

struct BitsSet(T)
{
	T[] bitmap;

	int opApply(scope int delegate(size_t) dg)
	{
		foreach (size_t slotIndex, T slotBits; bitmap)
		{
			while (slotBits != 0)
			{
				// Extract lowest set isolated bit
				// 111000 -> 001000; 0 -> 0
				T lowestSetBit = slotBits & -slotBits;

				size_t lowestSetBitIndex = bsf(slotBits);
				if (int res = dg(slotIndex * T.sizeof * 8 + lowestSetBitIndex)) return res;

				// Disable lowest set isolated bit
				// 111000 -> 110000
				slotBits ^= lowestSetBit;
			}
		}

		return 0;
	}
}
