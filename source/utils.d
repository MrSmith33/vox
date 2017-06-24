/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils;

import std.traits : isIntegral;

version(Posix)
{
	ubyte[] allocate(size_t bytes, bool is_executable)
	{
		import core.sys.posix.sys.mman : mmap, MAP_ANON, PROT_READ,
			PROT_WRITE, PROT_EXEC, MAP_PRIVATE, MAP_FAILED;
		if (!bytes) return null;

		int protection = PROT_READ | PROT_WRITE | (is_executable ? PROT_EXEC : 0);

		auto p = mmap(null, bytes, protection, MAP_PRIVATE | MAP_ANON, -1, 0);
		if (p is MAP_FAILED) return null;
		return cast(ubyte[])p[0 .. bytes];
	}

	bool deallocate(ubyte[] b)
	{
		import core.sys.posix.sys.mman : munmap;
		if (b.ptr) munmap(b.ptr, b.length) == 0 || assert(0);
		return true;
	}
}
else version(Windows)
{
	import core.sys.windows.windows : VirtualAlloc, VirtualFree, MEM_COMMIT,
		PAGE_READWRITE, MEM_RELEASE, PAGE_EXECUTE_READWRITE, MEM_RESERVE;

	ubyte[] allocate(size_t bytes, bool is_executable)
	{
		if (!bytes) return null;

		int protection = is_executable ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE;

		auto p = VirtualAlloc(null, bytes, MEM_COMMIT | MEM_RESERVE, protection);
		if (p == null)
			return null;
		return cast(ubyte[])p[0 .. bytes];
	}

	bool deallocate(ubyte[] b)
	{
		return b.ptr is null || VirtualFree(b.ptr, 0, MEM_RELEASE) != 0;
	}
}

ubyte[] alloc_executable_memory(size_t bytes)
{
	return allocate(bytes, true);
}

bool free_executable_memory(ubyte[] bytes)
{
	return deallocate(bytes);
}

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

T alignValue(T)(T value, T alignment) pure
{
	return cast(T)((value + (alignment-1)) & ~(alignment-1));
}

T paddingSize(T)(T address, T alignment)
{
	return cast(T)(alignValue(address, alignment) - address);
}

struct ScaledNumberFmt(T)
{
	T value;
	void toString()(scope void delegate(const(char)[]) sink)
	{
		int scale = calcScale(value);
		auto scaledValue = scaled(value, -scale);
		int digits = numDigitsInNumber(scaledValue);
		import std.format : formattedWrite;
		sink.formattedWrite("%*.*f%s", digits, 3-digits, scaledValue, scaleSuffixes[scaleToScaleIndex(scale)]);
	}
}

auto scaledNumberFmt(T)(T value)
{
	return ScaledNumberFmt!T(value);
}

import std.datetime : Duration;
auto scaledNumberFmt(Duration value, double scale = 1)
{
	double seconds = value.total!"hnsecs" / 10_000_000.0;
	return ScaledNumberFmt!double(seconds * scale);
}

// -24 .. 24, with step of 3. Or -8 to 8 with step of 1
immutable string[] scaleSuffixes = ["y","z","a","f","p","n","u","m","","K","M","G","T","P","E","Z","Y"];

int numDigitsInNumber(Num)(const Num val)
{
	import std.math: abs;
	ulong absVal = cast(ulong)abs(val);
	int numDigits = 1;

	while (absVal >= 10)
	{
		absVal /= 10;
		++numDigits;
	}

	return numDigits;
}

int calcScale(Num)(Num val)
{
	import std.algorithm: clamp;
	import std.math: abs, floor, ceil, log10;
	static int signum(T)(const T x) nothrow
	{
	    return (x > 0) - (x < 0);
	}

	auto lg = log10(abs(val));
	int logSign = signum(lg);
	double absLog = abs(lg);

	int scale;
	if (lg < 0)
		scale = cast(int)(ceil(absLog/3.0))*3;
	else
		scale = cast(int)(floor(absLog/3.0))*3;

	int clampedScale = clamp(scale * logSign, -24, 24);

	return clampedScale;
}

int scaleToScaleIndex(int scale)
{
	return scale / 3 + 8; // -24..24 -> -8..8 -> 0..16
}

double scaled(Num)(Num num, int scale)
{
	import std.math: pow;
	return num * pow(10.0, scale);
}
