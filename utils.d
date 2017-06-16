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

void printMemBytes(void* ptr, size_t bytes, size_t width = 8)
{
	size_t lines = bytes % width == 0 ? bytes/width : bytes/width + 1;
	printMemLines(ptr, lines, width);
}

void printMemLines(void* ptr, size_t lines, size_t width = 8)
{
	foreach(i; 0..lines)
	{
		auto offset = i * width;
		import std.stdio : writefln;
		writefln("%(%02x %)", ptr[offset..offset+width]);
	}
}

void printHex(ubyte[] buffer, size_t lineLength)
{
	import std.stdio;
	size_t index = 0;
	if (lineLength)
		while (index + lineLength <= buffer.length)
	{
		writefln("%(%02X%)", buffer[index..index+lineLength]);
		index += lineLength;
	}

	if (index < buffer.length)
		writefln("%(%02X%)", buffer[index..buffer.length]);
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

struct ArraySink {
	import amd64asm;
	ubyte[] buffer;
	size_t length;
	ubyte[] data() { return buffer[0..length]; }
	void reserve(size_t bytes) {
		if (buffer.length - length < bytes) {
			buffer.length = nextPOT(buffer.length + bytes);
		}
	}
	void reset() { length = 0; }
	void pad(size_t numBytes) {
		reserve(numBytes);
		length += numBytes;
	}
	void put(ubyte val) {
		reserve(1);
		buffer[length++] = val;
	}
	void put(ubyte[] val) {
		reserve(val.length);
		buffer[length..length+val.length] = val;
		length += val.length;
	}
	void put(Int)(Int value)
		if (isIntegral!Int)
	{
		reserve(value.sizeof);
		static if (value.sizeof >= 1) {
			put((value >>  0) & 0xFF);
		}
		static if (value.sizeof >= 2) {
			put((value >>  8) & 0xFF);
		}
		static if (value.sizeof >= 4) {
			put((value >> 16) & 0xFF);
			put((value >> 24) & 0xFF);
		}
		static if (value.sizeof >= 8) {
			put((value >> 32) & 0xFF);
			put((value >> 40) & 0xFF);
			put((value >> 48) & 0xFF);
			put((value >> 56) & 0xFF);
		}
	}
	void put(Imm8 u8) {
		put(u8.value);
	}
	void put(Imm16 u16) {
		put(u16.value);
	}
	void put(Imm32 u32) {
		put(u32.value);
	}
	void put(Imm64 u64) {
		put(u64.value);
	}
}
