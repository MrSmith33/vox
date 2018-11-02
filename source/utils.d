/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils;

import std.traits : isIntegral;
public import std.algorithm : min, max;
import std.stdio;
import std.string : format;

enum size_t PAGE_SIZE = 4096;

version(Posix)
{
	ubyte[] allocate(size_t bytes, void* location, bool is_executable)
	{
		import core.sys.posix.sys.mman : mmap, MAP_ANON, PROT_READ,
			PROT_WRITE, PROT_EXEC, MAP_PRIVATE, MAP_FAILED;
		if (!bytes) return null;

		int protection = PROT_READ | PROT_WRITE | (is_executable ? PROT_EXEC : 0);

		auto p = mmap(location, bytes, protection, MAP_PRIVATE | MAP_ANON, -1, 0);
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
	import core.sys.windows.windows : GetLastError, VirtualAlloc, VirtualFree, VirtualProtect, MEM_COMMIT,
		PAGE_READWRITE, MEM_RELEASE, PAGE_EXECUTE_READWRITE, MEM_RESERVE;

	ubyte[] allocate(size_t bytes, void* location, MemType memoryType)
	{
		if (!bytes) return null;

		int protection;

		final switch(memoryType)
		{
			case MemType.RW:  protection = PAGE_READWRITE; break;
			case MemType.RWX: protection = PAGE_EXECUTE_READWRITE; break;
		}

		auto p = VirtualAlloc(location, bytes, MEM_COMMIT | MEM_RESERVE, protection);

		if (p == null)
		{
			import std.stdio;
			import std.windows.syserror;
			int errCode = GetLastError();
			writefln("allocate(%s:bytes, %s:location, %s:memoryType", bytes, location, memoryType);
			writeln(sysErrorString(errCode));
			assert(false, "VirtualAlloc alloc failed");
			return null;
		}

		return cast(ubyte[])p[0 .. bytes];
	}

	bool deallocate(ubyte[] b)
	{
		return b.ptr is null || VirtualFree(b.ptr, 0, MEM_RELEASE) != 0;
	}

	void markAsRW(void* addr, size_t numPages)
	{
		uint val;
		VirtualProtect(addr, numPages*PAGE_SIZE, PAGE_READWRITE, &val);
	}

	void testAdresses()
	{
		import std.stdio;
		import std.windows.syserror;
		import core.sys.windows.windows;
		size_t successful;
		size_t failed;
		size_t bytes = PAGE_SIZE * 1024;
		foreach(ulong loc; 0..16 * 16)
		{
			void* location = cast(void*)(loc*64*1024*1024);
			auto p = VirtualAlloc(location, bytes, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);

			if (p == null)
			{
				int errCode = GetLastError();
				writefln("Fail loc %s err '%s'", location, sysErrorString(errCode));
				++failed;
			}
			else
			{
				++successful;
				VirtualFree(p, 0, MEM_RELEASE);
				writefln("Success loc %s ptr %s", location, p);
			}
		}

		writefln("s %s", successful);
		writefln("f %s", failed);
	}
}

enum MemType
{
	RW,
	RWX
}

ubyte[] alloc_executable_memory(size_t bytes)
{
	return allocate(bytes, cast(void*)0x4000_0000UL, MemType.RWX);
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

T alignValue(T)(T value, T alignment) pure
{
	return cast(T)((value + (alignment-1)) & ~(alignment-1));
}

T paddingSize(T)(T address, T alignment)
{
	return cast(T)(alignValue(address, alignment) - address);
}

public import core.time : MonoTime, Duration, usecs, dur;
MonoTime currTime() { return MonoTime.currTime(); }

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

struct Buffer(T)
{
	import std.experimental.allocator.gc_allocator;
	alias allocator = GCAllocator.instance;

	T* bufPtr;
	uint capacity;
	T[] buf() { return bufPtr[0..capacity]; }
	// Must be kept private since it can be used to check for avaliable space
	// when used as output range
	uint length;

	// postblit
	this(this)
	{
		import core.memory;
		void[] tmp = allocator.allocate(capacity*T.sizeof);
		T* newBufPtr = cast(T*)tmp.ptr;
		newBufPtr[0..length] = bufPtr[0..length];
		bufPtr = newBufPtr;
		GC.addRange(bufPtr, capacity * T.sizeof, typeid(T));
	}

	bool empty() { return length == 0; }

	void put(T[] items ...)
	{
		reserve(items.length);
		bufPtr[length..length+items.length] = items;
		length += cast(uint)items.length;
	}

	void put(R)(R itemRange)
	{
		foreach(item; itemRange)
			put(item);
	}

	void stealthPut(T item)
	{
		reserve(1);
		bufPtr[length] = item;
	}

	/// Increases length and returns void-initialized slice to be filled by user
	T[] voidPut(size_t howMany)
	{
		reserve(howMany);
		length += howMany;
		return buf[length-howMany..length];
	}

	ref T opIndex(size_t at)
	{
		return bufPtr[at];
	}

	ref T back() { return bufPtr[length-1]; }

	inout(T[]) data() inout {
		return bufPtr[0..length];
	}

	alias opSlice = data;

	void clear() nothrow {
		length = 0;
	}

	void reserve(size_t items)
	{
		if (capacity - length < items)
		{
			import core.memory;
			GC.removeRange(bufPtr);
			size_t newCapacity = nextPOT(capacity + items);
			void[] tmp = buf;
			allocator.reallocate(tmp, newCapacity*T.sizeof);
			bufPtr = cast(T*)tmp.ptr;
			capacity = cast(uint)(tmp.length / T.sizeof);
			GC.addRange(bufPtr, capacity * T.sizeof, typeid(T));
		}
	}

	void removeInPlace(size_t index)
	{
		if (index+1 != length)
		{
			bufPtr[index] = bufPtr[length-1];
		}
		--length;
	}

	void unput(size_t numItems)
	{
		length = cast(uint)(length - numItems);
	}
}

struct IndentTextSink
{
	import std.range : repeat;
	TextSink sink;
	int indentSize = 2;
	private int indent;

	void putIndent() { sink.putf("%s", ' '.repeat(indent)); }
	void put(in char[] str) { putIndent; sink.put(str); }
	void putf(Args...)(const(char)[] fmt, Args args) { putIndent; sink.putf(fmt, args); }
	void putfln(Args...)(const(char)[] fmt, Args args) { putIndent; sink.putfln(fmt, args); }
	void putln(const(char)[] str = null) { putIndent; sink.putln(str); }

	void push() { indent += indentSize; }
	void pop() { indent -= indentSize; }
}

struct TextSink
{
	import std.format : formattedWrite;
	import std.string : stripRight;

	Buffer!char data;

	void clear() { data.clear(); }
	string text() { return stripRight(cast(string)data.data); }

	void put(in char[] str)
	{
		if (str.length == 0) return;
		data.put(str);
		data.stealthPut('\0');
	}

	void putf(Args...)(const(char)[] fmt, Args args) { formattedWrite(&this, fmt, args); }
	void putfln(Args...)(const(char)[] fmt, Args args) { formattedWrite(&this, fmt, args); put("\n"); }
	void putln(const(char)[] str = null) { put(str); put("\n"); }
}

struct FixedBuffer(T)
{
	T* bufPtr;
	uint capacity;
	void setBuffer(uint[] newBuffer) {
		bufPtr = newBuffer.ptr;
		assert(bufPtr);
		assert(newBuffer.length <= uint.max, "capacity overflow");
		capacity = cast(uint)newBuffer.length;
		length = 0;
	}
	T[] buf() { return bufPtr[0..capacity]; }

	uint length;

	bool empty() { return length == 0; }

	void put(T[] items ...)
	{
		reserve(items.length);
		bufPtr[length..length+items.length] = items;
		length += cast(uint)items.length;
	}

	void put(R)(R itemRange)
	{
		foreach(item; itemRange)
			put(item);
	}

	void stealthPut(T item)
	{
		reserve(1);
		bufPtr[length] = item;
	}

	/// Increases length and returns void-initialized slice to be filled by user
	T[] voidPut(size_t howMany)
	{
		reserve(howMany);
		length += howMany;
		return buf[length-howMany..length];
	}

	ref T opIndex(size_t at)
	{
		return bufPtr[at];
	}

	ref T back() { return bufPtr[length-1]; }

	inout(T[]) data() inout {
		return bufPtr[0..length];
	}

	void clear() nothrow {
		length = 0;
	}

	void reserve(size_t items)
	{
		if (capacity - length < items)
		{
			assert(false, format("out of memory: capacity %s, length %s, requested %s", capacity, length, items));
		}
	}

	T[] freePart()
	{
		return bufPtr[length..capacity];
	}
}

struct Win32Allocator
{
	import core.sys.windows.windows;

	enum PAGE_SIZE = 4096;
	void* bufferPtr;
	size_t reservedBytes;
	size_t committedBytes;
	size_t allocatedBytes;

	bool reserve(size_t size)
	{
		reservedBytes = divCeil(size, PAGE_SIZE) * PAGE_SIZE; // round up to page size
		bufferPtr = VirtualAlloc(null, reservedBytes, MEM_RESERVE, PAGE_NOACCESS);
		version(print_allocator) writefln("reserve %s, ptr %X", size, bufferPtr);
		return bufferPtr !is null;
	}

	void releaseMemory()
	{
		VirtualFree(bufferPtr, 0, MEM_RELEASE);
		bufferPtr = null;
		reservedBytes = 0;
		committedBytes = 0;
		allocatedBytes = 0;
	}

	void[] allocate(size_t numBytes)
	{
		version(print_allocator) writef("allocate %s, %s -> ", numBytes, this);
		scope(exit) version(print_allocator) writeln(this);
		if (numBytes == 0) return null;

		size_t newAllocatedBytes = allocatedBytes + numBytes;

		if (newAllocatedBytes > committedBytes) // commit more
		{
			size_t newCommittedBytes = alignValue(newAllocatedBytes, PAGE_SIZE);
			size_t bytesToCommit = newCommittedBytes - committedBytes;
			void* result = VirtualAlloc(bufferPtr + committedBytes, bytesToCommit, MEM_COMMIT, PAGE_READWRITE);
			if (result is null) return null;
			committedBytes = newCommittedBytes;
		}

		void* ptr = bufferPtr + allocatedBytes;
		allocatedBytes = newAllocatedBytes;

		return ptr[0..numBytes];
	}

	bool deallocate(void[] block)
	{
		version(print_allocator) writefln("deallocate %s", block.length);
		if (block.ptr + block.length == bufferPtr + allocatedBytes)
		{
			// Shrink allocated part if block is at the end of allocated area
			allocatedBytes -= block.length;
		}
		return true;
	}

	bool deallocateAll()
	{
		allocatedBytes = 0;
		return true;
	}

	bool reallocate(ref void[] block, size_t newSize)
	{
		version(print_allocator) writefln("\nreallocate ptr %X size %s -> %s", block.ptr, block.length, newSize);

		if (block.ptr + block.length == bufferPtr + allocatedBytes)
		{
			if (block.length >= newSize)
			{
				// Shrink if this is the last allocated block
				allocatedBytes = allocatedBytes - (block.length - newSize);
				block = block.ptr[0..newSize];
				version(print_allocator) writeln("  shrink last block");
				return true;
			}

			// Expand block that is the last allocated one
			void[] extra = allocate(newSize - block.length);
			if (extra.ptr !is null)
			{
				block = block.ptr[0..newSize];
				version(print_allocator) writefln("  expand last block %X:%s", block.ptr, block.length);
				return true;
			}
			return false;
		}

		if (block.length >= newSize)
		{
			// Dont reallocate if already satisfies
			block = block.ptr[0..newSize];
			version(print_allocator) writeln("  shrink block in place");
			return true;
		}

		// attempt full reallocation / block is null
		void[] newBlock = allocate(newSize);
		version(print_allocator) writefln("  reallocate block %X:%s", newBlock.ptr, newBlock.length);
		if (newBlock.ptr !is null)
		{
			newBlock[0..block.length] = block;
			block = newBlock;
			return true;
		}

		return false;
	}
}

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
