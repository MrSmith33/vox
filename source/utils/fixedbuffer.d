/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils.fixedbuffer;

///
FixedBuffer!T fixedBuffer(T)(T[] buffer)
{
	FixedBuffer!T res;
	res.setBuffer(cast(ubyte[])buffer);
	return res;
}

///
struct FixedBuffer(T)
{
	import utils : format;

	T* bufPtr;
	uint capacity;
	uint length;
	//uint committedBytes;

	ulong byteLength() { return cast(ulong)length * T.sizeof; }

	void setBuffer(ubyte[] newBuffer){//, uint committedBytes) {
		bufPtr = cast(T*)newBuffer.ptr;
		assert(bufPtr);
		size_t bufLen = newBuffer.length / T.sizeof;
		assert(bufLen <= uint.max, format("capacity overflow buf size %s", newBuffer.length));
		capacity = cast(uint)bufLen;
		//this.committedBytes = committedBytes;
		length = 0;
	}
	T[] buf() { return bufPtr[0..capacity]; }

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
