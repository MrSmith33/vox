/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module utils.buffer;

struct Buffer(T)
{
	import utils : nextPOT, format;
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
		assert(at < capacity, format("opIndex(%s), capacity %s", at, capacity));
		return bufPtr[at];
	}

	ref T back() { return bufPtr[length-1]; }

	T[] data() {
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
