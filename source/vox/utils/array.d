/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.utils.array;

// Optimal for 1, 2, 4 byte items.
// Best with POT sized items
// Can store inline up to 8 bytes
struct Array(T)
{
	import vox.utils : isPowerOfTwo, nextPOT, divCeil, min, max, writefln, ArrayArena, format;

	// Can be 0
	enum uint NUM_INLINE_ITEMS = size_t.sizeof / T.sizeof;
	enum uint MIN_EXTERNAL_BYTES = max(ArrayArena.MIN_BLOCK_BYTES, nextPOT((NUM_INLINE_ITEMS + 1) * T.sizeof));

	private uint _length;
	private uint _capacity = NUM_INLINE_ITEMS;

	union
	{
		// Used when length <= NUM_INLINE_ITEMS
		private T[NUM_INLINE_ITEMS] inlineItems;

		// Used when length > NUM_INLINE_ITEMS
		private T* externalArray;
	}

	bool empty() { return _length == 0; }
	uint length() { return _length; }
	uint opDollar() { return _length; }
	uint capacity() { return _capacity; }
	ref T front() { return this[0]; }
	ref T back() { return this[$-1]; }
	void clear() { _length = 0; }

	ref T opIndex(size_t index)
	{
		assert(index < _capacity, format("opIndex(%s), capacity %s", index, _capacity));
		static if (NUM_INLINE_ITEMS > 0) {
			if (_capacity == NUM_INLINE_ITEMS) return inlineItems[index];
		}

		return externalArray[index];
	}

	Array!T dup(ref ArrayArena arena)
	{
		Array!T copy = this;

		static if (NUM_INLINE_ITEMS > 0) {
			if (_capacity == NUM_INLINE_ITEMS) return copy;
		}

		size_t byteCapacity = nextPOT(_capacity * T.sizeof);

		// When we have empty array with NUM_INLINE_ITEMS == 0 and no allocated external array
		if (byteCapacity == 0) return copy;

		ubyte[] block = (cast(ubyte*)externalArray)[0..byteCapacity];

		ubyte[] newBlock = arena.allocBlock(block.length);
		newBlock[] = block;
		copy.externalArray = cast(T*)newBlock.ptr;
		return copy;
	}

	void voidPut(ref ArrayArena arena, uint howMany)
	{
		if (_length + howMany > _capacity) extend(arena, howMany);
		_length += howMany;
	}

	void put(ref ArrayArena arena, T[] items...)
	{
		if (_length + items.length > _capacity) extend(arena, cast(uint)items.length);

		_length += items.length;
		this[_length-items.length..$][] = items;
	}

	void putFront(ref ArrayArena arena, T item)
	{
		putAt(arena, 0, item);
	}

	// shifts items to the right
	void putAt(ref ArrayArena arena, size_t at, T[] items...)
	{
		replaceAt(arena, at, 0, items);
	}

	void replaceAt(ref ArrayArena arena, size_t at, size_t numItemsToRemove, T[] itemsToInsert)
	{
		assert(at + numItemsToRemove <= _length);

		size_t numItemsToInsert = itemsToInsert.length;

		replaceAtVoid(arena, at, numItemsToRemove, numItemsToInsert);
		this[at..at+numItemsToInsert][] = itemsToInsert;
	}

	void replaceAtVoid(ref ArrayArena arena, size_t at, size_t numItemsToRemove, size_t numItemsToInsert)
	{
		assert(at + numItemsToRemove <= _length);

		if (numItemsToInsert == numItemsToRemove)
		{
			// no resize or moves needed
		}
		else
		{
			ptrdiff_t delta = numItemsToInsert - numItemsToRemove;

			if (_length + delta > _capacity) extend(arena, cast(uint)delta);

			scope(exit) _length += delta;

			size_t start = at + numItemsToRemove;
			size_t numItemsToMove = _length - start;
			T* ptr = externalArray + start;

			static if (NUM_INLINE_ITEMS > 0) {
				if (_capacity == NUM_INLINE_ITEMS) ptr = inlineItems.ptr + start;
			}

			import core.stdc.string : memmove;
			memmove(ptr + delta, ptr, numItemsToMove * T.sizeof);
		}
	}

	void unput(size_t numItems)
	{
		_length = cast(uint)(_length - numItems);
	}

	void reserve(ref ArrayArena arena, uint howMany)
	{
		if (_length + howMany > _capacity) extend(arena, howMany);
	}

	// returns memory to arena and zeroes the length
	void free(ref ArrayArena arena) {
		scope(exit) {
			externalArray = null;
			_length = 0;
			_capacity = NUM_INLINE_ITEMS;
		}
		static if (NUM_INLINE_ITEMS > 0) {
			if (_capacity == NUM_INLINE_ITEMS) return; // no-op
		}

		size_t byteCapacity = nextPOT(_capacity * T.sizeof);
		ubyte[] oldBlock = (cast(ubyte*)externalArray)[0..byteCapacity];
		arena.freeBlock(oldBlock);
	}

	// extend the storage
	private void extend(ref ArrayArena arena, uint items)
	{
		uint byteCapacityNeeded = cast(uint)nextPOT((_length + items) * T.sizeof);
		//writefln("extend %s", _capacity);
		if (_capacity == NUM_INLINE_ITEMS) {
			ubyte[] newBlock = arena.allocBlock(max(byteCapacityNeeded, MIN_EXTERNAL_BYTES));
			static if (NUM_INLINE_ITEMS > 0) {
				ubyte[] oldBlock = cast(ubyte[])inlineItems[];
				newBlock[0..oldBlock.length] = oldBlock;
			}
			externalArray = cast(T*)newBlock.ptr;
			_capacity = cast(uint)(newBlock.length / T.sizeof);
			//writefln("  1 cap %s", _capacity);
			return;
		}

		size_t byteCapacity = nextPOT(_capacity * T.sizeof);
		ubyte[] block = (cast(ubyte*)externalArray)[0..byteCapacity];
		resizeSmallArray(arena, block, byteCapacityNeeded);
		externalArray = cast(T*)block.ptr;
		_capacity = cast(uint)(block.length / T.sizeof);
		//writefln("  2 cap %s", _capacity);
	}

	// Doubles the size of block
	private void resizeSmallArray(ref ArrayArena arena, ref ubyte[] oldBlock, size_t newLength) {
		assert(isPowerOfTwo(oldBlock.length));
		assert(oldBlock.length >= ArrayArena.MIN_BLOCK_BYTES);
		assert(newLength >= ArrayArena.MIN_BLOCK_BYTES, "too small");

		ubyte[] newBlock = arena.allocBlock(newLength);
		newBlock[0..oldBlock.length] = oldBlock;
		arena.freeBlock(oldBlock);
		oldBlock = newBlock;
	}

	inout(T)[] opSlice() inout
	{
		static if (NUM_INLINE_ITEMS > 0) {
			if (_capacity == NUM_INLINE_ITEMS) return inlineItems.ptr[0.._length];
		}
		return externalArray[0.._length];
	}

	inout(T)[] opSlice(size_t from, size_t to) inout
	{
		return this[][from..to];
	}

	void removeInPlace(size_t at)
	{
		if (at+1 != _length)
		{
			this[at] = this[_length-1];
		}
		--_length;
	}

	void removeByShift(size_t at, size_t numToRemove = 1)
	{
		size_t to = at;
		size_t from = at + numToRemove;
		while(from < _length)
		{
			this[to] = this[from];
			++to;
			++from;
		}
		_length -= numToRemove;
	}

	void toString(scope void delegate(const(char)[]) sink) const {
		import std.format : formattedWrite;
		sink("[");
		size_t i;
		foreach(const ref T item; opSlice()) {
			if (i > 0) sink(", ");
			sink.formattedWrite("%s", item);
			++i;
		}
		sink("]");
	}
}

struct InvertedArray(T)
{
	import vox.utils : isPowerOfTwo, nextPOT, divCeil, min, max, writefln, ArrayArena, format;
	import std.range : retro;
	Array!(T) array;

	bool empty() { return array._length == 0; }
	uint length() { return array._length; }
	uint opDollar() { return array._length; }
	uint capacity() { return array._capacity; }
	ref T front() { return array[$-1]; }
	ref T back() { return array[0]; }
	void clear() { array._length = 0; }

	ref T opIndex(size_t index)
	{
		assert(index < array._capacity, format("opIndex(%s), capacity %s", index, array._capacity));
		static if (array.NUM_INLINE_ITEMS > 0) {
			if (array._capacity == array.NUM_INLINE_ITEMS) return array.inlineItems[array._length - index - 1];
		}

		return array.externalArray[array._length - index - 1];
	}

	InvertedArray!T dup(ref ArrayArena arena) {
		InvertedArray!T copy = this;
		copy.array = copy.array.dup(arena);
		return copy;
	}

	void put(ref ArrayArena arena, T item) {
		array.putAt(arena, 0, item);
	}

	// shifts items to the right
	void putAt(ref ArrayArena arena, size_t at, T[] items...) {
		array.replaceAt(arena, array._length - at, 0, items);
	}

	void replaceAt(A)(ref ArrayArena arena, size_t at, size_t numItemsToRemove, A itemsToInsert) {
		assert(at + numItemsToRemove <= array._length);

		size_t numItemsToInsert = itemsToInsert.length;

		array.replaceAtVoid(arena, array._length-at-numItemsToRemove, numItemsToRemove, numItemsToInsert);
		foreach(i; 0..numItemsToInsert)
			this[at+numItemsToInsert-i-1] = itemsToInsert[i];
	}

	void unput(size_t numItems) {
		array.removeByShift(0, numItems);
	}

	void reserve(ref ArrayArena arena, uint howMany) {
		if (array._length + howMany > array._capacity) array.extend(arena, howMany);
	}

	// returns memory to arena and zeroes the length
	void free(ref ArrayArena arena) {
		array.free(arena);
	}

	auto opSlice() {
		static if (array.NUM_INLINE_ITEMS > 0) {
			if (array._capacity == array.NUM_INLINE_ITEMS) return array.inlineItems.ptr[0..array._length].retro;
		}
		return array.externalArray[0..array._length].retro;
	}

	auto opSlice(size_t from, size_t to) {
		return this[][from..to];
	}

	void removeByShift(size_t at, size_t numToRemove = 1) {
		array.removeByShift(array._length - at - numToRemove, numToRemove);
	}

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		sink("[");
		size_t i;
		foreach(ref T item; opSlice()) {
			if (i > 0) sink(", ");
			sink.formattedWrite("%s", item);
			++i;
		}
		sink("]");
	}
}

