/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module utils.array;

// Optimal for 1, 2, 4 byte items.
// Best with POT sized items
// Can store inline up to 8 bytes
struct Array(T)
{
	import utils : isPowerOfTwo, nextPOT, PAGE_SIZE, max, writefln, ArrayArena;

	// Can be 0
	enum NUM_INLINE_ITEMS = 8 / T.sizeof;
	enum MIN_EXTERNAL_BYTES = max(ArrayArena.MIN_BLOCK_BYTES, nextPOT((NUM_INLINE_ITEMS + 1) * T.sizeof));
	enum MIN_EXTERNAL_ITEMS = MIN_EXTERNAL_BYTES / T.sizeof;
	enum NUM_ITEMS_PER_PAGE = PAGE_SIZE / T.sizeof;

	private uint _length;
	private uint _capacity = NUM_INLINE_ITEMS;

	union
	{
		// Used when length <= NUM_INLINE_ITEMS
		private T[NUM_INLINE_ITEMS] inlineItems;

		// Used when length <= page_size / T.sizeof
		private T* externalArray;

		// Used when length > page_size / T.sizeof
		// Points to T*[page_size / ptr.sizeof]
		private T** chunkedArray;
	}

	bool empty() { return _length == 0; }
	uint length() { return _length; }
	uint opDollar() { return _length; }
	alias capacity = _capacity;
	ref T back() { return this[$-1]; }

	ref T opIndex(size_t index)
	{
		static if (NUM_INLINE_ITEMS > 0)
			if (_capacity == NUM_INLINE_ITEMS)
				return inlineItems[index];

		if (capacity <= NUM_ITEMS_PER_PAGE)
			return externalArray[index];

		size_t chunkIndex = index / NUM_ITEMS_PER_PAGE;
		size_t chunkPos = index % NUM_ITEMS_PER_PAGE;
		return chunkedArray[chunkIndex][chunkPos];
	}

	void put(ref ArrayArena arena, T item)
	{
		if (_length == _capacity) extend(arena);

		this[_length] = item;
		++_length;
	}

	// extend the storage
	private void extend(ref ArrayArena arena)
	{
		//writefln("extend %s", _capacity);
		if (_capacity == NUM_INLINE_ITEMS) {
			ubyte[] newBlock = arena.allocBlock(MIN_EXTERNAL_BYTES);
			static if (NUM_INLINE_ITEMS > 0) {
				ubyte[] oldBlock = cast(ubyte[])inlineItems[];
				newBlock[0..oldBlock.length] = oldBlock;
			}
			externalArray = cast(T*)newBlock.ptr;
			_capacity = MIN_EXTERNAL_ITEMS;
			//writefln("  1 cap %s", _capacity);
		}
		else if (_capacity < NUM_ITEMS_PER_PAGE)
		{
			size_t byteCapacity = nextPOT(_capacity * T.sizeof);
			ubyte[] block = (cast(ubyte*)externalArray)[0..byteCapacity];
			doubleSmallArray(arena, block);
			externalArray = cast(T*)block.ptr;
			_capacity = cast(uint)(block.length / T.sizeof);
			//writefln("  2 cap %s", _capacity);
		}
		else if (_capacity == NUM_ITEMS_PER_PAGE)
		{
			assert(nextPOT(_capacity * T.sizeof) == PAGE_SIZE);
			ubyte[] topBlock = arena.allocBlock((ubyte*).sizeof * 2);
			T** newChunkedArray = cast(T**)topBlock.ptr;
			newChunkedArray[0] = externalArray;
			ubyte[] extraBlock = arena.allocBlock(PAGE_SIZE);
			newChunkedArray[1] = cast(T*)extraBlock.ptr;
			chunkedArray = newChunkedArray;
			_capacity += NUM_ITEMS_PER_PAGE;
			//writefln("  3 cap %s", _capacity);
		}
		else
		{
			size_t numPages = _capacity / NUM_ITEMS_PER_PAGE;
			size_t pageCapacity = nextPOT(numPages);
			if (numPages == pageCapacity)
			{
				// extend page array
				ubyte[] pageArray = cast(ubyte[])chunkedArray[0..pageCapacity];
				assert(pageArray.length < PAGE_SIZE);
				doubleSmallArray(arena, pageArray);
				chunkedArray = cast(T**)pageArray;
			}
			//writefln("extend pages %s -> %s", numPages, numPages + 1);
			ubyte[] extraBlock = arena.allocBlock(PAGE_SIZE);
			chunkedArray[numPages] = cast(T*)extraBlock.ptr;
			_capacity += NUM_ITEMS_PER_PAGE;
			//writefln("  4 cap %s", _capacity);
		}
	}

	// Doubles the size of block
	private void doubleSmallArray(ref ArrayArena arena, ref ubyte[] oldBlock) {
		assert(isPowerOfTwo(oldBlock.length));
		assert(oldBlock.length >= 16);
		assert(oldBlock.length <= PAGE_SIZE);

		size_t newLength = oldBlock.length * 2;
		ubyte[] newBlock = arena.allocBlock(newLength);
		newBlock[0..oldBlock.length] = oldBlock;
		arena.freeBlock(oldBlock);
		oldBlock = newBlock;
	}

	int opApply(scope int delegate(size_t, ref T) dg) {
		size_t index;
		foreach (ref T item; this) {
			if (int res = dg(index, item))
				return res;
			++index;
		}
		return 0;
	}

	int opApply(scope int delegate(ref T) dg) {
		static if (NUM_INLINE_ITEMS > 0) {
			if (_capacity == NUM_INLINE_ITEMS) {
				foreach (ref T item; inlineItems[0.._length])
					if (int res = dg(item))
						return res;
				return 0;
			}
		}

		if (_capacity < NUM_ITEMS_PER_PAGE)
		{
			foreach (ref T item; externalArray[0.._length])
				if (int res = dg(item))
					return res;
			return 0;
		}

		size_t numPages = _capacity / NUM_ITEMS_PER_PAGE;
		size_t lastPageItems = _capacity - ((numPages - 1) * NUM_ITEMS_PER_PAGE);
		foreach (i, T* subArray; chunkedArray[0..numPages])
		{
			size_t length = (i < numPages) ? NUM_ITEMS_PER_PAGE : lastPageItems;
			foreach (ref T item; subArray[0..length])
				if (int res = dg(item))
					return res;
		}
		return 0;
	}
}
