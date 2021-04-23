/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module utils.arrayarena;

struct FreeList
{
	import utils : writef, writefln;
	void* head;
	ubyte[] get(size_t size) {
		if (head) {
			void** linkPtr = cast(void**)head;
			head = linkPtr[0];
			return (cast(ubyte*)linkPtr)[0..size];
		}
		return null;
	}
	void put(ubyte[] block) {
		void** linkPtr = cast(void**)block.ptr;
		linkPtr[0] = head;
		head = cast(void*)block.ptr;
	}
}

//version = USE_MIMALLOC;

version(USE_MIMALLOC) {
	extern(C) void* mi_malloc(size_t size);
	extern(C) void mi_free(void* p);
}

struct ArrayArena
{
	import core.stdc.stdlib : malloc, free;
	import utils : max, nextPOT, Arena, bsr, isPowerOfTwo, PAGE_SIZE, writefln, format;

	// arenas for buffers from 16 to 65536 bytes
	enum NUM_ARENAS = 13;
	enum MIN_BLOCK_BYTES = 16;
	enum MAX_BLOCK_BYTES = 65_536;

	private Arena!ubyte[NUM_ARENAS] arenas;
	private size_t[NUM_ARENAS] arenaLengths;
	private FreeList[NUM_ARENAS] freeLists;

	void setBuffers(ubyte[] smallBuffers, ubyte[] pageBuffer) {
		size_t sizePerArena = smallBuffers.length / NUM_ARENAS;
		foreach(i, ref arena; arenas[0..$-1])
			arena.setBuffer(smallBuffers[i*sizePerArena..(i+1)*sizePerArena], 0);
		arenas[$-1].setBuffer(pageBuffer, 0); // separate buffer for big pages
	}

	size_t byteLength() {
		size_t total;
		foreach(ref arena; arenas) total += arena.byteLength;
		return total;
	}

	size_t reservedBytes() {
		size_t total;
		foreach(ref arena; arenas) total += arena.reservedBytes;
		return total;
	}
	size_t committedBytes() {
		size_t total;
		foreach(ref arena; arenas) total += arena.committedBytes;
		return total;
	}

	T[] allocArray(T)(size_t length) {
		size_t blockSize = max(nextPOT(length * T.sizeof), MIN_BLOCK_BYTES);
		ubyte[] newBlock = allocBlock(blockSize);
		return (cast(T*)newBlock.ptr)[0..length];
	}

	void freeArray(T)(ref T[] array) {
		size_t blockSize = max(nextPOT(array.length * T.sizeof), MIN_BLOCK_BYTES);
		ubyte* ptr = cast(ubyte*)array.ptr;
		freeBlock(ptr[0..blockSize]);
		array = null;
	}

	version(USE_MIMALLOC)
	{
		ubyte[] allocBlock(size_t size) {
			assert(isPowerOfTwo(size));
			return (cast(ubyte*)mi_malloc(size))[0..size];
		}

		void freeBlock(ubyte[] block) {
			if (block.ptr is null) return;
			return mi_free(block.ptr);
		}
	}
	else
	{
		ubyte[] allocBlock(size_t size) {
			assert(isPowerOfTwo(size));
			assert(size >= MIN_BLOCK_BYTES);
			if (size > MAX_BLOCK_BYTES) {
				return (cast(ubyte*)malloc(size))[0..size];
			}
			uint index = sizeToIndex(size);
			ubyte[] block = freeLists[index].get(size);
			if (block) {
				assert(arenas[index].contains(block.ptr), format("allocBlock %s, freeList get %X, %s", size, block.ptr, block.length));
				return block;
			}
			++arenaLengths[index];
			ubyte[] result = arenas[index].voidPut(size);
			return result;
		}

		void freeBlock(ubyte[] block) {
			if (block.ptr is null) return;
			assert(isPowerOfTwo(block.length));
			assert(block.length >= MIN_BLOCK_BYTES);
			if (block.length > MAX_BLOCK_BYTES) {
				return free(block.ptr);
			}
			uint index = sizeToIndex(block.length);
			freeLists[index].put(block);
		}
	}

	void clear() {
		foreach(ref arena; arenas) arena.clear;
		freeLists[] = FreeList.init;
		arenaLengths[] = 0;
	}

	private uint sizeToIndex(size_t size) {
		// from 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536
		//   to  0  1  2   3   4   5    6    7    8    9    10    11    12
		uint index = bsr(size) - 4;
		return index;
	}
}

