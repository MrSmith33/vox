/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module utils.hash;

import utils : write, writef, writeln, isPowerOfTwo, format, swap, formattedWrite, enforce;

enum StoreValues : bool { no = false, yes = true }

struct HashMap(Key, Value, Key emptyKey) {
	mixin HashTablePart!(KeyBucket!(Key, emptyKey), StoreValues.yes);
}

/// Uses special key value to mark empty buckets
struct KeyBucket(Key, Key emptyKey)
{
	static assert(isPowerOfTwo(Key.sizeof), format("%s == %s NPOT", Key.stringof, Key.sizeof));
	Key key = emptyKey;

	bool empty() const { return key == emptyKey; }
	bool used() const { return key != emptyKey; }
	bool canInsert(Key _key) const { return key == emptyKey || key == _key; }
	void assignKey(Key _key) { key = _key; }
	void clear() { key = emptyKey; }
	static bool isValidKey(Key _key) { return _key != emptyKey; }
	static void clearBuckets(typeof(this)[] keyBuckets) { keyBuckets[] = typeof(this).init; }
}

/// If store_values = no then table works like hashset
// Compiler doesn't optimize `index % _capacity` into `index & (_capacity - 1)`
mixin template HashTablePart(KeyBucketT, StoreValues store_values)
{
	import utils : isPowerOfTwo, max, writefln, ArrayArena;

	private uint _length; // num of used buckets
	private uint _capacity; // capacity. Always power of two
	private enum MIN_CAPACITY = 4;
	private enum bool SINGLE_ALLOC = Key.sizeof == Value.sizeof;

	bool empty() const { return _length == 0; }
	uint length() const { return _length; }
	uint capacity() const { return _capacity; }
	private uint maxLength() const { return (_capacity / 4) * 3; } // Should be optimized into shr + lea

	static if (store_values) {
		enum Bucket_size = KeyBucketT.sizeof + Value.sizeof;
		mixin HashMapImpl;
	} else {
		enum Bucket_size = KeyBucketT.sizeof;
		mixin HashSetImpl;
	}

	/// Returns false if no value was deleted, true otherwise
	bool remove(ref ArrayArena arena, Key key) {
		if (_length == 0) return false;
		size_t index = getHash(key) & (_capacity - 1); // % capacity
		size_t searched_dib = 0;
		while (true) { // Find entry to delete
			if (keyBuckets[index].empty) return false;
			if (keyBuckets[index].key == key) break; // found the item
			size_t current_initial_bucket = getHash(keyBuckets[index].key) & (_capacity - 1); // % capacity
			ptrdiff_t current_dib = index - current_initial_bucket;
			if (current_dib < 0) current_dib += _capacity;
			if (searched_dib > current_dib) return false; // item must have been inserted here
			++searched_dib;
			index = (index + 1) & (_capacity - 1); // % capacity
		}
		while (true) { // Backward shift
			size_t nextIndex = (index + 1) & (_capacity - 1); // % capacity
			if (keyBuckets[nextIndex].empty) break; // Found stop bucket (empty)
			size_t current_initial_bucket = getHash(keyBuckets[nextIndex].key) & (_capacity - 1); // % capacity
			if (current_initial_bucket == nextIndex) break; // Found stop bucket (0 DIB)
			keyBuckets[index].key = keyBuckets[nextIndex].key; // shift item left
			values[index] = values[nextIndex]; // shift item left
			index = nextIndex;
		}
		keyBuckets[index].clear; // Mark empty
		--_length;
		return true;
	}

	pragma(inline, true)
	private static size_t getHash(Key key) {
		import std.traits : isIntegral;
		static if (isIntegral!Key) return cast(size_t)key;
		else return hashOf(key);
	}

	private size_t findIndex(Key key) inout
	{
		if (_length == 0) return size_t.max;
		auto index = getHash(key) & (_capacity - 1); // % capacity
		size_t searched_dib = 0;
		while (true) {
			if (keyBuckets[index].empty) return size_t.max;
			if (keyBuckets[index].key == key) return index;
			size_t current_initial_bucket = getHash(keyBuckets[index].key) & (_capacity - 1); // % capacity
			ptrdiff_t current_dib = index - current_initial_bucket;
			if (current_dib < 0) current_dib += _capacity;
			if (searched_dib > current_dib) return size_t.max; // item must have been inserted here
			++searched_dib;
			index = (index + 1) & (_capacity - 1); // % capacity
		}
	}

	void free(ref ArrayArena arena) {
		static if (SINGLE_ALLOC) {
			size_t size = max(ArrayArena.MIN_BLOCK_BYTES, Bucket_size * _capacity);
			arena.freeBlock((cast(ubyte*)keyBuckets)[0..size]);
		} else {
			size_t keySize = max(ArrayArena.MIN_BLOCK_BYTES, KeyBucketT.sizeof * _capacity);
			arena.freeBlock((cast(ubyte*)keyBuckets)[0..keySize]);
			static if (store_values) {
				size_t valSize = max(ArrayArena.MIN_BLOCK_BYTES, Value.sizeof * _capacity);
				arena.freeBlock((cast(ubyte*)values)[0..valSize]);
			}
		}
		keyBuckets = null;
		values = null;
		_capacity = 0;
		_length = 0;
	}

	void clear() {
		KeyBucketT.clearBuckets(keyBuckets[0.._capacity]);
		_length = 0;
	}

	private void extend(ref ArrayArena arena)
	{
		uint newCapacity = _capacity ? _capacity * 2 : MIN_CAPACITY;
		auto oldKeyBuckets = keyBuckets;

		static if (store_values) {
			auto oldValues = values[0.._capacity];
		}

		auto oldCapacity = _capacity;

		static if (SINGLE_ALLOC) {
			size_t newSize = max(ArrayArena.MIN_BLOCK_BYTES, Bucket_size * newCapacity);
			ubyte[] newBlock = arena.allocBlock(newSize);
			keyBuckets = cast(KeyBucketT*)(newBlock.ptr);
			static if (store_values)
				values = cast(Value*)(newBlock.ptr + newCapacity*KeyBucketT.sizeof);
		} else {
			size_t newKeySize = max(ArrayArena.MIN_BLOCK_BYTES, KeyBucketT.sizeof * newCapacity);
			ubyte[] newKeysBlock = arena.allocBlock(newKeySize);
			keyBuckets = cast(KeyBucketT*)(newKeysBlock.ptr);
			static if (store_values) {
				size_t newValSize = max(ArrayArena.MIN_BLOCK_BYTES, Value.sizeof * newCapacity);
				ubyte[] newValuesBlock = arena.allocBlock(newValSize);
				values = cast(Value*)newValuesBlock.ptr;
			}
		}

		KeyBucketT.clearBuckets(keyBuckets[0..newCapacity]);
		_capacity = newCapacity; // put() below uses new capacity

		if (oldKeyBuckets) {
			_length = 0; // needed because `put` will increment per item
			foreach (i, ref bucket; oldKeyBuckets[0..oldCapacity]) {
				if (bucket.used) {
					static if (store_values) put(arena, bucket.key, oldValues[i]);
					else put(arena, bucket.key);
				}
			}

			static if (SINGLE_ALLOC) {
				size_t oldSize = max(ArrayArena.MIN_BLOCK_BYTES, Bucket_size * oldCapacity);
				arena.freeBlock((cast(ubyte*)oldKeyBuckets)[0..oldSize]);
			} else {
				size_t oldKeySize = max(ArrayArena.MIN_BLOCK_BYTES, KeyBucketT.sizeof * oldCapacity);
				arena.freeBlock((cast(ubyte*)oldKeyBuckets)[0..oldKeySize]);
				static if (store_values) {
					size_t oldValSize = max(ArrayArena.MIN_BLOCK_BYTES, Value.sizeof * oldCapacity);
					arena.freeBlock((cast(ubyte*)oldValues)[0..oldValSize]);
				}
			}
		}
	}
}

// http://codecapsule.com/2013/11/11/robin-hood-hashing
// http://codecapsule.com/2013/11/17/robin-hood-hashing-backward-shift-deletion/
// DIB - distance to initial bucket
mixin template HashMapImpl()
{
	static assert(isPowerOfTwo(Value.sizeof));
	KeyBucketT* keyBuckets;
	Value* values;

	alias KeyT = Key;
	alias ValueT = Value;

	Value* put(ref ArrayArena arena, Key key, Value value)
	{
		assert(KeyBucketT.isValidKey(key), "Invalid key");
		if (_length == maxLength) extend(arena);
		size_t index = getHash(key) & (_capacity - 1); // % capacity
		size_t inserted_dib = 0;
		while (true) {
			if (keyBuckets[index].key == key) { // same key
				values[index] = value;
				return &values[index];
			}
			if (keyBuckets[index].empty) { // bucket is empty
				++_length;
				keyBuckets[index].assignKey(key);
				values[index] = value;
				return &values[index];
			}
			size_t current_initial_bucket = getHash(keyBuckets[index].key) & (_capacity - 1); // % capacity
			ptrdiff_t current_dib = index - current_initial_bucket;
			if (current_dib < 0) current_dib += _capacity;
			// swap inserted item with current only if DIB(current) < DIB(inserted item)
			if (inserted_dib > current_dib) {
				swap(key, keyBuckets[index].key);
				swap(value, values[index]);
				inserted_dib = current_dib;
			}
			++inserted_dib;
			index = (index + 1) & (_capacity - 1); // % capacity
		}
	}

	Value* getOrCreate(ref ArrayArena arena, Key key, out bool wasCreated, Value default_value = Value.init)
	{
		if (_length == 0) {
			wasCreated = true;
			return put(arena, key, default_value);
		}

		auto index = getHash(key) & (_capacity - 1); // % capacity
		size_t inserted_dib = 0;
		Value value;
		while (true) {
			if (keyBuckets[index].empty) { // bucket is empty
				++_length;
				keyBuckets[index].assignKey(key);
				values[index] = default_value;
				wasCreated = true;
				return &values[index];
			}
			if (keyBuckets[index].key == key) return &values[index];
			size_t current_initial_bucket = getHash(keyBuckets[index].key) & (_capacity - 1); // % capacity
			ptrdiff_t current_dib = index - current_initial_bucket;
			if (current_dib < 0) current_dib += _capacity;
			if (inserted_dib > current_dib) {
				value = default_value;
				wasCreated = true;
				swap(key, keyBuckets[index].key);
				swap(value, values[index]);
				inserted_dib = current_dib;
				++inserted_dib;
				index = (index + 1) & (_capacity - 1); // % capacity
				break; // item must have been inserted here
			}
			++inserted_dib;
			index = (index + 1) & (_capacity - 1); // % capacity
		}

		while (true) {
			if (keyBuckets[index].empty) { // bucket is empty
				++_length;
				keyBuckets[index].assignKey(key);
				values[index] = value;
				return &values[index];
			}
			size_t current_initial_bucket = getHash(keyBuckets[index].key) & (_capacity - 1); // % capacity
			ptrdiff_t current_dib = index - current_initial_bucket;
			if (current_dib < 0) current_dib += _capacity;
			// swap inserted item with current only if DIB(current) < DIB(inserted item)
			if (inserted_dib > current_dib) {
				swap(key, keyBuckets[index].key);
				swap(value, values[index]);
				inserted_dib = current_dib;
			}
			++inserted_dib;
			index = (index + 1) & (_capacity - 1); // % capacity
		}

		return &values[index];
	}

	inout(Value)* opBinaryRight(string op)(Key key) inout if (op == "in") {
		size_t index = findIndex(key);
		if (index == size_t.max) return null;
		return &values[index];
	}

	ref inout(Value) opIndex(Key key) inout {
		size_t index = findIndex(key);
		enforce(index != size_t.max, "Non-existing key access");
		return values[index];
	}

	Value get(Key key, Value default_value = Value.init) {
		size_t index = findIndex(key);
		if (index == size_t.max) return default_value;
		return values[index];
	}

	int opApply(scope int delegate(ref Value) del) {
		foreach (i, ref bucket; keyBuckets[0.._capacity])
			if (bucket.used)
				if (int ret = del(values[i]))
					return ret;
		return 0;
	}

	int opApply(scope int delegate(in Key, ref Value) del) {
		foreach (i, ref bucket; keyBuckets[0.._capacity])
			if (bucket.used)
				if (int ret = del(bucket.key, values[i]))
					return ret;
		return 0;
	}

	void toString()(scope void delegate(const(char)[]) sink) {
		sink.formattedWrite("[",);
		size_t index;
		foreach(key, value; this) {
			if (index > 0) sink(", ");
			sink.formattedWrite("%s:%s", key, value);
			++index;
		}
		sink.formattedWrite("]");
	}

	void dump() {
		//writef("{%08X}[", cast(size_t)keyBuckets & 0xFFFFFFFF);
		foreach (i, ref bucket; keyBuckets[0.._capacity]) {
			if (i > 0) write(", ");
			if (bucket.empty)
				writef("%s:<E>", i);
			else
				writef("%s:%s:%s", i, bucket.key, values[i]);
		}
		writeln("]");
	}
}
