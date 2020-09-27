/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

module ir.ir_small_set;

import std.bitmanip : bitfields;
import std.format : formattedWrite;
import all;

/// Stores 0-4 items inline
/// Allocates memory from IrFuncStorage.arrayBuffer for sets with > 3 slots
/// IrIndex.init is used as an empty key, it is illegal key
struct IrSmallSet
{
	union {
		// this is not deduplicated
		IrIndex[4] items; // items[0].kind != IrValueKind.array
		struct {
			// .kind == IrValueKind.array
			// offset into IrFuncStorage.arrayBuffer
			IrIndex arrayIndex;
			mixin(bitfields!(
				// Number of buckets with items in multihashset
				uint, "numUniqueKeys",   23,
				// capacity is bigCapacity
				uint, "capacityPower",  5,
				// 4 bits need to be clear, so IrIndex.kind == IrvalueKind.none
				// important for automatic index fixing after moving
				// when switching from built-in items to external array
				// item[1] must cleared
				uint, "", 4
			));
			mixin(bitfields!(
				// Total number of items inside multihashset (sum of values of all unique keys)
				uint, "numItems",   28,
				// 4 bits need to be clear, so IrIndex.kind == IrvalueKind.none
				// important for automatic index fixing after moving
				// when switching from built-in items to external array
				// item[2] must cleared
				uint, "", 4
			));
		}
	}

	struct Bucket
	{
		IrIndex key;
		// stores number of items with this key
		// always >= 1 for used items
		uint value;

		bool used() const { return key.isDefined; }
		bool empty() const { return key.isUndefined; }
	}

	bool isBig() const {
		return items[0].kind == IrValueKind.array;
	}

	private uint bigCapacity() const { return 1 << capacityPower; }

	uint capacity() const {
		if (isBig) return bigCapacity;
		else return 3;
	}

	uint length() const {
		if (isBig) return numItems;
		foreach_reverse(i, item; items) {
			if (item.isDefined) return cast(uint)(i+1);
		}
		return 0;
	}

	bool empty() const { return length == 0; }

	void free(IrFunction* ir) {
		if (isBig) {
			ir.freeIrArray(arrayIndex, bigCapacity);
		}
		items[] = IrIndex.init;
	}

	IrSmallSetIterator range(IrFunction* ir) const {
		return IrSmallSetIterator(this, ir);
	}

	void put(IrBuilder* builder, IrIndex key) {
		assert(key.isDefined, "Invalid key");
		if (isBig) {
			Bucket* buckets = cast(Bucket*)builder.ir.getArray(arrayIndex);
			uint _capacity = bigCapacity;
			uint maxLength = (_capacity / 4) * 3; // Should be optimized into shr + lea

			if (numUniqueKeys == maxLength) {
				// prevent unnecessary extend
				auto index = findIndex(buckets, key);
				if (index != size_t.max) return;

				uint newCapacity = _capacity * 2;
				IrIndex newArrayIndex = builder.allocateIrArray(newCapacity * 2); // Buckets are twice bigger than IrIndex
				Bucket* newBuckets = cast(Bucket*)builder.ir.getArray(newArrayIndex);
				newBuckets[0..newCapacity] = Bucket.init;

				// copy buckets
				numUniqueKeys = 0; // needed because `putKey` will increment per item
				numItems = 0;
				capacityPower = capacityPower + 1; // used by putKey
				foreach (Bucket oldBucket; buckets[0.._capacity]) {
					if (oldBucket.used) {
						putKey(newBuckets, oldBucket);
					}
				}

				// free old array
				builder.ir.freeIrArray(arrayIndex, _capacity * 2); // Buckets are twice bigger than IrIndex

				// update instance
				arrayIndex = newArrayIndex; // old index used for freeIrArray
				buckets = newBuckets;
			}

			putKey(buckets, Bucket(key, 1));
		}
		else
		{
			foreach (ref item; items) {
				if (item.isUndefined) {
					item = key;
					return;
				}
			}

			// no free slots, switch to multihashset
			IrIndex newArrayIndex = builder.allocateIrArray(16); // 8 Buckets are twice bigger than IrIndex
			Bucket* newBuckets = cast(Bucket*)builder.ir.getArray(newArrayIndex);
			auto itemsCopy = items;

			// update instance
			// clear bitfields and all lengths
			items[] = IrIndex.init;
			arrayIndex = newArrayIndex;
			capacityPower = 3; // 1 << 3 == 8

			newBuckets[0..8] = Bucket.init;
			//writefln("newBuckets %s %s %s", newArrayIndex, newBuckets, newBuckets[0..8]);
			foreach(item; itemsCopy)
				putKey(newBuckets, Bucket(item, 1));
			putKey(newBuckets, Bucket(key, 1));
		}
	}

	private void putKey(Bucket* buckets, Bucket bucket) {
		uint _capacity = bigCapacity;
		size_t index = getHash(bucket.key) & (_capacity - 1); // % capacity
		size_t inserted_dib = 0;
		while (true) {
			if (buckets[index].key == bucket.key) {
				//writefln("add %s %s %s %s, bigCap %s same", bucket.key, bucket.value, numItems, numUniqueKeys, _capacity);
				++buckets[index].value; // same key, bump the value
				numItems = numItems + 1;
				return;
			}
			if (buckets[index].empty) { // bucket is empty, we add new key
				//writefln("add %s %s %s %s, bigCap %s new", bucket.key, bucket.value, numItems, numUniqueKeys, _capacity);
				numUniqueKeys = numUniqueKeys + 1;
				numItems = numItems + bucket.value;
				buckets[index] = bucket;
				return;
			}
			size_t current_initial_bucket = getHash(buckets[index].key) & (_capacity - 1); // % capacity
			ptrdiff_t current_dib = index - current_initial_bucket;
			if (current_dib < 0) current_dib += _capacity;
			// swap inserted item with current only if DIB(current) < DIB(inserted item)
			if (inserted_dib > current_dib) {
				swap(bucket, buckets[index]);
				inserted_dib = current_dib;
			}
			++inserted_dib;
			index = (index + 1) & (_capacity - 1); // % capacity
		}
	}

	// Returns number of keys
	uint contains(IrFunction* ir, IrIndex key) {
		assert(key.isDefined, "Invalid key");
		if (isBig) {
			Bucket* buckets = cast(Bucket*)ir.getArray(arrayIndex);
			auto index = findIndex(buckets, key);
			if (index == size_t.max) return 0;
			return buckets[index].value;
		} else {
			uint count = 0;
			foreach (item; items)
				if (item == key) ++count;
			return count;
		}
	}

	/// Returns number of replaced keys
	uint replace(IrFunction* ir, IrIndex what, IrIndex byWhat) {
		assert(what.isDefined, "Invalid what");
		assert(byWhat.isDefined, "Invalid byWhat");
		if (isBig) {
			Bucket* buckets = cast(Bucket*)ir.getArray(arrayIndex);
			if (auto numRemoved = removeBig(buckets, what)) {
				putKey(buckets, Bucket(byWhat, numRemoved));
				return true;
			}
		} else {
			uint numReplaced = 0;
			foreach (i, ref item; items) {
				if (item == what) {
					++numReplaced;
					item = byWhat;
				}
			}
			return numReplaced;
		}
		return 0;
	}

	/// Returns number of deleted keys
	uint remove(IrFunction* ir, IrIndex what)
	{
		if (isBig) { // external array
			Bucket* buckets = cast(Bucket*)ir.getArray(arrayIndex);
			return removeBig(buckets, what);
		}
		else
			return removeSmall(what);
	}

	private uint removeSmall(IrIndex key) {
		// no harm if key or items[i] is null here
		uint numRemoved = 0;
		foreach (i, ref item; items) {
			if (item == key) {
				++numRemoved;
				item = IrIndex.init;
			} else {
				swap(items[i - numRemoved], item);
			}
		}
		return numRemoved;
	}

	private uint removeBig(Bucket* buckets, IrIndex key) {
		if (numUniqueKeys == 0) return 0;
		uint _capacity = bigCapacity;
		size_t index = getHash(key) & (_capacity - 1); // % capacity
		size_t searched_dib = 0;
		while (true) { // Find entry to delete
			if (buckets[index].empty) return 0; // empty bucket
			if (buckets[index].key == key) {
				//writefln("remove %s %s %s %s bigCap %s", buckets[index].key, buckets[index].value, numItems, numUniqueKeys, _capacity);
				numUniqueKeys = numUniqueKeys - 1;
				numItems = numItems - buckets[index].value;
				break; // found the item
			}
			size_t current_initial_bucket = getHash(buckets[index].key) & (_capacity - 1); // % capacity
			ptrdiff_t current_dib = index - current_initial_bucket;
			if (current_dib < 0) current_dib += _capacity;
			if (searched_dib > current_dib) return 0; // item must have been inserted here
			++searched_dib;
			index = (index + 1) & (_capacity - 1); // % capacity
		}
		uint value = buckets[index].value;
		while (true) { // Backward shift
			size_t nextIndex = (index + 1) & (_capacity - 1); // % capacity
			if (buckets[nextIndex].empty) break; // Found stop bucket (empty)
			size_t current_initial_bucket = getHash(buckets[nextIndex].key) & (_capacity - 1); // % capacity
			if (current_initial_bucket == nextIndex) break; // Found stop bucket (0 DIB)
			buckets[index] = buckets[nextIndex]; // shift item left
			index = nextIndex;
		}
		buckets[index] = Bucket.init; // Mark empty
		return value;
	}

	pragma(inline, true)
	private static uint getHash(IrIndex key) {
		return key.asUint;// * 0xdeece66d + 0xb;
	}

	private size_t findIndex(Bucket* buckets, IrIndex key) const
	{
		if (numUniqueKeys == 0) return size_t.max;
		uint _capacity = bigCapacity;
		auto index = getHash(key) & (_capacity - 1); // % capacity
		size_t searched_dib = 0;
		while (true) {
			if (buckets[index].empty) return size_t.max; // empty key
			if (buckets[index].key == key) return index;
			size_t current_initial_bucket = getHash(buckets[index].key) & (_capacity - 1); // % capacity
			ptrdiff_t current_dib = index - current_initial_bucket;
			if (current_dib < 0) current_dib += _capacity;
			if (searched_dib > current_dib) return size_t.max; // item must have been inserted here
			++searched_dib;
			index = (index + 1) & (_capacity - 1); // % capacity
		}
	}
}

struct IrSmallSetIterator
{
	IrSmallSet array;
	IrFunction* ir;

	int opApply(scope int delegate(IrIndex, uint) dg)
	{
		if (array.isBig) { // external hashset
			IrSmallSet.Bucket* buckets = cast(IrSmallSet.Bucket*)ir.getArray(array.arrayIndex);
			uint _capacity = array.bigCapacity;
			foreach(IrSmallSet.Bucket bucket; buckets[0.._capacity]) {
				if (bucket.used)
					if (int res = dg(bucket.key, bucket.value))
						return res;
			}
		} else { // small array
			auto copy = array;
			foreach(item; copy.items) {
				if (item.isUndefined) continue; // skip. This may be deleted item after which there is more
				// count the item
				uint count = 0;
				foreach (ref item2; copy.items)
					if (item2 == item) {
						++count;
						item2 = IrIndex.init; // delete item in copy, so we dont visit it more than once
					}
				if (int res = dg(item, count)) return res;
			}
		}
		return 0;
	}
}
