/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

module ir.ir_small_array;

import std.bitmanip : bitfields;
import std.format : formattedWrite;
import all;

/// Stores 0-2 items inline
/// Allocates memory from IrFuncStorage.arrayBuffer for arrays with > 2 slots
struct IrSmallArray
{
	union {
		IrIndex[2] items; // .kind != IrValueKind.array
		struct {
			// .kind == IrValueKind.array
			// offset into IrFuncStorage.arrayBuffer
			IrIndex arrayIndex;
			mixin(bitfields!(
				// Number of items inside big array
				uint, "arrayLength",   23,
				// capacity is bigCapacity
				uint, "capacityPower",  5,
				// 4 bits need to be clear, so IrIndex.kind == IrvalueKind.none
				// important for automatic index fixing after moving
				// when switching from built-in items to external array
				// item[1] must cleared
				uint, "", 4
			));
		}
	}

	private uint bigCapacity() { return 1 << capacityPower; }

	uint capacity()
	{
		if (isBig) return bigCapacity;
		else return 2;
	}

	uint length()
	{
		if (items[0].kind == IrValueKind.array)
			return arrayLength;
		else if (items[1].isDefined)
			return 2;
		else if (items[0].isDefined)
			return 1;
		else
			return 0;
	}

	bool empty() { return length == 0; }

	void free(IrFunction* ir)
	{
		if (isBig)
		{
			ir.freeIrArray(arrayIndex, bigCapacity);
		}
		items[0] = IrIndex();
		items[1] = IrIndex();
	}

	bool isBig()
	{
		return items[0].kind == IrValueKind.array;
	}

	alias opCall = range;
	IrSmallArrayIterator range(IrFunction* ir)
	{
		return IrSmallArrayIterator(&this, ir);
	}

	void replaceAll(IrFunction* ir, IrIndex what, IrIndex byWhat)
	{
		foreach (ref IrIndex item; range(ir))
		{
			if (item == what) item = byWhat;
		}
	}

	bool contains(IrFunction* ir, IrIndex what)
	{
		foreach (IrIndex item; range(ir))
		{
			if (item == what) return true;
		}
		return false;
	}

	/// Returns true if replacement was performed
	bool replaceFirst(IrFunction* ir, IrIndex what, IrIndex byWhat)
	{
		foreach (ref IrIndex item; range(ir))
		{
			if (item == what) {
				item = byWhat;
				return true;
			}
		}
		return false;
	}

	// asserts if not found
	uint findFirst(IrFunction* ir, IrIndex what)
	{
		//writefln("findFirst %s %s", what, data(ir));
		foreach (size_t index, ref IrIndex item; range(ir))
		{
			if (item == what) return cast(uint)index;
		}
		assert(false, "Item not found");
	}

	// returns uint.max if not found
	uint tryFindFirst(IrFunction* ir, IrIndex what)
	{
		foreach (size_t index, ref IrIndex item; range(ir))
		{
			if (item == what) return cast(uint)index;
		}
		return uint.max;
	}

	/// Removes all occurences of what
	/// Preserves order of items
	uint removeStable(IrFunction* ir, IrIndex what)
	{
		if (isBig) // external array
		{
			uint numRemoved = 0;
			IrIndex* array = ir.getArray(arrayIndex);

			foreach(i, ref IrIndex item; array[0..arrayLength])
			{
				if (item == what)
					++numRemoved;
				else
					array[i - numRemoved] = item;
			}
			arrayLength = arrayLength - numRemoved;
			afterRemoveBig(ir, array); // free array if needed
			return numRemoved;
		}
		else return removeSmall(ir, what); // 0, 1, 2
	}

	/// Removes all occurences of what
	/// Alters order of items
	uint removeUnstable(IrFunction* ir, IrIndex what)
	{
		if (isBig) // external array
		{
			uint tempLength = arrayLength;
			IrIndex* array = ir.getArray(arrayIndex);

			uint i = 0;
			while (i < tempLength)
			{
				if (array[i] == what) {
					--tempLength;
					array[i] = array[tempLength];
				}
				else
					++i;
			}
			uint numRemoved = arrayLength - tempLength;
			arrayLength = tempLength;
			afterRemoveBig(ir, array); // free array if needed
			return numRemoved;
		}
		else return removeSmall(ir, what); // 0, 1, 2
	}

	private uint removeSmall(IrFunction* ir, IrIndex what)
	{
		// no harm if what or items[0/1] is null here
		if (items[0] == what) {
			// remove first item in-place
			if (items[1] == what) {
				// and second too
				items[0] = IrIndex();
				items[1] = IrIndex();
				return 2;
			} else {
				// only first
				items[0] = items[1];
				items[1] = IrIndex();
				return 1;
			}
		} else if (items[1] == what) {
			// remove second item in-place
			items[1] = IrIndex();
			return 1;
		}
		return 0;
	}

	private void afterRemoveBig(IrFunction* ir, IrIndex* array)
	{
		switch (arrayLength)
		{
			case 0:
				IrIndex arrayIndexCopy = arrayIndex;
				ir.freeIrArray(arrayIndex, bigCapacity);
				items[0] = IrIndex();
				items[1] = IrIndex();
				break;
			case 1:
				IrIndex arrayIndexCopy = arrayIndex;
				uint _capacity = bigCapacity;
				items[0] = array[0];
				items[1] = IrIndex();
				ir.freeIrArray(arrayIndexCopy, _capacity);
				break;
			case 2:
				IrIndex arrayIndexCopy = arrayIndex;
				uint _capacity = bigCapacity;
				items[0] = array[0];
				items[1] = array[1];
				ir.freeIrArray(arrayIndexCopy, _capacity);
				break;
			default: break;
		}
	}

	IrIndex[] data(IrFunction* ir)
	{
		if (isBig)
		{
			IrIndex* array = ir.getArray(arrayIndex);
			return array[0..arrayLength];
		}
		else
		{
			if (items[1].isDefined)
			{
				return items[0..2];
			}
			else if (items[0].isDefined)
			{
				return items[0..1];
			}
			else
			{
				return null;
			}
		}
	}

	ref IrIndex opIndex(size_t index, IrFunction* ir)
	{
		if (isBig)
		{
			IrIndex* array = ir.getArray(arrayIndex);
			assert(index < arrayLength);
			return array[index];
		}
		else
		{
			switch(index)
			{
				case 0:
					assert(items[0].isDefined, "Accessing index 0, when length is 0");
					return items[0];
				case 1:
					assert(items[0].isDefined, "Accessing index 1, when length is < 2");
					return items[1];
				default:
					assert(false);
			}
		}
	}

	void append(IrBuilder* builder, IrIndex itemData)
	{
		assert(itemData.kind != IrValueKind.array, "array is not storable inside IrSmallArray");
		assert(itemData.kind != IrValueKind.none, "IrValueKind.none is not storable inside IrSmallArray");
		if (isBig)
		{
			uint _capacity = bigCapacity;
			IrIndex* array = builder.ir.getArray(arrayIndex);
			if (arrayLength < _capacity)
			{
				//writefln("append cap %s len %s %s", _capacity, arrayLength, array[0..arrayLength]);
				array[arrayLength] = itemData;
				arrayLength = arrayLength + 1;
			}
			else
			{
				//writefln("append cap %s %s len %s extend %s %s", capacityPower, _capacity, arrayLength, arrayIndex, array[0..arrayLength]);

				if (builder.tryExtendArray(arrayIndex, _capacity))
				{
					// extend was performed in-place
					array[arrayLength] = itemData;

					// update instance
					arrayLength = arrayLength + 1;
					capacityPower = capacityPower + 1;
				}
				else
				{
					// allocate twice bigger array
					uint newCapacity = _capacity * 2;
					IrIndex newArrayIndex = builder.allocateIrArray(newCapacity);
					//writefln("  alloc cap %s %s", newCapacity, newArrayIndex);
					IrIndex* newArray = builder.ir.getArray(newArrayIndex);
					// copy old data
					newArray[0..arrayLength] = array[0..arrayLength];
					// add new item
					newArray[arrayLength] = itemData;
					// free old array
					builder.ir.freeIrArray(arrayIndex, _capacity);

					// update instance
					arrayLength = arrayLength + 1;
					capacityPower = capacityPower + 1;
					arrayIndex = newArrayIndex;
				}

				//writefln("  len %s cap %s %s index %s", arrayLength, capacityPower, 1 << capacityPower, arrayIndex);
			}
		}
		else
		{
			if (!items[0].isDefined)
			{
				items[0] = itemData;
			}
			else if (!items[1].isDefined)
			{
				items[1] = itemData;
			}
			else
			{
				IrIndex newArrayIndex = builder.allocateIrArray(4);
				IrIndex* newArray = builder.ir.getArray(newArrayIndex);
				newArray[0] = items[0];
				newArray[1] = items[1];
				newArray[2] = itemData;

				// update instance
				items[1] = IrIndex(); // clear bitfields
				arrayIndex = newArrayIndex;
				capacityPower = 2; // 1 << 2 == 4
				arrayLength = 3;
				//writefln("  len %s cap %s %s index %s", arrayLength, capacityPower, 1 << capacityPower, arrayIndex);
			}
		}
	}

	void toString(scope void delegate(const(char)[]) sink)
	{
		if (isBig)
			sink.formattedWrite("[%s items]", arrayLength);
		else if (items[1].isDefined)
			sink.formattedWrite("[%s, %s]", items[0], items[1]);
		else if (items[0].isDefined)
			sink.formattedWrite("[%s]", items[0]);
		else sink("[]");
	}
}

unittest
{
	import std.stdio;
	ubyte[1024] irBuf, tempBuf;
	IrFunction ir;
	CompilationContext context;
	IrBuilder builder;

	context.irStorage.arrayBuffer.setBuffer(irBuf[], 1024);
	context.tempBuffer.setBuffer(tempBuf[], 1024);

	builder.context = &context;
	builder.ir = &ir;
	ir.arrayPtr = context.irStorage.arrayBuffer.nextPtr;

	IrSmallArray vec;

	assert(vec.length == 0);
	assert(!vec.isBig);

	vec.append(&builder, IrIndex(1, IrValueKind.instruction));
	assert(vec.length == 1);
	assert(!vec.isBig);

	vec.append(&builder, IrIndex(2, IrValueKind.instruction));
	assert(vec.length == 2);
	assert(!vec.isBig);

	vec.append(&builder, IrIndex(3, IrValueKind.instruction));
	assert(vec.length == 3);
	assert(vec.isBig);

	vec.append(&builder, IrIndex(4, IrValueKind.instruction));
	assert(vec.length == 4);
	assert(vec.isBig);
}

struct IrSmallArrayIterator
{
	IrSmallArray* array;
	IrFunction* ir;

	int opApply(scope int delegate(size_t, ref IrIndex) dg)
	{
		return opApplyImpl!2(dg);
	}

	int opApply(scope int delegate(ref IrIndex) dg)
	{
		return opApplyImpl!1(dg);
	}

	int opApplyImpl(uint size, Del)(scope Del dg)
	{
		if (array.isBig) // external array
		{
			IrIndex* data = ir.getArray(array.arrayIndex);
			uint length = array.arrayLength;
			foreach(size_t index, ref IrIndex item; data[0..length])
			{
				static if (size == 2) {
					if (int res = dg(index, item)) return res;
				} else {
					if (int res = dg(item)) return res;
				}
			}
		}
		else // 0, 1, 2
		{
			if (!array.items[0].isDefined) return 0; // length 0

			static if (size == 2) {
				if (int res = dg(0, array.items[0])) return res; // length 1
				if (array.items[1].isDefined)
				{
					if (int res = dg(1, array.items[1])) return res; // length 2
				}
			} else {
				if (int res = dg(array.items[0])) return res; // length 1
				if (array.items[1].isDefined)
				{
					if (int res = dg(array.items[1])) return res; // length 2
				}
			}
		}
		return 0;
	}
}
