/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// IR that implements idea of storing everything (almost)
/// inside a single array;
/// Stores instructions in a linked list
module ir.small_vector;

import std.bitmanip : bitfields;
import std.format : formattedWrite;
import all;

/// Used for linked list
@(IrValueKind.listItem)
struct ListItem
{
	IrIndex itemIndex;
	IrIndex nextItem;
}

/// Allows storing 2 items inline, or linked list's info if more than 2 items are needed
struct SmallVector
{
	union {
		IrIndex[2] items; // .kind != IrValueKind.listItem
		struct {
			IrIndex firstListItem; // .kind == IrValueKind.listItem
			uint listLength;
		}
	}

	uint length()
	{
		if (items[0].kind == IrValueKind.listItem)
			return listLength;
		else if (items[1].isDefined)
			return 2;
		else if (items[0].isDefined)
			return 1;
		else
			return 0;
	}

	bool isBig()
	{
		return items[0].kind == IrValueKind.listItem;
	}

	SmallVectorIterator range(ref IrFunction ir)
	{
		return SmallVectorIterator(&this, &ir);
	}

	void replaceAll(ref IrFunction ir, IrIndex what, IrIndex byWhat)
	{
		foreach (ref IrIndex item; range(ir))
		{
			if (item == what) item = byWhat;
		}
	}

	/// Returns true if replacement was performed
	bool replaceFirst(ref IrFunction ir, IrIndex what, IrIndex byWhat)
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

	/// Removes all occurences of what
	void remove(ref IrFunction ir, IrIndex what)
	{
		if (isBig) // linked list
		{
			IrIndex prevIndex;
			IrIndex curIndex = firstListItem;
			while (curIndex.isDefined)
			{
				ListItem* cur = &ir.get!ListItem(curIndex);
				if (cur.itemIndex == what) {
					if (prevIndex.isDefined) {
						ListItem* prev = &ir.get!ListItem(prevIndex);
						prev.nextItem = cur.nextItem;
					} else {
						firstListItem = cur.nextItem;
					}
					--listLength;
				} else {
					prevIndex = curIndex;
				}
				curIndex = cur.nextItem;
			}
		}
		else // 0, 1, 2
		{
			// no harm if what or items[0/1] is null here
			if (items[0] == what) {
				// remove first item in-place
				if (items[1] == what) {
					// and second too
					items[0] = IrIndex();
				} else {
					// only first
					items[0] = items[1];
				}
				items[1] = IrIndex();
			} else if (items[1] == what) {
				// remove second item in-place
				items[1] = IrIndex();
			}
		}
	}

	ref IrIndex opIndex(size_t index, ref IrFunction ir)
	{
		size_t len = length;
		assert(index < len);
		if (len < 3) return items[index];

		return opIndexSecond(index, ir);
	}

	// second part of opIndex. Workaround bug https://issues.dlang.org/show_bug.cgi?id=19384
	private ref IrIndex opIndexSecond(size_t index, ref IrFunction ir)
	{
		foreach(i, ref val; range(ir))
			if (i == index)
				return val;
		assert(false);
	}

	void append(IrBuilder* builder, IrIndex itemData)
	{
		assert(itemData.kind != IrValueKind.listItem, "listItem is not storable inside SmallVector");
		assert(itemData.kind != IrValueKind.none, "IrValueKind.none is not storable inside SmallVector");
		if (isBig)
		{
			IrIndex newListItemIndex = builder.append!ListItem;
			ListItem* listItem = &builder.ir.get!ListItem(newListItemIndex);
			*listItem = ListItem(itemData, firstListItem);
			firstListItem = newListItemIndex;
			++listLength;
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
				IrIndex arrayIndex = builder.append!ListItem(3);
				ListItem* itemArray = &builder.ir.get!ListItem(arrayIndex);
				itemArray[2] = ListItem(itemData);
				itemArray[1] = ListItem(items[1], arrayIndex.indexOf!ListItem(2));
				itemArray[0] = ListItem(items[0], arrayIndex.indexOf!ListItem(1));
				firstListItem = arrayIndex;
				listLength = 3;
			}
		}
	}

	void toString(scope void delegate(const(char)[]) sink)
	{
		if (isBig)
			sink.formattedWrite("[%s items]", listLength);
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

	context.irBuffer.setBuffer(irBuf[]);
	context.tempBuffer.setBuffer(tempBuf[]);

	builder.context = &context;
	builder.ir = &ir;
	ir.storage = context.irBuffer.freePart[0..0];

	SmallVector vec;

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

struct SmallVectorIterator
{
	SmallVector* vector;
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
		if (vector.isBig) // linked list
		{
			IrIndex next = vector.firstListItem;
			size_t seqIndex = 0;
			while (next.isDefined)
			{
				ListItem* listItem = &ir.get!ListItem(next);
				next = listItem.nextItem;
				static if (size == 2) {
					if (int res = dg(seqIndex, listItem.itemIndex))
						return res;
					++seqIndex;
				} else {
					if (int res = dg(listItem.itemIndex))
						return res;
				}
			}
		}
		else // 0, 1, 2
		{
			if (vector.items[0].isDefined)
			{
				static if (size == 2) {
					if (int res = dg(0, vector.items[0])) return res;
					if (vector.items[1].isDefined)
					{
						if (int res = dg(1, vector.items[1])) return res;
					}
				} else {
					if (int res = dg(vector.items[0])) return res;
					if (vector.items[1].isDefined)
					{
						if (int res = dg(vector.items[1])) return res;
					}
				}
			}
		}
		return 0;
	}
}
