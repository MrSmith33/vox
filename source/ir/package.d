/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// IR that implements idea of storing everything (almost)
/// inside a single array;
/// Stores instructions in a linked list
module ir;

public import ir.dump;
public import ir.ir_builder;
public import ir.ir_function;
public import ir.ir_index;
public import ir.ir_instructions;
public import ir.ir_module;

//import std.stdio;
//import std.string : format;
//import std.bitmanip : bitfields;
import std.format : formattedWrite;

import all;

//version = standalone;
version (standalone) void main()
{
	// function i32 $sign () {
	//    |  @start:0
	//   1|    %0 = i32 o_param
	//   2|    %1 = i1  o_icmp   l i32 %0, i64 0
	//         branch i1 %1 @then_1, @else_1
	//    |  @then_1:1 in[@start]
	//         jmp @blk_2
	//    |  @else_1:2 in[@start]
	//   7|    %2 = i1  o_icmp   g i32 %0, i64 0
	//         branch i1 %2 @then_2, @else_2
	//    |  @then_2:3 in[@else_1]
	//         jmp @blk_1
	//    |  @else_2:4 in[@else_1]
	//         jmp @blk_1
	//    |  @blk_1:5 in[@then_2, @else_2]
	//  13|    %4 = i32 phi.1(i64 1 @3, i64 0 @4)
	//         jmp @blk_2
	//    |  @blk_2:6 in[@then_1, @blk_1]
	//  15|    %3 = i32 phi.0(i32 -1 @1, i32 %4 @5)
	//         return i32 %3
	// }

	// i32 sign(i32 number) {
	//     i32 result;
	//     if (number < 0) result = 0-1;
	//     else if (number > 0) result = 1;
	//     else result = 0;
	//     return result;
	// }
	writefln("start");
	Driver driver;
	driver.initialize(null);
	scope(exit) driver.releaseMemory;

	IrBuilder builder;
	IrFunction ir;

	ir.returnType = IrValueType.i32;
	ir.name = driver.context.idMap.getOrReg("sign");

	//i32 sign(i32 number)
	builder.begin(&ir, &driver.context);
	IrIndex param0Index = builder.addInstruction!IrInstrParameter(ir.entryBasicBlock);
	ir.get!IrInstrParameter(param0Index).index = 0;
	IrIndex param0Value = ir.get!IrInstrHeader(param0Index).result;
	builder.addJump(ir.entryBasicBlock);
	//{
	IrIndex start_block = builder.addBasicBlock();
	builder.addBlockTarget(ir.entryBasicBlock, start_block);
	builder.sealBlock(start_block);
	//	i32 result;
	IrIndex zeroVal = driver.context.addConstant(IrConstant(0));
	IrVar resultVar = IrVar(Identifier(0), builder.newIrVarId());
	builder.writeVariable(start_block, resultVar, zeroVal);
	IrLabel scope1ExitLabel = IrLabel(start_block);
	IrIndex then_1_block = builder.addBasicBlock();
	IrIndex else_1_block = builder.addBasicBlock();
	//	if (number < 0)
	auto branch1 = builder.addBinBranch(start_block, IrBinaryCondition.l, param0Value, zeroVal);

	builder.addBlockTarget(start_block, then_1_block);
	builder.sealBlock(then_1_block);
	builder.addBlockTarget(start_block, else_1_block);
	builder.sealBlock(else_1_block);
	//		result = 0-1;
	IrIndex minusOneVal = driver.context.addConstant(IrConstant(-1));
	builder.writeVariable(then_1_block, resultVar, minusOneVal);
	builder.addJumpToLabel(then_1_block, scope1ExitLabel);
	//	else
	//	{
	//		if (number > 0)
	auto branch2 = builder.addBinBranch(else_1_block, IrBinaryCondition.g, param0Value, zeroVal);

	IrIndex then_2_block = builder.addBasicBlock();
	IrIndex else_2_block = builder.addBasicBlock();
	builder.addBlockTarget(else_1_block, then_2_block);
	builder.sealBlock(then_2_block);
	builder.addBlockTarget(else_1_block, else_2_block);
	builder.sealBlock(else_2_block);
	//			result = 1;
	IrIndex oneVal = driver.context.addConstant(IrConstant(1));
	builder.writeVariable(then_2_block, resultVar, oneVal);
	builder.addJumpToLabel(then_2_block, scope1ExitLabel);
	//		else
	//			result = 0;
	builder.writeVariable(else_2_block, resultVar, zeroVal);
	builder.addJumpToLabel(else_2_block, scope1ExitLabel);
	//	}
	IrIndex currentBlock = scope1ExitLabel.blockIndex;
	builder.sealBlock(currentBlock);
	//	return result;
	builder.addReturn(currentBlock, builder.readVariable(currentBlock, resultVar));
	//}

	builder.sealBlock(ir.exitBasicBlock);

	FuncDumpSettings dumpSettings;
	dumpSettings.dumper = &dumpIrInstr;
	dumpFunction(&ir, &driver.context, dumpSettings);
}


/// Convenience struct for Id + num suffix
struct IrName
{
	Identifier id;
	uint suffix;
}

/// Describes what IrIndex is pointing at
/// Is used as UDA on instructions
enum IrValueKind : ubyte
{
	none, /// Used for undefined indicies
	listItem, /// Indicates items of linked list in SmallVector
	instruction,
	basicBlock,
	constant,
	phi,
	memoryAddress,
	stackSlot,
	virtualRegister,
	physicalRegister,
}

struct IrLabel
{
	/// If numPredecessors == 0, is null
	/// If numPredecessors == 1, points to first predecessor
	/// If numPredecessors > 1,  points to a new block
	IrIndex blockIndex;
	///
	uint numPredecessors;
}

/// Stores numeric constant data
@(IrValueKind.constant)
struct IrConstant
{
	this(long value) {
		this.i64 = value;
	}

	ubyte numSignedBytes() {
		if (cast(byte)(i64 & 0xFF) == i64)
			return 1;
		else if (cast(short)(i64 & 0xFFFF) == i64)
			return 2;
		else if (cast(int)(i64 & 0xFFFF_FFFF) == i64)
			return 4;
		else
			return 8;
	}

	ubyte numUnsignedBytes() {
		if (cast(ubyte)(i64 & 0xFF) == i64)
			return 1;
		else if (cast(ushort)(i64 & 0xFFFF) == i64)
			return 2;
		else if (cast(uint)(i64 & 0xFFFF_FFFF) == i64)
			return 4;
		else
			return 8;
	}

	union {
		bool i1;
		byte i8;
		short i16;
		int i32;
		long i64;
	}
}


struct IrVarId { uint id; alias id this; }
struct IrVar { Identifier name; IrVarId id; IrValueType type; }

enum IrValueType : ubyte
{
	void_t,
	i32,
	i64,
	//f32,
	//f64,

	ptr,
}

struct BlockVarPair
{
	IrIndex blockId;
	IrVarId varId;
}

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

	SmallVectorRange range(ref IrFunction ir)
	{
		return SmallVectorRange(&this, &ir);
	}

	void replaceAll(ref IrFunction ir, IrIndex what, IrIndex byWhat)
	{
		foreach (ref IrIndex item; range(ir))
		{
			if (item == what) item = byWhat;
		}
	}

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
	IrFunction ir;
	ir.storage.setBuffer(new uint[1024]);
	ir.temp.setBuffer(new uint[1024]);
	SmallVector vec;

	assert(vec.length == 0);
	assert(!vec.isBig);

	vec.append(&ir, IrIndex(1, IrValueKind.instruction));
	assert(vec.length == 1);
	assert(!vec.isBig);

	vec.append(&ir, IrIndex(2, IrValueKind.instruction));
	assert(vec.length == 2);
	assert(!vec.isBig);

	vec.append(&ir, IrIndex(3, IrValueKind.instruction));
	assert(vec.length == 3);
	assert(vec.isBig);

	vec.append(&ir, IrIndex(4, IrValueKind.instruction));
	assert(vec.length == 4);
	assert(vec.isBig);
}

struct SmallVectorRange
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
