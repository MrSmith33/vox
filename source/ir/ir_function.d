/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ir.ir_function;

import std.string : format;

import all;

/// Allows associating a single uint sized item with any object in original IR
/// IR must be immutable (no new items must added)
/// Mirror is stored in temp memory of context
struct IrMirror(T)
{
	static assert(T.sizeof == uint.sizeof, "T size must be equal to uint.sizeof");

	// Mirror of original IR
	private uint[] irMirror;

	void create(CompilationContext* context, IrFunction* ir)
	{
		irMirror = context.tempBuffer.voidPut(ir.storageLength);
		irMirror[] = 0;
	}

	ref T opIndex(IrIndex irIndex)
	{
		return *cast(T*)&irMirror[irIndex.storageUintIndex];
	}
}

struct IrFunction
{
	uint[] storage;
	/// number of uints allocated from storage
	uint storageLength;

	/// Special block. Automatically created. Program start. Created first.
	IrIndex entryBasicBlock;
	/// Special block. Automatically created. All returns must jump to it.
	IrIndex exitBasicBlock;
	// The last created basic block
	IrIndex lastBasicBlock;
	///
	uint numBasicBlocks;

	/// First virtual register in linked list
	IrIndex firstVirtualReg;
	/// Last virtual register in linked list
	IrIndex lastVirtualReg;
	/// Total number of virtual registers
	uint numVirtualRegisters;

	///
	IrValueType returnType;
	///
	Identifier name;
	///
	CallConv* callingConvention;

	BlockIterator blocks() { return BlockIterator(&this); }
	BlockReverseIterator blocksReverse() { return BlockReverseIterator(&this); }

	alias getBlock = get!IrBasicBlockInstr;
	alias getPhi = get!IrPhiInstr;
	alias getVirtReg = get!IrVirtualRegister;

	ref T get(T)(IrIndex index)
	{
		assert(index.kind != IrValueKind.none, "null index");
		assert(index.kind == getIrValueKind!T, format("%s != %s", index.kind, getIrValueKind!T));
		return *cast(T*)(&storage[index.storageUintIndex]);
	}

	void assignSequentialBlockIndices()
	{
		uint index;
		foreach (idx, ref IrBasicBlockInstr block; blocks)
		{
			block.seqIndex = index++;
		}
	}
}

struct BlockIterator
{
	IrFunction* ir;
	int opApply(scope int delegate(IrIndex, ref IrBasicBlockInstr) dg) {
		IrIndex next = ir.entryBasicBlock;
		while (next.isDefined)
		{
			IrBasicBlockInstr* block = &ir.getBlock(next);
			if (int res = dg(next, *block))
				return res;
			next = block.nextBlock;
		}
		return 0;
	}
}

struct BlockReverseIterator
{
	IrFunction* ir;
	int opApply(scope int delegate(IrIndex, ref IrBasicBlockInstr) dg) {
		IrIndex prev = ir.exitBasicBlock;
		while (prev.isDefined)
		{
			IrBasicBlockInstr* block = &ir.getBlock(prev);
			if (int res = dg(prev, *block))
				return res;
			prev = block.prevBlock;
		}
		return 0;
	}
}
