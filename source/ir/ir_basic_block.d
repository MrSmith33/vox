/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// Basic block
module ir.ir_basic_block;

import std.bitmanip : bitfields;
import all;

/// Must end with one of block_exit_... instructions
/// Only single loop end must be predecessor of loop header
/// first and last instructions point to this basic block in prevInstr, nextInstr respectively
@(IrValueKind.basicBlock)
struct IrBasicBlock
{
	IrIndex firstInstr; // null or first instruction
	IrIndex lastInstr; // null or last instruction
	IrIndex prevBlock; // null only if this is entryBasicBlock
	IrIndex nextBlock; // null only if this is exitBasicBlock
	IrIndex firstPhi; // may be null

	PhiIterator phis(IrFunction* ir) { return PhiIterator(ir, &this); }
	InstrIterator instructions(IrFunction* ir) { return InstrIterator(ir, firstInstr); }
	InstrReverseIterator instructionsReverse(IrFunction* ir) { return InstrReverseIterator(ir, lastInstr); }
	bool hasPhis() { return firstPhi.isDefined; }

	SmallVector predecessors;
	SmallVector successors;

	uint seqIndex;

	mixin(bitfields!(
		/// True if all predecessors was added
		bool, "isSealed",     1,
		/// True if block_exit instruction is in place
		bool, "isFinished",   1,
		// if true, block is loop header and has incoming back edges
		bool, "isLoopHeader", 1,
		// true if block was created to split critial edge
		bool, "replacesCriticalEdge", 1,
		// used for block ordering
		bool, "visitFlag", 1,
		uint, "",             3
	));

	IrName name;
}
//pragma(msg, "BB size: ", cast(int)IrBasicBlock.sizeof, " bytes");

void removeAllPhis(ref IrBasicBlock block)
{
	block.firstPhi = IrIndex();
}

bool isCriticalEdge(ref IrBasicBlock predBlock, ref IrBasicBlock succBlock)
{
	return predBlock.successors.length > 1 && succBlock.predecessors.length > 1;
}

void removeBlockFromChain(IrFunction* ir, IrBasicBlock* block)
{
	if (block.prevBlock.isDefined)
	{
		IrBasicBlock* left = &ir.getBlock(block.prevBlock);
		left.nextBlock = block.nextBlock;
	}

	if (block.nextBlock.isDefined)
	{
		IrBasicBlock* right = &ir.getBlock(block.nextBlock);
		right.prevBlock = block.prevBlock;
	}
}

// blockIndex must not be start block, but may be exit block of IrFunction
// used for block ordering
void linkBlockBefore(IrFunction* ir, IrIndex blockIndex, IrIndex beforeIndex)
{
	IrBasicBlock* before = &ir.getBlock(beforeIndex);
	IrBasicBlock* block = &ir.getBlock(blockIndex);

	// check if already in correct order
	if (before.prevBlock == blockIndex) return;

	removeBlockFromChain(ir, block);

	// insert before 'before' block
	{
		block.prevBlock = before.prevBlock;
		if (before.prevBlock.isDefined)
		{
			IrBasicBlock* left = &ir.getBlock(before.prevBlock);
			left.nextBlock = blockIndex;
		}
		before.prevBlock = blockIndex;
		block.nextBlock = beforeIndex;
	}
}

// blockIndex must not be start block, but may be exit block of IrFunction
// used for block ordering
void linkBlockAfter(IrFunction* ir, IrIndex blockIndex, IrIndex afterIndex)
{
	IrBasicBlock* after = &ir.getBlock(afterIndex);
	IrBasicBlock* block = &ir.getBlock(blockIndex);

	// check if already in correct order
	if (after.nextBlock == blockIndex) return;

	removeBlockFromChain(ir, block);

	// insert after 'after' block
	{
		block.nextBlock = after.nextBlock;
		if (after.nextBlock.isDefined)
		{
			IrBasicBlock* right = &ir.getBlock(after.nextBlock);
			right.prevBlock = blockIndex;
		}
		after.nextBlock = blockIndex;
		block.prevBlock = afterIndex;
	}
}

struct PhiIterator
{
	IrFunction* ir;
	IrBasicBlock* block;
	int opApply(scope int delegate(IrIndex, ref IrPhi) dg) {
		IrIndex next = block.firstPhi;
		while (next.isDefined)
		{
			IrPhi* phi = &ir.get!IrPhi(next);
			IrIndex indexCopy = next;

			// save current before invoking delegate, which can remove current phi
			next = phi.nextPhi;

			if (int res = dg(indexCopy, *phi))
				return res;
		}
		return 0;
	}
}

struct InstrIterator
{
	IrFunction* ir;
	IrIndex firstInstr;
	int opApply(scope int delegate(IrIndex, ref IrInstrHeader) dg) {
		IrIndex current = firstInstr;
		// will be 'none' if no instructions in basic block
		// first and last instructions point to basick block in prevInstr, nextInstr
		while (current.isInstruction)
		{
			IrIndex indexCopy = current;
			IrInstrHeader* header = &ir.get!IrInstrHeader(current);

			// save current before invoking delegate, which can remove current instruction
			current = header.nextInstr(ir, indexCopy);

			if (int res = dg(indexCopy, *header))
				return res;
		}
		return 0;
	}
}

struct InstrReverseIterator
{
	IrFunction* ir;
	IrIndex lastInstr;
	int opApply(scope int delegate(IrIndex, ref IrInstrHeader) dg) {
		IrIndex current = lastInstr;
		// will be 'none' if no instructions in basic block
		while (current.isInstruction)
		{
			IrIndex indexCopy = current;
			IrInstrHeader* header = &ir.get!IrInstrHeader(current);

			// save current before invoking delegate, which can remove current instruction
			current = header.prevInstr(ir, indexCopy);

			if (int res = dg(indexCopy, *header))
				return res;
		}
		return 0;
	}
}
