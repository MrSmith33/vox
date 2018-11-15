/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// Basic block
module ir.ir_basic_block;

import std.bitmanip : bitfields;
import all;

/// Must end with one of block_exit_... instructions
@(IrValueKind.basicBlock)
struct IrBasicBlock
{
	IrIndex firstInstr; // null or first instruction
	IrIndex lastInstr; // null or last instruction
	IrIndex prevBlock; // null only if this is entryBasicBlock
	IrIndex nextBlock; // null only if this is exitBasicBlock
	IrIndex firstPhi; // may be null

	PhiIterator phis(ref IrFunction ir) { return PhiIterator(&ir, &this); }
	InstrIterator instructions(ref IrFunction ir) { return InstrIterator(&ir, &this); }
	InstrReverseIterator instructionsReverse(ref IrFunction ir) { return InstrReverseIterator(&ir, &this); }

	SmallVector predecessors;
	SmallVector successors;

	uint seqIndex;

	mixin(bitfields!(
		/// True if all predecessors was added
		bool, "isSealed",   1,
		/// True if block_exit instruction is in place
		bool, "isFinished", 1,
		uint, "",           6
	));

	IrName name;
}
//pragma(msg, "BB size: ", cast(int)IrBasicBlock.sizeof, " bytes");

struct PhiIterator
{
	IrFunction* ir;
	IrBasicBlock* block;
	int opApply(scope int delegate(IrIndex, ref IrPhi) dg) {
		IrIndex next = block.firstPhi;
		while (next.isDefined)
		{
			IrPhi* phi = &ir.get!IrPhi(next);
			if (int res = dg(next, *phi))
				return res;
			next = phi.nextPhi;
		}
		return 0;
	}
}

struct InstrIterator
{
	IrFunction* ir;
	IrBasicBlock* block;
	int opApply(scope int delegate(IrIndex, ref IrInstrHeader) dg) {
		IrIndex current = block.firstInstr;
		while (current.isDefined)
		{
			IrIndex indexCopy = current;
			IrInstrHeader* header = &ir.get!IrInstrHeader(current);

			// save current before invoking delegate, which can remove current instruction
			current = header.nextInstr;

			if (int res = dg(indexCopy, *header))
				return res;
		}
		return 0;
	}
}

struct InstrReverseIterator
{
	IrFunction* ir;
	IrBasicBlock* block;
	int opApply(scope int delegate(IrIndex, ref IrInstrHeader) dg) {
		IrIndex current = block.lastInstr;
		while (current.isDefined)
		{
			IrIndex indexCopy = current;
			IrInstrHeader* header = &ir.get!IrInstrHeader(current);

			// save current before invoking delegate, which can remove current instruction
			current = header.prevInstr;

			if (int res = dg(indexCopy, *header))
				return res;
		}
		return 0;
	}
}
