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

	PhiIterator phis(IrFunction* ir) return { return PhiIterator(ir, &this); }
	InstrIterator instructions(IrFunction* ir) { return InstrIterator(ir, firstInstr); }
	InstrReverseIterator instructionsReverse(IrFunction* ir) { return InstrReverseIterator(ir, lastInstr); }
	bool hasPhis() { return firstPhi.isDefined; }

	IrSmallArray predecessors;
	IrSmallArray successors;

	// Top 4 bits must be 0 at the time of inlining, so that IrIndex fixing can be performed
	mixin(bitfields!(
		/// used for sequential block indexing
		uint, "seqIndex",    23,
		/// True if all predecessors was added
		bool, "isSealed",     1,
		/// If true, sealBlock does nothing
		/// Used to prevent sealing loop header block until whole body is generated
		bool, "preventSeal",  1,
		/// True if block_exit instruction is in place
		bool, "isFinished",   1,
		// if true, block is loop header and has incoming back edges
		bool, "isLoopHeader", 1,
		// true if block was created to split critial edge
		bool, "replacesCriticalEdge", 1,
		// used for block ordering
		bool, "visitFlag",    1,
		uint, "",             3,
	));
}
//pragma(msg, "BB size: ", cast(int)IrBasicBlock.sizeof, " bytes");

void removeAllPhis(ref IrBasicBlock block)
{
	block.firstPhi = IrIndex();
}

void removeAllInstrs(ref IrBasicBlock block)
{
	block.firstInstr = IrIndex();
	block.lastInstr = IrIndex();
}

/// INPUT:
///           D >----,
///   A --critical--> B
///    `----> C
/// Edge from A to B is called critical when A has 2+ successors and B has 2+ predecessors
bool isCriticalEdge(ref IrBasicBlock predBlock, ref IrBasicBlock succBlock)
{
	return predBlock.successors.length > 1 && succBlock.predecessors.length > 1;
}

/// INPUT:
///   A1 -> A -> A2  or  A1 -> A  or  A -> A2
/// OUTPUT:
///     A1 --> A2    or     A1    or    A2
void removeBlockFromChain(IrFunction* ir, IrBasicBlock* blockA)
{
	if (blockA.prevBlock.isDefined)
	{
		IrBasicBlock* left = ir.getBlock(blockA.prevBlock);
		left.nextBlock = blockA.nextBlock;
	}

	if (blockA.nextBlock.isDefined)
	{
		IrBasicBlock* right = ir.getBlock(blockA.nextBlock);
		right.prevBlock = blockA.prevBlock;
	}
}

/// blockB must not be an entry block, but may be exit block of IrFunction
/// used for block ordering
/// INPUT:
///   A1 -> A -> A2  or  A1 -> A
///   B1 -> B
/// OUTPUT:
///   A1 -> A2  or  A1
///   B1 -> A -> B -> B2
void linkSingleBlockBefore(IrFunction* ir, IrIndex blockA, IrIndex blockB)
{
	IrBasicBlock* b = ir.getBlock(blockB);
	IrBasicBlock* a = ir.getBlock(blockA);

	// check if already in correct order
	if (b.prevBlock == blockA) return;

	removeBlockFromChain(ir, a);

	// insert 'a' before 'b'
	{
		a.prevBlock = b.prevBlock;
		if (b.prevBlock.isDefined)
		{
			IrBasicBlock* left = ir.getBlock(b.prevBlock);
			left.nextBlock = blockA;
		}
		b.prevBlock = blockA;
		a.nextBlock = blockB;
	}
}

// blockA must not be an entry block, but may be exit block of IrFunction.
// used for block ordering.
// INPUT:
//   A1 -> A -> A2
//   B -> B2  or  B
// OUTPUT:
//   A1 -> A2
//   B -> A -> B2  or  B -> A
void moveBlockAfter(IrFunction* ir, IrIndex blockA, IrIndex blockB)
{
	IrBasicBlock* a = ir.getBlock(blockA);
	IrBasicBlock* b = ir.getBlock(blockB);

	// check if already in correct order
	if (b.nextBlock == blockA) return;

	removeBlockFromChain(ir, a);

	// insert 'a' after 'b'
	{
		a.nextBlock = b.nextBlock;
		if (b.nextBlock.isDefined)
		{
			IrBasicBlock* right = ir.getBlock(b.nextBlock);
			right.prevBlock = blockA;
		}
		b.nextBlock = blockA;
		a.prevBlock = blockB;
	}
}

// blockB must not be an entry block
// INPUT:
//   A1 -> A -> A2
//   B1 -> B -> B2
// OUTPUT:
//   -> A2 (A2.prevBlock is untouched)
//   B1 -> (B1.nextBlock is untouched)
//   A1 -> A -> B -> B2
void makeBlocksSequential(IrFunction* ir, IrIndex blockA, IrIndex blockB)
{
	IrBasicBlock* a = ir.getBlock(blockA);
	IrBasicBlock* b = ir.getBlock(blockB);

	a.nextBlock = blockB;
	b.prevBlock = blockA;
	//writefln("%s -> %s", blockA, blockB);
}


// INPUT:
//   A(IAF, IAL): IAF...IAN1,IAN2...IAL
//   B(IBF, IBL): IBF...IBL
//   - IAF First Instruction of block A
//   - IAL Last Instruction of block A
//   - IAN1,IAN2 instructions between IAF and IAL, IAN1 is followed by IAN2
//   - IBF First Instruction of block B
//   - IBL Last Instruction of block B
//   - IAF...IAN1 may be empty sequence, which means IAN2 == IAF, IAN1 == A
//   - IAN2 may equal IAL or IAF
//   - IBF may equal IBL
// OUTPUT:
//   A(IAF, IBL): IAF...IAN1,IBF...IBL
// Instructions of block B are now owned by block A.
// Fixes nextInstr / prevInstr of instrucitons.
// Fixes firstInstr / lastInstr of block A.
void concatBlockInstructions(IrFunction* ir, IrIndex blockA, IrIndex IAN2, IrIndex IBF, IrIndex IBL)
{
	IrBasicBlock* a = ir.getBlock(blockA);

	//writefln("A %s %s...%s,%s...%s", blockA, a.firstInstr, ir.prevInstr(IAN2), IAN2, a.lastInstr);
	//writefln("B %s...%s", IBF, IBL);

	if (a.firstInstr == IAN2)
	{
		a.firstInstr = IBF;
		ir.prevInstr(a.firstInstr) = blockA;
	}
	else
	{
		IrIndex IAN1 = ir.prevInstr(IAN2);
		ir.nextInstr(IAN1) = IBF;
		ir.prevInstr(IBF) = IAN1;
	}

	a.lastInstr = IBL;
	ir.nextInstr(a.lastInstr) = blockA;
	//writefln("A %s %s...%s", blockA, a.firstInstr, a.lastInstr);
}

// Block B replaced some other block A
// Fixes predecessors of B with provided array
// Fixes successors of predecessors of A to point to B
void fixBlockPreds(IrFunction* ir, IrIndex blockA, IrIndex blockB, IrSmallArray predecessorsA)
{
	IrBasicBlock* b = ir.getBlock(blockB);

	b.predecessors = predecessorsA;
	foreach (ref IrIndex pred; b.predecessors.range(ir)) {
		ir.getBlock(pred).successors.replaceFirst(ir, blockA, blockB);
	}
}

// Block B replaced some other block A
// Fixes successors of B
// Fixes predecessors of successors of A to point to B
void fixBlockSucc(IrFunction* ir, IrIndex blockA, IrIndex blockB, IrSmallArray successorsA)
{
	IrBasicBlock* b = ir.getBlock(blockB);

	//writefln("fixBlockSucc %s -> %s %s -> %s", blockA, blockB, b.successors, successorsA);

	b.successors = successorsA;
	foreach (ref IrIndex succ; b.successors.range(ir)) {
		ir.getBlock(succ).predecessors.replaceFirst(ir, blockA, blockB);
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
			IrPhi* phi = ir.getPhi(next);
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
		// first / last instructions point to basic block in prevInstr / nextInstr respectively
		while (current.isInstruction)
		{
			IrIndex indexCopy = current;
			IrInstrHeader* header = ir.getInstr(current);

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
			IrInstrHeader* header = ir.getInstr(current);

			// save current before invoking delegate, which can remove current instruction
			current = header.prevInstr(ir, indexCopy);

			if (int res = dg(indexCopy, *header))
				return res;
		}
		return 0;
	}
}
