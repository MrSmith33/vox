/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Liveness info analisys
/// Phi functions use their arguments at the last instruction of corresponding basic block
module be.liveness;

import std.algorithm : min, max, sort, swap;
import std.array : empty;
import std.bitmanip : BitArray, bitfields;
import std.format : formattedWrite, FormatSpec;
import std.range : chain;
import std.range : repeat;
import std.stdio : writeln, write, writef, writefln, stdout;
import std.string : format;

import all;

/// Implementation of:
/// "Linear Scan Register Allocation on SSA Form"

/*
// buildIntervals
for each block b in reverse order do
	live = union of successor.liveIn for each successor of b

	for each phi function phi of successors of b do
		live.add(phi.inputOf(b))

	for each opd in live do
		intervals[opd].addRange(b.from, b.to)

	for each operation op of b in reverse order do
		for each output operand opd of op do
			intervals[opd].setFrom(op.id)
			live.remove(opd)
		for each input operand opd of op do
			intervals[opd].addRange(b.from, op.id)
			live.add(opd)
		// extension
		if op requires two operand form and op is not commutative
			we need to extend range of right-most opd by 1

	for each phi function phi of b do
		live.remove(phi.output)

	if b is loop header then
		loopEnd = last block of the loop starting at b
		for each opd in live do
			intervals[opd].addRange(b.from, loopEnd.to)
	b.liveIn = live
*/
//version = LivePrint;
void pass_live_intervals(CompilationContext* context, ModuleDeclNode* mod, FunctionDeclNode* fun, LivenessInfo* liveness)
{
	if (fun.isExternal) return;

	IrFunction* lirData = context.getAst!IrFunction(fun.backendData.lirData);
	//lirData.orderBlocks;
	//lirData.assignSequentialBlockIndices;
	//dumpFunction(context, lirData);
	pass_live_intervals_func(context, lirData, liveness);

	if (context.printLiveIntervals && context.printDumpOf(fun))
	{
		liveness.dump(context, lirData);

		FuncDumpSettings set;
		set.printLiveness = true;
		set.printVregLiveness = true;
		set.printPregLiveness = true;
		set.printLivenessLinearIndex = true;
		set.printBlockFlags = true;

		IrDumpContext dumpCtx = {
			context : context,
			ir : lirData,
			settings : &set,
			liveness : liveness
		};
		dumpFunction(&dumpCtx);
	}
}

void pass_live_intervals_func(CompilationContext* context, IrFunction* ir, LivenessInfo* liveness)
{
	liveness.initStorage(context, ir);
	liveness.assignSequentialIndices(context, ir);
	liveness.setIntervalUsesLength(context, ir);

	void liveAdd(IrIndex someOperand)
	{
		context.assertf(someOperand.isVirtReg, "not vreg, but %s", someOperand.kind);
		version(LivePrint) writefln("[LIVE] liveAdd %s #%s", someOperand, someOperand.storageUintIndex);
		liveness.bitmap.live[someOperand.storageUintIndex] = true;
	}

	void liveRemove(IrIndex someOperand)
	{
		context.assertf(someOperand.isVirtReg, "not vreg, but %s", someOperand.kind);
		version(LivePrint) writefln("[LIVE] liveRemove %s #%s", someOperand, someOperand.storageUintIndex);
		liveness.bitmap.live[someOperand.storageUintIndex] = false;
	}

	// algorithm start
	// for each block b in reverse order do
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		// Is also where phi functions are located
		uint blockFromPos = liveness.linearIndicies.basicBlock(blockIndex);
		uint blockToPos = liveness.linearIndicies.instr(block.lastInstr);
		version(LivePrint) writefln("[LIVE] % 3s %s", blockFromPos, blockIndex);

		// live = union of successor.liveIn for each successor of block
		liveness.bitmap.liveBuckets[] = 0;
		foreach (IrIndex succIndex; block.successors.range(ir))
		{
			foreach (size_t i, size_t bucket; liveness.bitmap.blockLiveInBuckets(succIndex, ir))
				liveness.bitmap.liveBuckets[i] |= bucket;
		}

		// for each phi function phi of successors of block do
		//     live.add(phi.inputOf(block))
		foreach (IrIndex succIndex; block.successors.range(ir))
			foreach (IrIndex phiIndex, ref IrPhi phi; ir.getBlock(succIndex).phis(ir))
				foreach (i, ref IrPhiArg arg; phi.args(ir))
					if (arg.basicBlock == blockIndex)
						if (arg.value.isVirtReg)
						{
							liveAdd(arg.value);
							LiveInterval* it = liveness.vint(arg.value);
							it.prependUse(UsePosition(blockToPos, UseKind.phi));
						}

		//writef("in %s %s live:", blockIndex, liveness.linearIndicies.basicBlock(blockIndex));
		//foreach (size_t index; liveness.bitmap.live.bitsSet)
		//	writef(" %s", index);
		//writeln;

		// for each opd in live do
		foreach (size_t index; liveness.bitmap.live.bitsSet)
		{
			// intervals[opd].addRange(block.from, block.to)
			liveness.vint(index).addRange(context, blockFromPos, blockToPos);
			version(LivePrint) writefln("[LIVE] addRange vreg.#%s [%s; %s)", index,
				blockFromPos, blockToPos);
		}

		// for each operation op of b in reverse order do
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(ir))
		{
			IrIndex result = instrHeader.tryGetResult(ir); // nullable
			IrIndex[] args = instrHeader.args(ir);

			uint linearInstrIndex = liveness.linearIndicies.instr(instrIndex);
			version(LivePrint) writefln("[LIVE]   % 3s %s", linearInstrIndex, instrIndex);

			// -------------- Assign interval hints --------------
			// if non-mov instruction assigns to phys register,
			// movs must follow instruction immidiately matching the order of results
			// if non-mov instruction accepts 1 or more phys registers, then
			// it must be preceded by movs from vregs to pregs in matching order
			// Example:
			//   eax = v.20 // args[0]
			//   ecx = v.45 // args[2]
			//   optional non-mov instruction (for call, which has extendFixedArgRange)
			//   edx, ecx = some_instr(eax, v.100, ecx) // edx is results[0]
			//   optional non-mov instruction (for call, which has extendFixedResultRange)
			//   v.200 = edx // results[0] (aka result)
			//   v.300 = ecx // results[1]

			InstrInfo instrInfo = context.machineInfo.instrInfo[instrHeader.op];
			//writefln("isMov %s %s", cast(Amd64Opcode)instrHeader.op, instrInfo.isMov);
			if (instrInfo.isMov) {
				IrIndex from = args[0];
				IrIndex to = result;
				if (from.isPhysReg && to.isVirtReg) {
					liveness.vint(to).storageHint = from;
				} else if (from.isVirtReg && to.isPhysReg) {
					liveness.vint(from).storageHint = to;
				}
			}

			// for each output operand opd of op do
			if (instrHeader.hasResult) {
				if (result.isVirtReg) {
					liveness.get(result).setFrom(context, linearInstrIndex);
					LiveInterval* it = liveness.vint(result);
					it.prependUse(UsePosition(linearInstrIndex, UseKind.instruction));
					liveRemove(result);
				} else if (result.isPhysReg && !instrInfo.isMov) {
					uint physRegResultOffset = linearInstrIndex+ENUM_STEP;
					// needed to account for sub/add rsp instructions around call
					if (instrHeader.extendFixedResultRange)
						physRegResultOffset += ENUM_STEP;
					// non-mov, extend fixed interval to the next instr (which must be mov from that phys reg)
					liveness.pint(result).addRange(context, linearInstrIndex, physRegResultOffset);
				}
			}

			uint physRegArgsOffset = 0;
			foreach(IrIndex arg; args) {
				if (arg.isVirtReg) {
					LiveInterval* it = liveness.vint(arg);
					it.addRange(context, blockFromPos, linearInstrIndex);
					it.prependUse(UsePosition(linearInstrIndex, UseKind.instruction));
					liveAdd(arg);
				}
				else if (arg.isPhysReg) {
					physRegArgsOffset += ENUM_STEP;
				}
			}

			if (physRegArgsOffset != 0)
			{
				// needed to account for sub/add rsp instructions around call
				if (instrHeader.extendFixedArgRange)
					physRegArgsOffset += ENUM_STEP;
			}

			// extension
			// if op requires two operand form and op is not commutative and arg0 != arg1
			//   we need to extend range of right-most opd by 1
			//   see more info in register allocator
			if (instrInfo.isResultInDst && instrHeader.numArgs == 2 && !instrInfo.isCommutative)
			{
				IrIndex arg0 = args[0];
				IrIndex arg1 = args[1];
				if ( !sameIndexOrPhysReg(arg0, arg1) ) {
					if (arg1.isVirtReg || arg1.isPhysReg) {
						liveness.get(arg1).addRange(context, blockFromPos, linearInstrIndex+1);
					}
				}
			}

			if (!instrInfo.isMov)
			{
				foreach(IrIndex arg; args)
				{
					if (arg.isPhysReg)
					{
						// non-mov, extend fixed interval to the preceding mov instr
						liveness.pint(arg).addRange(context, linearInstrIndex - physRegArgsOffset, linearInstrIndex);
						physRegArgsOffset -= ENUM_STEP;
					}
				}
			}

			// add fixed intervals fo function calls
			if (instrInfo.isCall)
			{
				IrIndex callee = args[0];
				CallConv* cc = context.types.getCalleeCallConv(callee, ir, context);
				IrIndex[] volatileRegs = cc.volatileRegs;
				foreach(IrIndex reg; volatileRegs) {
					liveness.pint(reg).addRange(context, linearInstrIndex, linearInstrIndex+1);
				}
			}
		}

		// for each phi function phi of b do
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			// live.remove(phi.output)
			if (phi.result.isVirtReg) {
				liveRemove(phi.result);
				liveness.get(phi.result).setFrom(context, blockFromPos);
			}
		}

		// if b is loop header then
		if (block.isLoopHeader)
		{
			// We need to find the loop block with the max position
			// Use loop header as starting block in case it is in max position
			uint maxPos = blockToPos;
			IrIndex loopEnd = blockIndex;
			//     loopEnd = last block of the loop starting at b
			foreach(IrIndex pred; block.predecessors.range(ir)) {
				uint blockEndPos = liveness.linearIndicies[ir.getBlock(pred).lastInstr];
				if (blockEndPos > maxPos) {
					maxPos = blockEndPos;
					loopEnd = pred;
				}
			}

			if (loopEnd != blockIndex) // skip if header jumps to itself
			for (IrIndex loopBlockIndex = block.nextBlock;;)
			{
				IrBasicBlock* loopBlock = &ir.getBlock(loopBlockIndex);
				size_t[] liveIns = liveness.bitmap.blockLiveInBuckets(loopBlockIndex, ir);
				// add live in of loop header to all blocks of the loop
				foreach(i, ref size_t bucket; liveIns)
					bucket |= liveness.bitmap.liveBuckets[i];
				if (loopBlockIndex == loopEnd) break;
				loopBlockIndex = loopBlock.nextBlock;
			}

			//     for each opd in live do
			//         intervals[opd].addRange(b.from, loopEnd.to)
			foreach (size_t index; liveness.bitmap.live.bitsSet)
			{
				liveness.vint(index).addRange(context, blockFromPos, maxPos);
			}
		}

		// b.liveIn = live
		size_t[] liveIns = liveness.bitmap.blockLiveInBuckets(blockIndex, ir);
		liveIns[] = liveness.bitmap.liveBuckets;
		//writefln("liveBuckets %s %s %s", blockIndex, liveIns.ptr, liveness.bitmap.blockLiveInBits(blockIndex, ir));
	}

	// Reset length from 0 to actual length
	liveness.resetIntervalUsesLength(context, ir);
}

struct LiveBitmap
{
	// We store a bit array per basic block. Each bit shows liveness of virtual register per block
	// Padding aligns number of bits to multiple of size_t bits.
	// This way there is a whole number of size_ts per basic block
	// With padding we can copy size_t's directly between live and liveIn, without bit twiddling

	size_t numBucketsPerBlock;

	// [block0:[vreg index..., padding], block1:[vreg index..., padding]]
	size_t[] liveInBuckets;

	// [vreg index..., padding]
	size_t[] liveBuckets;
	BitArray live;

	void allocSets(CompilationContext* c, uint numBucketsPerBlock, uint numBlocks) {
		this.numBucketsPerBlock = numBucketsPerBlock;
		uint numBucketsTotal = numBucketsPerBlock * numBlocks;
		liveInBuckets = c.allocateTempArray!size_t(numBucketsTotal);
		liveInBuckets[] = 0;

		liveBuckets = c.allocateTempArray!size_t(numBucketsPerBlock);
		liveBuckets[] = 0;
		live = BitArray(liveBuckets, numBucketsPerBlock * size_t.sizeof * 8);
	}

	size_t[] blockLiveInBuckets(IrIndex blockIndex, IrFunction* ir)
	{
		size_t from = blockIndex.storageUintIndex * numBucketsPerBlock;
		size_t to = from + numBucketsPerBlock;
		return liveInBuckets[from..to];
	}

	BitArray blockLiveInBits(IrIndex blockIndex, IrFunction* ir)
	{
		size_t from = blockIndex.storageUintIndex * numBucketsPerBlock;
		size_t to = from + numBucketsPerBlock;
		return BitArray(liveInBuckets[from..to], numBucketsPerBlock * size_t.sizeof * 8);
	}
}

// position 0 is owned by entry block and can't be owned by any phi function
// so all use positions are > 0
enum MIN_USE_POS = 0;
enum MAX_USE_POS = (1 << 28) - 1;
enum ENUM_STEP = 2;

enum UseKind : ubyte {
	instruction,
	phi
}

struct UsePosition
{
	this(uint _pos, UseKind _kind)
	{
		pos = _pos;
		kind = _kind;
	}

	mixin(bitfields!(
		uint,    "pos",  28,
		UseKind, "kind",  4
	));

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		final switch (kind) {
			case UseKind.instruction: sink.formattedWrite("(%s instr)", pos); break;
			case UseKind.phi: sink.formattedWrite("(%s phi)", pos); break;
		}
	}
}

struct LiveInterval
{
	Array!LiveRange ranges;
	UsePosition[] uses;

	uint from() {
		if (ranges.length > 0) return ranges[0].from;
		return MAX_USE_POS;
	}
	uint to() {
		if (ranges.length > 0) return ranges.back.to;
		return MAX_USE_POS;
	}

	IrIndex reg;
	IrIndex definition; // phys or virt reg
	IrIndex storageHint;
	IntervalIndex parent; // prev split interval to the left
	IntervalIndex child; // next split interval to the right

	bool isFixed() { return definition.isPhysReg; }
	bool isSplitChild() { return !parent.isNull; }
	bool isSplit() { return !child.isNull; }

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		sink.formattedWrite("int(");
		if (definition.isDefined) sink.formattedWrite("%s, ", definition);
		sink.formattedWrite("%s, %s", ranges, uses);
		if (!parent.isNull) sink.formattedWrite(", par %s", parent);
		if (!child.isNull) sink.formattedWrite(", child %s", child);
		sink(")");
	}

	/// Set hint for register allocator
	void setVirtRegHint(IrIndex hint) {
		storageHint = hint;
	}

	// returns rangeId pointing to range covering position or one to the right of pos.
	// returns NULL if no ranges left after pos.
	LiveRangeIndex getRightRange(uint position)
	{
		foreach(i, range; ranges) {
			if (position < range.to)
				return LiveRangeIndex(i);
		}
		return LiveRangeIndex.NULL;
	}

	LiveRangeIndex getRightRangeInclusive(uint position)
	{
		foreach(i, range; ranges) {
			if (position <= range.to)
				return LiveRangeIndex(i);
		}
		return LiveRangeIndex.NULL;
	}

	// returns rangeId pointing to range covering position or one to the left of pos.
	// returns NULL if empty interval or no ranges to the left
	LiveRangeIndex getLeftRange(uint position)
	{
		LiveRangeIndex result = LiveRangeIndex.NULL;
		foreach(i, range; ranges) {
			if (position >= range.from)
				return result;
			result = LiveRangeIndex(i);
		}
		return result;
	}

	// sets the definition position
	void setFrom(CompilationContext* context, uint from) {
		version(LivePrint) writefln("[LIVE] setFrom vreg.%s from %s", virtReg, from);

		if (ranges.empty) { // can happen if vreg had no uses (it is probably dead or used in phi above definition)
			addRange(context, from, from);
		} else {
			ranges[0].from = from;
		}
	}

	void prependUse(UsePosition use) {
		//writefln("prependUse %s %s", definition, use);
		uses[$-1] = use;
		--uses.length;
	}

	// bounds are from block start to block end of the same block
	// from is always == to block start for virtual intervals
	void addRange(CompilationContext* context, uint from, uint to)
	{
		version(LivePrint) writefln("[LIVE] addRange %s [%s; %s)", definition, from, to);
		LiveRange newRange = LiveRange(from, to);

		size_t cur = 0;
		size_t len = ranges.length;

		while (cur < len)
		{
			LiveRange* r = &ranges[cur];

			if (r.canBeMergedWith(newRange))
			{
				// merge all intersecting ranges into one
				r.merge(newRange);

				++cur;
				size_t firstToRemove = cur;

				while (cur < len && r.canBeMergedWith(ranges[cur])) {
					r.merge(ranges[cur]);
					++cur;
				}
				ranges.removeByShift(firstToRemove, cur-firstToRemove);

				return;
			}
			else if (to < r.from)
			{
				// we found insertion point before cur
				ranges.putAt(context.arrayArena, newRange, cur);
				return;
			}

			++cur;
		}

		// insert after last, no merge/insertion was made
		ranges.put(context.arrayArena, newRange);
	}

	bool hasUseAt(uint pos) {
		foreach (UsePosition use; uses) {
			if (use.pos == pos) return true;
		}
		return false;
	}

	uint firstUse() {
		if (uses.length == 0) return MAX_USE_POS;
		return uses[0].pos;
	}

	uint nextUseAfter(uint after)
	{
		uint closest = MAX_USE_POS;
		foreach_reverse (UsePosition use; uses)
		{
			if (use.pos <= after) break;
			closest = use.pos;
		}
		return closest;
	}

	bool coversPosition(uint position)
	{
		foreach(range; ranges) {
			if (position < range.from)
				return false;
			else if (position < range.to)
				return true;
			// position >= to
		}
		return false;
	}

	// internal. Returns true for exclusive end too
	bool coversPosition_dump(uint position)
	{
		foreach(range; ranges) {
			if (position < range.from)
				return false;
			else if (position <= range.to)
				return true;
			// position >= to
		}
		return false;
	}

	// retains uses before `pos`, returns uses >= `pos`
	UsePosition[] splitUsesBefore(uint pos) {
		foreach(i, use; uses) {
			if (use.pos >= pos) {
				UsePosition[] copy = uses;
				uses = uses[0..i];
				return copy[i..$];
			}
		}
		return null;
	}
}


///
struct LivenessInfo
{
	LiveBitmap bitmap;

	// invariant: all ranges of one interval are sorted by `from` and do not intersect
	Array!LiveInterval intervals;
	uint numFixedIntervals;
	/// instructionIndex -> seqIndex
	/// blockIndex -> seqIndex
	IrMirror!uint linearIndicies;
	// maps even indicies to instructions and basic blocks
	IrIndex[] evenIndexToIrIndex;
	uint maxLinearIndex; // max value stored in linearIndicies

	auto virtualIntervals() { return intervals[numFixedIntervals..$]; }
	auto physicalIntervals() { return intervals[0..numFixedIntervals]; }
	auto splitIntervals(IrFunction* ir) { return intervals[numFixedIntervals+ir.numVirtualRegisters..$]; }

	LiveInterval* vint(size_t virtSeqIndex) { return &intervals[numFixedIntervals+virtSeqIndex]; }
	LiveInterval* vint(IrIndex index) { return &intervals[numFixedIntervals + index.storageUintIndex]; }
	LiveInterval* pint(IrIndex physReg) { return &intervals[physReg.physRegIndex]; }

	void initStorage(CompilationContext* context, IrFunction* ir) {

		uint numVregs = ir.numVirtualRegisters;
		uint numBucketsPerBlock = cast(uint)divCeil(numVregs, size_t.sizeof * 8);
		bitmap.allocSets(context, numBucketsPerBlock, ir.numBasicBlocks);

		intervals.clear;
		numFixedIntervals = cast(uint)context.machineInfo.registers.length;
		intervals.voidPut(context.arrayArena, numFixedIntervals + ir.numVirtualRegisters);

		size_t i;
		foreach (ref LiveInterval it; physicalIntervals)
		{
			it = LiveInterval();
			it.reg = it.definition = context.machineInfo.registers[i].index;
			++i;
		}
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; ir.virtualRegsiters) {
			LiveInterval it = { definition : vregIndex };
			*vint(vregIndex.storageUintIndex) = it;
		}

		linearIndicies.createBasicBlockMirror(context, ir);
		linearIndicies.createInstrMirror(context, ir);
	}

	void assignSequentialIndices(CompilationContext* context, IrFunction* ir) {
		// enumerate all basic blocks, instructions
		// phi functions use index of the block
		// we can assume all blocks to have at least single instruction (exit instruction)
		evenIndexToIrIndex = cast(IrIndex[])context.tempBuffer.voidPut(ir.numBasicBlocks + ir.numInstructions);
		uint index = 0;
		foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
		{
			version(LivePrint) writefln("[LIVE] %s %s", index * ENUM_STEP, blockIndex);
			// Allocate index for block start
			linearIndicies.basicBlock(blockIndex) = index * ENUM_STEP;
			evenIndexToIrIndex[index] = blockIndex;
			++index;
			// enumerate instructions
			foreach (IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
			{
				version(LivePrint) writefln("[LIVE]   %s %s", index * ENUM_STEP, instrIndex);
				linearIndicies.instr(instrIndex) = index * ENUM_STEP;
				evenIndexToIrIndex[index] = instrIndex;
				++index;
			}
		}
		maxLinearIndex = index * ENUM_STEP;
	}

	// First we set length
	// Then we assign last element for each use and decrement the length
	// We track definition users for instructions
	void setIntervalUsesLength(CompilationContext* context, IrFunction* ir) {
		foreach (ref LiveInterval it; virtualIntervals)
		{
			IrVirtualRegister* vreg = &ir.getVirtReg(it.definition);
			context.assertf(it.uses.length == 0, "non empty uses %s of %s; %s", it.uses.length, it.definition, it.uses);
			uint numUses = vreg.definition.isPhi ? vreg.users.length : vreg.users.length + 1; // with definition
			it.uses = cast(UsePosition[])context.tempBuffer.voidPut(numUses);
		}
	}

	// At the end we set the length one more time
	// This way uses are in correct order
	void resetIntervalUsesLength(CompilationContext* context, IrFunction* ir) {
		foreach (ref LiveInterval it; virtualIntervals)
		{
			IrVirtualRegister* vreg = &ir.getVirtReg(it.definition);
			context.assertf(it.uses.length == 0, "non empty uses %s of %s; %s", it.uses.length, it.definition, it.uses);
			uint numUses = vreg.definition.isPhi ? vreg.users.length : vreg.users.length + 1; // with definition
			it.uses = it.uses.ptr[0..numUses];
		}
	}

	IntervalIndex indexOf(LiveInterval* it) {
		return IntervalIndex(it - &intervals.front());
	}

	bool isBlockStartAt(IrFunction* ir, uint pos, ref IrIndex next) {
		while (next.isDefined)
		{
			IrBasicBlock* block = &ir.getBlock(next);
			uint blockFromPos = linearIndicies.basicBlock(next);
			if (pos == blockFromPos) return true;
			uint blockToPos = linearIndicies.instr(block.lastInstr);
			if (pos <= blockToPos) return false;
			next = block.nextBlock;
		}
		return false;
	}

	IrIndex getRegFor(IrIndex index, uint pos)
	{
		if (index.isVirtReg)
		{
			IntervalIndex intIndex = numFixedIntervals + index.storageUintIndex;
			LiveInterval* it = &intervals[intIndex];
			while (it.to < pos) {
				if (it.child.isNull) return IrIndex.init;
				it = &intervals[it.child];
			}
			return it.reg;
		}
		else if (index.isPhysReg)
		{
			return index;
		}
		writefln("%s", index);
		assert(false);
	}

	LiveInterval* get(IrIndex index)
	{
		if (index.isVirtReg)
		{
			return &intervals[numFixedIntervals + index.storageUintIndex];
		}
		else if (index.isPhysReg)
		{
			return &intervals[index.physRegIndex];
		}
		assert(false);
	}

	// returns new interval or old if no split is possible
	IntervalIndex splitBefore(CompilationContext* context, IrFunction* lir, IntervalIndex parentInterval, uint before)
	{
		LiveInterval* it = &intervals[parentInterval];
		uint optimalPos = before;//findSplitPos(context, lir, it, before);
		LiveRangeIndex rightIndex = it.getRightRange(optimalPos-1);
		//writefln("  splitBefore1 %s %s %s", parentInterval, optimalPos, rightIndex);
		if (rightIndex == MAX_USE_POS) {
			return parentInterval;
		}
		return splitBefore(context, parentInterval, optimalPos, rightIndex);
	}

	uint findSplitPos(CompilationContext* context, IrFunction* lir, LiveInterval* it, uint before)
	{
		uint minPos = it.from + 1;
		uint maxPos = before;
		IrIndex maxBlock;
		uint maxBlockPos;

		foreach (IrIndex blockIndex, ref IrBasicBlock block; lir.blocks)
		{
			uint blockPos = linearIndicies.basicBlock(blockIndex);
			if (blockPos > maxPos) break;
			maxBlockPos = blockPos;
		}

		// max pos between minPos and maxPos
		if (maxBlockPos >= minPos) return maxBlockPos;

		// no block boundaries found
		return maxPos;
	}

	IntervalIndex splitBefore(CompilationContext* context, IntervalIndex parentInterval, uint before, LiveRangeIndex rightIndex)
	{
		LiveInterval* it = &intervals[parentInterval];
		auto right = &it.ranges[rightIndex];
		//writefln("  splitBefore %s %s %s", parentInterval, before, right.from);

		assert(before >= it.from);

		Array!LiveRange newRanges;

		if (right.from < before-1)
		{
			newRanges.put(context.arrayArena, LiveRange(before, right.to)); // piece of splitted range
			right.to = before-1;
			foreach(range; it.ranges[rightIndex+1..$])
				newRanges.put(context.arrayArena, range);
			it.ranges.unput(it.ranges.length - rightIndex - 1); // dont remove splitted range
		}
		else
		{
			foreach(range; it.ranges[rightIndex..$])
				newRanges.put(context.arrayArena, range);
			it.ranges.unput(it.ranges.length - rightIndex);
		}

		auto childInterval = IntervalIndex(intervals.length);
		it.child = childInterval;

		LiveInterval rightInterval = {
			ranges : newRanges,
			uses : it.splitUsesBefore(before-1),
			definition : it.definition,
			parent : parentInterval
		};

		intervals.put(context.arrayArena, rightInterval);
		//writefln("    left %s", *it);
		//writefln("    right %s", rightInterval);
		assert(it.ranges.length > 0);
		assert(rightInterval.ranges.length > 0);
		assert(rightInterval.uses.length > 0);

		return childInterval;
	}

	void dump(ref TextSink sink, CompilationContext* context, IrFunction* lir) {
		void dumpSub(ref Array!LiveInterval intervals)
		{
			foreach (i, it; intervals) {
				if (it.isFixed && it.ranges.empty) continue;

				if (it.isFixed) sink.put("  p");
				else sink.put("  v");

				if (it.reg.isDefined)
					sink.putf("% 3s %s [%2s]:", i,
						IrIndexDump(it.definition, context, lir),
						IrIndexDump(it.reg, context, lir));
				else
					sink.putf("% 3s %s [no reg]:", i, it.definition);

				foreach(rIndex, range; it.ranges) {
					if (rIndex > 0) sink.put(" ");
					sink.putf(" [%s; %s)", range.from, range.to);
				}

				foreach(UsePosition use; it.uses) {
					sink.putf(" %s", use);
				}
				if (!it.parent.isNull) sink.putf(" parent: v %s", it.parent);
				if (!it.child.isNull) sink.putf(" child: v %s", it.child);

				sink.putln;
			}
		}
		sink.putfln("intervals %s", context.idString(lir.backendData.name));
		dumpSub(intervals);
		//dump2;
	}
	void dump(CompilationContext* context, IrFunction* lir)
	{
		TextSink sink;
		dump(sink, context, lir);
		sink.text.writeln;
	}
	void dump2() {
		writefln("intervals");
		foreach (i, it; intervals) {
			writefln("% 2s %s", i, it);
			//foreach (j, r; it.ranges) {
			//	writefln("  % 2s %s", j, r);
			//}
		}
	}
}

uint firstIntersection(LiveInterval* a, LiveInterval* b)
{
	size_t len_a = a.ranges.length;
	if (len_a == 0) return MAX_USE_POS;

	size_t len_b = b.ranges.length;
	if (len_b == 0) return MAX_USE_POS;

	size_t i_a = 0;
	size_t i_b = 0;

	LiveRange r_a = a.ranges[i_a];
	LiveRange r_b = b.ranges[i_b];

	while (true)
	{
		if (r_a.intersectsWith(r_b)) {
			return max(r_a.from, r_b.from);
		} else if (r_a.from < r_b.from) {
			if (i_a == len_a) return MAX_USE_POS;
			r_a = a.ranges[i_a];
			++i_a;
		} else { // r_b.from > r_a.from
			if (i_b == len_b) return MAX_USE_POS;
			r_b = b.ranges[i_b];
			++i_b;
		}
	}
}

struct IntervalIndex
{
	this(size_t idx) { index = cast(uint)idx; }
	uint index = uint.max;
	alias index this;
	enum NULL = IntervalIndex();
	bool isNull() { return index == uint.max; }
	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		if (isNull) sink("it_null");
		else sink.formattedWrite("it%s", index);
	}
}

/// [from; to)
struct LiveRange
{
	uint from;
	uint to;

	bool contains(uint pos) {
		if (pos < from) return false;
		if (pos >= to) return false;
		return true;
	}
	bool containsInclusive(uint pos) {
		if (pos < from) return false;
		if (pos > to) return false;
		return true;
	}
	void merge(LiveRange other) {
		from = min(from, other.from);
		to = max(to, other.to);
	}
	bool canBeMergedWith(const LiveRange other) {
		if (to + 2 < other.from) return false;
		if (from > other.to + 2) return false;
		return true;
	}

	bool intersectsWith(const LiveRange other) {
		if (to <= other.from) return false;
		if (from >= other.to) return false;
		return true;
	}

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		sink.formattedWrite("[%s; %s)", from, to);
	}
}

struct LiveRangeIndex {
	this(size_t id) { this.id = cast(uint)id; }
	enum LiveRangeIndex NULL = LiveRangeIndex(uint.max);
	uint id = uint.max;
	bool isNull() { return id == NULL; }
	alias id this;

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		if (id == uint.max) sink("max");
		else sink.formattedWrite("%s", id);
	}
}
