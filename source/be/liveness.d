/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Liveness info analisys
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
	lirData.assignSequentialBlockIndices;
	pass_live_intervals_func(context, lirData, liveness);

	if (context.printLiveIntervals && context.printDumpOf(fun))
	{
		liveness.dump(context, fun);

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

	// enumerate all basic blocks, instructions
	// phi functions use index of the block
	// we can assume all blocks to have at least single instruction (exit instruction)
	uint enumerationIndex = 0;
	enum ENUM_STEP = 2;
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		version(LivePrint) writefln("[LIVE] %s %s", enumerationIndex, blockIndex);
		// Allocate index for block start
		liveness.linearIndicies[blockIndex] = enumerationIndex;
		enumerationIndex += ENUM_STEP;
		// enumerate instructions
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(*ir))
		{
			version(LivePrint) writefln("[LIVE]   %s %s", enumerationIndex, instrIndex);
			liveness.linearIndicies[instrIndex] = enumerationIndex;
			enumerationIndex += ENUM_STEP;
		}
	}
	liveness.maxLinearIndex = enumerationIndex;

	void liveAdd(IrIndex someOperand)
	{
		context.assertf(someOperand.isVirtReg, "not vreg, but %s", someOperand.kind);
		version(LivePrint) writefln("[LIVE] liveAdd %s #%s", someOperand, ir.getVirtReg(someOperand).seqIndex);
		liveness.bitmap.live[ir.getVirtReg(someOperand).seqIndex] = true;
	}

	void liveRemove(IrIndex someOperand)
	{
		context.assertf(someOperand.isVirtReg, "not vreg, but %s", someOperand.kind);
		version(LivePrint) writefln("[LIVE] liveRemove %s #%s", someOperand, ir.getVirtReg(someOperand).seqIndex);
		liveness.bitmap.live[ir.getVirtReg(someOperand).seqIndex] = false;
	}

	// algorithm start
	// for each block b in reverse order do
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		// Is also where phi functions are located
		uint blockFromPos = liveness.linearIndicies[blockIndex];
		version(LivePrint) writefln("[LIVE] % 3s %s", blockFromPos, blockIndex);

		// live = union of successor.liveIn for each successor of block
		liveness.bitmap.liveBuckets[] = 0;
		foreach (IrIndex succIndex; block.successors.range(*ir))
		{
			foreach (size_t i, size_t bucket; liveness.bitmap.blockLiveInBuckets(succIndex, ir))
				liveness.bitmap.liveBuckets[i] |= bucket;
		}

		// for each phi function phi of successors of block do
		//     live.add(phi.inputOf(block))
		foreach (IrIndex succIndex; block.successors.range(*ir))
			foreach (IrIndex phiIndex, ref IrPhi phi; ir.getBlock(succIndex).phis(*ir))
				foreach (i, ref IrPhiArg arg; phi.args(*ir))
					if (arg.basicBlock == blockIndex)
						if (arg.value.isVirtReg)
							liveAdd(arg.value);

		//writef("in @%s live:", liveness.linearIndicies[blockIndex]);
		//foreach (size_t index; live.bitsSet)
		//	writef(" %s", index);
		//writeln;

		// for each opd in live do
		foreach (size_t index; liveness.bitmap.live.bitsSet)
		{
			// intervals[opd].addRange(block.from, block.to)
			liveness.vint(index).addRange(context, blockFromPos, liveness.linearIndicies[block.lastInstr]);
			version(LivePrint) writefln("[LIVE] addRange vreg.#%s [%s; %s)", index,
				blockFromPos, liveness.linearIndicies[block.lastInstr]);
		}

		// for each operation op of b in reverse order do
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(*ir))
		{
			uint linearInstrIndex = liveness.linearIndicies[instrIndex];
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
				IrIndex from = instrHeader.args[0];
				IrIndex to = instrHeader.result;
				if (from.isPhysReg && to.isVirtReg) {
					liveness.vint(ir.getVirtReg(to).seqIndex).storageHint = from;
				} else if (from.isVirtReg && to.isPhysReg) {
					liveness.vint(ir.getVirtReg(from).seqIndex).storageHint = to;
				}
			}

			// for each output operand opd of op do
			if (instrHeader.hasResult) {
				if (instrHeader.result.isVirtReg) {
					liveness.get(ir, instrHeader.result).setFrom(context, instrHeader.result, linearInstrIndex);
					liveRemove(instrHeader.result);
				} else if (instrHeader.result.isPhysReg && !instrInfo.isMov) {
					int physRegResultOffset = linearInstrIndex+ENUM_STEP;
					// needed to account for sub/add rsp instructions around call
					if (instrHeader.extendFixedResultRange)
						physRegResultOffset += ENUM_STEP;
					// non-mov, extend fixed interval to the next instr (which must be mov from that phys reg)
					liveness.pint(instrHeader.result).addRange(context, linearInstrIndex, physRegResultOffset);
				}
			}

			int physRegArgsOffset = 0;
			foreach(IrIndex arg; instrHeader.args) {
				if (arg.isVirtReg) {
					liveness.get(ir, arg).addRange(context, blockFromPos, linearInstrIndex);
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
				IrIndex arg0 = instrHeader.args[0];
				IrIndex arg1 = instrHeader.args[1];
				if ( !sameIndexOrPhysReg(arg0, arg1) ) {
					if (arg1.isVirtReg || arg1.isPhysReg) {
						liveness.get(ir, arg1).addRange(context, blockFromPos, linearInstrIndex+1);
					}
				}
			}

			if (!instrInfo.isMov)
			{
				foreach(IrIndex arg; instrHeader.args)
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
				IrIndex callee = instrHeader.args[0];
				CallConv* cc = context.types.getCalleeCallConv(callee, *ir, context);
				IrIndex[] volatileRegs = cc.volatileRegs;
				foreach(IrIndex reg; volatileRegs) {
					liveness.pint(reg).addRange(context, linearInstrIndex, linearInstrIndex+1);
				}
			}
		}

		// for each phi function phi of b do
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(*ir))
		{
			// live.remove(phi.output)
			if (phi.result.isVirtReg) {
				liveRemove(phi.result);
				liveness.get(ir, phi.result).setFrom(context, phi.result, blockFromPos);
			}
		}

		// if b is loop header then
		if (block.isLoopHeader)
		{
			// We need to find the loop block with the max position
			// Use loop header as starting block in case it is in max position
			uint maxPos = liveness.linearIndicies[block.lastInstr];
			IrIndex loopEnd = blockIndex;
			//     loopEnd = last block of the loop starting at b
			foreach(IrIndex pred; block.predecessors.range(*ir)) {
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
		size_t from = ir.getBlock(blockIndex).seqIndex * numBucketsPerBlock;
		size_t to = from + numBucketsPerBlock;
		return liveInBuckets[from..to];
	}

	BitArray blockLiveInBits(IrIndex blockIndex, IrFunction* ir)
	{
		size_t from = ir.getBlock(blockIndex).seqIndex * numBucketsPerBlock;
		size_t to = from + numBucketsPerBlock;
		return BitArray(liveInBuckets[from..to], numBucketsPerBlock * size_t.sizeof * 8);
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
	uint maxLinearIndex; // max value stored in linearIndicies

	auto virtualIntervals() { return intervals[numFixedIntervals..intervals.length]; }
	auto physicalIntervals() { return intervals[0..numFixedIntervals]; }

	LiveInterval* vint(size_t virtSeqIndex) { return &intervals[numFixedIntervals+virtSeqIndex]; }
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
			it.reg = context.machineInfo.registers[i].index;
			it.isFixed = true;
			++i;
		}
		foreach (ref LiveInterval it; virtualIntervals)
			it = LiveInterval();

		linearIndicies.create(context, ir);
	}

	IntervalIndex indexOf(LiveInterval* it) {
		return IntervalIndex(it - &intervals.front());
	}

	IrIndex getRegFor(IrIndex index, int pos, IrFunction* ir)
	{
		if (index.isVirtReg)
		{
			IntervalIndex intIndex = numFixedIntervals + ir.getVirtReg(index).seqIndex;
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

	LiveInterval* get(IrFunction* ir, IrIndex index)
	{
		if (index.isVirtReg)
		{
			return &intervals[numFixedIntervals + ir.getVirtReg(index).seqIndex];
		}
		else if (index.isPhysReg)
		{
			return &intervals[index.physRegIndex];
		}
		assert(false);
	}

	// returns new interval
	IntervalIndex splitBefore(CompilationContext* context, IntervalIndex parentInterval, int before)
	{
		LiveInterval* it = &intervals[parentInterval];
		LiveRangeIndex rightIndex = it.getRightRange(before);
		return splitBefore(context, parentInterval, before, rightIndex);
	}

	IntervalIndex splitBefore(CompilationContext* context, IntervalIndex parentInterval, int before, LiveRangeIndex rightIndex)
	{
		//writefln("splitBefore %s %s", parentInterval, before);
		LiveInterval* it = &intervals[parentInterval];
		auto right = &it.ranges[rightIndex];

		assert(before >= it.from);

		Array!LiveRange newRanges;

		if (right.from < before)
		{
			newRanges.put(context.arrayArena, LiveRange(before, right.to)); // piece of splitted range
			right.to = before;
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
			definition : it.definition,
			parent : parentInterval
		};

		intervals.put(context.arrayArena, rightInterval);

		return childInterval;
	}

	void dump(ref TextSink sink, CompilationContext* context, FunctionDeclNode* fun) {
		void dumpSub(ref Array!LiveInterval intervals)
		{
			foreach (i, it; intervals) {
				if (it.isFixed && it.ranges.empty) continue;

				if (it.isFixed) sink.put("  p");
				else sink.put("  v");

				if (it.reg.isDefined)
					sink.putf("% 3s %s [%2s]:", i, it.definition,
						context.machineInfo.regName(it.reg));
				else
					sink.putf("% 3s %s [no reg]:", i, it.definition);

				foreach(rIndex, range; it.ranges) {
					if (rIndex > 0) sink.put(" ");
					sink.putf(" [%s; %s)", range.from, range.to);
				}
				if (!it.parent.isNull) sink.putf(" parent: v %s", it.parent);
				if (!it.child.isNull) sink.putf(" child: v %s", it.child);

				sink.putln;
			}
		}
		sink.putfln("intervals %s", context.idString(fun.backendData.name));
		dumpSub(intervals);
		//dump2;
	}
	void dump(CompilationContext* context, FunctionDeclNode* fun)
	{
		TextSink sink;
		dump(sink, context, fun);
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

struct LiveInterval
{
	Array!LiveRange ranges;
	int from() {
		if (ranges.length > 0) return ranges[0].from;
		return int.max;
	}
	int to() {
		if (ranges.length > 0) return ranges.back.to;
		return int.max;
	}
	IrIndex reg;
	IrIndex definition; // null if isFixed
	//RegClass regClass;
	bool isFixed;
	IrIndex storageHint;
	IntervalIndex parent; // null if is original interval (leftmost), otherwise points to original interval
	IntervalIndex child; // next split interval to the right

	bool isSplitChild() { return !parent.isNull; }
	bool isSplit() { return !child.isNull; }

	void toString(scope void delegate(const(char)[]) sink) {
		import std.format : formattedWrite;
		sink.formattedWrite("int(");
		if (definition.isDefined) sink.formattedWrite("%s, ", definition);
		sink.formattedWrite("%s", ranges);
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
	LiveRangeIndex getRightRange(int position)
	{
		foreach(i, range; ranges) {
			if (position < range.to)
				return LiveRangeIndex(i);
		}
		return LiveRangeIndex.NULL;
	}

	// returns rangeId pointing to range covering position or one to the left of pos.
	// returns NULL if empty interval or no ranges to the left
	LiveRangeIndex getLeftRange(int position)
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
	void setFrom(CompilationContext* context, IrIndex virtReg, int from) {
		version(LivePrint) writefln("[LIVE] setFrom vreg.%s from %s", virtReg, from);
		definition = virtReg;

		if (ranges.empty) { // can happen if vreg had no uses (it is probably dead or used in phi above definition)
			addRange(context, from, from);
		} else {
			ranges[0].from = from;
		}
	}

	// bounds are from block start to block end of the same block
	// from is always == to block start for virtual intervals
	void addRange(CompilationContext* context, int from, int to)
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

	bool coversPosition(int position)
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
	bool coversPosition_dump(int position)
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
}

int firstIntersection(LiveInterval* a, LiveInterval* b)
{
	size_t len_a = a.ranges.length;
	if (len_a == 0) return int.max;

	size_t len_b = b.ranges.length;
	if (len_b == 0) return int.max;

	size_t i_a = 0;
	size_t i_b = 0;

	LiveRange r_a = a.ranges[i_a];
	LiveRange r_b = b.ranges[i_b];

	while (true)
	{
		if (r_a.intersectsWith(r_b)) {
			return max(r_a.from, r_b.from);
		} else if (r_a.from < r_b.from) {
			if (i_a == len_a) return int.max;
			r_a = a.ranges[i_a];
			++i_a;
		} else { // r_b.from > r_a.from
			if (i_b == len_b) return int.max;
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
}

/// [from; to)
struct LiveRange
{
	int from;
	int to;

	bool contains(int pos) {
		if (pos < from) return false;
		if (pos >= to) return false;
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
