/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Liveness info analisys
module be.liveness;

import std.array : empty;
import std.string : format;
import std.stdio : writeln, write, writef, writefln, stdout;
import std.format : formattedWrite, FormatSpec;
import std.range : repeat;
import std.range : chain;
import std.bitmanip : bitfields;
import std.algorithm : min, max, sort, swap;

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
void pass_live_intervals(ref CompilationContext context)
{
	LiveBitmap liveBitmap;
	foreach (FunctionDeclNode* fun; context.mod.functions)
	{
		if (fun.isExternal) continue;

		fun.backendData.liveIntervals = new FunctionLiveIntervals(fun.backendData.lirData);
		pass_live_intervals_func(context, *fun.backendData.liveIntervals, *fun.backendData.lirData, liveBitmap);

		if (context.printLiveIntervals)
		{
			TextSink sink;
			fun.backendData.liveIntervals.dump(sink, context);
			writeln(sink.text);
		}
	}
}

void pass_live_intervals_func(ref CompilationContext context, ref FunctionLiveIntervals liveIntervals, ref IrFunction ir, ref LiveBitmap liveBitmap)
{
	context.assertf(ir.backendData.callingConvention !is null, "Calling convention is null");

	size_t numVregs = ir.numVirtualRegisters;
	size_t numBucketsPerBlock = divCeil(numVregs, size_t.sizeof * 8);
	liveBitmap.allocSets(numBucketsPerBlock, ir.numBasicBlocks);

	liveIntervals.initIntervals(&context);
	liveIntervals.linearIndicies.create(&context, &ir);

	// enumerate all basic blocks, instructions
	// phi functions use index of the block
	// we can assume all blocks to have at least single instruction (exit instruction)
	uint enumerationIndex = 0;
	enum ENUM_STEP = 2;
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		version(LivePrint) writefln("[LIVE] %s %s", enumerationIndex, blockIndex);
		// Allocate index for block start
		liveIntervals.linearIndicies[blockIndex] = enumerationIndex;
		enumerationIndex += ENUM_STEP;
		// enumerate instructions
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			version(LivePrint) writefln("[LIVE]   %s %s", enumerationIndex, instrIndex);
			liveIntervals.linearIndicies[instrIndex] = enumerationIndex;
			enumerationIndex += ENUM_STEP;
		}
	}

	void liveAdd(IrIndex someOperand)
	{
		context.assertf(someOperand.isVirtReg, "not vreg, but %s", someOperand.kind);
		version(LivePrint) writefln("[LIVE] liveAdd %s #%s", someOperand, ir.getVirtReg(someOperand).seqIndex);
		liveBitmap.live[ir.getVirtReg(someOperand).seqIndex] = true;
	}

	void liveRemove(IrIndex someOperand)
	{
		context.assertf(someOperand.isVirtReg, "not vreg, but %s", someOperand.kind);
		version(LivePrint) writefln("[LIVE] liveRemove %s #%s", someOperand, ir.getVirtReg(someOperand).seqIndex);
		liveBitmap.live[ir.getVirtReg(someOperand).seqIndex] = false;
	}

	size_t[] blockLiveIn(IrIndex blockIndex)
	{
		size_t from = ir.getBlock(blockIndex).seqIndex * numBucketsPerBlock;
		size_t to = from + numBucketsPerBlock;
		return liveBitmap.liveInBuckets[from..to];
	}

	// algorithm start
	// for each block b in reverse order do
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		// Is also where phi functions are located
		uint blockFromPos = liveIntervals.linearIndicies[blockIndex];
		version(LivePrint) writefln("[LIVE] % 3s %s", blockFromPos, blockIndex);

		// live = union of successor.liveIn for each successor of block
		liveBitmap.liveData[] = 0;
		foreach (IrIndex succIndex; block.successors.range(ir))
		{
			foreach (size_t i, size_t bucket; blockLiveIn(succIndex))
				liveBitmap.liveBuckets[i] |= bucket;
		}

		// for each phi function phi of successors of block do
		//     live.add(phi.inputOf(block))
		foreach (IrIndex succIndex; block.successors.range(ir))
			foreach (IrIndex phiIndex, ref IrPhi phi; ir.getBlock(succIndex).phis(ir))
				foreach (i, ref IrPhiArg arg; phi.args(ir))
					if (arg.basicBlock == blockIndex)
						if (arg.value.isVirtReg)
							liveAdd(arg.value);

		//writef("in @%s live:", liveIntervals.linearIndicies[blockIndex]);
		//foreach (size_t index; live.bitsSet)
		//	writef(" %s", index);
		//writeln;

		// for each opd in live do
		foreach (size_t index; liveBitmap.live.bitsSet)
		{
			// intervals[opd].addRange(block.from, block.to)
			IntervalIndex interval = liveIntervals.intervalIndex(VregIntervalIndex(cast(uint)index));
			liveIntervals.addRange(interval, blockFromPos,
				liveIntervals.linearIndicies[block.lastInstr]);
			version(LivePrint) writefln("[LIVE] addRange vreg.#%s [%s; %s)", index,
				blockFromPos,
				liveIntervals.linearIndicies[block.lastInstr]);
		}

		// for each operation op of b in reverse order do
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(ir))
		{
			uint linearInstrIndex = liveIntervals.linearIndicies[instrIndex];
			version(LivePrint) writefln("[LIVE]   % 3s %s", linearInstrIndex, instrIndex);

			// -------------- Assign interval hints --------------
			// if non-mov instruction assigns to phys register,
			// movs must follow instruction immidiately matching the order of results
			// if non-mov instruction accepts 1 or more phys registers, then
			// it must be preceded by movs from vregs to pregs in matching order
			// Example:
			// eax = v.20 // args[0]
			// ecx = v.45 // args[2]
			// edx, ecx = some_instr(eax, v.100, ecx) // edx is results[0]
			// v.200 = edx // results[0] (aka result)
			// v.300 = ecx // results[1]

			InstrInfo instrInfo = context.machineInfo.instrInfo[instrHeader.op];
			//writefln("isMov %s %s", cast(Amd64Opcode)instrHeader.op, instrInfo.isMov);
			if (instrInfo.isMov)
			{
				IrIndex from = instrHeader.args[0];
				IrIndex to = instrHeader.result;
				if (from.isPhysReg && to.isVirtReg)
				{
					auto vii = VregIntervalIndex(ir.getVirtReg(to).seqIndex);
					liveIntervals.setVirtRegHint(vii, from);
				}
				else if (from.isVirtReg && to.isPhysReg)
				{
					auto vii = VregIntervalIndex(ir.getVirtReg(from).seqIndex);
					liveIntervals.setVirtRegHint(vii, to);
				}
			}
			else
			{
				if (instrHeader.hasResult && instrHeader.result.isPhysReg)
				{
					// add fixed interval to the next instr
					IntervalIndex interval = liveIntervals.intervalIndex(PregIntervalIndex(instrHeader.result));
					liveIntervals.addRange(interval, linearInstrIndex, linearInstrIndex+2);
				}
			}

			// for each output operand opd of op do
			if (instrHeader.hasResult)
			{
				if (instrHeader.result.isVirtReg)
				{
					// intervals[opd].setFrom(op.id)
					auto vii = VregIntervalIndex(ir.getVirtReg(instrHeader.result).seqIndex);
					liveIntervals.setFrom(vii, instrHeader.result, linearInstrIndex);
					version(LivePrint)
						writefln("[LIVE] setFrom vreg.#%s %s",
							ir.getVirtReg(instrHeader.result).seqIndex, linearInstrIndex);
					// live.remove(opd)
					liveRemove(instrHeader.result);
				}
				else if (instrHeader.result.isPhysReg && !instrInfo.isMov)
				{
					// non-mov, extend fixed interval to the next instr (which must be mov from that phys reg)
					IntervalIndex interval = liveIntervals.intervalIndex(PregIntervalIndex(instrHeader.result));
					liveIntervals.addRange(interval, linearInstrIndex, linearInstrIndex+2);
				}
			}

			int numPhysRegArgs = 0;
			// for each input operand opd of op do
			foreach(IrIndex arg; instrHeader.args)
			{
				if (arg.isVirtReg)
				{
					// intervals[opd].addRange(b.from, op.id)
					auto seqIndex = ir.getVirtReg(arg).seqIndex;
					auto vii = VregIntervalIndex(seqIndex);
					IntervalIndex interval = liveIntervals.intervalIndex(vii);
					liveIntervals.addRange(interval, blockFromPos, linearInstrIndex);
					version(LivePrint) writefln("[LIVE] addRange %s #%s [%s; %s)",
						liveIntervals[interval].definition, seqIndex,
						blockFromPos, linearInstrIndex);

					// live.add(opd)
					liveAdd(arg);
				}
				else if (arg.isPhysReg)
				{
					++numPhysRegArgs;
				}
			}

			// extension
			// if op requires two operand form and op is not commutative and arg0 != arg1
			//   we need to extend range of right-most opd by 1
			if (instrInfo.isTwoOperandForm && !instrInfo.isCommutative)
			{
				IrIndex arg0 = instrHeader.args[0];
				IrIndex arg1 = instrHeader.args[1];
				if (arg1.isVirtReg)
				if (arg0 != arg1)
				{
					auto vii = VregIntervalIndex(ir.getVirtReg(arg1).seqIndex);
					IntervalIndex interval = liveIntervals.intervalIndex(vii);
					liveIntervals.addRange(interval, blockFromPos, linearInstrIndex+1);
					version(LivePrint) writefln("[LIVE] addRange +1 %s", arg1);
				}
			}

			if (!instrInfo.isMov)
			{
				foreach(IrIndex arg; instrHeader.args)
				{
					if (arg.isPhysReg)
					{
						// non-mov, extend fixed interval to the preceding mov instr
						IntervalIndex interval = liveIntervals.intervalIndex(PregIntervalIndex(arg));
						liveIntervals.addRange(interval, linearInstrIndex - numPhysRegArgs*2, linearInstrIndex);
						--numPhysRegArgs;
					}
				}
			}

			// add fixed intervals fo function calls
			if (instrInfo.isCall)
			{
				FunctionIndex calleeIndex = instrHeader.preheader!IrInstrPreheader_call.calleeIndex;
				FunctionDeclNode* callee = context.mod.functions[calleeIndex];
				CallConv* cc = callee.backendData.callingConvention;
				IrIndex[] volatileRegs = cc.volatileRegs;
				foreach(IrIndex reg; volatileRegs) {
					IntervalIndex interval = liveIntervals.intervalIndex(PregIntervalIndex(reg));
					liveIntervals.addRange(interval, linearInstrIndex, linearInstrIndex+1);
				}
			}
		}

		// for each phi function phi of b do
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			// live.remove(phi.output)
			if (phi.result.isVirtReg) {
				liveRemove(phi.result);
				auto vii = VregIntervalIndex(ir.getVirtReg(phi.result).seqIndex);
				liveIntervals.setFrom(vii, phi.result, blockFromPos);
			}
		}

		// if b is loop header then
		if (block.isLoopHeader)
		{
			// We need to find the loop block with the max position
			// Use loop header as starting block in case it is in max position
			uint maxPos = liveIntervals.linearIndicies[block.lastInstr];
			IrIndex loopEnd = blockIndex;

			//     loopEnd = last block of the loop starting at b
			foreach(IrIndex pred; block.predecessors.range(ir)) {
				uint blockEndPos = liveIntervals.linearIndicies[ir.getBlock(pred).lastInstr];
				if (blockEndPos > maxPos) {
					maxPos = blockEndPos;
					loopEnd = pred;
				}
			}

			version(LivePrint) writefln("[LIVE] loop %s end %s [%s; %s)", blockIndex, loopEnd, blockFromPos, maxPos);

			//     for each opd in live do
			//         intervals[opd].addRange(b.from, loopEnd.to)
			foreach (size_t index; liveBitmap.live.bitsSet)
			{
				// intervals[opd].addRange(block.from, block.to)
				IntervalIndex interval = liveIntervals.intervalIndex(VregIntervalIndex(cast(uint)index));
				liveIntervals.addRange(interval, blockFromPos, maxPos);
				version(LivePrint) writefln("[LIVE] addRange loop %s #%s [%s; %s)",
					liveIntervals[interval].definition, index, blockFromPos, maxPos);
			}
		}

		// b.liveIn = live
		blockLiveIn(blockIndex)[] = liveBitmap.liveBuckets;
	}
}

struct LiveBitmap
{
	import std.bitmanip : BitArray;
	// We store a bit array per basic block. Each bit shows liveness of operand per block
	// Padding aligns number of bits to multiple of size_t bits.
	// This way there is a whole number of size_ts per basic block
	// With padding we can copy size_t's directly between live and liveIn, without bit twiddling

	// [block0:[IrArgumentId..., padding], block1:[IrArgumentId..., padding]]
	BitArray liveIn;
	size_t[] liveInData;
	size_t[] liveInBuckets;

	// [IrArgumentId..., padding]
	BitArray live;
	size_t[] liveData;
	size_t[] liveBuckets;

	void allocSets(size_t numBucketsPerBlock, size_t numBlocks) {
		//writefln("alloc buckets %s blocks %s", numBucketsPerBlock, numBlocks);
		if (liveData.length < numBucketsPerBlock)
			liveData.length = numBucketsPerBlock;
		liveBuckets = liveData[0..numBucketsPerBlock];
		// liveData is nulled for each basic block, so we skip nulling
		live = BitArray(liveData, numBucketsPerBlock * size_t.sizeof * 8);

		size_t numBucketsTotal = numBucketsPerBlock * numBlocks;
		if (liveInData.length < numBucketsTotal)
			liveInData.length = numBucketsTotal;
		liveInData[] = 0; // unset all bits
		liveInBuckets = liveInData[0..numBucketsTotal];
		liveIn = BitArray(liveInData, numBucketsTotal * size_t.sizeof * 8);
	}
}


///
struct FunctionLiveIntervals
{
	IrFunction* ir;
	// invariant: all ranges of one interval are sorted by `from` and do not intersect
	Buffer!LiveRange ranges;
	Buffer!LiveInterval intervals;
	size_t numFixedIntervals;
	/// instructionIndex -> seqIndex
	/// blockIndex -> seqIndex
	IrMirror!uint linearIndicies;

	LiveInterval[] virtualIntervals() { return intervals.data[numFixedIntervals..intervals.length]; }
	LiveInterval[] physicalIntervals() { return intervals.data[0..numFixedIntervals]; }

	void initIntervals(CompilationContext* context) {
		numFixedIntervals = context.machineInfo.registers.length;
		intervals.voidPut(numFixedIntervals + ir.numVirtualRegisters);
		foreach (i, ref it; physicalIntervals)
		{
			it = LiveInterval();
			it.reg = context.machineInfo.registers[i].index;
			it.isFixed = true;
		}
		foreach (i, ref it; virtualIntervals)
			it = LiveInterval();
		ranges.reserve(ir.numVirtualRegisters);
	}

	IrIndex getRegFor(IrIndex index)
	{
		if (index.isVirtReg)
		{
			auto vii = VregIntervalIndex(ir.getVirtReg(index).seqIndex);
			return intervals[intervalIndex(vii)].reg;
		}
		else if (index.isPhysReg)
		{
			return index;
		}
		assert(false);
	}

	/// Set hint for register allocator
	void setVirtRegHint(VregIntervalIndex vii, IrIndex hint) {
		intervals[intervalIndex(vii)].storageHint = hint;
	}

	bool intervalCoversPosition(NodeIndex cur, int position)
	{
		while (!cur.isNull)
		{
			auto r = &ranges[cur];
			if (position < r.from)
				return false;
			else if (position >= r.to)
				cur = r.nextIndex;
			else // from <= position < to
				return true;
		}
		return false;
	}

	int firstIntersection(NodeIndex a, NodeIndex b)
	{
		while (true)
		{
			auto ra = &ranges[a];
			auto rb = &ranges[b];
			if (ra.intersectsWith(*rb)) {
				return max(ra.from, rb.from);
			}
			else if (ra.from < rb.from) {
				a = ra.nextIndex;
				if (a.isNull) return int.max;
			}
			else { // rb.from > ra.from
				b = rb.nextIndex;
				if (b.isNull) return int.max;
			}
		}
	}

	ref LiveInterval opIndex(NodeIndex rangeId) { return intervals[ranges[rangeId].intervalIndex]; }
	ref LiveInterval opIndex(IntervalIndex intId) { return intervals[intId]; }
/*
	ref LiveInterval opIndex(IrOperandId opdId) { return intervals[numFixedIntervals + opdId]; }

	IntervalIndex intervalIndex(NodeIndex rangeId) { return ranges[rangeId].intervalIndex; }
	IntervalIndex intervalIndex(IrOperandId opdId) { return IntervalIndex(numFixedIntervals + opdId); }
	IrOperandId operandId(NodeIndex rangeId) { return operandId(ranges[rangeId].intervalIndex); }
	IrOperandId operandId(IntervalIndex intId) {
		assert(intId >= numFixedIntervals);
		return IrOperandId(intId- numFixedIntervals);
	}
*/
	IntervalIndex intervalIndex(PregIntervalIndex regRef) {
		return IntervalIndex(regRef.index);
	}
	IntervalIndex intervalIndex(VregIntervalIndex virtInterval) {
		return IntervalIndex(numFixedIntervals + virtInterval);
	}
	/*
	IntervalIndex intervalIndex(IrIndex someReg) {
		assert(someReg.isSomeReg, format("someReg is %s", someReg.kind));
		if (someReg.isVirtReg) {
			auto vii = VregIntervalIndex(ir.getVirtReg(someReg).seqIndex);
			return IntervalIndex(numFixedIntervals + vii);
		} else if (someReg.isPhysReg) {
			return IntervalIndex(someReg.physRegIndex);
		}
		assert(false);
	}*/

	// returns rangeId pointing to range covering position or one to the right of pos.
	// returns -1 if no ranges left after pos.
	NodeIndex advanceRangeId(NodeIndex cur, int position)
	{
		while (!cur.isNull)
		{
			auto r = &ranges[cur];
			if (position < r.from)
				return cur;
			else if (position >= r.to)
				cur = r.nextIndex;
			else // from <= position < to
				return cur;
		}
		return cur;
	}

	// bounds are from block start to block end of the same block
	// from is always == to block start for virtual intervals
	void addRange(IntervalIndex interval, int from, int to)
	{
		assert(interval < intervals.length,
			format("interval >= intervals.length, %s >= %s",
				interval, intervals.length));
		LiveRange newRange = LiveRange(from, to, interval);
		NodeIndex firstMergeId;

		// merge all intersecting ranges into one
		NodeIndex cur = intervals[interval].first;
		while (!cur.isNull)
		{
			auto r = &ranges[cur];

			if (r.canBeMergedWith(newRange))
			{
				if (firstMergeId.isNull)
				{
					// first merge
					firstMergeId = cur;
					r.merge(newRange);
				}
				else
				{
					// second+ merge
					ranges[firstMergeId].merge(*r);
					cur = deleteRange(cur); // sets cur to next range
					continue;
				}
			}
			else if (to < r.from)
			{
				if (!firstMergeId.isNull) return; // don't insert after merge

				// we found insertion point before cur, no merge was made
				insertRangeBefore(cur, newRange);
				return;
			}

			cur = r.nextIndex;
		}

		if (firstMergeId.isNull)
		{
			// insert after last (cur is NULL), no merge/insertion was made
			appendRange(interval, newRange);
		}
	}

	// sets the definition position
	void setFrom(VregIntervalIndex vii, IrIndex virtReg, int from) {
		IntervalIndex interval = intervalIndex(vii);
		intervals[interval].definition = virtReg;
		NodeIndex cur = intervals[interval].first;
		if (cur.isNull) { // can happen if vreg had no uses (it is probably dead)
			addRange(interval, from, from);
		} else {
			ranges[cur].from = from;
		}
	}

	void insertRangeBefore(NodeIndex beforeRange, LiveRange range)
	{
		NodeIndex index = NodeIndex(ranges.length);

		LiveRange* next = &ranges[beforeRange];
		range.prevIndex = next.prevIndex;
		range.nextIndex = beforeRange;
		next.prevIndex = index;

		if (!range.prevIndex.isNull)
			ranges[range.prevIndex].nextIndex = index;
		else
			intervals[range.intervalIndex].first = index;

		ranges.put(range);
	}

	void appendRange(IntervalIndex interval, LiveRange range)
	{
		NodeIndex last = intervals[interval].last;
		NodeIndex index = NodeIndex(ranges.length);

		if (last.isNull)
		{
			intervals[range.intervalIndex].first = index;
			intervals[range.intervalIndex].last = index;
		}
		else
		{
			LiveRange* prev = &ranges[last];
			range.prevIndex = last;
			range.nextIndex = prev.nextIndex;
			prev.nextIndex = index;

			if (!range.nextIndex.isNull)
				ranges[range.nextIndex].prevIndex = index;
		}

		ranges.put(range);
	}

	void moveRange(NodeIndex fromIndex, NodeIndex toIndex)
	{
		if (fromIndex == toIndex) return;
		ranges[toIndex] = ranges[fromIndex];
		auto range = &ranges[toIndex];
		if (!range.prevIndex.isNull) ranges[range.prevIndex].nextIndex = toIndex;
		if (!range.nextIndex.isNull) ranges[range.nextIndex].prevIndex = toIndex;
		auto it = &intervals[range.intervalIndex];
		if (fromIndex == it.first) it.first = toIndex;
		if (fromIndex == it.last) it.last = toIndex;
	}

	// returns rangeId of the next range
	NodeIndex deleteRange(NodeIndex rangeIndex)
	{
		auto range = &ranges[rangeIndex];

		auto it = &intervals[range.intervalIndex];
		if (rangeIndex == it.first) it.first = range.nextIndex;
		if (rangeIndex == it.last) it.last = range.prevIndex;

		NodeIndex lastIndex = NodeIndex(ranges.length-1);
		NodeIndex nextIndex = range.nextIndex;

		if (range.prevIndex != NodeIndex.NULL)
			ranges[range.prevIndex].nextIndex = range.nextIndex;
		if (range.nextIndex != NodeIndex.NULL)
			ranges[range.nextIndex].prevIndex = range.prevIndex;

		if (nextIndex == lastIndex) nextIndex = rangeIndex;

		moveRange(lastIndex, rangeIndex);
		ranges.unput(1);

		return nextIndex;
	}

	void dump(ref TextSink sink, ref CompilationContext context) {
		void dumpSub(LiveInterval[] intervals)
		{
			foreach (i, it; intervals) {
				NodeIndex cur = it.first;
				if (it.isFixed && cur.isNull) continue;

				if (it.isFixed) sink.put("  p");
				else sink.put("  v");

				if (cur.isNull) {
					sink.putfln("% 3s: null", i);
					continue;
				}

				if (it.reg.isDefined)
					sink.putf("% 3s %s [%2s]:", i, it.definition,
						context.machineInfo.regName(it.reg));
				else
					sink.putf("% 3s %s [no reg]:", i, it.definition);

				while (!cur.isNull)
				{
					auto r = &ranges[cur];
					sink.putf(" [%s; %s)", r.from, r.to);
					cur = r.nextIndex;
				}
				sink.putln;
			}
		}
		sink.putln("intervals:");
		dumpSub(intervals.data[0..intervals.length]);
	}
}

///
struct LiveInterval
{
	NodeIndex first;
	NodeIndex last;
	IrIndex reg;
	IrIndex definition; // null if isFixed
	//RegClass regClass;
	bool isFixed;
	IrIndex storageHint;
}

struct IntervalIndex
{
	this(size_t idx) { index = cast(uint)idx; }
	uint index = uint.max;
	alias index this;
	enum NULL = IntervalIndex();
	bool isNull() { return index == uint.max; }
}

struct VregIntervalIndex
{
	this(size_t idx) { index = cast(uint)idx; }
	uint index = uint.max;
	alias index this;
	enum NULL = VregIntervalIndex();
	bool isNull() { return index == uint.max; }
}

struct PregIntervalIndex
{
	this(IrIndex physReg) {
		assert(physReg.isPhysReg);
		index = physReg.physRegIndex;
	}
	uint index = uint.max;
	alias index this;
	enum NULL = PregIntervalIndex();
	bool isNull() { return index == uint.max; }
}

/// [from; to)
struct LiveRange
{
	int from;
	int to;
	IntervalIndex intervalIndex;
	NodeIndex prevIndex = NodeIndex.NULL;
	NodeIndex nextIndex = NodeIndex.NULL;
	bool isLast() { return nextIndex == NodeIndex.NULL; }
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
}

struct NodeIndex {
	this(size_t id) { this.id = cast(uint)id; }
	enum NodeIndex NULL = NodeIndex(uint.max);
	uint id = uint.max;
	bool isNull() { return id == NULL; }
	alias id this;
}
