/**
Copyright: Copyright (c) 2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// This is the simplest complete (not yet) compiler for C-like lang
module compiler1;

import std.array : empty;
import std.string : format;
import std.stdio : writeln, write, writef, writefln, stdout;
import std.format : formattedWrite, FormatSpec;
import std.range : repeat;
import std.range : chain;
import std.bitmanip : bitfields;
import std.algorithm : min, max, sort, swap;

import all;

//    #         #####   ##   ##  #######  #     #  #######   #####    #####
//    #           #      #   #   #        ##    #  #        #     #  #     #
//    #           #      #   #   #        # #   #  #        #        #
//    #           #       # #    #####    #  #  #  #####     #####    #####
//    #           #       # #    #        #   # #  #              #        #
//    #           #        #     #        #    ##  #        #     #  #     #
//    ######    #####      #     #######  #     #  #######   #####    #####
// -----------------------------------------------------------------------------


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

	for each phi function phi of b do
		live.remove(phi.output)

	if b is loop header then
		loopEnd = last block of the loop starting at b
		for each opd in live do
			intervals[opd].addRange(b.from, loopEnd.to)
	b.liveIn = live
*/
/*
void pass_live_intervals(ref CompilationContext ctx)
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

	foreach (IrFunction* fun; ctx.mod.irModule.functions)
	{
		fun.assignSequentialBlockIndices();
		size_t numValues = fun.operands.length;
		size_t numBucketsPerBlock = divCeil(numValues, size_t.sizeof * 8);
		allocSets(numBucketsPerBlock, fun.basicBlocks.length);
		fun.liveIntervals.initIntervals(numValues, numValues, ctx);

		void liveAdd(IrOperandId opdId)
		{
			if (opdId.isNull) return;
			live[opdId] = true;
		}

		void liveRemove(IrOperandId opdId) {
			if (opdId.isNull) return;
			live[opdId] = false;
		}

		size_t[] blockLiveIn(BasicBlockIndex blockIndex)
		{
			size_t from = fun.basicBlocks[blockIndex].seqIndex * numBucketsPerBlock;
			size_t to = from + numBucketsPerBlock;
			return liveInBuckets[from..to];
		}

		// algorithm start
		// for each block b in reverse order do
		foreach (ref IrBasicBlock block; fun.basicBlocks.range)
		{
			// live = union of successor.liveIn for each successor of block
			liveData[] = 0;
			foreach (BasicBlockIndex succIndex; block.successors)
			{
				foreach (size_t i, size_t bucket; blockLiveIn(succIndex))
					liveBuckets[i] |= bucket;
			}

			// for each phi function phi of successors of block do
			//     live.add(phi.inputOf(block))
			foreach (BasicBlockIndex succIndex; block.successors)
				foreach (ref IrRef phiRef; fun.basicBlocks[succIndex].phis)
					foreach (ref IrPhiArg arg; fun.phis[phiRef.index].args)
						if (arg.blockIndex == block.index)
							liveAdd(fun.getOperand(arg.value));

			//writef("in @%s live:", block.index);
			//foreach (size_t index; live.bitsSet)
			//	writef(" %s", index);
			//writeln;

			// for each opd in live do
			foreach (size_t index; live.bitsSet)
			{
				// intervals[opd].addRange(block.from, block.to)
				IntervalId intId = fun.liveIntervals.intervalId(IrOperandId(cast(uint)index));
				fun.liveIntervals.addRange(intId, cast(int)block.firstInstr, cast(int)block.lastInstr);
			}

			void eachArg(IrRef opd, size_t opId) {
				IrOperandId opdId = fun.getOperand(opd);
				if (opdId.isNull) return;

				// intervals[opd].addRange(block.from, op.id)
				IntervalId intId = fun.liveIntervals.intervalId(opdId);
				fun.liveIntervals.addRange(intId, cast(int)block.firstInstr, cast(int)opId);

				// live.add(opd)
				liveAdd(opdId);
			}

			// for each operation op of b in reverse order do
				// last instr, no return operand
				switch(block.exit.type) with(IrJump.Type) {
					case ret1, branch: eachArg(block.exit.value, block.lastInstr); break;
					default: break;
				}
			foreach_reverse(blk_i, ref instr; fun.instructions[block.firstInstr..block.lastInstr+1])
			{
				size_t index = block.firstInstr + blk_i;
				// for each output operand opd of op do
				if (instr.returnsValue)
				{
					// intervals[opd].setFrom(op.id)
					fun.liveIntervals.addDefinition(instr.result, instr.type, cast(int)index);
					// live.remove(opd)
					liveRemove(instr.result);
				}

				// for each input operand opd of op do
				foreach(IrRef opd; instr.args)
				{
					// intervals[opd].addRange(b.from, op.id)
					// live.add(opd)
					eachArg(opd, index);
				}

				switch(instr.op) with(IrOpcode)
				{
					case o_call:
						foreach (reg; fun.callingConvention.volatileRegs)
						{
							IntervalId intId = fun.liveIntervals.intervalId(reg);
							fun.liveIntervals.addRange(intId, cast(int)index, cast(int)index);
						}
						break;

					default:
						break;
				}
			}

			// for each phi function phi of b do
			foreach (ref IrRef phiRef; block.phis)
			{
				// live.remove(phi.output)
				liveRemove(fun.phis[phiRef.index].result);
			}

			// TODO
			// if b is loop header then
			//     loopEnd = last block of the loop starting at b
			//     for each opd in live do
			//         intervals[opd].addRange(b.from, loopEnd.to)

			// b.liveIn = live
			blockLiveIn(block.index)[] = liveBuckets;
		}
	}
}

///
struct FunctionLiveIntervals
{
	// invariant: all ranges of one interval are sorted by `from` and do not intersect
	Buffer!LiveRange ranges;
	LiveInterval[] intervals;
	size_t numFixedIntervals;

	this(this)
	{
		intervals = intervals.dup;
	}

	LiveInterval[] virtualIntervals() { return intervals[numFixedIntervals..$]; }
	LiveInterval[] physicalIntervals() { return intervals[0..numFixedIntervals]; }

	void initIntervals(size_t numIntervals, size_t reserveRanges, ref CompilationContext ctx) {
		numFixedIntervals = ctx.machineInfo.numIntegerRegisters;
		intervals.length = numFixedIntervals + numIntervals;
		foreach (i, ref it; physicalIntervals)
		{
			it.reg = ctx.machineInfo.regs[i];
			it.isFixed = true;
		}
		ranges.reserve(reserveRanges);
	}

	/// Set hint for register allocator
	void setStorageHint(IrOperandId opdId, StorageHint storageHint) {
		this[opdId].storageHint = storageHint;
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

	ref LiveInterval opIndex(IntervalId intId) { return intervals[intId]; }
	ref LiveInterval opIndex(IrOperandId opdId) { return intervals[numFixedIntervals + opdId]; }
	ref LiveInterval opIndex(NodeIndex rangeId) { return intervals[ranges[rangeId].intervalId]; }

	IntervalId intervalId(NodeIndex rangeId) { return ranges[rangeId].intervalId; }
	IntervalId intervalId(IrOperandId opdId) { return IntervalId(numFixedIntervals + opdId); }
	IntervalId intervalId(RegisterRef regRef) { return IntervalId(regRef.index); }
	IrOperandId operandId(NodeIndex rangeId) { return operandId(ranges[rangeId].intervalId); }
	IrOperandId operandId(IntervalId intId) { assert(intId >= numFixedIntervals); return IrOperandId(intId- numFixedIntervals); }
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
	// from is always == to block start
	// checks for interval being null
	void addRange(IntervalId interval, int from, int to)
	{
		if (interval.isNull) return;
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
	void addDefinition(IrOperandId opdId, IrValueType type, int from) {
		IntervalId interval = intervalId(opdId);
		NodeIndex cur = intervals[interval].first;
		intervals[interval].regClass = typeToRegClass(type);
		if (!cur.isNull) {
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
			intervals[range.intervalId].first = index;

		ranges.put(range);
	}

	void appendRange(IntervalId interval, LiveRange range)
	{
		NodeIndex last = intervals[interval].last;
		NodeIndex index = NodeIndex(ranges.length);

		if (last.isNull)
		{
			intervals[range.intervalId].first = index;
			intervals[range.intervalId].last = index;
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
		auto it = &intervals[range.intervalId];
		if (fromIndex == it.first) it.first = toIndex;
		if (fromIndex == it.last) it.last = toIndex;
	}

	// returns rangeId of the next range
	NodeIndex deleteRange(NodeIndex rangeIndex)
	{
		auto range = &ranges[rangeIndex];

		auto it = &intervals[range.intervalId];
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

	void dump(ref TextSink sink) {
		void dumpSub(LiveInterval[] intervals)
		{
			foreach (i, it; intervals) {
				NodeIndex cur = it.first;
				if (cur.isNull) {
					sink.putfln("% 3s: null", i);
					continue;
				}
				if (it.reg.isNull)
					sink.putf("% 3s [no reg]:", i);
				else
					sink.putf("% 3s [%s %2s]:", i, it.reg.regClass, it.reg);

				while (!cur.isNull)
				{
					auto r = &ranges[cur];
					sink.putf(" [%s; %s)", r.from, r.to);
					cur = r.nextIndex;
				}
				sink.putln;
			}
		}
		sink.putln("fixed intervals:");
		dumpSub(intervals[0..numFixedIntervals]);
		sink.putln("virtual intervals:");
		dumpSub(intervals[numFixedIntervals..$]);
	}
}

///
struct LiveInterval
{
	NodeIndex first;
	NodeIndex last;
	RegisterRef reg;
	RegClass regClass;
	bool isFixed;
	StorageHint storageHint;
}

struct IntervalId
{
	this(size_t idx) { index = cast(uint)idx; }
	uint index = uint.max;
	alias index this;
	enum NULL = IntervalId();
	bool isNull() { return index == uint.max; }
}

/// [from; to)
struct LiveRange
{
	int from;
	int to;
	IntervalId intervalId;
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
		if (to < other.from) return false;
		if (from > other.to) return false;
		return true;
	}

	bool intersectsWith(const LiveRange other) {
		if (to <= other.from) return false;
		if (from >= other.to) return false;
		return true;
	}
}
*/
//     ######   #######    ####      #     #        #          ###      ####
//     #     #  #         #    #     #     #        #         #   #    #    #
//     #     #  #        #          ###    #        #        #     #  #
//     ######   #####    #   ###    # #    #        #        #     #  #
//     #   #    #        #     #   #####   #        #        #     #  #
//     #    #   #         #    #   #   #   #        #         #   #    #    #
//     #     #  #######    #####  ##   ##  ######   ######     ###      ####
// -----------------------------------------------------------------------------


/// Does linear scan register allocation.
/// Uses live intervals produced by pass_live_intervals
///
/+
void pass_linear_scan(ref CompilationContext ctx) {
	LinearScan linearScan;
	linearScan.setup(&ctx);
	foreach (fun; ctx.mod.irModule.functions) {
		linearScan.assignHints(*fun, *fun.callingConvention);
		linearScan.scanFun(*fun);
		linearScan.resolve(*fun);
	}
}

/// Implementation of:
/// "Optimized Interval Splitting in a Linear Scan Register Allocator"
/// "Linear Scan Register Allocation on SSA Form"
struct LinearScan
{
	NodeIndex[] unhandledStorage;
	Buffer!NodeIndex active;
	Buffer!NodeIndex inactive;
	Buffer!NodeIndex handled;
	PhysRegisters physRegs;
	CompilationContext* ctx;
	FunctionLiveIntervals* live;

	void setup(CompilationContext* ctx)
	{
		this.ctx = ctx;
		physRegs.setup;
	}

	void assignHints(ref IrFunction fun, ref CallConv callConv)
	{
		// assign paramter registers and memory locations
		foreach(i, ref instr; fun.parameters)
		{
			callConv.setParamHint(fun, instr, i);
		}

		// assign register hint to return values according to call conv
		foreach (ref IrBasicBlock block; fun.basicBlocks.range)
		{
			if (block.exit.type == IrJump.Type.ret1)
			{
				callConv.setReturnHint(fun, block.exit.value);
			}
		}
	}

	/*
		LinearScan
		unhandled = list of intervals sorted by increasing start positions
		active = { }; inactive = { }; handled = { }

		while unhandled != { } do
			current = pick and remove first interval from unhandled
			position = start position of current

			// check for intervals in active that are handled or inactive
			for each interval it in active do
				if it ends before position then
					move it from active to handled
				else if it does not cover position then
					move it from active to inactive

			// check for intervals in inactive that are handled or active
			for each interval it in inactive do
				if it ends before position then
					move it from inactive to handled
				else if it covers position then
					move it from inactive to active

			// find a register for current
			TryAllocateFreeReg
			if allocation failed then AllocateBlockedReg

			if current has a register assigned then add current to active
	*/
	void scanFun(ref IrFunction fun)
	{
		import std.container.binaryheap;

		live = &fun.liveIntervals;
		scope(exit) live = null;

		int cmp(NodeIndex a, NodeIndex b) {
			return live.ranges[a].from > live.ranges[b].from;
		}

		// unhandled = list of intervals sorted by increasing start positions
		// active = { }; inactive = { }; handled = { }
		BinaryHeap!(NodeIndex[], cmp) unhandled;
		unhandled.assume(assumeSafeAppend(unhandledStorage), 0);
		active.clear;
		inactive.clear;
		handled.clear;

		unhandled.acquire(null);
		foreach (it; live.virtualIntervals)
			if (!it.first.isNull)
				unhandled.insert(it.first);
		//writefln("unhandled %s", unhandled);

		foreach (it; live.physicalIntervals)
			if (!it.first.isNull)
				inactive.put(it.first);

		// while unhandled != { } do
		while (!unhandled.empty)
		{
			// current = pick and remove first interval from unhandled
			NodeIndex currentId = unhandled.front;
			unhandled.removeFront;

			// position = start position of current
			int position = live.ranges[currentId].from;
			//writefln("current %s pos %s", live.ranges[currentId].intervalId, position);

			size_t index;
			// // check for intervals in active that are handled or inactive
			// for each interval it in active do
			while (index < active.length)
			{
				NodeIndex rangeId0 = active[index];
				NodeIndex rangeId = active[index] = live.advanceRangeId(rangeId0, position);
				LiveRange range = live.ranges[rangeId0];

				// if it ends before position then
				if (rangeId.isNull)
				{
					// move it from active to handled
					//writefln("move %s active -> handled", range.intervalId);
					//handled.put(rangeId);
					active.removeInPlace(index);
				}
				// else if it does not cover position then
				else if(!live.intervalCoversPosition(rangeId, position))
				{
					// move it from active to inactive
					//writefln("%s isLast %s less %s", rangeId, range.isLast, range.to < position);
					//writefln("move %s active -> inactive", range.intervalId);
					inactive.put(rangeId);
					active.removeInPlace(index);
				}
				else
					++index;
			}

			index = 0;
			// check for intervals in inactive that are handled or active
			// for each interval it in inactive do
			while (index < inactive.length)
			{
				NodeIndex rangeId = inactive[index];

				// if it ends before position then
				if (live.ranges[rangeId].isLast && live.ranges[rangeId].to < position)
				{
					// move it from inactive to handled
					//writefln("move %s inactive -> handled", live.ranges[rangeId].intervalId);
					//handled.put(rangeId);
					inactive.removeInPlace(index);
				}
				// 	else if it covers position then
				else if(live.intervalCoversPosition(rangeId, position))
				{
					// move it from inactive to active
					//writefln("move %s inactive -> active", live.ranges[rangeId].intervalId);
					active.put(rangeId);
					inactive.removeInPlace(index);
				}
				else
					++index;
			}

			// find a register for current
			bool success = tryAllocateFreeReg(fun, currentId);

			// if allocation failed then AllocateBlockedReg
			if (!success)
				allocateBlockedReg(fun, currentId, position);

			// if current has a register assigned then add current to active
			if (!(*live)[currentId].reg.isNull)
			{
				//writefln("move current %s -> active", live.ranges[currentId].intervalId);
				active.put(currentId);
			}
		}

		unhandledStorage = unhandled.release;
	}

	/*
	TRYALLOCATEFREEREG
		set freeUntilPos of all physical registers to maxInt

		for each interval it in active do
			freeUntilPos[it.reg] = 0
		for each interval it in inactive intersecting with current do
			freeUntilPos[it.reg] = next intersection of it with current

		reg = register with highest freeUntilPos
		if freeUntilPos[reg] = 0 then
			// no register available without spilling
			allocation failed
		else if current ends before freeUntilPos[reg] then
			// register available for the whole interval
			current.reg = reg
		else
			// register available for the first part of the interval
			current.reg = reg
			split current before freeUntilPos[reg]
	*/
	bool tryAllocateFreeReg(ref IrFunction fun, NodeIndex currentId)
	{
		// set freeUntilPos of all physical registers to maxInt
		physRegs.resetFreeUntilPos;

		LiveInterval* currentIt = &fun.liveIntervals[currentId];
		int currentEnd = live.ranges[currentIt.last].to;

		// for each interval it in active do
		//     freeUntilPos[it.reg] = 0
		foreach (rangeId; active.data)
		{
			auto it = fun.liveIntervals[rangeId];
			assert(!it.reg.isNull);
			physRegs[it.reg].freeUntilPos = 0;
		}

		// check inactive : TODO

		// reg = register with highest freeUntilPos
		int maxPos = 0;
		RegisterRef reg;

		// reg stored in hint
		RegisterRef hintReg = currentIt.storageHint.getRegHint;

		PhysRegister[] candidates = physRegs.getClassRegs(currentIt.regClass);
		foreach (i, ref PhysRegister r; candidates)
		{
			if (r.freeUntilPos > maxPos) {
				maxPos = r.freeUntilPos;
				reg = r.regRef;
			}
		}

		if (maxPos == 0)
		{
			return false;
		}
		else
		{
			if (!hintReg.isNull)
			{
				if (currentEnd < physRegs[hintReg].freeUntilPos)
					reg = hintReg;
			}

			if (currentEnd < maxPos)
			{
				currentIt.reg = reg;
				return true;
			}
			else
			{
				// split
				return false;
			}
		}
	}

	int nextUseAfter(ref IrFunction fun, IrOperandId opdId, int after)
	{
		ListInfo!NodeIndex usesList = fun.operands[opdId].usesList;
		int closest = int.max;
		for (NodeIndex cur = usesList.first; !cur.isNull; cur = fun.opdUses.nodes[cur].nextIndex)
		{
			IrRef userRef = fun.opdUses.nodes[cur].data;
			int pos = fun.linearPosition(userRef);
			if (pos > after && pos < closest)
			{
				closest = pos;
			}
		}
		return closest;
	}

	/*
	ALLOCATEBLOCKEDREG
		set nextUsePos of all physical registers to maxInt

		for each interval it in active do
			nextUsePos[it.reg] = next use of it after start of current
		for each interval it in inactive intersecting with current do
			nextUsePos[it.reg] = next use of it after start of current

		reg = register with highest nextUsePos
		if first usage of current is after nextUsePos[reg] then
			// all other intervals are used before current,
			// so it is best to spill current itself
			assign spill slot to current
			split current before its first use position that requires a register
		else
			// spill intervals that currently block reg
			current.reg = reg
			split active interval for reg at position
			split any inactive interval for reg at the end of its lifetime hole

		// make sure that current does not intersect with
		// the fixed interval for reg
		if current intersects with the fixed interval for reg then
			split current before this intersection
	*/
	void allocateBlockedReg(ref IrFunction fun, NodeIndex currentRangeId, int currentStart)
	{
		writefln("allocateBlockedReg of range %s, start %s", currentRangeId, currentStart);
		// set nextUsePos of all physical registers to maxInt
		physRegs.resetNextUsePos();

		// for each interval it in active do
		//     nextUsePos[it.reg] = next use of it after start of current
		foreach (rangeId; active.data)
		{
			LiveInterval* it = &fun.liveIntervals[rangeId];
			assert(!it.reg.isNull);
			if (it.isFixed)
			{
				physRegs[it.reg].nextUsePos = currentStart;
			}
			else
			{
				IrOperandId opdId = live.operandId(rangeId);
				physRegs[it.reg].nextUsePos = nextUseAfter(fun, opdId, currentStart);
			}
			writefln("nextUsePos of %s is %s", rangeId, physRegs[it.reg].nextUsePos);
		}

		// for each interval it in inactive intersecting with current do
		//     nextUsePos[it.reg] = next use of it after start of current
		foreach (rangeId; inactive.data)
		{
			LiveInterval* it = &fun.liveIntervals[rangeId];
			assert(!it.reg.isNull);
			if (it.isFixed)
			{
				int fistrIntersection = live.firstIntersection(currentRangeId, rangeId);
				physRegs[it.reg].nextUsePos = min(fistrIntersection, physRegs[it.reg].nextUsePos);
			}
			else
			{
				IrOperandId opdId = live.operandId(rangeId);
				physRegs[it.reg].nextUsePos = nextUseAfter(fun, opdId, currentStart);
			}
			writefln("nextUsePos of %s is %s", rangeId, physRegs[it.reg].nextUsePos);
		}

		// reg = register with highest nextUsePos
		LiveInterval* currentIt = &fun.liveIntervals[currentRangeId];
		PhysRegister[] regs = physRegs.getClassRegs(currentIt.regClass);
		int maxUsePos = 0;
		RegisterRef reg;
		foreach (i, ref PhysRegister r; regs)
		{
			if (r.nextUsePos > maxUsePos) {
				maxUsePos = r.nextUsePos;
				reg = r.regRef;
			}
		}

		ctx.unreachable;
		assert(false);
	}

	/*
	// Resolve
	for each control flow edge from predecessor to successor do
		for each interval it live at begin of successor do
			if it starts at begin of successor then
				phi = phi function defining it
				opd = phi.inputOf(predecessor)
				if opd is a constant then
					moveFrom = opd
				else
					moveFrom = location of intervals[opd] at end of predecessor
			else
				moveFrom = location of it at end of predecessor
			moveTo = location of it at begin of successor
			if moveFrom ≠ moveTo then
				mapping.add(moveFrom, moveTo)
		mapping.orderAndInsertMoves()
	*/
	void resolve(ref IrFunction fun)
	{

	}
}

RegClass typeToRegClass(IrValueType type)
{
	if (type == IrValueType.i1) return RegClass.flags;
	return RegClass.gpr;
}



struct PhysRegister
{
	RegisterRef regRef;
	int freeUntilPos;
	int nextUsePos;
}

struct PhysRegisters
{
	PhysRegister[] gpr;
	PhysRegister[] flags;

	ref PhysRegister opIndex(RegisterRef reg) {
		final switch(reg.regClass) {
			case RegClass.gpr: return gpr[reg];
			case RegClass.flags: return flags[reg];
		}
	}

	void setup()
	{
		gpr.length = 0;
		flags ~= PhysRegister(RegisterRef(0, 0, RegClass.flags));
		gpr ~= PhysRegister(RegisterRef(0, Register.AX, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(1, Register.CX, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(2, Register.DX, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(3, Register.R8, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(4, Register.R9, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(5, Register.R10, RegClass.gpr));
		gpr ~= PhysRegister(RegisterRef(6, Register.R11, RegClass.gpr));
	}

	void resetFreeUntilPos()
	{
		foreach (ref reg; chain(gpr, flags)) reg.freeUntilPos = int.max;
	}

	void resetNextUsePos()
	{
		foreach (ref reg; chain(gpr, flags)) reg.nextUsePos = int.max;
	}

	PhysRegister[] getClassRegs(RegClass cls)
	{
		final switch(cls) {
			case RegClass.gpr: return gpr;
			case RegClass.flags: return flags;
		}
	}
}

struct OperandLocation
{
	this(RegisterRef reg) {
		type = OpdLocType.physicalRegister;
		this.reg = reg;
	}
	static OperandLocation makeConstant(IrRef irRef) {
		assert(irRef.kind == IrValueKind.con);
		OperandLocation res;
		final switch(irRef.constKind)
		{
			case IrConstKind.literal: res.type = OpdLocType.constant; break;
			case IrConstKind.stackSlotId: res.type = OpdLocType.stackSlot; break;
		}

		res.irRef = irRef;
		return res;
	}

	bool isConstant() { return type == OpdLocType.constant; }

	OpdLocType type;
	union
	{
		IrRef irRef; // ref to instruction, phi or constant
		RegisterRef reg;
	}
}
enum OpdLocType : ubyte
{
	constant,
	virtualRegister,
	physicalRegister,
	memoryAddress,
	stackSlot,
}
MoveType calcMoveType(OpdLocType dst, OpdLocType src)
{
	final switch(dst) with(OpdLocType) {
		case constant: return MoveType.invalid;
		case virtualRegister: return MoveType.invalid;
		case physicalRegister:
			final switch(src) with(OpdLocType) {
				case constant: return MoveType.const_to_reg;
				case virtualRegister: return MoveType.invalid;
				case physicalRegister: return MoveType.reg_to_reg;
				case memoryAddress: return MoveType.mem_to_reg;
				case stackSlot: return MoveType.stack_to_reg;
			}
		case memoryAddress:
			final switch(src) with(OpdLocType) {
				case constant: return MoveType.const_to_mem;
				case virtualRegister: return MoveType.invalid;
				case physicalRegister: return MoveType.reg_to_mem;
				case memoryAddress: return MoveType.invalid;
				case stackSlot: return MoveType.invalid;
			}
		case stackSlot:
			final switch(src) with(OpdLocType) {
				case constant: return MoveType.const_to_stack;
				case virtualRegister: return MoveType.invalid;
				case physicalRegister: return MoveType.reg_to_stack;
				case memoryAddress: return MoveType.invalid;
				case stackSlot: return MoveType.invalid;
			}
	}
}

enum MoveType
{
	invalid,
	const_to_reg,
	const_to_stack,
	reg_to_reg,
	reg_to_stack,
	stack_to_reg,
	const_to_mem,
	reg_to_mem,
	mem_to_reg,
}

struct StorageHint
{
	union {
		RegisterRef reg;       // if hintType == register
		StackSlotId stackSlot; // if hintType == stack
	}
	RegisterRef getRegHint() {
		if (isSet && hintType == StorageHintType.register) return reg;
		return RegisterRef();
	}
	mixin(bitfields!(
		/// Value came from memory location. No need to spill.
		/// True for parameters that are passed through stack, not regs
		bool,            "defByMem",   1,
		bool,            "isSet",      1,
		bool,            "isParameter",1,
		StorageHintType, "hintType",   1,
		uint,            "",           4,
	));
}

enum StorageHintType : ubyte
{
	register,
	stack
}

+/

enum RegClass : ubyte
{
	gpr,
	flags
}

/// Is used in IrValue to indicate the register that stores given IrValue
struct RegisterRef
{
	this(size_t index, ubyte nativeReg, RegClass regClass) {
		this.index = cast(ubyte)index;
		this.nativeReg = nativeReg;
		this.regClass = regClass;
	}
	ubyte index = ubyte.max;
	ubyte nativeReg; // stores actual id of register (EAX, EDX etc)
	RegClass regClass;
	alias index this;
	bool isNull() { return index == ubyte.max; }
}

//                    #####   #######     #       ####   #    #
//                   #     #     #        #      #    #  #   #
//                   #           #       ###    #        #  #
//                    #####      #       # #    #        ###
//                         #     #      #####   #        #  #
//                   #     #     #      #   #    #    #  #   #
//                    #####      #     ##   ##    ####   #    #
// -----------------------------------------------------------------------------

/+
/// Arranges items on the stack according to calling convention
void pass_stack_layout(ref CompilationContext ctx) {
	IrModule* mod = &ctx.mod.irModule;
	foreach (IrFunction* func; mod.functions)
	{
		enum STACK_ITEM_SIZE = 8; // x86_64
		int numParams = cast(int)func.numParameters;

		auto layout = &func.stackLayout;
		layout.reservedBytes = layout.numLocals * STACK_ITEM_SIZE;
		//writefln("%s", layout.numLocals);
		//writefln("%s", layout.numParams);

		// ++        slot index
		// param2    1  rsp + 20     \
		// param1    0  rsp + 18     / numParams = 2
		// ret addr     rsp + 10
		// local1    2  rsp +  8     \
		// local2    3  rsp +  0     / numLocals = 2   <-- RSP
		// --

		//writefln("numSlots %s numLocals %s numParams %s", numSlots, numLocals, numParams);
		//writefln("layout %s", layout.reservedBytes);

		/*
		if (USE_FRAME_POINTER)
		{
			// ++        varIndex
			// param2    1              \
			// param1    0  rbp + 2     / numParams = 2
			// ret addr     rbp + 1
			// rbp      <-- rbp + 0
			// local1    2  rbp - 1     \
			// local2    3  rbp - 2     / numLocals = 2
			// --
			if (isParameter) // parameter
			{
				index = 2 + varIndex;
			}
			else // local variable
			{
				index = -(varIndex - numParams + 1);
			}
			baseReg = Register.BP;
		}*/

		int localIndex = 0;
		foreach (ref slot; layout.slots)
		{
			if (slot.isParameter)
			{
				slot.offset = (layout.numLocals + slot.paramIndex + 1) * STACK_ITEM_SIZE;
			}
			else
			{
				slot.offset = (layout.numLocals - localIndex - 1) * STACK_ITEM_SIZE;
				++localIndex;
			}
		}
	}
}

struct StackLayout
{
	int reservedBytes;
	int numParams() { return cast(uint)slots.length - numLocals; }
	int numLocals;
	StackSlot[] slots;

	this(this)
	{
		slots = slots.dup;
	}

	/// paramIndex == -1 for non-params
	IrRef addStackItem(ulong size, ulong alignment, bool isParameter, ushort paramIndex)
	{
		assert(size > 0);
		assert(alignment > 0);

		auto id = StackSlotId(cast(uint)(slots.length));
		auto slot = StackSlot(size, alignment, isParameter, paramIndex);

		if (!isParameter) ++numLocals;

		slots ~= slot;
		return IrRef(id);
	}
}

struct StackSlot
{
	ulong size;
	ulong alignment;
	bool isParameter;
	ushort paramIndex;
	ushort numUses;
	int offset;
	void addUser() { ++numUses; }
}

struct StackSlotId
{
	uint id = uint.max;
	alias id this;
	bool isNull() { return id == uint.max; }
}
+/

struct MachineInfo
{
	size_t numIntegerRegisters() { return regs.length; }
	RegisterRef[] regs;
}

__gshared MachineInfo mach_info_x86_64 = MachineInfo(
	/*[RegisterRef(0, Register.AX, RegClass.gpr),
	RegisterRef(1, Register.CX, RegClass.gpr),
	RegisterRef(2, Register.DX, RegClass.gpr),
	RegisterRef(3, Register.R8, RegClass.gpr),
	RegisterRef(4, Register.R9, RegClass.gpr),
	RegisterRef(5, Register.R10, RegClass.gpr),
	RegisterRef(6, Register.R11, RegClass.gpr)]*/
	);

/// Info needed for calling convention implementation
struct CallConv
{
	RegisterRef[] paramsInRegs;
	RegisterRef returnReg;
	RegisterRef[] volatileRegs;

	bool isParamOnStack(size_t parIndex) {
		return parIndex >= paramsInRegs.length;
	}
	/*
	void setParamHint(ref IrFunction fun, ref IrInstruction instr, size_t parIndex) {
		StorageHint hint;
		hint.isSet = true;
		if (parIndex < paramsInRegs.length)
		{
			hint.hintType = StorageHintType.register;
			hint.reg = paramsInRegs[parIndex];
		}
		else
		{
			hint.defByMem = true;
			hint.hintType = StorageHintType.stack;
			hint.stackSlot = StackSlotId(cast(uint)(parIndex - paramsInRegs.length));
		}
		hint.isParameter = true;
		fun.liveIntervals.setStorageHint(instr.result, hint);
	}
	void setReturnHint(ref IrFunction fun, IrIndex irRef) {
		StorageHint hint;
		hint.isSet = true;
		hint.reg = returnReg;
		fun.setStorageHint(irRef, hint);
	}*/
}

__gshared CallConv win64_call_conv = CallConv(
	/*[RegisterRef(1, Register.CX, RegClass.gpr), // indicies into PhysRegisters.gpr
	RegisterRef(2, Register.DX, RegClass.gpr),
	RegisterRef(3, Register.R8, RegClass.gpr),
	RegisterRef(4, Register.R9, RegClass.gpr)],
	RegisterRef(0, Register.AX, RegClass.gpr),  // return reg

	[RegisterRef(0, Register.AX, RegClass.gpr), // volatile regs
	RegisterRef(1, Register.CX, RegClass.gpr),
	RegisterRef(2, Register.DX, RegClass.gpr),
	RegisterRef(3, Register.R8, RegClass.gpr),
	RegisterRef(4, Register.R9, RegClass.gpr),
	RegisterRef(5, Register.R10, RegClass.gpr),
	RegisterRef(6, Register.R11, RegClass.gpr)]*/
);
/*
/// State of single machine register
struct RegisterState
{
	IrRef storedValue;
	mixin(bitfields!(
		/// True if value in register needs to be spilled before using register
		bool,        "isDirty",  1,
		uint,        "",         7,
	));
	bool hasValue() { return storedValue.isDefined; }
}

/// Stores info about all avaliable registers of the same class
/// Classes are (general purpose aka GPR, floating point FP, flags)
struct RegisterClass
{
	enum MAX_REGS = 16;
	// Index of register here corrensponds to machine-specific register index
	RegisterState[MAX_REGS] regStates;
	// Stores indicies of registers that hold n first parameters in calling convention
	RegisterRef[MAX_REGS] paramRegs;
	int numParamRegs;
	// Stores registers that can be used to load a value. Can contain value at the same time
	RegisterRef[MAX_REGS] freeRegsStack;
	int numFreeRegs;

	// Mark register `reg` as used by value `valueRef`
	void markAsUsed(RegisterRef reg, IrRef valueRef)
	{
		writefln("markAsUsed R%s %%%s", cast(Register)reg.index, valueRef.index);
		assert(reg.isDefined);
		assert(valueRef.isDefined);
		regStates[reg.index].storedValue = valueRef;
	}

	void markAsDirty(RegisterRef reg)
	{
		writefln("markAsDirty R%s", cast(Register)reg.index);
		regStates[reg.index].isDirty = true;
	}

	void markAsFree(RegisterRef reg)
	{
		writefln("markAsFree R%s dirty %s", cast(Register)reg.index, regStates[reg.index].isDirty);
		regStates[reg.index].isDirty = false;
		freeRegsStack[numFreeRegs] = reg;
		++numFreeRegs;
	}

	// Does linear search and removes register from stack of free registers
	void removeFromFree(RegisterRef removedReg)
	{
		writefln("removeFromFree R%s", cast(Register)removedReg.index);
		foreach(i, regRef; freeRegsStack[0..numFreeRegs])
		if (regRef == removedReg)
		{
			freeRegsStack[i] = freeRegsStack[numFreeRegs-1];
			--numFreeRegs;
			return;
		}
		assert(false, "Removed register is not found");
	}

	// Returns free register from stack, or undefined otherwise
	RegisterRef tryAlloc(IrRef valueRef)
	{
		if (numFreeRegs == 0) return RegisterRef(); // return undefined ref
		--numFreeRegs;
		RegisterRef regRef = freeRegsStack[numFreeRegs];
		writefln("tryAlloc R%s", cast(Register)regRef.index);
		//assert(!regStates[regRef.index].isDirty);
		regStates[regRef.index].storedValue = valueRef;
		return regRef;
	}

	// Searches all register for spill candidate
	RegisterRef spillAlloc()
	{
		if (numFreeRegs == 0) return RegisterRef(); // return undefined ref
		return freeRegsStack[--numFreeRegs];
	}

	void printFree() {
		writef("free %s regs : ", numFreeRegs);
		foreach(reg; freeRegsStack[0..numFreeRegs])
		{
			writef("%s ", cast(Register)reg.index);
		}
	}
}

struct MachineState
{
	void setup() {
		// Microsoft x64 calling convention
		// The registers RAX, RCX, RDX, R8, R9, R10, R11 are considered volatile (caller-saved).
		// The registers RBX, RBP, RDI, RSI, RSP, R12, R13, R14, and R15 are considered nonvolatile (callee-saved).
		gprRegs.numFreeRegs = 7;
		gprRegs.freeRegsStack[0..gprRegs.numFreeRegs] = [
			cast(RegisterRef)Register.AX,
			cast(RegisterRef)Register.CX,
			cast(RegisterRef)Register.DX,
			cast(RegisterRef)Register.R8,
			cast(RegisterRef)Register.R9,
			cast(RegisterRef)Register.R10,
			cast(RegisterRef)Register.R11,
		];

		gprRegs.numParamRegs = 4;
		gprRegs.paramRegs[0..gprRegs.numParamRegs] = [
			cast(RegisterRef)Register.CX,
			cast(RegisterRef)Register.DX,
			cast(RegisterRef)Register.R8,
			cast(RegisterRef)Register.R9,
		];

		flagsReg.numFreeRegs = 1;
		flagsReg.freeRegsStack[0] = RegisterRef(0);
	}

	/// General purpose registers. Store integers, pointers, bools
	RegisterClass gprRegs;
	RegisterClass flagsReg;
}
*/
