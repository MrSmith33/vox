/// Copyright: Copyright (c) 2018-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Liveness info storage
/// Phi functions use their arguments at the last instruction of corresponding basic block
module be.reg_alloc.liveness_info;

import std.array : empty;
import std.stdio : writeln, write, writef, writefln, stdout;
import std.string : format;

import all;


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

	void allocSets(CompilationContext* c, uint numBucketsPerBlock, uint numBlocks) {
		this.numBucketsPerBlock = numBucketsPerBlock;
		uint numBucketsTotal = numBucketsPerBlock * numBlocks;
		liveInBuckets = c.allocateTempArray!size_t(numBucketsTotal);
		liveInBuckets[] = 0;

		liveBuckets = c.allocateTempArray!size_t(numBucketsPerBlock);
		liveBuckets[] = 0;
	}

	size_t[] blockLiveInBuckets(IrIndex blockIndex)
	{
		size_t from = blockIndex.storageUintIndex * numBucketsPerBlock;
		size_t to = from + numBucketsPerBlock;
		return liveInBuckets[from..to];
	}
}


///
struct LivenessInfo
{
	LiveBitmap bitmap;

	// invariant: all ranges of one interval are sorted by `from` and do not intersect
	Array!LiveInterval intervals;
	uint numFixedIntervals;
	ubyte[NUM_REG_CLASSES] physRegOffsetByClass;
	/// instructionIndex -> seqIndex
	/// blockIndex -> seqIndex
	IrMirror!uint linearIndicies;
	// maps even indicies to instructions and basic blocks
	IrIndex[] evenIndexToIrIndex;
	uint maxLinearIndex; // max value stored in linearIndicies

	auto virtualIntervals() { return intervals[numFixedIntervals..$]; }
	auto physicalIntervals() { return intervals[0..numFixedIntervals]; }

	LiveInterval* vint(size_t virtSeqIndex) { return &intervals[numFixedIntervals+virtSeqIndex]; }
	LiveInterval* vint(IrIndex index) { return &intervals[numFixedIntervals + index.storageUintIndex]; }
	LiveInterval* pint(IrIndex physReg) { return &intervals[physRegOffsetByClass[physReg.physRegClass] + physReg.physRegIndex]; }
	LiveInterval* pint(PhysReg physReg) { return &intervals[physRegOffsetByClass[physReg.regClass] + physReg.regIndex]; }
	IntervalIndex vindex(size_t virtSeqIndex) { return IntervalIndex(numFixedIntervals+virtSeqIndex); }
	IntervalIndex pindex(size_t physSeqIndex) { return IntervalIndex(physSeqIndex); }

	void initStorage(CompilationContext* context, IrFunction* ir) {

		uint numVregs = ir.numVirtualRegisters;
		uint numBucketsPerBlock = cast(uint)divCeil(numVregs, size_t.sizeof * 8);
		bitmap.allocSets(context, numBucketsPerBlock, ir.numBasicBlocks);

		intervals.clear;
		numFixedIntervals = cast(uint)context.machineInfo.numRegisters;
		intervals.voidPut(context.arrayArena, numFixedIntervals + ir.numVirtualRegisters);

		ubyte physOffset = 0;
		foreach(ubyte regClass; 0..NUM_REG_CLASSES) {
			physRegOffsetByClass[regClass] = physOffset;
			auto numRegsPerClass = context.machineInfo.numRegsPerClass[regClass];
			foreach(regIndex; 0..numRegsPerClass) {
				LiveInterval* it = &intervals[physOffset+regIndex];
				*it = LiveInterval();
				it.reg = it.definition = IrIndex(regIndex, ArgType.QWORD, regClass);
				it.regClass = regClass;
			}
			physOffset += numRegsPerClass;
		}

		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; ir.virtualRegisters) {
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
			IrVirtualRegister* vreg = ir.getVirtReg(it.definition);
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
			IrVirtualRegister* vreg = ir.getVirtReg(it.definition);
			context.assertf(it.uses.length == 0, "non empty uses %s of %s; %s", it.uses.length, it.definition, it.uses);
			uint numUses = vreg.definition.isPhi ? vreg.users.length : vreg.users.length + 1; // with definition
			it.uses = it.uses.ptr[0..numUses];
		}
	}

	bool isBlockStartAt(IrFunction* ir, uint pos, ref IrIndex next) {
		if ((pos & 1) == 1) pos += 1;

		while (next.isDefined)
		{
			IrBasicBlock* block = ir.getBlock(next);
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
			while (true) {
				if (it.coversPosition(pos)) {
					break;
				} else {
					assert(!it.child.isNull, format("no children left to find reg, pos %s %s %s", pos, intIndex, *it));
					intIndex = it.child;
					it = &intervals[it.child];
				}
			}
			assert(it.reg.isDefined, format("%s %s null reg", intIndex, *it));
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
		// Make sure that we split on odd position
		if ((optimalPos & 1) == 0) optimalPos -= 1;

		// we want to take register from interval starting at the same position
		// happens for multiple phi functions in the same block
		// if first is allocated interval with bigger first use position
		if (optimalPos <= it.from) {
			//writefln("  splitBefore before start %s %s take register from interval starting at %s", parentInterval, before, it.from);
			return parentInterval;
		}

		LiveRangeIndex rightIndex = it.getRightRange(optimalPos);

		if (rightIndex.isNull) {
			//writefln("  splitBefore after end %s %s %s split at max pos", parentInterval, optimalPos, rightIndex);
			return parentInterval;
		}

		//writefln("  splitBefore1 %s %s %s", parentInterval, optimalPos, rightIndex);
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

		LiveInterval rightInterval = {
			ranges : newRanges,
			uses : it.splitUsesBefore(before),
			definition : it.definition,
			parent : parentInterval,
			child : it.child,
			regClass : it.regClass,
		};

		if (!it.child.isNull)
			intervals[it.child].parent = childInterval;
		it.child = childInterval;

		intervals.put(context.arrayArena, rightInterval);
		//writefln("    left %s %s", parentInterval, *it);
		//writefln("    right %s %s", childInterval, rightInterval);
		assert(it.ranges.length > 0);
		assert(rightInterval.ranges.length > 0);

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
