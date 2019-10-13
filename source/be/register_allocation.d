/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Linear scan register allocation pass
///
/// Notes:
/// Non-split current interval does not intersect with inactive intervals because of SSA form
///
module be.register_allocation;

import std.array : empty;
import std.string : format;
import std.stdio : writeln, write, writef, writefln, stdout;
import std.format : formattedWrite, FormatSpec;
import std.range : repeat;
import std.range : chain;
import std.bitmanip : bitfields;
import std.algorithm : min, max, sort, swap;

import all;

//version = RAPrint;
//version = RAPrint_resolve;

/// Does linear scan register allocation.
/// Uses live intervals produced by pass_live_intervals
void pass_linear_scan(LinearScan* linearScan)
{
	CompilationContext* c = linearScan.context;
	assert(!linearScan.fun.isExternal);

	linearScan.scanFun();

	if (c.printLirRA && c.printDumpOf(linearScan.fun)) {
		IrFunction* lirData = c.getAst!IrFunction(linearScan.fun.backendData.lirData);
		dumpFunction(c, lirData);
		linearScan.livePtr.dump(c, lirData);
	}
}

struct RegisterState
{
	IrIndex index;
	IntervalIndex activeInterval;
	bool isAllocatable;
	bool isUsed;
	bool isCalleeSaved;
	uint blockPos;
	uint usePos;

	void setUsePos(uint pos) {
		usePos = min(usePos, pos);
	}

	void setBlockPos(uint pos) {
		blockPos = min(blockPos, pos);
		usePos = min(usePos, pos);
	}
}

struct PhysRegisters
{
	RegisterState[] gpr;
	const(IrIndex)[] allocatableRegs;

	ref RegisterState opIndex(IrIndex reg) {
		assert(reg.isPhysReg, format("%s", reg));
		return gpr[reg.physRegIndex];
	}

	void setup(CompilationContext* c, FunctionDeclNode* fun, MachineInfo* machineInfo)
	{
		CallConv* callConv = fun.backendData.getCallConv(c);
		allocatableRegs = callConv.allocatableRegs;
		if (c.debugRegAlloc) allocatableRegs = allocatableRegs[0..5]; // only 5 regs are available for tests
		gpr.length = machineInfo.registers.length;

		foreach(i, ref RegisterState reg; gpr)
		{
			reg = RegisterState(machineInfo.registers[i].index);
		}

		foreach(i, reg; allocatableRegs)
		{
			opIndex(reg).isAllocatable = true;
		}

		foreach(i, reg; callConv.calleeSaved)
		{
			opIndex(reg).isCalleeSaved = true;
		}
	}

	void resetBlockPos()
	{
		foreach (ref reg; gpr) {
			if (reg.isAllocatable) {
				reg.usePos = MAX_USE_POS;
				reg.blockPos = MAX_USE_POS;
			} else {
				reg.usePos = 0;
				reg.blockPos = 0; // prevent allocation
			}
		}
	}

	void resetUsePos()
	{
		foreach (ref reg; gpr) {
			if (reg.isAllocatable)
				reg.usePos = MAX_USE_POS;
			else
				reg.usePos = 0; // prevent allocation
		}
	}

	void markAsUsed(IrIndex reg)
	{
		assert(gpr[reg.physRegIndex].isAllocatable);
		gpr[reg.physRegIndex].isUsed = true;
	}
}

// data shared between all split children of interval
// identified by vreg index
struct VregState
{
	IrIndex spillSlot;
}

/// Implementation of:
/// "Optimized Interval Splitting in a Linear Scan Register Allocator"
/// "Linear Scan Register Allocation on SSA Form"
struct LinearScan
{
	IntervalIndex[] unhandledStorage; // TODO: remove GC storage here
	VregState[] vregState;
	Array!IntervalIndex activeVirtual;
	Array!IntervalIndex activeFixed;
	Array!IntervalIndex inactiveVirtual;
	Array!IntervalIndex inactiveFixed;
	// List of handled split intervals that begin in the middle of basic block
	// Intervals are with increasing start position
	Array!IntervalIndex pendingMoveSplits;
	PhysRegisters physRegs;
	CompilationContext* context;
	IrBuilder* builder;
	FunctionDeclNode* fun;

	LivenessInfo* livePtr;
	ref LivenessInfo live() { return *livePtr; }
	IrFunction* lir;

	void freeMem() {
		activeVirtual.free(context.arrayArena);
		activeFixed.free(context.arrayArena);
		inactiveVirtual.free(context.arrayArena);
		inactiveFixed.free(context.arrayArena);
		pendingMoveSplits.free(context.arrayArena);
	}

	void checkActiveImpl(uint position, ref Array!IntervalIndex active, ref Array!IntervalIndex inactive) {
		size_t index;
		// // check for intervals in active that are handled or inactive
		// for each interval it in active do
		while (index < active.length)
		{
			IntervalIndex activeId = active[index];
			LiveInterval* activeInt = &live.intervals[activeId];

			// already spilled
			if (activeInt.reg.isStackSlot)
			{
				version(RAPrint) writefln("  move %s active -> handled, spilled to %s", activeId, activeInt.reg);
				active.removeInPlace(index);
				continue;
			}

			LiveRangeIndex rangeId = activeInt.getRightRangeInclusive(position);

			// if it ends before position then
			if (rangeId.isNull)
			{
				// move it from active to handled
				version(RAPrint) writefln("  move %s active -> handled", activeId);
				active.removeInPlace(index);
			}
			// else if it does not cover position then
			else if(!activeInt.ranges[rangeId].containsInclusive(position))
			{
				// move it from active to inactive
				version(RAPrint) writefln("  move %s active -> inactive", activeId);
				active.removeInPlace(index);
				inactive.put(context.arrayArena, activeId);
			}
			else
			{
				++index;
			}
		}
	}

	void checkActive(uint position) {
		checkActiveImpl(position, activeFixed, inactiveFixed);
		checkActiveImpl(position, activeVirtual, inactiveVirtual);
	}

	void checkInactiveImpl(uint position, ref Array!IntervalIndex active, ref Array!IntervalIndex inactive) {
		size_t index = 0;
		// check for intervals in inactive that are handled or active
		// for each interval it in inactive do
		while (index < inactive.length)
		{
			IntervalIndex inactiveId = inactive[index];
			LiveInterval* inactiveInt = &live.intervals[inactiveId];
			LiveRangeIndex rangeId = inactiveInt.getRightRangeInclusive(position);

			// if it ends before position then
			if (rangeId.isNull)
			{
				// move it from inactive to handled
				version(RAPrint) writefln("  move %s inactive -> handled", inactiveId);
				inactive.removeInPlace(index);
			}
			// 	else if it covers position then
			else if(inactiveInt.ranges[rangeId].containsInclusive(position))
			{
				// move it from inactive to active
				version(RAPrint) writefln("  move %s inactive -> active", inactiveId);
				active.put(context.arrayArena, inactiveId);
				inactive.removeInPlace(index);
			}
			else
				++index;
		}
	}

	void checkInactive(uint position) {
		checkInactiveImpl(position, activeFixed, inactiveFixed);
		checkInactiveImpl(position, activeVirtual, inactiveVirtual);
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
	void scanFun()
	{
		import std.container.binaryheap;
		lir = context.getAst!IrFunction(fun.backendData.lirData);
		physRegs.setup(context, fun, context.machineInfo);
		vregState = context.allocateTempArray!VregState(lir.numVirtualRegisters);

		//writefln("\nstart scan of %s", context.idString(lir.backendData.name));
		scope(exit) {
			lir = null;
		}

		int cmp(IntervalIndex a, IntervalIndex b) {
			return live.intervals[a].from > live.intervals[b].from;
		}

		// unhandled = list of intervals sorted by increasing start positions
		// active = { }; inactive = { }; handled = { }
		BinaryHeap!(IntervalIndex[], cmp) unhandled;
		unhandled.assume(assumeSafeAppend(unhandledStorage), 0);
		activeVirtual.clear;
		activeFixed.clear;
		inactiveVirtual.clear;
		inactiveFixed.clear;
		pendingMoveSplits.clear;

		unhandled.acquire(null);
		foreach (ref LiveInterval it; live.virtualIntervals)
			if (!it.ranges.empty)
				unhandled.insert(live.indexOf(&it));

		foreach (ref LiveInterval it; live.physicalIntervals)
			if (!it.ranges.empty)
				inactiveFixed.put(context.arrayArena, live.indexOf(&it));

		IrIndex currentBlock = lir.entryBasicBlock;

		// while unhandled != { } do
		while (!unhandled.empty)
		{
			// current = pick and remove first interval from unhandled
			IntervalIndex currentIndex = unhandled.front;
			LiveInterval* currentInterval = &live.intervals[currentIndex];
			unhandled.removeFront;

			// position = start position of current
			uint position = currentInterval.from;
			version(RAPrint) writefln("current %s %s pos %s firstUse %s", currentIndex, *currentInterval, position, currentInterval.firstUse);

			checkActive(position);
			checkInactive(position);

			if (currentInterval.isSplitChild)
			{
				version(RAPrint) writefln("  current is split child");
				if (!live.isBlockStartAt(lir, position, currentBlock))
				{
					version(RAPrint) writefln("  current is the middle of the block %s", currentBlock);
					pendingMoveSplits.put(context.arrayArena, currentIndex);
				}

				// skip allocation if this interval has spill slot assigned
				if (currentInterval.reg.isStackSlot) {
					version(RAPrint) writefln("  current is spilled to %s. skip allocation", currentInterval.reg);
					continue;
				}
			}

			// find a register for current
			bool success = tryAllocateFreeReg(currentIndex, unhandled);

			// if allocation failed then AllocateBlockedReg
			if (!success) {
				allocateBlockedReg(fun, currentIndex, position, unhandled);
			}

			currentInterval = &live.intervals[currentIndex]; // intervals may have reallocated

			version(RAPrint) writefln("  alloc current %s %s %s to %s", currentIndex, currentInterval, *currentInterval, IrIndexDump(currentInterval.reg, context, lir));

			// if current has a register assigned then add current to active
			if (currentInterval.reg.isPhysReg)
			{
				// don't add stack slot assigned interval to active
				version(RAPrint) writefln("  move current %s %s -> active", currentIndex, *currentInterval);
				activeVirtual.put(context.arrayArena, currentIndex);
			}

			version(RAPrint) writeln;
		}

		//live.dump(context, lir);

		MoveSolver moveSolver = MoveSolver(context);
		moveSolver.setup(fun);
		fixInstructionArgs(fun);
		// this pass inserts moves for spills and loads
		resolveInsertSplitMoves(moveSolver);
		// this pass inserts moves to resolve two-operand form,
		// so it needs to be after `resolveInsertSplitMoves`
		fixInstructionResults(fun);
		resolveControlFlow(moveSolver);
		genSaveCalleeSavedRegs(fun.backendData.stackLayout);

		moveSolver.release();
		lir.removeAllPhis;

		if (context.validateIr) validateIrFunction(context, lir);

		unhandledStorage = unhandled.release;

		//writefln("finished scan of %s\n", context.idString(lir.backendData.name));
	}

	/*
	TRYALLOCATEFREEREG
		set freeUntilPos of all physical registers to maxInt

		for each interval it in active do
			freeUntilPos[it.reg] = 0
		for each interval it in inactive intersecting with current do
			freeUntilPos[it.reg] = next intersection of it with current
		// clarification: if current intersects active and inactive, freeUntilPos is 0

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
	bool tryAllocateFreeReg(T)(IntervalIndex currentId, ref T unhandled)
	{
		// set usePos of all physical registers to maxInt
		physRegs.resetUsePos;

		LiveInterval* currentIt = &live.intervals[currentId];
		uint currentEnd = currentIt.to;

		static struct FreeUntilPos {
			uint num;
			void toString(scope void delegate(const(char)[]) sink) {
				if (num == MAX_USE_POS) formattedWrite(sink, "max");
				else formattedWrite(sink, "%s", num);
			}
		}

		// for each interval it in active do
		//     usePos[it.reg] = 0
		version(RAPrint) writeln("  active:");
		foreach (IntervalIndex activeId; activeFixed) {
			LiveInterval* it = &live.intervals[activeId];
			physRegs[it.reg].usePos = 0;
			version(RAPrint) writefln("   intp %s %s reg %s (next use 0)", activeId, IrIndexDump(it.definition, context, lir), IrIndexDump(it.reg, context, lir));
		}
		foreach (IntervalIndex activeId; activeVirtual) {
			LiveInterval* it = &live.intervals[activeId];
			physRegs[it.reg].usePos = 0;
			version(RAPrint) writefln("   intv %s %s %s reg %s (next use 0)", activeId, it, *it, IrIndexDump(it.reg, context, lir));
		}

		// for each interval it in inactive intersecting with current do
		//   usePos[it.reg] = next intersection of it with current
		version(RAPrint) writeln("  inactive:");
		foreach (IntervalIndex inactiveId; inactiveFixed) {
			LiveInterval* it = &live.intervals[inactiveId];
			version(RAPrint) writef("   intp %s %s (next use %s)", IrIndexDump(it.reg, context, lir), IrIndexDump(it.definition, context, lir), FreeUntilPos(physRegs[it.reg].usePos));

			// if current intersects both active and inactive, usePos stays 0
			if (physRegs[it.reg].usePos == 0) { version(RAPrint) writeln;
				continue;
			}
			// in case there is no intersection will return MAX_USE_POS (noop)
			uint inactiveIntersection = firstIntersection(currentIt, it);

			// Register may be already occupied by active or inactive interval, so preserve it's use pos
			physRegs[it.reg].usePos = min(physRegs[it.reg].usePos, inactiveIntersection);
			version(RAPrint) writefln(":=%s", FreeUntilPos(inactiveIntersection));
		}

		// We only need to check inactive intervals when current interval was split
		// because otherwise SSA form is intact, and there may not be intersections with inactive intervals
		if (currentIt.isSplitChild)
		{
			foreach (IntervalIndex inactiveId; inactiveVirtual) {
				LiveInterval* it = &live.intervals[inactiveId];
				version(RAPrint) writef("   intv %s %s (next use %s)", IrIndexDump(it.reg, context, lir), it.definition, FreeUntilPos(physRegs[it.reg].usePos));

				// if current intersects both active and inactive, usePos stays 0
				if (physRegs[it.reg].usePos == 0) { version(RAPrint) writeln;
					continue;
				}
				// in case there is no intersection will return MAX_USE_POS (noop)
				uint inactiveIntersection = firstIntersection(currentIt, it);
				// Register may be already occupied by active or inactive interval, so preserve it's use pos
				physRegs[it.reg].usePos = min(physRegs[it.reg].usePos, inactiveIntersection);
				version(RAPrint) writefln(":=%s", FreeUntilPos(inactiveIntersection));
			}
		}

		version(RAPrint) writefln("  Try alloc free reg for %s %s", *currentIt, currentId);

		// reg = register with highest usePos
		uint maxPos = 0;
		IrIndex reg;

		const IrIndex[] candidates = physRegs.allocatableRegs;
		version(RAPrint) write("  candidates:");
		foreach (i, IrIndex r; candidates)
		{
			version(RAPrint) writef(" %s:%s", IrIndexDump(r, context, lir), FreeUntilPos(physRegs[r].usePos));

			if (physRegs[r].usePos > maxPos) {
				maxPos = physRegs[r].usePos;
				reg = r;
			}
		}
		version(RAPrint) writeln;

		// reg stored in hint
		IrIndex hintReg = currentIt.storageHint;

		if (maxPos == 0)
		{
			// no register available without spilling
			version(RAPrint) writeln("    no register available without spilling");
			// special case
			// if current is interval without uses we can just spill the whole interval
			// this happens when interval is used at the beginning of the loop, then split in the middle
			// the tail part then contains no uses
			if (currentIt.uses.empty)
			{
				assignSpillSlot(fun.backendData.stackLayout, currentIt);
				version(RAPrint) writeln("    spill whole interval because of no uses");
				return true;
			}
			return false;
		} else {
			version(RAPrint) writefln("    hint is %s", IrIndexDump(hintReg, context, lir));
			if (hintReg.isDefined) {
				if (currentEnd < physRegs[hintReg].usePos) {
					// register available for the whole interval
					currentIt.reg = hintReg;
					currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(context, currentIt.definition), context);
					physRegs.markAsUsed(hintReg);
					version(RAPrint) writefln("    alloc hint %s", IrIndexDump(hintReg, context, lir));
					return true;
				} else {
					version(RAPrint) writefln("    hint %s is not available for the whole interval", IrIndexDump(hintReg, context, lir));
					// use hint reg with spill if it is one of max use regs
					if (physRegs[hintReg].usePos == maxPos) reg = hintReg;
				}
			}

			if (currentEnd < maxPos) {
				// register available for the whole interval
				currentIt.reg = reg;
				currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(context, currentIt.definition), context);
				physRegs.markAsUsed(reg);
				version(RAPrint) writefln("    alloc %s", IrIndexDump(reg, context, lir));
				return true;
			} else {
				// split
				version(RAPrint) writefln("    alloc %s + split at %s", IrIndexDump(reg, context, lir), maxPos);
				splitBefore(currentId, maxPos, unhandled);
				currentIt = &live.intervals[currentId]; // intervals may have reallocated

				currentIt.reg = reg;
				currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(context, currentIt.definition), context);
				physRegs.markAsUsed(reg);
				return true;
			}
		}
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
	// Returns new interval
	void allocateBlockedReg(T)(FunctionDeclNode* fun, IntervalIndex currentId, uint currentStart, ref T unhandled)
	{
		physRegs.resetBlockPos;

		LiveInterval* currentIt = &live.intervals[currentId];
		assert(currentIt.ranges.length);
		version(RAPrint) writefln("allocateBlockedReg of int %s start %s", currentId, currentStart);

		foreach (IntervalIndex activeId; activeVirtual) {
			LiveInterval* it = &live.intervals[activeId];
			physRegs[it.reg].setUsePos = it.nextUseAfter(currentStart);
			physRegs[it.reg].activeInterval = activeId;
			version(RAPrint) writefln("active virt usePos of %s %s use %s block %s", activeId, *it, physRegs[it.reg].usePos, physRegs[it.reg].blockPos);
		}
		// We only need to check inactive intervals when current interval was split
		// because otherwise SSA form is intact, and there may not be intersections with inactive intervals
		if (currentIt.isSplitChild) {
			foreach (inactiveId; inactiveVirtual) {
				LiveInterval* it = &live.intervals[inactiveId];
				physRegs[it.reg].setUsePos = it.nextUseAfter(currentStart);
				version(RAPrint) writefln("inactive virt usePos of %s %s use %s block %s", inactiveId, *it, physRegs[it.reg].usePos, physRegs[it.reg].blockPos);
			}
		}
		foreach (IntervalIndex activeId; activeFixed) {
			LiveInterval* it = &live.intervals[activeId];
			physRegs[it.reg].setBlockPos = 0;
			physRegs[it.reg].activeInterval = IntervalIndex.NULL;
			version(RAPrint) writefln("active fixed usePos of %s %s use %s block %s", activeId, *it, physRegs[it.reg].usePos, physRegs[it.reg].blockPos);
		}
		foreach (inactiveId; inactiveFixed) {
			LiveInterval* it = &live.intervals[inactiveId];
			uint intersection = firstIntersection(currentIt, it);
			if (intersection != MAX_USE_POS)
			{
				physRegs[it.reg].setBlockPos = intersection;
			}
			version(RAPrint) writefln("inactive fixed usePos of %s %s use %s block %s", inactiveId, *it, physRegs[it.reg].usePos, physRegs[it.reg].blockPos);
		}

		// reg = register with highest usePos
		uint maxPos = 0;
		IrIndex reg;
		foreach (i, IrIndex r; physRegs.allocatableRegs) {
 			if (physRegs[r].usePos > maxPos) {
 				// if register was only available for the start of interval
 				// then all uses may have moved into split child
				maxPos = physRegs[r].usePos;
				reg = r;
			}
		}
		version(RAPrint) writefln("    candidate %s usePos %s", IrIndexDump(reg, context, lir), maxPos);

		uint firstUse = currentIt.firstUse;

		if (maxPos < firstUse)
		{
			version(RAPrint) writefln("  spill current maxUse %s firstUse %s", maxPos, firstUse);
			// all other intervals are used before current, so it is best to spill current itself
			assignSpillSlot(fun.backendData.stackLayout, currentIt);
			// split current before its first use position that requires a register
			splitBefore(currentId, firstUse, unhandled);
			currentIt = &live.intervals[currentId]; // intervals may have reallocated
			version(RAPrint) writefln("    spill current %s", currentId);
		}
		else
		{
			// spill intervals that currently block reg
			currentIt.reg = reg;
			currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(context, currentIt.definition), context);
			physRegs.markAsUsed(reg);

			//	split active interval for reg at position
			void splitActiveOrInactive(IntervalIndex index)
			{
				LiveInterval* activeIt = &live.intervals[index];
				// split before current start. Add to unhandled so we get split move registered
				IntervalIndex newInterval = splitBefore(index, currentStart, unhandled);

				LiveInterval* splitActiveIt = &live.intervals[newInterval];
				assignSpillSlot(fun.backendData.stackLayout, splitActiveIt);

				// if there is a use position in spilled interval, split before use
				uint nextActiveUse = splitActiveIt.nextUseAfter(currentStart);
				if (nextActiveUse != MAX_USE_POS)
				{
					splitBefore(newInterval, nextActiveUse, unhandled);
					version(RAPrint) writefln("      split before use %s", newInterval);
				}
			}

			IntervalIndex activeIndex = physRegs[reg].activeInterval;
			context.assertf(!activeIndex.isNull, "%s", IrIndexDump(reg, context, lir)); // null means that it is fixed interval. But we filter out fixed interval above
			splitActiveOrInactive(activeIndex);
			version(RAPrint) writefln("    split active interval %s", activeIndex);

			//	split any inactive interval for reg at the end of its lifetime hole
			foreach (inactiveId; inactiveVirtual)
			{
				LiveInterval* it = &live.intervals[inactiveId];
				if (sameIndexOrPhysReg(it.reg, reg))
				{
					splitActiveOrInactive(inactiveId);
					version(RAPrint) writefln("    split inactive interval %s", inactiveId);
				}
			}

			if (currentIt.exclusivePosition(physRegs[reg].blockPos))
			{
				splitBefore(currentId, physRegs[reg].blockPos, unhandled);
			}

			version(RAPrint) writefln("    spill currently active interval %s before %s for %s",
				activeIndex, currentStart, IrIndexDump(reg, context, lir));
		}
	}

	// auto adds new interval to unhandled
	IntervalIndex splitBefore(T)(IntervalIndex it, uint before, ref T unhandled)
	{
		IntervalIndex newInterval = live.splitBefore(context, lir, it, before);
		if (newInterval != it) unhandled.insert(newInterval);
		return newInterval;
	}

	IntervalIndex splitBefore(T)(IntervalIndex it, uint before, LiveRangeIndex rangeIndex, ref T unhandled)
	{
		IntervalIndex newInterval = live.splitBefore(context, it, before, rangeIndex);
		if (newInterval != it) unhandled.insert(newInterval);
		return newInterval;
	}

	void assignSpillSlot(ref StackLayout stackLayout, LiveInterval* it)
	{
		assert(it.definition.isVirtReg);
		VregState* state = &vregState[it.definition.storageUintIndex];
		if (state.spillSlot.isDefined)
		{
			// use cached slot
			it.reg = state.spillSlot;
			version(RAPrint) writefln("assignSpillSlot %s use cached %s", it.definition, state.spillSlot);
		}
		else
		{
			IrIndex type = lir.getValueType(context, it.definition);
			IrIndex slot = stackLayout.addStackItem(context, type, StackSlotKind.local, 0);
			state.spillSlot = slot; // cache slot
			version(RAPrint) writefln("assignSpillSlot %s new slot %s", it.definition, slot);
			it.reg = slot;
		}
		// TODO: need to fix instructions to use stack slots after RA
		//       Not relevant yet
		//       We always provide register for all uses
		//       simple way would be to allow src arguments be stack slots
	}

	// Replaces all uses of virtual registers with physical registers or stack slots
	void fixInstructionArgs(FunctionDeclNode* fun)
	{
		// fix uses first, because we may copy arg to definition below
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; lir.virtualRegsiters)
		{
			foreach (size_t i, IrIndex userIndex; vreg.users.range(lir))
			{
				final switch (userIndex.kind) with(IrValueKind)
				{
					case none, listItem, virtualRegister, physicalRegister, constant, global, basicBlock, stackSlot, type, func, constantAggregate: assert(false);
					case instruction:
						uint pos = live.linearIndicies.instr(userIndex);
						IrIndex reg = live.getRegFor(vregIndex, pos);
						foreach (ref IrIndex arg; lir.get!IrInstrHeader(userIndex).args(lir))
							if (arg == vregIndex)
							{
								arg = reg;
							}
						break;
					case phi:
						foreach (size_t i, ref IrPhiArg phiArg; lir.get!IrPhi(userIndex).args(lir))
							if (phiArg.value == vregIndex)
							{
								IrIndex lastInstr = lir.getBlock(phiArg.basicBlock).lastInstr;
								uint pos = live.linearIndicies.instr(lastInstr);
								IrIndex reg = live.getRegFor(vregIndex, pos);
								//writefln("set %s arg reg %s -> %s at %s", userIndex, phiArg.value, reg, pos);
								phiArg.value = reg;
							}
						break;
					case variable: assert(false);
				}
			}
		}
	}

	void fixInstructionResults(FunctionDeclNode* fun)
	{
		// fix definitions
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; lir.virtualRegsiters)
		{
			switch(vreg.definition.kind) with(IrValueKind)
			{
				case instruction:
					uint pos = live.linearIndicies.instr(vreg.definition);
					IrIndex reg = live.getRegFor(vregIndex, pos);

					IrIndex instrIndex = vreg.definition;
					IrInstrHeader* instrHeader = &lir.get!IrInstrHeader(instrIndex);
					instrHeader.result(lir) = reg;

					fixTwoOperandForm(instrIndex, instrHeader);
					break;
				case phi:
					IrPhi* irPhi = &lir.get!IrPhi(vreg.definition);
					uint pos = live.linearIndicies.basicBlock(irPhi.blockIndex);
					IrIndex reg = live.getRegFor(vregIndex, pos);
					irPhi.result = reg;
					break;
				default: assert(false);
			}
		}
	}

	void fixTwoOperandForm(IrIndex instrIndex, IrInstrHeader* instrHeader)
	{
		InstrInfo instrInfo = context.machineInfo.instrInfo[instrHeader.op];

		// doesn't require two operant form
		if (!instrInfo.isResultInDst) return;

		// Insert mov for instructions requiring two-operand form (like x86 xor)
		if (instrHeader.numArgs == 2)
		{
			// Rewrite
			// input: r1 = r2 op r3
			// as
			// output: r1 = r2
			// output: r1 = r1 op r3
			//
			// if r2 != r3
			// {
			//     if r1 == r2 { // input: r1 = r1 op r3
			//         output: r1 = r1 op r3
			//     } else if r1 == r3 { // input: "r1 = r2 op r1"
			//         if (op.isCommutative) { // input: "r1 = r3 op r2"
			//             output: "r1 = r1 op r2"
			//         } else { // input: "r1 = r2 op r1"
			//             // error, we cannot swap r2 with r1 here to get r2 = r1 op r2
			//             // because r2 may be used later
			//             // this case is handled inside liveness analysis pass
			//         }
			//     } else { // input: "r1 = r2 op r3"
			//         output: "mov r1 r2"
			//         output: "r1 = op r1 r3"
			//     }
			// }
			// else // input: "r1 = op r2 r2"
			// {
			//     output: "mov r1 r2" if r1 != r2
			//     output: "r1 = op r1 r1"
			// }

			IrIndex r1 = instrHeader.result(lir);
			IrIndex r2 = instrHeader.arg(lir, 0);
			IrIndex r3 = instrHeader.arg(lir, 1);

			if ( !sameIndexOrPhysReg(r2, r3) ) // r2 != r3
			{
				if ( sameIndexOrPhysReg(r1, r2) ) // r1 == r2
				{
					// "r1 = op r1 r3", noop
				}
				else if ( sameIndexOrPhysReg(r1, r3) ) // r1 = op r2 r1
				{
					if (instrInfo.isCommutative) // r1 = op r1 r2
					{
						instrHeader.arg(lir, 0) = r1;
						instrHeader.arg(lir, 1) = r2;
					}
					else
					{
						//InstrInfo instrInfo2 = context.machineInfo.instrInfo[instrHeader.op];
						writefln("%s %s %s %s %s", cast(Amd64Opcode)instrHeader.op, r1, r2, r3, instrInfo.isCommutative);
						context.internal_error("Unhandled non-commutative instruction in RA, %s %s",
							context.idString(fun.backendData.name), instrIndex);
					}
				}
				else // r1 = op r2 r3; all different
				{
					// mov r1 r2
					ExtraInstrArgs extra = {addUsers : false, result : r1};
					InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, r2);
					builder.insertBeforeInstr(instrIndex, instr.instruction);
					// r1 = op r1 r3
					instrHeader.arg(lir, 0) = r1;
				}
			}
			else // r2 == r3
			{
				if ( !sameIndexOrPhysReg(r1, r2) )
				{
					// mov r1 r2
					ExtraInstrArgs extra = {addUsers : false, result : r1};
					InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, r2);
					builder.insertBeforeInstr(instrIndex, instr.instruction);
				}
				// r1 = op r1 r1
				instrHeader.arg(lir, 0) = r1;
			}

			// validation
			context.assertf(sameIndexOrPhysReg(instrHeader.result(lir), instrHeader.arg(lir, 0)),
				"two-operand form not ensured res(%s) != arg0(%s)", IrIndexDump(instrHeader.result(lir), context, lir), IrIndexDump(instrHeader.arg(lir, 0), context, lir));
		}
		else if (instrHeader.numArgs == 1)
		{
			// Rewrite
			// input: r1 = op r2
			// as
			// output: r1 = r2
			// output: r1 = op r1
			//
			IrIndex r1 = instrHeader.result(lir);
			IrIndex r2 = instrHeader.arg(lir, 0);

			if ( !sameIndexOrPhysReg(r1, r2) )
			{
				ExtraInstrArgs extra = {addUsers : false, result : r1};
				InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, r2);
				builder.insertBeforeInstr(instrIndex, instr.instruction);
			}
			instrHeader.arg(lir, 0) = r1;
		}
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
	// Insert moves between sub-intervals on control flow edges where different sub-intervals meet
	void resolveControlFlow(ref MoveSolver moveSolver)
	{
		version(RAPrint_resolve) writefln("resolve");

		foreach (IrIndex succIndex, ref IrBasicBlock succBlock; lir.blocks)
		{
			foreach (IrIndex predIndex; succBlock.predecessors.range(lir))
			{
				IrBasicBlock* predBlock = &lir.getBlock(predIndex);
				resolveEdge(moveSolver, predIndex, *predBlock, succIndex, succBlock);
			}
		}
	}

	void resolveEdge(
		ref MoveSolver moveSolver,
		IrIndex predIndex, ref IrBasicBlock predBlock,
		IrIndex succIndex, ref IrBasicBlock succBlock)
	{
		// those are already handled at split site
		// we have no linearIndicies for new blocks
		if (predBlock.replacesCriticalEdge || succBlock.replacesCriticalEdge) return;

		uint succPos = live.linearIndicies.basicBlock(succIndex);
		uint predPos = live.linearIndicies.instr(predBlock.lastInstr);

		moveSolver.reset();

		import std.bitmanip : BitArray;
		BitArray succLiveIn = live.bitmap.blockLiveInBits(succIndex, lir);

		//
		version(RAPrint_resolve) writefln("  edge %s -> %s", predIndex, succIndex);
		foreach(size_t index; succLiveIn.bitsSet)
		{
			LiveInterval* interval = live.vint(index); // always returns first part of split intervals
			if (!interval.isSplit) continue; // skip intervals that weren't split

			IrIndex vreg = interval.definition;
			version(RAPrint_resolve) writefln("    %s %s -> %s", vreg, succPos, predPos);
			IrIndex succLoc = live.getRegFor(vreg, succPos);
			IrIndex predLoc = live.getRegFor(vreg, predPos);
			if (!predLoc.isDefined) continue; // inteval doesn't exist in pred

			if (succLoc != predLoc)
			{
				version(RAPrint_resolve) writefln("    vreg %s, pred %s succ %s", vreg, IrIndexDump(predLoc, context, lir), IrIndexDump(succLoc, context, lir));
				IrArgSize argSize = getValueTypeArgSize(vreg, lir, context);
				moveSolver.addMove(predLoc, succLoc, argSize);
			}
		}

		foreach (IrIndex phiIndex, ref IrPhi phi; succBlock.phis(lir))
		{
			version(RAPrint_resolve) writef("    phi %s res %s", phiIndex, IrIndexDump(phi.result, context, lir));
			foreach (size_t arg_i, ref IrPhiArg arg; phi.args(lir))
			{
				if (arg.basicBlock == predIndex)
				{
					IrIndex moveFrom = arg.value;
					IrIndex moveTo = phi.result;

					version(RAPrint_resolve) writefln(" arg %s %s", arg.basicBlock, IrIndexDump(arg.value, context, lir));

					IrArgSize argSize = getValueTypeArgSize(phi.result, lir, context);
					moveSolver.addMove(moveFrom, moveTo, argSize);
				}
			}
		}

		if (moveSolver.numWrittenNodes == 0) return;

		if (isCriticalEdge(predBlock, succBlock))
		{
			IrIndex movesTarget = splitCriticalEdge(predIndex, predBlock, succIndex, succBlock);
			// add to the back
			moveSolver.placeMovesBeforeInstr(builder, lir.getBlock(movesTarget).lastInstr);
		}
		else if (predBlock.successors.length > 1)
		{
			IrIndex movesTarget = succIndex;
			// add to the front
			moveSolver.placeMovesBeforeInstr(builder, lir.getBlock(movesTarget).firstInstr);
		}
		else
		{
			context.assertf(predBlock.successors.length == 1,
				"predBlock.successors.length %s", predBlock.successors.length);
			IrIndex movesTarget = predIndex;
			// add to the back
			moveSolver.placeMovesBeforeInstr(builder, lir.getBlock(movesTarget).lastInstr);
		}
	}

	/// Critical edge is edge between predecessor and successor
	/// where predecessor has more that 1 successor and
	/// successor has more than 1 predecessor
	/// Splitting of this edge is done by inserting new basic block on the edge
	/// This new block is then returned
	/// successor list of predecessor is updated as well as predecessor list of successor
	/// Phi functions are not updated
	IrIndex splitCriticalEdge(
		IrIndex predIndex, ref IrBasicBlock predBlock,
		IrIndex succIndex, ref IrBasicBlock succBlock)
	{
		// place block right after precessor to get better codegen
		// TODO: may need to invert branch conditions for that
		IrIndex newBlock = builder.appendBasicBlockSlot;
		linkBlockAfter(lir, newBlock, predIndex);

		version(RAPrint_resolve) writefln("Split critical edge %s -> %s with %s", predIndex, succIndex, newBlock);
		foreach (ref IrIndex succ; predBlock.successors.range(lir)) {
			if (succ == succIndex) {
				succ = newBlock;
				break;
			}
		}
		foreach (ref IrIndex pred; succBlock.predecessors.range(lir)) {
			if (pred == predIndex) {
				pred = newBlock;
				break;
			}
		}
		lir.getBlock(newBlock).predecessors.append(builder, predIndex);
		lir.getBlock(newBlock).successors.append(builder, succIndex);
		lir.getBlock(newBlock).isSealed = true;
		builder.emitInstr!LirAmd64Instr_jmp(newBlock);
		lir.getBlock(newBlock).isFinished = true;
		lir.getBlock(newBlock).replacesCriticalEdge = true;
		return newBlock;
	}

	// Insert moves between sub-intervals in the middle of basic blocks
	void resolveInsertSplitMoves(ref MoveSolver moveSolver)
	{
		moveSolver.reset();
		uint prevPos = 0;
		version(RAPrint) writefln("Fix splits");

		void insertPrevMoves() {
			if (prevPos == 0) return;
			if (moveSolver.numWrittenNodes == 0) return;

			IrIndex insertBeforeInstr = live.evenIndexToIrIndex[prevPos/2];
			context.assertf(insertBeforeInstr.isInstruction, "%s", insertBeforeInstr);
			version(RAPrint) writefln("   insert %s moves before %s at %s", moveSolver.numWrittenNodes, insertBeforeInstr, prevPos);
			moveSolver.placeMovesBeforeInstr(builder, insertBeforeInstr);
			moveSolver.reset();
		}

		//IrIndex movesTarget = predIndex;
		foreach (IntervalIndex id; pendingMoveSplits)
		{
			LiveInterval* it = &live.intervals[id]; // always returns first part of split intervals
			uint splitPos = it.from;
			// insert all moves on prev position
			if (prevPos != splitPos) insertPrevMoves;

			LiveInterval* parentIt = &live.intervals[it.parent];
			version(RAPrint) writefln("  %s:%s split at %s, parent %s at %s", id, it.definition, it.from, it.parent, parentIt.to);
			context.assertf(prevPos <= it.from, "Wrong order");
			IrIndex moveFrom = parentIt.reg;
			IrIndex moveTo = it.reg;
			version(RAPrint) writefln("   from %s to %s", IrIndexDump(moveFrom, context, lir), IrIndexDump(moveTo, context, lir));
			if (moveFrom != moveTo)
			{

				IrIndex vreg = it.definition;
				IrArgSize argSize = getValueTypeArgSize(vreg, lir, context);
				moveSolver.addMove(moveFrom, moveTo, argSize);
				prevPos = splitPos;
			}
		}
		insertPrevMoves;
	}

	void genSaveCalleeSavedRegs(ref StackLayout stackLayout)
	{
		IrIndex entryBlock = lir.entryBasicBlock;
		IrIndex exitBlock = lir.exitBasicBlock;
		foreach(reg; physRegs.gpr)
		{
			if (reg.isCalleeSaved && reg.isUsed)
			{
				IrIndex slot = stackLayout.addStackItem(context, makeBasicTypeIndex(IrValueType.i64), StackSlotKind.local, 0);

				// save register
				ExtraInstrArgs extra1 = { argSize : IrArgSize.size64 };
				IrIndex instrStore = builder.emitInstr!LirAmd64Instr_store(extra1, slot, reg.index);
				builder.prependBlockInstr(entryBlock, instrStore);

				// restore register
				ExtraInstrArgs extra = { result : reg.index };
				InstrWithResult instrLoad = builder.emitInstr!LirAmd64Instr_load(extra, slot);
				builder.insertBeforeLastInstr(exitBlock, instrLoad.instruction);
			}
		}
	}
}


/// Reorders a set of moves, to produce correct behavior
/// Nodes can be in 3 states:
///   RO - value is only read. Those are not added to writtenNodes array.
///   RW - value is read 1 or more times and written 1 time. Indicies of these nodes are at the beginning of writtenNodes
///   WO - value is only written. Indicies are at the end of writtenNodes.
struct MoveSolver
{
	CompilationContext* context;

	ValueInfo[] stackSlots;
	ValueInfo[] registers;
	ValueInfo anyConstant;
	IrIndex* writtenNodesPtr;
	size_t savedBufLength;
	uint numWrittenNodes;
	uint numWriteOnlyValues;

	// allocate buffers
	// takes unique ownership of tempBuffer
	// Inits all register and stack slot infos
	// after each `placeMovesBeforeInstr` call they need to be in init state for reuse
	void setup(FunctionDeclNode* fun)
	{
		savedBufLength = context.tempBuffer.length;

		size_t numRegs = context.machineInfo.registers.length;
		registers = context.allocateTempArray!ValueInfo(cast(uint)numRegs);
		size_t numStackSlots = fun.backendData.stackLayout.slots.length;
		stackSlots = context.allocateTempArray!ValueInfo(cast(uint)numStackSlots);

		writtenNodesPtr = cast(IrIndex*)context.tempBuffer.nextPtr;
	}

	void reset()
	{
		assert(anyConstant == ValueInfo.init);
		assert(numWriteOnlyValues == 0);
		assert(numWrittenNodes == 0);
	}

	// releases unique ownership of tempBuffer
	void release()
	{
		context.tempBuffer.length = savedBufLength;

		savedBufLength = 0;
		stackSlots = null;
		registers = null;
		writtenNodesPtr = null;
		assert(anyConstant == ValueInfo.init);
		assert(numWriteOnlyValues == 0);
		assert(numWrittenNodes == 0);
	}

	ref ValueInfo getInfo(IrIndex index) {
		switch(index.kind) {
			case IrValueKind.constant: return anyConstant;
			case IrValueKind.stackSlot: return stackSlots[index.storageUintIndex];
			case IrValueKind.physicalRegister:  return registers[index.physRegIndex];
			default: context.internal_error("getInfo(%s)", index); assert(false);
		}
	}

	void addMove(IrIndex fromIndex, IrIndex toIndex, IrArgSize argSize)
	{
		assert(fromIndex.isDefined);
		assert(toIndex.isDefined);
		if (fromIndex == toIndex) return;

		ValueInfo* from = &getInfo(fromIndex);
		ValueInfo* to = &getInfo(toIndex);

		from.onRead(fromIndex);
		// no longer write only
		if (from.numReads == 1 && from.readFrom.isDefined) {
			wo_to_rw(from.arrayPos);
		}

		context.assertf(toIndex.isPhysReg || toIndex.isStackSlot, "toIndex is %s", toIndex.kind);
		context.assertf(!to.readFrom.isDefined, "Second write to %s detected", toIndex);

		to.onWrite(fromIndex, toIndex, argSize);
		to.arrayPos = numWrittenNodes;
		context.tempBuffer.put(toIndex.asUint);
		++numWrittenNodes;
		++numWriteOnlyValues;

		if (to.numReads > 0) {
			wo_to_rw(to.arrayPos);
		}
	}

	void print() {
		IrIndex[] writtenNodes = writtenNodesPtr[0..numWrittenNodes];
		foreach(IrIndex index; writtenNodes[0..$-numWriteOnlyValues])
			writef("(%s %s) ", index, getInfo(index));
		write("| ");
		foreach(IrIndex index; writtenNodes[$-numWriteOnlyValues..$])
			writef("(%s %s) ", index, getInfo(index));
		writeln;
	}

	void wo_to_rw(uint arrayPos)
	{
		IrIndex[] writtenNodes = writtenNodesPtr[0..numWrittenNodes];
		assert(numWriteOnlyValues > 0);
		size_t from = arrayPos;
		size_t to = numWrittenNodes - numWriteOnlyValues;
		if (from != to) {
			swap(writtenNodes[from], writtenNodes[to]);
			getInfo(writtenNodes[from]).arrayPos = cast(uint)from;
			getInfo(writtenNodes[to]).arrayPos = cast(uint)to;
		}
		--numWriteOnlyValues;
	}

	void rw_to_wo(uint arrayPos)
	{
		IrIndex[] writtenNodes = writtenNodesPtr[0..numWrittenNodes];
		++numWriteOnlyValues;
		size_t from = arrayPos;
		size_t to = numWrittenNodes - numWriteOnlyValues;
		if (from != to) {
			swap(writtenNodes[from], writtenNodes[to]);
			getInfo(writtenNodes[from]).arrayPos = cast(uint)from;
			getInfo(writtenNodes[to]).arrayPos = cast(uint)to;
		}
	}

	void removeItem(ValueInfo* item)
	{
		IrIndex[] writtenNodes = writtenNodesPtr[0..numWrittenNodes];
		size_t from = numWrittenNodes-1;
		size_t to = item.arrayPos;
		if (from != to) {
			writtenNodes[to] = writtenNodes[from];
			getInfo(writtenNodes[to]).arrayPos = cast(uint)to;
		}
		*item = ValueInfo();
		--numWrittenNodes;
		context.tempBuffer.unput(1);
	}

	void placeMovesBeforeInstr(IrBuilder* builder, IrIndex beforeInstr)
	{
		while (numWrittenNodes)
		{
			IrIndex toIndex = writtenNodesPtr[numWrittenNodes-1];
			ValueInfo* to = &getInfo(toIndex);
			IrIndex fromIndex = to.readFrom;
			ValueInfo* from = &getInfo(fromIndex);

			assert(!(fromIndex.isStackSlot && toIndex.isStackSlot));

			if (to.numReads == 0)
			{
				version(RAPrint_resolve) writefln("insert move %s <- %s", toIndex, fromIndex);
				if (fromIndex.isStackSlot)
				{
					ExtraInstrArgs extra = { result : toIndex, argSize : to.argSize };
					InstrWithResult instr = builder.emitInstr!LirAmd64Instr_load(extra, fromIndex);
					builder.insertBeforeInstr(beforeInstr, instr.instruction);
				}
				else // from is reg or constant
				{
					if (toIndex.isStackSlot) // con or reg -> stack slot
					{
						ExtraInstrArgs extra = { argSize : to.argSize };
						IrIndex instr = builder.emitInstr!LirAmd64Instr_store(extra, toIndex, fromIndex);
						builder.insertBeforeInstr(beforeInstr, instr);
					}
					else // con or reg -> reg
					{
						ExtraInstrArgs extra = { result : toIndex, argSize : to.argSize };
						InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, fromIndex);
						builder.insertBeforeInstr(beforeInstr, instr.instruction);
					}
				}

				--from.numReads;
				--numWriteOnlyValues;
				removeItem(to);

				if (from.numReads == 0 && from.readFrom.isDefined)
					rw_to_wo(from.arrayPos);
			}
			else // to.numReads > 0
			{
				// process cycled items
				// from is non-constant in this scope

				// to <-- from <-- from.readFrom
				// mark from as removed and rewrite as:
				// to <-- from.readFrom

				version(RAPrint_resolve) writefln("xchg from %s to %s %s", *from, *to, toIndex);

				IrIndex instr = builder.emitInstr!LirAmd64Instr_xchg(ExtraInstrArgs(), fromIndex, from.readFrom);
				builder.insertBeforeInstr(beforeInstr, instr);

				if (from.readFrom == toIndex) {
					// handle case when from.readFrom == to
					// ... to <-- from <-- to ...
					removeItem(to);
				} else {
					to.readFrom = from.readFrom;
					--from.numReads;
				}
				removeItem(from);
			}
		}
	}
}

struct ValueInfo
{
	uint arrayPos; // index into writtenNodes
	IrIndex readFrom; // can be null
	ushort numReads;
	IrArgSize argSize; // used for memory moves

	void onRead(IrIndex self)
	{
		++numReads;
	}

	void onWrite(IrIndex from, IrIndex self, IrArgSize argSize)
	{
		assert(readFrom.isUndefined, "Second write detected");
		readFrom = from;
		this.argSize = argSize;
	}
}
