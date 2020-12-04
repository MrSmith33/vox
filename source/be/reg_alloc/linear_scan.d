/// Copyright: Copyright (c) 2018-2020 Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Linear scan register allocation pass
///
/// Notes:
/// Non-split current interval does not intersect with inactive intervals because of SSA form
///
/// Splits need to be at the odd position. This gives:
///  a) no intervals of form: [x; x)
///  b) no problems with two operand form mov instruction before two operand instruction
///     two operand form can insert mov from first operand into result register
///     and in case first operand's interval was split it would start at the instruction and not include the mov
///     as result register allocator may allocate arg0 register to reloaded argument,
///     but reload will precede the mov, thus overwriting the value
///     100 | v0 = add v1, v2   | eax(v0) = add ecx(v1), ecx(v2)
///     v0 [100; 200) eax
///     v1 [50; 100) ecx
///     v2 [40; 99) s0 [100; 109) ecx
///    becomes
///      99|    ecx = load i32* s3  // split move overwrites ecx that is used in the next instruction
///      99|    eax = mov ecx       // fix two operand form
///     100|    eax = add eax, ecx
///
/// As result of splits being on odd positions another invariant becomes available:
///   All ranges have distinct `from` and `to` positions. Zero length ranges become impossible.
///   range.from < range.to
module be.reg_alloc.linear_scan;

import std.array : empty;
import std.string : format;
import std.stdio : writeln, write, writef, writefln, stdout;
import std.format : formattedWrite, FormatSpec;
import std.range : repeat;
import std.range : chain;
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
		dumpFunction(c, lirData, "RA");
		linearScan.livePtr.dump(c, lirData);
	}
}

struct RegisterState
{
	IntervalIndex activeInterval;
}

pure ubyte arrayPhysRegIndex(IrIndex reg) {
	return cast(ubyte)(reg.physRegClass * NUM_REGS_PER_CLASS + reg.physRegIndex);
}

struct PhysRegisters
{
	RegisterState[NUM_TOTAL_REGS] registers;
	uint[NUM_TOTAL_REGS] usePos;
	uint[NUM_TOTAL_REGS] blockPos;
	//bitmask per class
	FullRegSet volatileRegs;
	FullRegSet calleeSavedRegs;
	FullRegSet usedRegs;

	ref RegisterState opIndex(IrIndex reg) return {
		assert(reg.isPhysReg, format("%s", reg));
		return registers[reg.physRegClass * NUM_REGS_PER_CLASS + reg.physRegIndex];
	}

	void setup(CompilationContext* c, IrFunction* lir, MachineInfo* machineInfo)
	{
		CallConv* callConv = lir.getCallConv(c);

		if (c.debugRegAlloc) {
			volatileRegs = callConv.volatileRegs.lowest(5); // only 5 regs are available for tests
			calleeSavedRegs = FullRegSet.init;
		} else {
			volatileRegs = callConv.volatileRegs;
			calleeSavedRegs = callConv.calleeSaved;
			//if (!c.useFramePointer)
			//	calleeSavedRegs |= callConv.framePointer; // add frame pointer to register pool
		}
		usedRegs = FullRegSet.init;
		registers = typeof(registers).init;
	}

	void setUsePos(IrIndex reg, uint pos) {
		usePos[reg.physRegClass * NUM_REGS_PER_CLASS + reg.physRegIndex] = min(usePos[reg.physRegClass * NUM_REGS_PER_CLASS + reg.physRegIndex], pos);
	}

	void setBlockPos(IrIndex reg, uint pos) {
		blockPos[reg.physRegClass * NUM_REGS_PER_CLASS + reg.physRegIndex] = min(blockPos[reg.physRegClass * NUM_REGS_PER_CLASS + reg.physRegIndex], pos);
		usePos[reg.physRegClass * NUM_REGS_PER_CLASS + reg.physRegIndex] = min(usePos[reg.physRegClass * NUM_REGS_PER_CLASS + reg.physRegIndex], pos);
	}

	void resetBlockPos()
	{
		usePos[] = MAX_USE_POS;
		blockPos[] = MAX_USE_POS;
	}

	void resetUsePos()
	{
		usePos[] = MAX_USE_POS;
	}

	void markAsUsed(IrIndex reg)
	{
		usedRegs |= PhysReg(cast(ubyte)reg.physRegClass, cast(ubyte)reg.physRegIndex);
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
	// unhandled = list of intervals sorted by increasing start positions
	IntervalPriorityQueue unhandled;
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

	IrIndex scratchSpillSlot;

	void freeMem() {
		activeVirtual.free(context.arrayArena);
		activeFixed.free(context.arrayArena);
		inactiveVirtual.free(context.arrayArena);
		inactiveFixed.free(context.arrayArena);
		pendingMoveSplits.free(context.arrayArena);
		unhandled.free(context.arrayArena);
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

			LiveRangeIndex rangeId = activeInt.getRightRange(position);

			// if it ends before position then
			if (rangeId.isNull)
			{
				// move it from active to handled
				version(RAPrint) writefln("  move %s active -> handled", activeId);
				active.removeInPlace(index);
			}
			// else if it does not cover position then
			else if(!activeInt.ranges[rangeId].contains(position))
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
			LiveRangeIndex rangeId = inactiveInt.getRightRange(position);

			// if it ends before position then
			if (rangeId.isNull)
			{
				// move it from inactive to handled
				version(RAPrint) writefln("  move %s inactive -> handled", inactiveId);
				inactive.removeInPlace(index);
			}
			// 	else if it covers position then
			else if(inactiveInt.ranges[rangeId].contains(position))
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
		physRegs.setup(context, lir, context.machineInfo);
		vregState = context.allocateTempArray!VregState(lir.numVirtualRegisters);

		//writefln("\nstart scan of %s", context.idString(lir.backendData.name));
		scope(exit) {
			lir = null;
		}

		// active = { }; inactive = { }; handled = { }
		activeVirtual.clear;
		activeFixed.clear;
		inactiveVirtual.clear;
		inactiveFixed.clear;
		pendingMoveSplits.clear;
		unhandled.clear;

		size_t i = 0;
		foreach (ref LiveInterval it; live.virtualIntervals) {
			if (!it.ranges.empty)
				unhandled.insert(context.arrayArena, live.vindex(i), it.from);
			++i;
		}
		i = 0;
		foreach (ref LiveInterval it; live.physicalIntervals) {
			if (!it.ranges.empty)
				inactiveFixed.put(context.arrayArena, live.pindex(i));
			++i;
		}

		IrIndex currentBlock = lir.entryBasicBlock;

		// while unhandled != { } do
		while (!unhandled.empty)
		{
			// current = pick and remove first interval from unhandled
			IntervalIndex currentIndex = unhandled.removeFront;
			LiveInterval* currentInterval = &live.intervals[currentIndex];

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
					version(RAPrint) writefln("  current is in the middle of the block %s", currentBlock);
					pendingMoveSplits.put(context.arrayArena, currentIndex);
				}

				// skip allocation if this interval has spill slot assigned
				if (currentInterval.reg.isStackSlot) {
					version(RAPrint) writefln("  current is spilled to %s. skip allocation", currentInterval.reg);
					continue;
				}
			}

			ubyte regClass = currentInterval.regClass;

			// find a register for current
			bool success = tryAllocateFreeReg(currentIndex, regClass);

			// if allocation failed then AllocateBlockedReg
			if (!success) {
				allocateBlockedReg(fun, currentIndex, position, regClass);
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
	bool tryAllocateFreeReg(IntervalIndex currentId, ubyte regClass)
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
			physRegs.usePos[it.reg.arrayPhysRegIndex] = 0;
			version(RAPrint) writefln("   intp %s %s reg %s (next use 0)", activeId, IrIndexDump(it.definition, context, lir), IrIndexDump(it.reg, context, lir));
		}
		foreach (IntervalIndex activeId; activeVirtual) {
			LiveInterval* it = &live.intervals[activeId];
			physRegs.usePos[it.reg.arrayPhysRegIndex] = 0;
			version(RAPrint) writefln("   intv %s %s reg %s (next use 0)", activeId, *it, IrIndexDump(it.reg, context, lir));
		}

		// for each interval it in inactive intersecting with current do
		//   usePos[it.reg] = next intersection of it with current
		version(RAPrint) writeln("  inactive:");
		foreach (IntervalIndex inactiveId; inactiveFixed) {
			LiveInterval* it = &live.intervals[inactiveId];
			version(RAPrint) writef("   intp %s %s (next use %s)", IrIndexDump(it.reg, context, lir), IrIndexDump(it.definition, context, lir), FreeUntilPos(physRegs[it.reg].usePos));

			// if current intersects both active and inactive, usePos stays 0
			if (physRegs.usePos[it.reg.arrayPhysRegIndex] == 0) { version(RAPrint) writeln;
				continue;
			}
			// in case there is no intersection will return MAX_USE_POS (noop)
			uint inactiveIntersection = firstIntersection(currentIt, it);

			// Register may be already occupied by active or inactive interval, so preserve it's use pos
			physRegs.usePos[it.reg.arrayPhysRegIndex] = min(physRegs.usePos[it.reg.arrayPhysRegIndex], inactiveIntersection);
			version(RAPrint) writefln(":=%s", FreeUntilPos(inactiveIntersection));
		}

		// We only need to check inactive intervals when current interval was split
		// because otherwise SSA form is intact, and there may not be intersections with inactive intervals
		if (currentIt.isSplitChild)
		{
			foreach (IntervalIndex inactiveId; inactiveVirtual) {
				LiveInterval* it = &live.intervals[inactiveId];
				assert(it.reg.isPhysReg, "not a reg");
				version(RAPrint) writef("   intv %s %s (next use %s)", IrIndexDump(it.reg, context, lir), it.definition, FreeUntilPos(physRegs[it.reg].usePos));

				// if current intersects both active and inactive, usePos stays 0
				if (physRegs.usePos[it.reg.arrayPhysRegIndex] == 0) { version(RAPrint) writeln;
					continue;
				}
				// in case there is no intersection will return MAX_USE_POS (noop)
				uint inactiveIntersection = firstIntersection(currentIt, it);
				// Register may be already occupied by active or inactive interval, so preserve it's use pos
				physRegs.usePos[it.reg.arrayPhysRegIndex] = min(physRegs.usePos[it.reg.arrayPhysRegIndex], inactiveIntersection);
				version(RAPrint) writefln(":=%s", FreeUntilPos(inactiveIntersection));
			}
		}

		version(RAPrint) writefln("  Try alloc free reg for %s %s", *currentIt, currentId);

		// reg = register with highest usePos
		uint maxPos = 0;
		IrIndex reg;

		version(RAPrint) write("  candidates:");
		// prioritize volatile regs by first iterating them
		foreach (ubyte regIndex; physRegs.volatileRegs.classes[regClass]) {
			uint usePos = physRegs.usePos[regClass * NUM_REGS_PER_CLASS + regIndex];
			version(RAPrint) writef(" %s:%s", IrIndexDump(IrIndex(regIndex, ArgType.QWORD, regClass), context, lir), FreeUntilPos(usePos));
			if (usePos > maxPos) {
				maxPos = usePos;
				reg = IrIndex(regIndex, ArgType.QWORD, regClass);
			}
		}
		foreach (ubyte regIndex; physRegs.calleeSavedRegs.classes[regClass]) {
			uint usePos = physRegs.usePos[regClass * NUM_REGS_PER_CLASS + regIndex];
			version(RAPrint) writef(" %s:%s", IrIndexDump(IrIndex(regIndex, ArgType.QWORD, regClass), context, lir), FreeUntilPos(usePos));
			if (usePos > maxPos) {
				maxPos = usePos;
				reg = IrIndex(regIndex, ArgType.QWORD, regClass);
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
			if (hintReg.isVirtReg) hintReg = live.vint(hintReg).reg;
			if (hintReg.isDefined) {
				if (currentEnd < physRegs.usePos[hintReg.arrayPhysRegIndex]) {
					// register available for the whole interval
					currentIt.reg = hintReg;
					currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(context, currentIt.definition), context);
					physRegs.markAsUsed(hintReg);
					version(RAPrint) writefln("    alloc hint %s", IrIndexDump(hintReg, context, lir));
					return true;
				} else {
					version(RAPrint) writefln("    hint %s is not available for the whole interval", IrIndexDump(hintReg, context, lir));
					// use hint reg with spill if it is one of max use regs
					if (physRegs.usePos[hintReg.arrayPhysRegIndex] == maxPos) reg = hintReg;
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

				// prevent split at even x when interval starts at x-1
				if (currentIt.from + 1 == maxPos) return false;

				splitBefore(currentId, maxPos);
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
	void allocateBlockedReg(FunctionDeclNode* fun, IntervalIndex currentId, uint currentStart, ubyte regClass)
	{
		physRegs.resetBlockPos;

		LiveInterval* currentIt = &live.intervals[currentId];
		assert(currentIt.ranges.length);
		version(RAPrint) writefln("allocateBlockedReg of int %s start %s", currentId, currentStart);

		foreach (IntervalIndex activeId; activeVirtual) {
			LiveInterval* it = &live.intervals[activeId];
			physRegs.setUsePos(it.reg, it.nextUseAfter(currentStart));
			physRegs[it.reg].activeInterval = activeId;
			version(RAPrint) writefln("active virt usePos of %s %s use %s block %s", activeId, *it, physRegs[it.reg].usePos, physRegs[it.reg].blockPos);
		}
		// We only need to check inactive intervals when current interval was split
		// because otherwise SSA form is intact, and there may not be intersections with inactive intervals
		if (currentIt.isSplitChild) {
			foreach (inactiveId; inactiveVirtual) {
				LiveInterval* it = &live.intervals[inactiveId];
				physRegs.setUsePos(it.reg, it.nextUseAfter(currentStart));
				version(RAPrint) writefln("inactive virt usePos of %s %s use %s block %s", inactiveId, *it, physRegs[it.reg].usePos, physRegs[it.reg].blockPos);
			}
		}
		foreach (IntervalIndex activeId; activeFixed) {
			LiveInterval* it = &live.intervals[activeId];
			physRegs.setBlockPos(it.reg, 0);
			physRegs[it.reg].activeInterval = IntervalIndex.NULL;
			version(RAPrint) writefln("active fixed usePos of %s %s use %s block %s", activeId, *it, physRegs[it.reg].usePos, physRegs[it.reg].blockPos);
		}
		foreach (inactiveId; inactiveFixed) {
			LiveInterval* it = &live.intervals[inactiveId];
			uint intersection = firstIntersection(currentIt, it);
			if (intersection != MAX_USE_POS)
			{
				physRegs.setBlockPos(it.reg, intersection);
			}
			version(RAPrint) writefln("inactive fixed usePos of %s %s use %s block %s", inactiveId, *it, physRegs[it.reg].usePos, physRegs[it.reg].blockPos);
		}

		// reg = register with highest usePos
		uint maxPos = 0;
		IrIndex reg;

		foreach (ubyte regIndex; (physRegs.volatileRegs | physRegs.calleeSavedRegs).classes[regClass]) {
			uint usePos = physRegs.usePos[regClass * NUM_REGS_PER_CLASS + regIndex];
			if (usePos > maxPos) {
				// if register was only available for the start of interval
 				// then all uses may have moved into split child
				maxPos = usePos;
				reg = IrIndex(regIndex, ArgType.QWORD, regClass);
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
			splitBefore(currentId, firstUse);
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
				version(RAPrint) writefln("      split before current start %s", currentStart);
				IntervalIndex newInterval = splitBefore(index, currentStart);

				LiveInterval* splitActiveIt = &live.intervals[newInterval];
				assignSpillSlot(fun.backendData.stackLayout, splitActiveIt);

				// if there is a use position in spilled interval, split before use
				uint nextActiveUse = splitActiveIt.nextUseAfter(currentStart);
				if (nextActiveUse != MAX_USE_POS)
				{
					version(RAPrint) writefln("      split before use %s", newInterval);
					splitBefore(newInterval, nextActiveUse);
				}
			}

			IntervalIndex activeIndex = physRegs[reg].activeInterval;
			context.assertf(!activeIndex.isNull, "%s", IrIndexDump(reg, context, lir)); // null means that it is fixed interval. But we filter out fixed interval above
			version(RAPrint) writefln("    split active interval %s", activeIndex);
			splitActiveOrInactive(activeIndex);

			//	split any inactive interval for reg at the end of its lifetime hole
			foreach (inactiveId; inactiveVirtual)
			{
				LiveInterval* it = &live.intervals[inactiveId];
				if (sameIndexOrPhysReg(it.reg, reg))
				{
					version(RAPrint) writefln("    split inactive interval %s", inactiveId);
					splitActiveOrInactive(inactiveId);
				}
			}

			// if current intersects with the fixed interval for reg then
			if (currentIt.exclusivePosition(physRegs.blockPos[reg.arrayPhysRegIndex]))
			{
				// split current before this intersection
				splitBefore(currentId, physRegs.blockPos[reg.arrayPhysRegIndex]);
			}

			version(RAPrint) writefln("    spill currently active interval %s before %s for %s",
				activeIndex, currentStart, IrIndexDump(reg, context, lir));
		}
	}

	// auto adds new interval to unhandled
	IntervalIndex splitBefore(IntervalIndex it, uint before)
	{
		IntervalIndex newInterval = live.splitBefore(context, lir, it, before);
		if (newInterval != it) {
			unhandled.insert(context.arrayArena, newInterval, live.intervals[newInterval].from);
		}
		return newInterval;
	}

	IntervalIndex splitBefore(IntervalIndex it, uint before, LiveRangeIndex rangeIndex)
	{
		IntervalIndex newInterval = live.splitBefore(context, it, before, rangeIndex);
		if (newInterval != it) {
			unhandled.insert(context.arrayArena, newInterval, live.intervals[newInterval].from);
		}
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
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; lir.virtualRegisters)
		{
			foreach (IrIndex userIndex, uint numUses; vreg.users.range(lir))
			{
				final switch (userIndex.kind) with(IrValueKind)
				{
					case none, array, virtualRegister, physicalRegister, constant, constantAggregate, constantZero, global, basicBlock, stackSlot, type, func: assert(false);
					case instruction:
						uint pos = live.linearIndicies.instr(userIndex);
						IrIndex reg = live.getRegFor(vregIndex, pos);
						foreach (ref IrIndex arg; lir.getInstr(userIndex).args(lir))
							if (arg == vregIndex)
							{
								arg = reg;
							}
						break;

					case phi:
						IrPhi* phi = lir.getPhi(userIndex);
						IrIndex[] preds = lir.getBlock(phi.blockIndex).predecessors.data(lir);
						foreach (size_t i, ref IrIndex phiArg; phi.args(lir))
							if (phiArg == vregIndex)
							{
								IrIndex lastInstr = lir.getBlock(preds[i]).lastInstr;
								uint pos = live.linearIndicies.instr(lastInstr);
								IrIndex reg = live.getRegFor(vregIndex, pos);
								//writefln("set %s arg reg %s -> %s at %s", userIndex, phiArg, reg, pos);
								phiArg = reg;
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
		// args are already fixed
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; lir.virtualRegisters)
		{
			switch(vreg.definition.kind) with(IrValueKind)
			{
				case instruction:
					uint pos = live.linearIndicies.instr(vreg.definition);
					IrIndex resultReg = live.getRegFor(vregIndex, pos);

					IrIndex instrIndex = vreg.definition;
					IrInstrHeader* instrHeader = lir.getInstr(instrIndex);
					instrHeader.result(lir) = resultReg;

					InstrInfo instrInfo = context.machineInfo.instrInfo[instrHeader.op];

					// requires two operant form
					if (instrInfo.isResultInDst)
					{
						fixTwoOperandForm(instrInfo, instrIndex, instrHeader);
					}
					// optimization that can be safely disabled
					else
					{
						if (instrInfo.isMov)
						{
							IrIndex src = instrHeader.arg(lir, 0);
							if (resultReg == src)
							{
								// redundant mov
								//writefln("%s mov %s", resultReg, src);
								lir.removeInstruction(instrIndex);
							}
						}
					}
					break;

				case phi:
					IrPhi* irPhi = lir.getPhi(vreg.definition);
					uint pos = live.linearIndicies.basicBlock(irPhi.blockIndex);
					IrIndex reg = live.getRegFor(vregIndex, pos);
					irPhi.result = reg;
					break;

				default: assert(false);
			}
		}
	}

	void fixTwoOperandForm(InstrInfo instrInfo, IrIndex instrIndex, IrInstrHeader* instrHeader)
	{
		void makeMov(IrIndex to, IrIndex from)
		{
			IrArgSize sizeFrom;
			switch(from.kind) with(IrValueKind)
			{
				case stackSlot, global, func, constant, constantZero:
					sizeFrom = IrArgSize.size64; // use regular mov
					break;
				case physicalRegister:
					sizeFrom = cast(IrArgSize)from.physRegSize; // movzx for 8/16bit regs
					break;
				default:
					context.internal_error("fixTwoOperandForm %s", from);
			}

			IrIndex instr;
			ExtraInstrArgs extra = {addUsers : false, result : to};
			// zero extend 8 and 16 bit args to 32bit
			final switch(sizeFrom) with(IrArgSize)
			{
				case size8:
					instr = builder.emitInstr!(Amd64Opcode.movzx_btod)(extra, from).instruction;
					break;
				case size16:
					instr = builder.emitInstr!(Amd64Opcode.movzx_wtod)(extra, from).instruction;
					break;
				case size32, size64:
					instr = builder.emitInstr!(Amd64Opcode.mov)(extra, from).instruction;
					break;
				case size128, size256, size512: context.internal_error("Invalid constant size %s", sizeFrom);
			}
			builder.insertBeforeInstr(instrIndex, instr);
		}

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
			//             // because r2 will be overwritten. But r2 may be used later
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
					makeMov(r1, r2);
					// r1 = op r1 r3
					instrHeader.arg(lir, 0) = r1;
				}
			}
			else // r2 == r3
			{
				if ( !sameIndexOrPhysReg(r1, r2) )
				{
					// mov r1 r2
					makeMov(r1, r2);
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
				makeMov(r1, r2);
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
				IrBasicBlock* predBlock = lir.getBlock(predIndex);
				resolveEdge(moveSolver, predIndex, *predBlock, succIndex, succBlock);
			}
		}
	}

	IrArgSize getMoveArgSize(IrIndex value)
	{
		if (value.isPhysReg) return cast(IrArgSize)value.physRegSize;
		IrIndex type = getValueType(value, lir, context);
		if (value.isStackSlot) type = context.types.getPointerBaseType(type);
		return sizeToIrArgSize(context.types.typeSize(type), context);
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

		size_t[] succLiveIn = live.bitmap.blockLiveInBuckets(succIndex);

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
				IrArgSize argSize = getMoveArgSize(vreg);
				version(RAPrint_resolve) writefln("    vreg %s, pred %s succ %s size %s", vreg, IrIndexDump(predLoc, context, lir), IrIndexDump(succLoc, context, lir), argSize);
				moveSolver.addMove(predLoc, succLoc, argSize);
			}
		}

		foreach (IrIndex phiIndex, ref IrPhi phi; succBlock.phis(lir))
		{
			version(RAPrint_resolve) writef("    phi %s res %s", phiIndex, IrIndexDump(phi.result, context, lir));
			IrIndex[] preds = lir.getBlock(phi.blockIndex).predecessors.data(lir);
			foreach (size_t arg_i, ref IrIndex arg; phi.args(lir))
			{
				IrIndex argBlock = preds[arg_i];
				if (argBlock == predIndex)
				{
					IrIndex moveFrom = arg;
					IrIndex moveTo = phi.result;

					IrArgSize argSize = getMoveArgSize(phi.result);
					version(RAPrint_resolve) writefln(" arg %s %s size %s", argBlock, IrIndexDump(arg, context, lir), argSize);

					moveSolver.addMove(moveFrom, moveTo, argSize);
				}
			}
		}

		if (moveSolver.numWrittenNodes == 0) return;

		if (isCriticalEdge(predBlock, succBlock))
		{
			IrIndex movesTarget = splitCriticalEdge(predIndex, predBlock, succIndex, succBlock);
			// add to the back
			moveSolver.placeMovesBeforeInstr(builder, lir.getBlock(movesTarget).lastInstr, &getScratchSpillSlot);
		}
		else if (predBlock.successors.length > 1)
		{
			IrIndex movesTarget = succIndex;
			// add to the front
			moveSolver.placeMovesBeforeInstr(builder, lir.getBlock(movesTarget).firstInstr, &getScratchSpillSlot);
		}
		else
		{
			context.assertf(predBlock.successors.length == 1,
				"predBlock.successors.length %s", predBlock.successors.length);
			IrIndex movesTarget = predIndex;
			// add to the back
			moveSolver.placeMovesBeforeInstr(builder, lir.getBlock(movesTarget).lastInstr, &getScratchSpillSlot);
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
		moveBlockAfter(lir, newBlock, predIndex);

		version(RAPrint_resolve) writefln("Split critical edge %s -> %s with %s", predIndex, succIndex, newBlock);
		predBlock.successors.replaceFirst(lir, succIndex, newBlock);
		succBlock.predecessors.replaceFirst(lir, predIndex, newBlock);

		lir.getBlock(newBlock).predecessors.append(builder, predIndex);
		lir.getBlock(newBlock).successors.append(builder, succIndex);
		lir.getBlock(newBlock).isSealed = true;
		builder.emitInstr!(Amd64Opcode.jmp)(newBlock);
		lir.getBlock(newBlock).isFinished = true;
		lir.getBlock(newBlock).replacesCriticalEdge = true;
		return newBlock;
	}

	// Insert moves between sub-intervals in the middle of basic blocks
	void resolveInsertSplitMoves(ref MoveSolver moveSolver)
	{
		moveSolver.reset();
		uint prevPos = 0;
		version(RAPrint_resolve) writefln("Fix splits");

		void insertPrevMoves() {
			if (prevPos == 0) return;
			if (moveSolver.numWrittenNodes == 0) return;

			IrIndex insertBeforeInstr = live.evenIndexToIrIndex[prevPos/2];
			context.assertf(insertBeforeInstr.isInstruction, "%s %s", prevPos, insertBeforeInstr);
			version(RAPrint_resolve) writefln("   insert %s moves before %s at %s", moveSolver.numWrittenNodes, insertBeforeInstr, prevPos);
			moveSolver.placeMovesBeforeInstr(builder, insertBeforeInstr, &getScratchSpillSlot);
			moveSolver.reset();
		}

		//IrIndex movesTarget = predIndex;
		foreach (IntervalIndex id; pendingMoveSplits)
		{
			LiveInterval* it = &live.intervals[id]; // always returns first part of split intervals
			uint splitPos = it.from+1;
			assert((splitPos & 1) == 0, "Must be even pos");
			// insert all moves on prev position
			if (prevPos != splitPos) insertPrevMoves;

			LiveInterval* parentIt = &live.intervals[it.parent];
			version(RAPrint_resolve) writefln("  %s:%s split at %s, parent %s at %s", id, it.definition, it.from, it.parent, parentIt.to);
			context.assertf(prevPos <= splitPos, "Wrong order %s < %s", prevPos, splitPos);
			IrIndex moveFrom = parentIt.reg;
			IrIndex moveTo = it.reg;
			version(RAPrint_resolve) writefln("   from %s to %s", IrIndexDump(moveFrom, context, lir), IrIndexDump(moveTo, context, lir));
			if (moveFrom != moveTo)
			{
				IrIndex vreg = it.definition;
				IrArgSize argSize = getMoveArgSize(vreg);
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
		CallConv* callConv = lir.getCallConv(context);
		foreach(PhysReg reg; physRegs.calleeSavedRegs & physRegs.usedRegs)
		{
			// choose correct slot type
			IrArgSize size = callConv.calleeSavedSizePerClass[reg.regClass];
			IrIndex slotType;
			switch(size) with(IrArgSize) {
				case size64:
					slotType = makeBasicTypeIndex(IrValueType.i64);
					break;
				case size128:
					slotType = context.v128Type;
					break;
				default: context.internal_error("Size not implemented %s", size);
			}
			IrIndex slot = stackLayout.addStackItem(context, slotType, StackSlotKind.local, 0);

			// save register
			ExtraInstrArgs extra1 = { argSize : size };
			IrIndex instrStore = builder.emitInstr!(Amd64Opcode.store)(extra1, slot, IrIndex(reg, size));
			builder.prependBlockInstr(entryBlock, instrStore);

			// restore register
			ExtraInstrArgs extra = { result : IrIndex(reg, size), argSize : size };
			InstrWithResult instrLoad = builder.emitInstr!(Amd64Opcode.load)(extra, slot);
			builder.insertBeforeLastInstr(exitBlock, instrLoad.instruction);
		}
	}

	IrIndex getScratchSpillSlot() {
		if (scratchSpillSlot.isUndefined) {
			scratchSpillSlot = fun.backendData.stackLayout.addStackItem(context, makeBasicTypeIndex(IrValueType.i64), StackSlotKind.local, 0);
		}
		return scratchSpillSlot;
	}
}

struct IntervalPriority
{
	uint from;
	IntervalIndex interval;
}

// Tweaked to prefer sequential inserts
struct IntervalPriorityQueue
{
	Array!IntervalPriority queue;
	uint maxFrom = 0;
	uint numRemoved = 0;

	void clear()
	{
		queue.clear;
		maxFrom = 0;
		numRemoved = 0;
	}

	void free(ref ArrayArena arrayArena)
	{
		queue.free(arrayArena);
	}

	void insert(ref ArrayArena arrayArena, IntervalIndex interval, uint from)
	{
		if (from >= maxFrom)
		{
			// fast path for append (happens most of the time)
			queue.put(arrayArena, IntervalPriority(from, interval));
			maxFrom = from;
		}
		else
		{
			for(size_t i = numRemoved; i < queue.length; ++i)
			{
				if (queue[i].from > from)
				{
					// insert before i
					if (numRemoved > 0)
					{
						// shift to the left using one of removed slots
						for(size_t j = numRemoved; j < i; ++j)
							queue[j-1] = queue[j];
						--numRemoved;
						queue[i-1] = IntervalPriority(from, interval);
					}
					else
					{
						// shift to the right
						queue.putAt(arrayArena, i, IntervalPriority(from, interval));
					}
					break;
				}
			}
		}
	}

	IntervalIndex removeFront()
	{
		auto res = queue[numRemoved].interval;
		++numRemoved;
		return res;
	}

	bool empty() { return numRemoved == queue.length; }
	uint length() { return queue.length - numRemoved; }
}
