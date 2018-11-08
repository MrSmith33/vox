/**
Copyright: Copyright (c) 2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Linear scan register allocation pass
module register_allocation;

import std.array : empty;
import std.string : format;
import std.stdio : writeln, write, writef, writefln, stdout;
import std.format : formattedWrite, FormatSpec;
import std.range : repeat;
import std.range : chain;
import std.bitmanip : bitfields;
import std.algorithm : min, max, sort, swap;

import all;

version = RAPrint;

/// Does linear scan register allocation.
/// Uses live intervals produced by pass_live_intervals
void pass_linear_scan(ref CompilationContext context) {
	LinearScan linearScan;
	linearScan.context = &context;
	foreach (FunctionDeclNode* fun; context.mod.functions) {
		linearScan.scanFun(fun);
	}
}

struct PhysRegister
{
	IrIndex regRef;
	int freeUntilPos;
	int nextUsePos;
}

struct PhysRegisters
{
	PhysRegister[] gpr;

	ref PhysRegister opIndex(IrIndex reg) {
		return gpr[reg.storageUintIndex];
	}

	void setup(FunctionDeclNode* fun)
	{
		const IrIndex[] regs = fun.callingConvention.allocatableRegs;
		gpr.length = regs.length;
		foreach(i, reg; regs)
		{
			gpr[i] = PhysRegister(reg);
		}
	}

	void resetFreeUntilPos()
	{
		foreach (ref reg; gpr) reg.freeUntilPos = int.max;
	}

	void resetNextUsePos()
	{
		foreach (ref reg; gpr) reg.nextUsePos = int.max;
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
	CompilationContext* context;

	FunctionLiveIntervals* livePtr;
	ref FunctionLiveIntervals live() { return *livePtr; }
	IrFunction* lir;

	/*
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
*/
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
	void scanFun(FunctionDeclNode* fun)
	{
		import std.container.binaryheap;

		lir = fun.lirData;
		livePtr = fun.liveIntervals;
		physRegs.setup(fun);

		scope(exit) {
			TextSink sink;
			live.dump(sink);
			writeln(sink.text);

			lir = null;
			livePtr = null;
		}

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

		foreach (it; live.physicalIntervals)
			if (!it.first.isNull)
				inactive.put(it.first);

		writefln("intervals %s", live.intervals[live.numFixedIntervals..$]);

		// while unhandled != { } do
		while (!unhandled.empty)
		{
			// current = pick and remove first interval from unhandled
			NodeIndex currentId = unhandled.front;
			unhandled.removeFront;

			// position = start position of current
			int position = live.ranges[currentId].from;
			version(RAPrint) writefln("current %s pos %s", live.ranges[currentId].intervalIndex, position);
			version(RAPrint) writefln("  active len %s, inactive len %s", active.length, inactive.length);

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
					version(RAPrint) writefln("  move %s active -> handled", range.intervalIndex);
					//handled.put(rangeId);
					active.removeInPlace(index);
				}
				// else if it does not cover position then
				else if(!live.intervalCoversPosition(rangeId, position))
				{
					// move it from active to inactive
					version(RAPrint) writefln("  %s isLast %s less %s", rangeId, range.isLast, range.to < position);
					version(RAPrint) writefln("  move %s active -> inactive", range.intervalIndex);
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
					version(RAPrint) writefln("  move %s inactive -> handled", live.ranges[rangeId].intervalIndex);
					//handled.put(rangeId);
					inactive.removeInPlace(index);
				}
				// 	else if it covers position then
				else if(live.intervalCoversPosition(rangeId, position))
				{
					// move it from inactive to active
					version(RAPrint) writefln("  move %s inactive -> active", live.ranges[rangeId].intervalIndex);
					active.put(rangeId);
					inactive.removeInPlace(index);
				}
				else
					++index;
			}

			// find a register for current
			bool success = tryAllocateFreeReg(currentId);

			// if allocation failed then AllocateBlockedReg
			if (!success)
				allocateBlockedReg(currentId, position);

			// if current has a register assigned then add current to active
			if (live[currentId].reg.isDefined)
			{
				version(RAPrint) writefln("  move current %s -> active", live.ranges[currentId].intervalIndex);
				active.put(currentId);
			}
		}

		resolve();

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
	bool tryAllocateFreeReg(NodeIndex currentId)
	{
		// set freeUntilPos of all physical registers to maxInt
		physRegs.resetFreeUntilPos;

		LiveInterval* currentIt = &live[currentId];
		int currentEnd = live.ranges[currentIt.last].to;

		// for each interval it in active do
		//     freeUntilPos[it.reg] = 0
		foreach (rangeId; active.data)
		{
			auto it = live[rangeId];
			assert(it.reg.isDefined);
			physRegs[it.reg].freeUntilPos = 0;
		}

		// check inactive : TODO

		// reg = register with highest freeUntilPos
		int maxPos = 0;
		IrIndex reg;

		// reg stored in hint
		IrIndex hintReg = currentIt.storageHint;

		PhysRegister[] candidates = physRegs.gpr;
		foreach (i, ref PhysRegister r; candidates)
		{
			if (r.freeUntilPos > maxPos) {
				maxPos = r.freeUntilPos;
				reg = r.regRef;
			}
		}

		if (maxPos == 0)
		{
			// no register available without spilling
			version(RAPrint) writeln("    no register available without spilling");
			return false;
		}
		else
		{
			version(RAPrint) writefln("    hint is %s", hintReg);
			if (hintReg.isDefined)
			{
				if (currentEnd < physRegs[hintReg].freeUntilPos)
				{
					// register available for the whole interval
					currentIt.reg = hintReg;
					version(RAPrint) writefln("    alloc hint %s", hintReg);
					return true;
				}
				else
				{
					version(RAPrint) writefln("    hint %s is not available for the whole interval", hintReg);
				}
			}

			if (currentEnd < maxPos)
			{
				currentIt.reg = reg;
				version(RAPrint) writefln("    alloc %s", reg);
				return true;
			}
			else
			{
				// split
				version(RAPrint) writefln("    alloc + split %s", reg);
				return false;
			}
		}
	}

	int nextUseAfter(LiveInterval* it, int after)
	{
		int closest = int.max;
		foreach(IrIndex user; lir.getVirtReg(it.definition).users.range(lir))
		{
			int pos = live.linearIndicies[user];
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
	void allocateBlockedReg(NodeIndex currentRangeId, int currentStart)
	{
		writefln("allocateBlockedReg of range %s, start %s", currentRangeId, currentStart);
		// set nextUsePos of all physical registers to maxInt
		physRegs.resetNextUsePos();

		// for each interval it in active do
		//     nextUsePos[it.reg] = next use of it after start of current
		foreach (rangeId; active.data)
		{
			LiveInterval* it = &live[rangeId];
			assert(it.reg.isDefined);
			if (it.isFixed)
			{
				physRegs[it.reg].nextUsePos = currentStart;
			}
			else
			{
				physRegs[it.reg].nextUsePos = nextUseAfter(it, currentStart);
			}
			writefln("nextUsePos of %s is %s", rangeId, physRegs[it.reg].nextUsePos);
		}

		// for each interval it in inactive intersecting with current do
		//     nextUsePos[it.reg] = next use of it after start of current
		foreach (rangeId; inactive.data)
		{
			LiveInterval* it = &live[rangeId];
			assert(it.reg.isDefined);
			if (it.isFixed)
			{
				int fistrIntersection = live.firstIntersection(currentRangeId, rangeId);
				physRegs[it.reg].nextUsePos = min(fistrIntersection, physRegs[it.reg].nextUsePos);
			}
			else
			{
				physRegs[it.reg].nextUsePos = nextUseAfter(it, currentStart);
			}
			writefln("nextUsePos of %s is %s", rangeId, physRegs[it.reg].nextUsePos);
		}

		// reg = register with highest nextUsePos
		LiveInterval* currentIt = &live[currentRangeId];
		PhysRegister[] regs = physRegs.gpr;
		int maxUsePos = 0;
		IrIndex reg;
		foreach (i, ref PhysRegister r; regs)
		{
			if (r.nextUsePos > maxUsePos) {
				maxUsePos = r.nextUsePos;
				reg = r.regRef;
			}
		}

		context.unreachable;
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
	void resolve()
	{

	}
}
