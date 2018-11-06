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
/+
/// Does linear scan register allocation.
/// Uses live intervals produced by pass_live_intervals
void pass_linear_scan(ref CompilationContext ctx) {
	LinearScan linearScan;
	linearScan.setup(&ctx);
	foreach (fun; ctx.mod.irModule.functions) {
		linearScan.assignHints(*fun, *fun.callingConvention);
		linearScan.scanFun(*fun);
		linearScan.resolve(*fun);
	}
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

	ref PhysRegister opIndex(RegisterRef reg) {
		return gpr[reg];
	}

	void setup()
	{
		gpr.length = 0;
		gpr ~= PhysRegister(RegisterRef(0, Register.AX));
		gpr ~= PhysRegister(RegisterRef(1, Register.CX));
		gpr ~= PhysRegister(RegisterRef(2, Register.DX));
		gpr ~= PhysRegister(RegisterRef(3, Register.R8));
		gpr ~= PhysRegister(RegisterRef(4, Register.R9));
		gpr ~= PhysRegister(RegisterRef(5, Register.R10));
		gpr ~= PhysRegister(RegisterRef(6, Register.R11));
	}

	void resetFreeUntilPos()
	{
		foreach (ref reg; gpr) reg.freeUntilPos = int.max;
	}

	void resetNextUsePos()
	{
		foreach (ref reg; gpr) reg.nextUsePos = int.max;
	}

	PhysRegister[] getClassRegs(RegClass cls)
	{
		final switch(cls) {
			case RegClass.gpr: return gpr;
			case RegClass.flags: return flags;
		}
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
+/
