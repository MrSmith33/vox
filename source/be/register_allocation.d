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
		linearScan.livePtr.dump(c, linearScan.fun);
	}
}

struct RegisterState
{
	IrIndex index;
	IntervalIndex activeInterval;
	bool isAllocatable;
	bool isUsed;
	bool isCalleeSaved;
	int freeUntilPos;
	int nextUsePos;
}

struct PhysRegisters
{
	RegisterState[] gpr;
	const(IrIndex)[] allocatableRegs;

	ref RegisterState opIndex(IrIndex reg) {
		return gpr[reg.physRegIndex];
	}

	void setup(CompilationContext* c, FunctionDeclNode* fun, MachineInfo* machineInfo)
	{
		CallConv* callConv = fun.backendData.getCallConv(c);
		allocatableRegs = callConv.allocatableRegs;
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

	void resetFreeUntilPos()
	{
		foreach (ref reg; gpr) {
			if (reg.isAllocatable)
				reg.freeUntilPos = int.max;
			else
				reg.freeUntilPos = 0; // prevent allocation
		}
	}

	void resetNextUsePos()
	{
		foreach (ref reg; gpr) {
			if (reg.isAllocatable)
				reg.nextUsePos = int.max;
			else
				reg.nextUsePos = 0; // prevent allocation
		}
	}

	void markAsUsed(IrIndex reg)
	{
		assert(gpr[reg.physRegIndex].isAllocatable);
		gpr[reg.physRegIndex].isUsed = true;
	}
}

/// Implementation of:
/// "Optimized Interval Splitting in a Linear Scan Register Allocator"
/// "Linear Scan Register Allocation on SSA Form"
struct LinearScan
{
	IntervalIndex[] unhandledStorage; // TODO: remove GC storage here
	Array!IntervalIndex active;
	Array!IntervalIndex inactive;
	PhysRegisters physRegs;
	CompilationContext* context;
	IrBuilder* builder;
	FunctionDeclNode* fun;

	LivenessInfo* livePtr;
	ref LivenessInfo live() { return *livePtr; }
	IrFunction* lir;

	void freeMem() {
		active.free(context.arrayArena);
		inactive.free(context.arrayArena);
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
		active.clear;
		inactive.clear;

		unhandled.acquire(null);
		foreach (ref LiveInterval it; live.virtualIntervals)
			if (!it.ranges.empty)
				unhandled.insert(live.indexOf(&it));

		foreach (ref LiveInterval it; live.physicalIntervals)
			if (!it.ranges.empty)
				inactive.put(context.arrayArena, live.indexOf(&it));

		void removeActive(size_t index, IntervalIndex activeId, LiveInterval* activeInt) {
			if (activeInt.reg.isPhysReg)
				physRegs[activeInt.reg].activeInterval = IntervalIndex.NULL;
			active.removeInPlace(index);
		}

		void addActive(IntervalIndex activeId, LiveInterval* activeInt) {
			if (activeInt.reg.isPhysReg)
				physRegs[activeInt.reg].activeInterval = activeId;
			active.put(context.arrayArena, activeId);
		}

		// while unhandled != { } do
		while (!unhandled.empty)
		{
			// current = pick and remove first interval from unhandled
			IntervalIndex currentIndex = unhandled.front;
			LiveInterval* currentInterval = &live.intervals[currentIndex];
			unhandled.removeFront;

			// position = start position of current
			int position = currentInterval.from;
			version(RAPrint) writefln("current %s pos %s", currentIndex, position);
			version(RAPrint) writefln("  active len %s, inactive len %s", active.length, inactive.length);

			size_t index;
			// // check for intervals in active that are handled or inactive
			// for each interval it in active do
			while (index < active.length)
			{
				IntervalIndex activeId = active[index];
				LiveInterval* activeInt = &live.intervals[activeId];
				LiveRangeIndex rangeId = activeInt.getRightRange(position);

				// if it ends before position then
				if (rangeId.isNull)
				{
					// move it from active to handled
					version(RAPrint) writefln("  move %s active -> handled", activeId);
					removeActive(index, activeId, activeInt);
				}
				// else if it does not cover position then
				else if(!activeInt.ranges[rangeId].contains(position))
				{
					// move it from active to inactive
					version(RAPrint) writefln("  move %s active -> inactive", activeId);
					removeActive(index, activeId, activeInt);
					inactive.put(context.arrayArena, activeId);
				}
				else
					++index;
			}

			index = 0;
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
					addActive(inactiveId, inactiveInt);
					inactive.removeInPlace(index);
				}
				else
					++index;
			}

			// find a register for current
			bool success = tryAllocateFreeReg(currentIndex, unhandled);

			// if allocation failed then AllocateBlockedReg
			if (!success) {
				allocateBlockedReg(fun, currentIndex, position, unhandled);
				currentInterval = &live.intervals[currentIndex]; // intervals may have reallocated
			}

			version(RAPrint) writefln("  alloc current %s to %s", currentIndex, IrIndexDump(currentInterval.reg, *context, *lir));

			// if current has a register assigned then add current to active
			if (currentInterval.reg.isPhysReg)
			{
				// don't add stack slot assigned interval to active
				version(RAPrint) writefln("  move current %s -> active", currentIndex);
				addActive(currentIndex, currentInterval);
			}

			version(RAPrint) writeln;
		}

		fixInstructionArgs(fun);
		//fun.backendData.liveIntervals.dump(context, fun);
		resolve(fun);
		genSaveCalleeSavedRegs(fun.backendData.stackLayout);

		if (context.validateIr) validateIrFunction(*context, *lir);

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
		// set freeUntilPos of all physical registers to maxInt
		physRegs.resetFreeUntilPos;

		LiveInterval* currentIt = &live.intervals[currentId];
		int currentEnd = currentIt.to;

		static struct FreeUntilPos {
			int num;
			void toString(scope void delegate(const(char)[]) sink) {
				if (num == int.max) formattedWrite(sink, "max");
				else formattedWrite(sink, "%s", num);
			}
		}

		// for each interval it in active do
		//     freeUntilPos[it.reg] = 0
		version(RAPrint) writeln("  active:");
		foreach (IntervalIndex activeId; active)
		{
			LiveInterval* it = &live.intervals[activeId];
			version(RAPrint) writefln("   int %s %s reg %s (next use 0)", activeId, it.definition, IrIndexDump(it.reg, *context, *lir));
			assert(it.reg.isDefined);
			physRegs[it.reg].freeUntilPos = 0;
		}

		// for each interval it in inactive intersecting with current do
		//   freeUntilPos[it.reg] = next intersection of it with current
		version(RAPrint) writeln("  inactive:");

		//if (currentIt.isSplitChild)
		//{
			// We only need to check inactive intervals when current interval was split
			// because otherwise SSA form is intact, and there may not be intersections with inactive intervals
			// TODO: but fixed intervals in inactive list are not in SSA form, need to keep them separate
			foreach (IntervalIndex inactiveId; inactive)
			{
				LiveInterval* it = &live.intervals[inactiveId];
				assert(it.reg.isDefined);

				version(RAPrint) writef("   int %s %s (next use %s)", IrIndexDump(it.reg, *context, *lir), it.definition, FreeUntilPos(physRegs[it.reg].freeUntilPos));

				// if current intersects both active and inactive, freeUntilPos stays 0
				if (physRegs[it.reg].freeUntilPos == 0) {
					version(RAPrint) writeln;
					continue;
				}

				// in case there is no intersection will return int.max (noop)
				int inactiveIntersection = firstIntersection(currentIt, it);
				// Register may be already occupied by active or inactive interval, so preserve it's use pos
				physRegs[it.reg].freeUntilPos = min(physRegs[it.reg].freeUntilPos, inactiveIntersection);

				version(RAPrint) writefln(":=%s", FreeUntilPos(inactiveIntersection));
			}
		//}

		version(RAPrint) writefln("  Try alloc free reg for %s #%s", currentIt.definition, currentId);

		// reg = register with highest freeUntilPos
		int maxPos = 0;
		IrIndex reg;

		// reg stored in hint
		IrIndex hintReg = currentIt.storageHint;

		const IrIndex[] candidates = physRegs.allocatableRegs;
		version(RAPrint) write("  candidates:");
		foreach (i, IrIndex r; candidates)
		{
			version(RAPrint) writef(" %s:%s", IrIndexDump(r, *context, *lir), FreeUntilPos(physRegs[r].freeUntilPos));

			if (physRegs[r].freeUntilPos > maxPos) {
				maxPos = physRegs[r].freeUntilPos;
				reg = r;
			}
		}
		version(RAPrint) writeln;

		if (maxPos == 0)
		{
			// no register available without spilling
			version(RAPrint) writeln("    no register available without spilling");
			return false;
		}
		else
		{
			version(RAPrint) writefln("    hint is %s", IrIndexDump(hintReg, *context, *lir));
			if (hintReg.isDefined)
			{
				if (currentEnd < physRegs[hintReg].freeUntilPos)
				{
					// register available for the whole interval
					currentIt.reg = hintReg;
					currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(*context, currentIt.definition), context);
					physRegs.markAsUsed(hintReg);
					version(RAPrint) writefln("    alloc hint %s", IrIndexDump(hintReg, *context, *lir));
					return true;
				}
				else
				{
					version(RAPrint) writefln("    hint %s is not available for the whole interval", IrIndexDump(hintReg, *context, *lir));
				}
			}

			if (currentEnd < maxPos)
			{
				currentIt.reg = reg;
				currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(*context, currentIt.definition), context);
				physRegs.markAsUsed(reg);
				version(RAPrint) writefln("    alloc %s", IrIndexDump(reg, *context, *lir));
				return true;
			}
			else
			{
				// split
				version(RAPrint) writefln("    alloc %s + split at %s", IrIndexDump(reg, *context, *lir), maxPos);
				//splitBefore(currentId, maxPos, unhandled);
				//currentIt.reg = reg;
				//currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(*context, currentIt.definition), context);
				//physRegs.markAsUsed(reg);
				return false;
			}
		}
	}

	int nextUseAfter(LiveInterval* it, int after)
	{
		int closest = int.max;
		foreach(IrIndex user; lir.getVirtReg(it.definition).users.range(*lir))
		{
			int pos = usePosition(user, it.definition);
			if (pos > after && pos < closest)
			{
				closest = pos;
			}
		}
		return closest;
	}

	int usePosition(IrIndex user, IrIndex usedValue)
	{
		if (user.isPhi)
		{
			foreach(i, ref IrPhiArg arg; lir.getPhi(user).args(*lir))
			{
				if (arg.value == usedValue)
				{
					IrIndex lastInstr = lir.getBlock(arg.basicBlock).lastInstr;
					return live.linearIndicies[lastInstr];
				}
			}
		}
		else
			return live.linearIndicies[user];
		return int.max;
	}

	int firstUse(LiveInterval* it)
	{
		int first = int.max;
		int start = it.from;
		//writefln("first use %s", it.definition);
		foreach(IrIndex user; lir.getVirtReg(it.definition).users.range(*lir))
		{
			int pos = usePosition(user, it.definition);
			//writefln("  %s %s", user, pos);
			if (pos >= start && pos < first)
			{
				first = pos;
			}
		}
		return first;
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
	void allocateBlockedReg(T)(FunctionDeclNode* fun, IntervalIndex currentId, int currentStart, ref T unhandled)
	{
		LiveInterval* currentIt = &live.intervals[currentId];
		//writefln("allocateBlockedReg of int %s start %s", currentId, currentStart);
		// set nextUsePos of all physical registers to maxInt
		physRegs.resetNextUsePos();

		if (currentIt.ranges.empty) assert(false);

		IntervalIndex maxUseIntervalIndex; // can be fixed interval
		int maxUsePos = 0;

		// for each interval it in active do
		//     nextUsePos[it.reg] = next use of it after start of current
		foreach (IntervalIndex activeId; active)
		{
			LiveInterval* it = &live.intervals[activeId];
			assert(it.reg.isDefined);
			if (it.isFixed) {
				physRegs[it.reg].activeInterval = IntervalIndex.NULL;
				continue;
			}

			int use;
			if (it.isFixed) {
				use = currentStart;
			}
			else
				use = nextUseAfter(it, currentStart);

			physRegs[it.reg].nextUsePos = use;
			physRegs[it.reg].activeInterval = activeId;
			if (use > maxUsePos && use != int.max) {
				maxUsePos = use;
				maxUseIntervalIndex = activeId;
			}

			//writefln("active nextUsePos of %s %s is %s", activeId, *it, physRegs[it.reg].nextUsePos);
		}

		// for each interval it in inactive intersecting with current do
		//     nextUsePos[it.reg] = next use of it after start of current

		//if (currentIt.isSplitChild)
		//{
			// We only need to check inactive intervals when current interval was split
			// because otherwise SSA form is intact, and there may not be intersections with inactive intervals
			// TODO: but fixed intervals in inactive list are not in SSA form, need to keep them separate
			foreach (inactiveId; inactive)
			{
				LiveInterval* it = &live.intervals[inactiveId];
				assert(it.reg.isDefined);
				if (it.isFixed) continue;

				int use;
				int intersection = firstIntersection(currentIt, it);
				use = min(intersection, physRegs[it.reg].nextUsePos);

				physRegs[it.reg].nextUsePos = use;
				if (use > maxUsePos && use != int.max) {
					maxUsePos = use;
					maxUseIntervalIndex = inactiveId;
				}
			}
		//}

		LiveInterval* maxUseInterval = &live.intervals[maxUseIntervalIndex];

		// reg = register with highest nextUsePos
		IrIndex reg = maxUseInterval.reg;
		int currentFirstUse = firstUse(currentIt);

		version(RAPrint) writefln("cur %s %s firstUse %s, reg %s maxUse %s",
			currentId, currentIt.definition, currentFirstUse, maxUseInterval.definition, maxUsePos);

		//if first usage of current is after nextUsePos[reg] then
		if (currentFirstUse > maxUsePos)
		//if (currentFirstUse >= maxUsePos)
		{
			// all other intervals are used before current,
			// so it is best to spill current itself

			// assign spill slot to current
			assignSpillSlot(fun.backendData.stackLayout, currentIt);

			// split current before its first use position that requires a register
			int firstUseWithReg = currentFirstUse; // TODO: need to search for first use position

			// don't split if first use is the last position
			if (firstUseWithReg < currentIt.to)
				splitBefore(currentId, firstUseWithReg, unhandled);

			currentIt = &live.intervals[currentId]; // intervals may have reallocated
			version(RAPrint) writefln("    spill current %s", currentId);
		}
		else
		{
			// spill intervals that currently block reg
			currentIt.reg = reg;
			currentIt.reg.physRegSize = typeToRegSize(lir.getValueType(*context, currentIt.definition), context);
			physRegs.markAsUsed(reg);

			//	split active interval for reg at position
			IntervalIndex activeIndex = physRegs[reg].activeInterval;
			context.assertf(!activeIndex.isNull, "%s", IrIndexDump(reg, *context, *lir)); // null means that it is fixed interval. But we filter out fixed interval above
			splitBefore(activeIndex, currentStart, unhandled);

			//	split any inactive interval for reg at the end of its lifetime hole
			foreach (inactiveId; inactive)
			{
				LiveInterval* it = &live.intervals[inactiveId];
				if (!it.isFixed && sameIndexOrPhysReg(it.reg, reg))
				{
					LiveRangeIndex before = it.getRightRange(currentStart);
					splitBefore(inactiveId, currentStart, before, unhandled);
				}
			}
			version(RAPrint) writefln("    spill currently active interval %s before %s for %s",
				activeIndex, currentStart, IrIndexDump(reg, *context, *lir));
		}

		//  make sure that current does not intersect with the fixed interval for reg
		//if current intersects with the fixed interval for reg then
		//	split current before this intersection
		int intersection = firstIntersection(currentIt, live.pint(reg));

		if (intersection != int.max)
			splitBefore(currentId, intersection, unhandled);
	}

	// auto adds new interval to unhandled
	void splitBefore(T)(IntervalIndex it, int before, ref T unhandled)
	{
		IntervalIndex newInterval = live.splitBefore(context, it, before);
		unhandled.insert(newInterval);
	}

	void splitBefore(T)(IntervalIndex it, int before, LiveRangeIndex rangeIndex, ref T unhandled)
	{
		IntervalIndex newInterval = live.splitBefore(context, it, before, rangeIndex);
		unhandled.insert(newInterval);
	}

	void assignSpillSlot(ref StackLayout stackLayout, LiveInterval* it)
	{
		IrIndex type = lir.getValueType(*context, it.definition);
		IrIndex slot = stackLayout.addStackItem(context, type, StackSlotKind.local, 0);
		writefln("assignSpillSlot %s %s", it.definition, slot);
		it.reg = slot;
		// TODO: need to fix instructions to use stack slots after RA
		// TODO: only moves on basic block boundaries are inserted for split intervals. Need to handle intrablock splits
	}

	// Replaces all uses of virtual registers with physical registers or stack slots
	void fixInstructionArgs(FunctionDeclNode* fun)
	{
		// fix uses first, because we may copy arg to definition below
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; lir.virtualRegsiters)
		{
			foreach (size_t i, IrIndex userIndex; vreg.users.range(*lir))
			{
				final switch (userIndex.kind) with(IrValueKind)
				{
					case none, listItem, virtualRegister, physicalRegister, constant, global, basicBlock, stackSlot, type, func, constantAggregate: assert(false);
					case instruction:
						int pos = live.linearIndicies[userIndex];
						IrIndex reg = live.getRegFor(vregIndex, pos, lir);
						foreach (ref IrIndex arg; lir.get!IrInstrHeader(userIndex).args)
							if (arg == vregIndex)
							{
								arg = reg;
							}
						break;
					case phi:
						foreach (size_t i, ref IrPhiArg phiArg; lir.get!IrPhi(userIndex).args(*lir))
							if (phiArg.value == vregIndex)
							{
								IrIndex lastInstr = lir.getBlock(phiArg.basicBlock).lastInstr;
								int pos = live.linearIndicies[lastInstr];
								IrIndex reg = live.getRegFor(vregIndex, pos, lir);
								phiArg.value = reg;
							}
						break;
					case variable: assert(false);
				}
			}
		}

		// fix definitions
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; lir.virtualRegsiters)
		{
			switch(vreg.definition.kind) with(IrValueKind)
			{
				case instruction:
					int pos = live.linearIndicies[vreg.definition];
					IrIndex reg = live.getRegFor(vregIndex, pos, lir);

					IrInstrHeader* instrHeader = &lir.get!IrInstrHeader(vreg.definition);
					instrHeader.result = reg;
					InstrInfo instrInfo = context.machineInfo.instrInfo[instrHeader.op];

					// Insert mov for instructions requiring two-operand form (like x86 xor)
					if (instrInfo.isResultInDst && instrHeader.numArgs == 2)
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

						IrIndex r1 = instrHeader.result;
						IrIndex r2 = instrHeader.args[0];
						IrIndex r3 = instrHeader.args[1];

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
									instrHeader.args[0] = r1;
									instrHeader.args[1] = r2;
								}
								else
								{
									InstrInfo instrInfo2 = context.machineInfo.instrInfo[instrHeader.op];
									writefln("%s %s %s %s %s", cast(Amd64Opcode)instrHeader.op, r1, r2, r3, instrInfo2.isCommutative);
									context.internal_error("Unhandled non-commutative instruction in RA, %s %s",
										context.idString(fun.backendData.name), vreg.definition);
								}
							}
							else // r1 = op r2 r3; all different
							{
								// mov r1 r2
								ExtraInstrArgs extra = {addUsers : false, result : r1};
								InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, r2);
								builder.insertBeforeInstr(vreg.definition, instr.instruction);
								// r1 = op r1 r3
								instrHeader.args[0] = r1;
							}
						}
						else // r2 == r3
						{
							if ( !sameIndexOrPhysReg(r1, r2) )
							{
								// mov r1 r2
								ExtraInstrArgs extra = {addUsers : false, result : r1};
								InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, r2);
								builder.insertBeforeInstr(vreg.definition, instr.instruction);
							}
							// r1 = op r1 r1
							instrHeader.args[0] = r1;
						}

						// validation
						context.assertf(sameIndexOrPhysReg(instrHeader.result, instrHeader.args[0]),
							"two-operand form not ensured res(%s) != arg0(%s)", IrIndexDump(instrHeader.result, *context, *lir), IrIndexDump(instrHeader.args[0], *context, *lir));
					}

					if (instrInfo.isResultInDst && instrHeader.numArgs == 1)
					{
						// Rewrite
						// input: r1 = op r2
						// as
						// output: r1 = r2
						// output: r1 = op r1
						//
						IrIndex r1 = instrHeader.result;
						IrIndex r2 = instrHeader.args[0];

						if ( !sameIndexOrPhysReg(r1, r2) )
						{
							ExtraInstrArgs extra = {addUsers : false, result : r1};
							InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, r2);
							builder.insertBeforeInstr(vreg.definition, instr.instruction);
						}
						instrHeader.args[0] = r1;
					}

					break;
				case phi:
					IrPhi* irPhi = &lir.get!IrPhi(vreg.definition);
					int pos = live.linearIndicies[irPhi.blockIndex];
					IrIndex reg = live.getRegFor(vregIndex, pos, lir);
					irPhi.result = reg;
					break;
				default: assert(false);
			}
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
	void resolve(FunctionDeclNode* fun)
	{
		version(RAPrint_resolve) writefln("resolve");

		MoveSolver moveSolver = MoveSolver(builder, context, fun);
		moveSolver.setup();
		scope(exit) moveSolver.release();

		void onEdge(
			IrIndex predIndex, ref IrBasicBlock predBlock,
			IrIndex succIndex, ref IrBasicBlock succBlock)
		{
			/// Critical edge is edge between predecessor and successor
			/// where predecessor has more that 1 successor and
			/// successor has more than 1 predecessor
			/// Splitting of this edge is done by inserting new basic block on the edge
			/// This new block is then returned
			/// successor list of predecessor is updated as well as predecessor list of successor
			/// Phi functions are not updated
			IrIndex splitCriticalEdge()
			{
				IrIndex newBlock = builder.addBasicBlock;
				version(RAPrint_resolve) writefln("Split critical edge %s -> %s with %s", predIndex, succIndex, newBlock);
				foreach (ref IrIndex succ; predBlock.successors.range(*lir)) {
					if (succ == succIndex) {
						succ = newBlock;
						break;
					}
				}
				foreach (ref IrIndex pred; succBlock.predecessors.range(*lir)) {
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

			// those are already handled at split site
			// we have no linearIndicies for new blocks
			if (predBlock.replacesCriticalEdge || succBlock.replacesCriticalEdge) return;

			int succPos = live.linearIndicies[succIndex];
			int predPos = live.linearIndicies[predBlock.lastInstr];

			moveSolver.onEdge();

			import std.bitmanip : BitArray;
			BitArray succLiveIn = live.bitmap.blockLiveInBits(succIndex, lir);

			//
			version(RAPrint_resolve) writefln("  edge %s -> %s", predIndex, succIndex);
			foreach(size_t index; succLiveIn.bitsSet)
			{
				LiveInterval* interval = live.vint(index); // always returns first part of split intervals
				if (!interval.isSplit) continue; // skip intervals that weren't split

				IrIndex vreg = interval.definition;
				IrIndex succLoc = live.getRegFor(vreg, succPos, lir);
				IrIndex predLoc = live.getRegFor(vreg, predPos, lir);
				if (!predLoc.isDefined) continue; // inteval doesn't exist in pred

				if (succLoc != predLoc)
				{
					version(RAPrint_resolve) writefln("    vreg %s, pred %s succ %s", vreg, IrIndexDump(predLoc, *context, *lir), IrIndexDump(succLoc, *context, *lir));
					IrArgSize argSize = getValueTypeArgSize(vreg, lir, context);
					moveSolver.addMove(predLoc, succLoc, argSize);
				}
			}

			foreach (IrIndex phiIndex, ref IrPhi phi; succBlock.phis(*lir))
			{
				version(RAPrint_resolve) writef("    phi %s res %s", phiIndex, phi.result);
				foreach (size_t arg_i, ref IrPhiArg arg; phi.args(*lir))
				{
					if (arg.basicBlock == predIndex)
					{
						IrIndex moveFrom = arg.value;
						IrIndex moveTo = phi.result;

						version(RAPrint_resolve) writefln(" arg %s %s", arg.basicBlock, arg.value);

						IrArgSize argSize = getValueTypeArgSize(phi.result, lir, context);
						moveSolver.addMove(moveFrom, moveTo, argSize);
					}
				}
			}

			if (moveSolver.numWrittenNodes == 0) return;

			IrIndex movesTarget = predIndex;
			bool addToBack = true;

			if (isCriticalEdge(predBlock, succBlock))
			{
				movesTarget = splitCriticalEdge();
			}
			else if (predBlock.successors.length > 1)
			{
				movesTarget = succIndex;
				addToBack = false;
			}
			else
			{
				context.assertf(predBlock.successors.length == 1,
					"predBlock.successors.length %s", predBlock.successors.length);
			}

			moveSolver.placeMoves(movesTarget);
		}

		foreach (IrIndex succIndex, ref IrBasicBlock succBlock; lir.blocks)
		{
			foreach (IrIndex predIndex; succBlock.predecessors.range(*lir))
			{
				IrBasicBlock* predBlock = &lir.getBlock(predIndex);
				onEdge(predIndex, *predBlock, succIndex, succBlock);
			}
		}

		foreach (IrIndex index, ref IrBasicBlock block; lir.blocks)
			block.removeAllPhis;
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

//version = print_move_resolve;

/// Reorders a set of moves, to produce correct behavior
struct MoveSolver
{
	IrBuilder* builder;
	CompilationContext* context;
	FunctionDeclNode* fun;

	ValueInfo[] stackSlots;
	ValueInfo[] registers;
	ValueInfo anyConstant;
	size_t numWrittenNodes;
	size_t savedBufLength;

	// allocate buffers
	// takes unique ownership of tempBuffer
	void setup()
	{
		savedBufLength = context.tempBuffer.length;

		size_t numRegs = context.machineInfo.registers.length;
		registers = context.allocateTempArray!ValueInfo(cast(uint)numRegs);
		size_t numStackSlots = fun.backendData.stackLayout.slots.length;
		stackSlots = context.allocateTempArray!ValueInfo(cast(uint)numStackSlots);
	}

	void onEdge()
	{
		// we don't care about fields in constant
		anyConstant = ValueInfo();
		numWrittenNodes = 0;
	}

	// releases unique ownership of tempBuffer
	void release()
	{
		context.tempBuffer.length = savedBufLength;

		savedBufLength = 0;
		stackSlots = null;
		registers = null;
		numWrittenNodes = 0;
	}

	ref ValueInfo getInfo(IrIndex index) {
		switch(index.kind) {
			case IrValueKind.constant: return anyConstant;
			case IrValueKind.stackSlot: return stackSlots[index.storageUintIndex];
			case IrValueKind.physicalRegister:  return registers[index.physRegIndex];
			default: context.internal_error("getInfo(%s)", index); assert(false);
		}
	}

	void addMove(IrIndex moveFrom, IrIndex moveTo, IrArgSize argSize)
	{
		assert(moveFrom.isDefined);
		assert(moveTo.isDefined);
		if (moveFrom == moveTo) return;

		getInfo(moveFrom).onRead(moveFrom);
		context.assertf(moveTo.isPhysReg || moveTo.isStackSlot, "moveTo is %s", moveTo.kind);
		context.assertf(!getInfo(moveTo).readFrom.isDefined, "Second write to %s detected", moveTo);
		getInfo(moveTo).onWrite(moveFrom, moveTo, argSize);
		context.tempBuffer.put(moveTo.asUint);
		++numWrittenNodes;
	}

	void placeMoves(IrIndex blockIndex)
	{
		IrIndex[] writtenNodes = cast(IrIndex[])context.tempBuffer.data[$-numWrittenNodes..$];
		size_t i;

		while (writtenNodes.length)
		{
			IrIndex toIndex = writtenNodes[i];
			ValueInfo* to = &getInfo(toIndex);

			void removeCurrent()
			{
				*to = ValueInfo();
				writtenNodes[i] = writtenNodes[$-1];
				--writtenNodes.length;
			}

			IrIndex fromIndex = to.readFrom;

			if (!fromIndex.isDefined)
			{
				// marked as removed node, skip
				removeCurrent();
				if (i >= writtenNodes.length) i = 0;
				continue;
			}

			ValueInfo* from = &getInfo(fromIndex);

			if (to.numReads == 0)
			{
				if (fromIndex.isStackSlot)
				{
					if (toIndex.isStackSlot) // stack slot -> stack slot
					{
						context.internal_error("slot -> slot");
					}
					else // stack slot -> reg
					{
						ExtraInstrArgs extra = { result : toIndex, argSize : to.argSize };
						InstrWithResult instr = builder.emitInstr!LirAmd64Instr_load(extra, fromIndex);
						builder.insertBeforeLastInstr(blockIndex, instr.instruction);
					}
				}
				else
				{
					if (toIndex.isStackSlot) // reg -> stack slot
					{
						ExtraInstrArgs extra = { argSize : to.argSize };
						IrIndex instr = builder.emitInstr!LirAmd64Instr_store(extra, toIndex, fromIndex);
						builder.insertBeforeLastInstr(blockIndex, instr);
					}
					else // reg -> reg
					{
						ExtraInstrArgs extra = { result : toIndex, argSize : to.argSize };
						InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, fromIndex);
						builder.insertBeforeLastInstr(blockIndex, instr.instruction);
					}
				}

				--from.numReads;
				removeCurrent();
			}
			else // to.numReads > 0
			{
				if (from.readFrom.isDefined && from.numReads == 1 && getInfo(from.readFrom).numReads == 1)
				{	// from is non-constant in this scope

					// to <-- from <-- from.readFrom
					// mark from as removed and rewrite as:
					// to <-- from.readFrom

					IrIndex instr = builder.emitInstr!LirAmd64Instr_xchg(ExtraInstrArgs(), fromIndex, from.readFrom);
					builder.insertBeforeLastInstr(blockIndex, instr);

					if (from.readFrom == toIndex)
					{
						// handle case when from.readFrom == to
						// to <-- from <-- to <-- from
						// rewrite as
						// from
						removeCurrent();
					}
					else
					{
						to.readFrom = from.readFrom;
						--from.numReads;
						++i;
					}

					// remove node from processing (we can't use removeInPlace, because we have no index)
					from.readFrom = IrIndex();
				}
				else
				{
					++i;
				}
			}

			if (i >= writtenNodes.length) i = 0;
		}
	}
}

struct ValueInfo
{
	ushort numReads;
	IrArgSize argSize; // used for memory moves
	IrIndex readFrom; // can be null

	void onRead(IrIndex self)
	{
		++numReads;
	}

	void onWrite(IrIndex from, IrIndex self, IrArgSize argSize)
	{
		readFrom = from;
		this.argSize = argSize;
	}
}
