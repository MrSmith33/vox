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

//version = RAPrint;

/// Does linear scan register allocation.
/// Uses live intervals produced by pass_live_intervals
void pass_linear_scan(ref CompilationContext context) {
	LinearScan linearScan;
	linearScan.context = &context;
	foreach (FunctionDeclNode* fun; context.mod.functions) {
		if (fun.isExternal) continue;
		linearScan.scanFun(fun);
	}
}

struct RegisterState
{
	IrIndex index;
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
		return gpr[reg.storageUintIndex];
	}

	void setup(FunctionDeclNode* fun, MachineInfo* machineInfo)
	{
		allocatableRegs = fun.backendData.callingConvention.allocatableRegs;
		gpr.length = machineInfo.registers.length;

		foreach(i, ref RegisterState reg; gpr)
		{
			reg = RegisterState(machineInfo.registers[i].index);
		}

		foreach(i, reg; allocatableRegs)
		{
			opIndex(reg).isAllocatable = true;
		}

		foreach(i, reg; fun.backendData.callingConvention.calleeSaved)
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
		assert(gpr[reg.storageUintIndex].isAllocatable);
		gpr[reg.storageUintIndex].isUsed = true;
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
	IrBuilder builder;

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

		lir = fun.backendData.lirData;
		builder.beginDup(lir, context);
		livePtr = fun.backendData.liveIntervals;
		physRegs.setup(fun, context.machineInfo);

		scope(exit) {
			//TextSink sink;
			//live.dump(sink);
			//writeln(sink.text);

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

		version(RAPrint) writefln("intervals %s", live.intervals.data[live.numFixedIntervals..$]);

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

		fixInstructionArgs();
		resolve(fun);
		genSaveCalleeSavedRegs(fun.backendData.stackLayout);
		//FuncDumpSettings settings;
		//settings.printBlockFlags = true;
		//settings.handlers = &lirAmd64DumpHandlers;
		//dumpFunction(*lir, *context, settings);
		if (context.validateIr) validateIrFunction(*context, *lir);

		unhandledStorage = unhandled.release;
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
	bool tryAllocateFreeReg(NodeIndex currentId)
	{
		// set freeUntilPos of all physical registers to maxInt
		physRegs.resetFreeUntilPos;

		LiveInterval* currentIt = &live[currentId];
		int currentEnd = live.ranges[currentIt.last].to;

		static struct FreeUntilPos {
			int num;
			void toString(scope void delegate(const(char)[]) sink) {
				if (num == int.max) formattedWrite(sink, "max");
				else formattedWrite(sink, "%s", num);
			}
		}

		// for each interval it in active do
		//     freeUntilPos[it.reg] = 0
		version(RAPrint) write("  active:");
		foreach (rangeId; active.data)
		{
			auto it = live[rangeId];
			assert(it.reg.isDefined);
			physRegs[it.reg].freeUntilPos = 0;
			version(RAPrint) writef(" %s:=0", it.reg);
		}
		version(RAPrint) writeln;

		// for each interval it in inactive intersecting with current do
		//   freeUntilPos[it.reg] = next intersection of it with current
		version(RAPrint) write("  inactive:");
		foreach (rangeId; inactive.data)
		{
			LiveInterval it = live[rangeId];
			assert(it.reg.isDefined);

			version(RAPrint) writef(" %s(%s)", it.reg, FreeUntilPos(physRegs[it.reg].freeUntilPos));

			// if current intersects both active and inactive, freeUntilPos stays 0
			if (physRegs[it.reg].freeUntilPos == 0) continue;

			// in case there is no intersection will return int.max (noop)
			int firstIntersection = live.firstIntersection(currentId, rangeId);
			physRegs[it.reg].freeUntilPos = firstIntersection;

			version(RAPrint) writef(":=%s", FreeUntilPos(firstIntersection));
		}
		version(RAPrint) writeln;

		version(RAPrint) writefln("  Try alloc free reg for %s #%s",
			currentIt.definition, live.ranges[currentId].intervalIndex);

		// reg = register with highest freeUntilPos
		int maxPos = 0;
		IrIndex reg;

		// reg stored in hint
		IrIndex hintReg = currentIt.storageHint;

		const IrIndex[] candidates = physRegs.allocatableRegs;
		version(RAPrint) write("  candidates:");
		foreach (i, IrIndex r; candidates)
		{
			version(RAPrint) writef(" %s:%s", r, FreeUntilPos(physRegs[r].freeUntilPos));

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
			version(RAPrint) writefln("    hint is %s", hintReg);
			if (hintReg.isDefined)
			{
				if (currentEnd < physRegs[hintReg].freeUntilPos)
				{
					// register available for the whole interval
					currentIt.reg = hintReg;
					physRegs.markAsUsed(hintReg);
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
				physRegs.markAsUsed(reg);
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
		foreach(IrIndex user; lir.getVirtReg(it.definition).users.range(*lir))
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
				int firstIntersection = live.firstIntersection(currentRangeId, rangeId);
				physRegs[it.reg].nextUsePos = min(firstIntersection, physRegs[it.reg].nextUsePos);
			}
			else
			{
				physRegs[it.reg].nextUsePos = nextUseAfter(it, currentStart);
			}
			writefln("nextUsePos of %s is %s", rangeId, physRegs[it.reg].nextUsePos);
		}

		// reg = register with highest nextUsePos
		LiveInterval* currentIt = &live[currentRangeId];
		const IrIndex[] candidates = physRegs.allocatableRegs;
		int maxUsePos = 0;
		IrIndex reg;
		foreach (i, IrIndex r; candidates)
		{
			if (physRegs[r].nextUsePos > maxUsePos) {
				maxUsePos = physRegs[r].nextUsePos;
				reg = r;
			}
		}
		// physRegs.markAsUsed(reg);

		context.unreachable;
		assert(false);
	}

	// Replaces all uses of virtual registers with physical registers or stack slots
	void fixInstructionArgs()
	{
		// fix uses first, because we may copy arg to result below
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; lir.virtualRegsiters)
		{
			IrIndex reg = live.getRegFor(vregIndex);
			foreach (size_t i, IrIndex userIndex; vreg.users.range(*lir))
			{
				final switch (userIndex.kind) with(IrValueKind)
				{
					case none, listItem, virtualRegister, physicalRegister, constant, global, basicBlock, stackSlot, type: assert(false);
					case instruction:
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
								phiArg.value = reg;
							}
						break;
					case memoryAddress: assert(false); // TODO
					case variable: assert(false);
				}
			}
		}

		// fix results
		foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; lir.virtualRegsiters)
		{
			IrIndex reg = live.getRegFor(vregIndex);
			switch(vreg.definition.kind) with(IrValueKind)
			{
				case instruction:
					IrInstrHeader* instrHeader = &lir.get!IrInstrHeader(vreg.definition);
					instrHeader.result = reg;
					InstrInfo instrInfo = context.machineInfo.instrInfo[instrHeader.op];

					// Insert mov for instructions requiring two-operand form (like x86 xor)
					if (instrInfo.isTwoOperandForm)
					{
						// Rewrite
						// r1 = r2 op r3
						// as
						// r1 = r2
						// r1 = r1 op r3
						//
						// if r2 != r3
						// {
						//     if r1 == r2 {
						//         "r1 = op r1 r3"
						//     } else if r1 == r3 {
						//         if (op.isCommutative) { // "r1 = op r3 r2"
						//             "r1 = op r1 r2"
						//         } else {
						//             // error
						//             // this case is handled inside liveness analysis
						//             //
						//         }
						//     }
						//     else
						//     {
						//         "mov r1 r2"
						//         "r1 = op r1 r3"
						//     }
						// }
						// else // "r1 = op r2 r2"
						// {
						//     "mov r1 r2" if r1 != r2
						//     "r1 = op r1 r1"
						// }

						IrIndex r1 = instrHeader.result;
						IrIndex r2 = instrHeader.args[0];
						IrIndex r3 = instrHeader.args[1];
						if (r2 != r3)
						{
							if (r1 == r2)
							{
								// "r1 = op r1 r3", noop
							}
							else if (r1 == r3) // r1 = op r2 r1
							{
								if (instrInfo.isCommutative) // r1 = op r1 r2
								{
									instrHeader.args[0] = r1;
									instrHeader.args[1] = r2;
								}
								else
								{
									context.internal_error("Unhandled non-commutative instruction in RA");
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
							if (r1 != r2)
							{
								ExtraInstrArgs extra = {addUsers : false, result : r1};
								InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, r2);
								builder.insertBeforeInstr(vreg.definition, instr.instruction);
							}
							instrHeader.args[0] = r1;
						}

						// validation
						context.assertf(instrHeader.result == instrHeader.args[0],
							"two-operand form not ensured res(%s) != arg0(%s)", instrHeader.result, instrHeader.args[0]);
					}

					break;
				case phi: lir.get!IrPhi(vreg.definition).result = reg; break;
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
		version(RAPrint) writefln("resolve");

		MoveSolver moveSolver = MoveSolver(&builder, context, fun);
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
				version(RAPrint) writefln("Split critical edge %s -> %s", predIndex, succIndex);
				IrIndex newBlock = builder.addBasicBlock;
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
				lir.getBlock(newBlock).predecessors.append(&builder, predIndex);
				lir.getBlock(newBlock).successors.append(&builder, succIndex);
				lir.getBlock(newBlock).isSealed = true;
				return newBlock;
			}

			moveSolver.onEdge();
			version(RAPrint) writefln("  edge %s -> %s", predIndex, succIndex);
			foreach (IrIndex phiIndex, ref IrPhi phi; succBlock.phis(*lir))
			{
				version(RAPrint) writef("    phi %s res %s", phiIndex, phi.result);
				foreach (size_t arg_i, ref IrPhiArg arg; phi.args(*lir))
				{
					if (arg.basicBlock == predIndex)
					{
						IrIndex moveFrom = arg.value;
						IrIndex moveTo = phi.result;

						version(RAPrint) writefln(" arg %s %s", arg.basicBlock, arg.value);

						moveSolver.addMove(moveFrom, moveTo);
					}
				}
			}

			IrIndex movesTarget = predIndex;
			bool addToBack = true;

			if (isCriticalEdge(predBlock, succBlock))
			{
				movesTarget = splitCriticalEdge();
				builder.emitInstr!LirAmd64Instr_jmp(movesTarget);
				lir.getBlock(movesTarget).isFinished = true;
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
			if (!succBlock.hasPhis) continue;

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
				IrIndex slot = stackLayout.addStackItem(context, makeBasicTypeIndex(IrValueType.i64), false, 0);

				// save register
				IrIndex instrStore = builder.emitInstr!LirAmd64Instr_store(ExtraInstrArgs(), slot, reg.index);
				builder.prependBlockInstr(entryBlock, instrStore);

				// restore register
				ExtraInstrArgs extra = {result : reg.index};
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
	FixedBuffer!IrIndex writtenNodesBuf;
	uint savedBufLength;

	// allocate buffers
	// takes unique ownership of tempBuffer
	void setup()
	{
		savedBufLength = context.tempBuffer.length;

		size_t numRegs = context.machineInfo.registers.length;
		registers = context.allocateTempArray!ValueInfo(cast(uint)numRegs);
		size_t numStackSlots = fun.backendData.stackLayout.slots.length;
		stackSlots = context.allocateTempArray!ValueInfo(cast(uint)numStackSlots);
		writtenNodesBuf.setBuffer(cast(ubyte[])context.tempBuffer.freePart);
	}

	void onEdge()
	{
		// we don't care about fields in constant
		anyConstant = ValueInfo();
		writtenNodesBuf.length = 0;
	}

	// releases unique ownership of tempBuffer
	void release()
	{
		context.tempBuffer.length = savedBufLength;

		savedBufLength = 0;
		stackSlots = null;
		registers = null;
		writtenNodesBuf = FixedBuffer!IrIndex();
	}

	ref ValueInfo getInfo(IrIndex loc) {
		switch(loc.kind) {
			case IrValueKind.constant: return anyConstant;
			case IrValueKind.stackSlot: return stackSlots[loc.storageUintIndex];
			case IrValueKind.physicalRegister:  return registers[loc.storageUintIndex];
			default: assert(false);
		}
	}

	void addMove(IrIndex moveFrom, IrIndex moveTo)
	{
		if (moveFrom == moveTo) return;

		getInfo(moveFrom).onRead(moveFrom);
		context.assertf(moveTo.isPhysReg || moveTo.isStackSlot, "moveTo is %s", moveTo.kind);
		context.assertf(!getInfo(moveTo).readFrom.isDefined, "Second write to %s detected", moveTo);
		getInfo(moveTo).onWrite(moveFrom, moveTo);
		writtenNodesBuf.put(moveTo);
	}

	void placeMoves(IrIndex blockIndex)
	{
		IrIndex[] writtenNodes = writtenNodesBuf.data;
		size_t i;

		while (writtenNodes.length)
		{
			IrIndex toIndex = writtenNodes[i];
			ValueInfo* to = &getInfo(toIndex);
			IrIndex fromIndex = to.readFrom;
			ValueInfo* from = &getInfo(fromIndex);

			void removeCurrent()
			{
				*to = ValueInfo();
				if (i+1 != writtenNodes.length)
				{
					writtenNodes[i] = writtenNodes[$-1];
				}
				--writtenNodes.length;
			}

			if (!fromIndex.isDefined)
			{
				// marked as removed node, skip
				removeCurrent();
			}
			else if (to.numReads == 0)
			{
				ExtraInstrArgs extra = {result : toIndex};
				InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(extra, fromIndex);
				builder.insertBeforeLastInstr(blockIndex, instr.instruction);

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
					++i;
			}

			if (i >= writtenNodes.length) i = 0;
		}
	}
}

struct ValueInfo
{
	uint numReads;
	IrIndex readFrom; // can be null

	void onRead(IrIndex self)
	{
		++numReads;
	}

	void onWrite(IrIndex from, IrIndex self)
	{
		readFrom = from;
	}
}
