/// Copyright: Copyright (c) 2018-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Given LIR produces a set of live intervals and "live in" bitmap
/// Implementation of:
/// "Linear Scan Register Allocation on SSA Form"
// TODO: backward propagation of hints
module be.reg_alloc.liveness_analysis;

import all;

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
	lirData.orderBlocks;
	lirData.assignSequentialBlockIndices;
	//dumpFunction(context, lirData, "Live");
	pass_live_intervals_func(context, lirData, liveness);

	if (context.printLiveIntervals && context.printDumpOf(fun))
	{
		liveness.dump(context, lirData);

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
			liveness : liveness,
			passName : "Live"
		};
		dumpFunction(&dumpCtx);
	}
}

void pass_live_intervals_func(CompilationContext* context, IrFunction* ir, LivenessInfo* liveness)
{
	liveness.initStorage(context, ir);
	liveness.assignSequentialIndices(context, ir);
	liveness.setIntervalUsesLength(context, ir);

	void liveAdd(IrIndex someOperand)
	{
		context.assertf(someOperand.isVirtReg, "not vreg, but %s", someOperand.kind);
		version(LivePrint) writefln("[LIVE] liveAdd %s #%s", someOperand, someOperand.storageUintIndex);
		liveness.bitmap.liveBuckets.setBitAt(someOperand.storageUintIndex);
	}

	void liveRemove(IrIndex someOperand)
	{
		context.assertf(someOperand.isVirtReg, "not vreg, but %s", someOperand.kind);
		version(LivePrint) writefln("[LIVE] liveRemove %s #%s", someOperand, someOperand.storageUintIndex);
		liveness.bitmap.liveBuckets.resetBitAt(someOperand.storageUintIndex);
	}

	// algorithm start
	// for each block b in reverse order do
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		// Is also where phi functions are located
		uint blockFromPos = liveness.linearIndices.basicBlock(blockIndex);
		uint blockToPos = liveness.linearIndices.instr(block.lastInstr);
		version(LivePrint) writefln("[LIVE] % 3s %s", blockFromPos, blockIndex);

		// live = union of successor.liveIn for each successor of block
		liveness.bitmap.liveBuckets[] = 0;
		foreach (IrIndex succIndex; block.successors.range(ir))
		{
			foreach (size_t i, size_t bucket; liveness.bitmap.blockLiveInBuckets(succIndex))
				liveness.bitmap.liveBuckets[i] |= bucket;
		}

		// for each phi function phi of successors of block do
		//     live.add(phi.inputOf(block))
		foreach (IrIndex succIndex; block.successors.range(ir))
		{
			foreach (IrIndex phiIndex, ref IrPhi phi; ir.getBlock(succIndex).phis(ir))
			{
				IrIndex[] phiPreds = ir.getBlock(phi.blockIndex).predecessors.data(ir);
				foreach (i, ref IrIndex phiArg; phi.args(ir))
				{
					if (phiPreds[i] == blockIndex)
					{
						if (phiArg.isVirtReg)
						{
							liveAdd(phiArg);
							LiveInterval* it = liveness.vint(phiArg);
							it.prependUse(UsePosition(blockToPos+2, UseKind.phi));
						}
					}
				}
			}
		}

		//writef("in %s %s live:", blockIndex, liveness.linearIndices.basicBlock(blockIndex));
		//foreach (size_t index; liveness.bitmap.live.bitsSet)
		//	writef(" %s", index);
		//writeln;

		// for each opd in live do
		foreach (size_t index; liveness.bitmap.liveBuckets.bitsSet)
		{
			// intervals[opd].addRange(block.from, block.to)
			liveness.vint(index).addRange(context, blockFromPos, blockToPos+2);
			version(LivePrint) writefln("[LIVE] addRange vreg.#%s [%s; %s)", index,
				blockFromPos, blockToPos);
		}

		// for each operation op of b in reverse order do
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(ir))
		{
			IrIndex result = instrHeader.tryGetResult(ir); // nullable
			IrIndex[] args = instrHeader.args(ir);

			uint linearInstrIndex = liveness.linearIndices.instr(instrIndex);
			version(LivePrint) writefln("[LIVE]   % 3s %s", linearInstrIndex, instrIndex);

			InstrInfo instrInfo = context.machineInfo.instrInfo[instrHeader.op];
			//writefln("isMov %s %s", cast(Amd64Opcode)instrHeader.op, instrInfo.isMov);
			// -------------- Assign interval hints --------------
			if (instrInfo.isMov) {
				IrIndex from = args[0];
				IrIndex to = result;
				if (from.isPhysReg && to.isVirtReg) {
					liveness.vint(to).storageHint = from;
				} else if (from.isVirtReg && to.isPhysReg) {
					liveness.vint(from).storageHint = to;
				}
			}

			// for each output operand opd of op do
			if (instrHeader.hasResult) {
				if (result.isVirtReg) {
					liveness.get(result).setFrom(context, linearInstrIndex);
					LiveInterval* it = liveness.vint(result);
					it.regClass = ir.getValueType(context, it.definition).isTypeFloat ? 1 : 0;
					it.prependUse(UsePosition(linearInstrIndex, UseKind.instruction));
					liveRemove(result);
				} else if (result.isPhysReg && !instrInfo.isMov) {
					uint physRegResultOffset = linearInstrIndex+ENUM_STEP;
					// needed to account for sub/add rsp instructions around call
					if (instrHeader.extendFixedResultRange)
						physRegResultOffset += ENUM_STEP;
					// non-mov, extend fixed interval to the next instr (which must be mov from that phys reg)
					liveness.pint(result).addRange(context, linearInstrIndex, physRegResultOffset);
				}
			}

			//               HANDLE IMPLICIT ARGS
			// if non-mov instruction assigns to phys register,
			// movs must follow instruction immediately matching the order of results
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
			uint physRegArgsOffset = 0;
			foreach(IrIndex arg; args) {
				if (arg.isVirtReg) {
					LiveInterval* it = liveness.vint(arg);
					it.addRange(context, blockFromPos, linearInstrIndex);
					it.prependUse(UsePosition(linearInstrIndex, UseKind.instruction));
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
				IrIndex arg0 = args[0];
				IrIndex arg1 = args[1];
				if ( !sameIndexOrPhysReg(arg0, arg1) ) {
					if (arg1.isVirtReg || arg1.isPhysReg) {
						liveness.get(arg1).addRange(context, blockFromPos, linearInstrIndex+1);
					}
				}
			}

			if (!instrInfo.isMov)
			{
				foreach(IrIndex arg; args)
				{
					if (arg.isPhysReg)
					{
						// non-mov, extend fixed interval to the preceding mov instr
						liveness.pint(arg).addRange(context, linearInstrIndex - physRegArgsOffset, linearInstrIndex);
						physRegArgsOffset -= ENUM_STEP;
					}
				}
			}

			// add fixed intervals for function calls
			if (instrInfo.isCall)
			{
				IrIndex callee = args[0];
				CallConv* cc = context.types.getCalleeCallConv(callee, ir, context);
				FullRegSet volatileRegs = cc.volatileRegs;
				foreach(PhysReg reg; volatileRegs) {
					liveness.pint(reg).addRange(context, linearInstrIndex, linearInstrIndex+1);
				}
			}
		}

		// for each phi function phi of b do
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			// live.remove(phi.output)
			if (phi.result.isVirtReg) {
				liveRemove(phi.result);
				liveness.get(phi.result).setFrom(context, blockFromPos);
				LiveInterval* it = liveness.vint(phi.result);
				it.regClass = ir.getValueType(context, it.definition).isTypeFloat ? 1 : 0;
			}
		}

		// if b is loop header then
		if (block.isLoopHeader)
		{
			// We need to find the loop block with the max position
			// Use loop header as starting block in case it is in max position
			uint maxPos = blockToPos+2;
			IrIndex loopEnd = blockIndex;
			//     loopEnd = last block of the loop starting at b
			foreach(IrIndex pred; block.predecessors.range(ir)) {
				uint blockEndPos = liveness.linearIndices[ir.getBlock(pred).lastInstr]+2;
				if (blockEndPos > maxPos) {
					maxPos = blockEndPos;
					loopEnd = pred;
				}
			}

			if (loopEnd != blockIndex) // skip if header jumps to itself
			for (IrIndex loopBlockIndex = block.nextBlock;;)
			{
				IrBasicBlock* loopBlock = ir.getBlock(loopBlockIndex);
				size_t[] liveIns = liveness.bitmap.blockLiveInBuckets(loopBlockIndex);

				// add live in of loop header to all blocks of the loop
				liveIns[] |= liveness.bitmap.liveBuckets[];

				if (loopBlockIndex == loopEnd) break;
				loopBlockIndex = loopBlock.nextBlock;
			}

			//     for each opd in live do
			//         intervals[opd].addRange(b.from, loopEnd.to)
			foreach (size_t index; liveness.bitmap.liveBuckets.bitsSet)
			{
				liveness.vint(index).addRange(context, blockFromPos, maxPos);
			}
		}

		// b.liveIn = live
		size_t[] liveIns = liveness.bitmap.blockLiveInBuckets(blockIndex);
		liveIns[] = liveness.bitmap.liveBuckets;
	}

	// create ranges for parameter registers in start block
	foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; ir.getBlock(ir.entryBasicBlock).instructions(ir))
	{
		InstrInfo instrInfo = context.machineInfo.instrInfo[instrHeader.op];
		if (instrInfo.isMov) {
			IrIndex from = instrHeader.arg(ir, 0);
			if (from.isPhysReg) {
				uint linearInstrIndex = liveness.linearIndices.instr(instrIndex);
				liveness.pint(from).addRange(context, 1, linearInstrIndex);
			}
			IrIndex result = instrHeader.result(ir);
			if (result.isVirtReg && from.isSomeReg)
			{
				liveness.vint(result).storageHint = from;
			}
		}
	}

	// Reset length from 0 to actual length
	liveness.resetIntervalUsesLength(context, ir);

	//if (context.validateIr)
	//foreach(interval; liveness.intervals) {
	//	LiveRange prev;
	//	foreach(i, range; interval.ranges) {
	//		context.assertf(range.from < range.to, "Invalid range %s [%s, %s)", i, range.from, range.to);
	//		if (i == 0) continue;
	//		context.assertf(prev.to < range.from, "Ranges are not sequential %s [%s, %s) %s [%s, %s)", i-1, prev.from, prev.to, i, range.from, range.to);
	//	}
	//}
}
