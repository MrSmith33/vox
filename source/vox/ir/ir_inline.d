/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Inliner implementation
module vox.ir.ir_inline;

import vox.all;

// will split basic block and replace call instruction with jump to callee body
// Returns first inserted instruction, with which to continue IR walk
IrIndex inline_call(IrBuilder* builder, IrFunction* calleeIr, IrIndex callInstrIndex, IrIndex callerBlockIndex)
{
	CompilationContext* c = builder.context;
	IrFunction* ir = builder.ir;

	IrIndex calleeEntryBlock = IrIndex(ir.numBasicBlocks + 0, IrValueKind.basicBlock);
	IrIndex calleeExitIndex  = IrIndex(ir.numBasicBlocks + 1, IrValueKind.basicBlock);

	//writefln("inline %s into %s", c.idString(calleeIr.name), c.idString(ir.name));

	// copy buffers of inlined function
	// this will place data of each buffer at the end of buffer for current function
	// update length of current function buffers
	// fix all indices
	appendIrStorage(ir, calleeIr, c);

	// Gather blocks
	IrBasicBlock* callerBlock = ir.getBlock(callerBlockIndex);
	IrIndex callerNextBlock = callerBlock.nextBlock;
	IrBasicBlock* calleeEntry = ir.getBlock(calleeEntryBlock);
	IrIndex calleeBodyIndex = calleeEntry.successors[0, ir];
	IrBasicBlock* calleeBody = ir.getBlock(calleeBodyIndex);
	IrBasicBlock* calleeExit = ir.getBlock(calleeExitIndex);

	// Split basic block before call instruction
	//   part before call remains in the same BB (callerBlockIndex)
	//   part after call is moved into exit block of callee (calleeExitIndex)
	//   callee entry block is dropped

	IrInstrHeader* callInstr = ir.getInstr(callInstrIndex);
	IrIndex[] callArgs = callInstr.args(ir)[1..$];

	// redirect parameter users to call argument
	foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; calleeEntry.instructions(ir))
	{
		if (instrHeader.op == IrOpcode.parameter)
		{
			IrInstr_parameter* param = ir.get!IrInstr_parameter(instrIndex);
			uint paramIndex = param.index(ir);
			IrIndex result = instrHeader.result(ir);
			removeUser(c, ir, callInstrIndex, callArgs[paramIndex]);
			builder.redirectVregUsersTo(result, callArgs[paramIndex]);
			builder.removeVirtualRegister(result);
		}
		else if (instrHeader.op == IrOpcode.jump)
		{
			// noop
		}
		else
		{
			c.internal_error("Unexpected instruction in entry block %s", cast(IrOpcode)instrHeader.op);
		}
	}

	// rewire return value to return register
	if (callInstr.hasResult)
	{
		IrIndex retInstrIndex = calleeExit.lastInstr;
		IrInstrHeader* calleeRetInstr = ir.getInstr(retInstrIndex);

		assert(calleeRetInstr.op == IrOpcode.ret_val);
		IrIndex returnVal = calleeRetInstr.arg(ir, 0);
		removeUser(c, ir, retInstrIndex, returnVal);
		builder.redirectVregUsersTo(callInstr.result(ir), returnVal);
		builder.removeVirtualRegister(callInstr.result(ir));
	}

	// Cache instructions of caller block
	IrIndex instrBeforeCall = ir.prevInstr(callInstrIndex); // may be block if call is firstInstr
	IrIndex callerLastInstr = callerBlock.lastInstr;

	// Cache successors of callerBlock. They will be owerwritten with concat
	IrSmallArray callerSuccessors = callerBlock.successors;

	// Insert callee body instructions insead of call instruction
	concatBlockInstructions(ir, callerBlockIndex, callInstrIndex, calleeBody.firstInstr, calleeBody.lastInstr);

	// If callee exit has single predecessor, then make it callee exit block
	if (calleeExit.predecessors.length == 1)
	{
		calleeExitIndex = calleeExit.predecessors[0, ir];
		calleeExit = ir.getBlock(calleeExitIndex);
	}

	// append instructions after call to the callee exit block, replacing the jump instruction
	// we include the call instruction as a marker of the end of inlined code
	if (calleeExitIndex == calleeBodyIndex)
	{
		// callee consists of a single basic block
		concatBlockInstructions(ir, callerBlockIndex, calleeExit.lastInstr, callInstrIndex, callerLastInstr);
	}
	else
	{
		concatBlockInstructions(ir, calleeExitIndex, calleeExit.lastInstr, callInstrIndex, callerLastInstr);
		// Fix successors and predecessors
		fixBlockSucc(ir, calleeBodyIndex, callerBlockIndex, calleeBody.successors);
		fixBlockSucc(ir, callerBlockIndex, calleeExitIndex, callerSuccessors);
		// Reorder blocks
		makeBlocksSequential(ir, callerBlockIndex, calleeBodyIndex);
		removeBlockFromChain(ir, calleeBody);
		makeBlocksSequential(ir, calleeExitIndex, callerNextBlock);
	}

	// repurpose call instruction as marker for walk loop to look for
	// it triggers removal of currently walked function from the stack of functions
	*ir.getInstr(callInstrIndex) = IrInstrHeader(IrOpcode.inline_marker);

	// Find the next instruction to visit
	if (instrBeforeCall.isBasicBlock) {
		// basic block means that no instructions preceded the call
		// inlined code begins with first instruction
		return callerBlock.firstInstr;
	}
	else {
		// inlined code replaced the call instruction
		return ir.nextInstr(instrBeforeCall);
	}
}
