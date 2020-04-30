/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Inliner implementation
module ir.ir_inline;

import all;

// will split basic block and replace call instruction with jump to callee body
// TODO: call graph is needed to detect recursive calls
void inline_call(IrBuilder* builder, IrFunction* calleeIr, IrIndex callInstrIndex, IrIndex callerBlockIndex)
{
	CompilationContext* c = builder.context;
	IrFunction* ir = builder.ir;

	IrIndex calleeEntryBlock = IrIndex(ir.numBasicBlocks + 0, IrValueKind.basicBlock);
	IrIndex calleeExitBlock  = IrIndex(ir.numBasicBlocks + 1, IrValueKind.basicBlock);

	//writefln("inline %s into %s", c.idString(calleeIr.backendData.name), c.idString(ir.backendData.name));

	// copy buffers of inlined function
	// this will place data of each buffer at the end of buffer for current function
	// update length of current function buffers
	// fix all indicies
	IrFunction calleeIrCopy = *calleeIr;
	appendIrStorage(ir, calleeIr, c);

	// split basic block before call instruction
	//   part before call remains in the same BB (callerBlockIndex)
	//   part after call is moved into exit block of callee (calleeExitBlock)
	//   callee entry block is dropped

	IrInstrHeader* callInstr = ir.getInstr(callInstrIndex);
	IrIndex[] callArgs = callInstr.args(ir)[1..$];

	IrBasicBlock* calleeEntry = ir.getBlock(calleeEntryBlock);
	foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; calleeEntry.instructions(ir))
	{
		if (instrHeader.op == IrOpcode.parameter)
		{
			// redirect parameter users to call argument
			IrInstr_parameter* param = ir.get!IrInstr_parameter(instrIndex);
			uint paramIndex = param.index(ir);
			IrIndex result = instrHeader.result(ir);
			builder.redirectVregUsersTo(result, callArgs[paramIndex]);
			removeUser(c, ir, callInstrIndex, callArgs[paramIndex]);
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

	IrBasicBlock* calleeExit = ir.getBlock(calleeExitBlock);
	IrIndex retInstrIndex = calleeExit.lastInstr;
	IrInstrHeader* calleeRetInstr = ir.getInstr(retInstrIndex);

	// rewire return value to return register
	if (callInstr.hasResult)
	{
		assert(calleeRetInstr.op == IrOpcode.ret_val);
		IrIndex returnVal = calleeRetInstr.arg(ir, 0);
		builder.redirectVregUsersTo(callInstr.result(ir), returnVal);
		removeUser(c, ir, retInstrIndex, returnVal);
		builder.removeVirtualRegister(callInstr.result(ir));
	}

	// replace call instruction with jump
	*callInstr = IrInstrHeader(IrOpcode.jump);

	IrBasicBlock* callerBlock = ir.getBlock(callerBlockIndex);

	// callee exit block inherits all instructions after the call
	// return instruction is dropped
	calleeExit.lastInstr = callerBlock.lastInstr;
	calleeExit.firstInstr = ir.nextInstr(callInstrIndex);
	ir.nextInstr(callInstrIndex) = callerBlockIndex;
	callerBlock.lastInstr = callInstrIndex;

	// Fix successors and predecessors
	calleeExit.successors = callerBlock.successors;
	foreach (ref IrIndex succ; callerBlock.successors.range(ir)) {
		ir.getBlock(succ).predecessors.replaceFirst(ir, callerBlockIndex, calleeExitBlock);
	}
	callerBlock.successors = calleeEntry.successors;
	IrIndex calleeBodyIndex = calleeEntry.successors[0, ir];
	ir.getBlock(calleeBodyIndex).predecessors[0, ir] = callerBlockIndex;

	// reorder blocks
	IrIndex callerNextBlock = callerBlock.nextBlock;
	makeBlocksSequential(ir, callerBlockIndex, calleeBodyIndex);
	makeBlocksSequential(ir, calleeExitBlock, callerNextBlock);
}
