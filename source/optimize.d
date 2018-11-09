/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module optimize;

import all;

alias FuncPass = void function(ref CompilationContext, ref IrFunction);

void apply_ir_func_pass(ref CompilationContext context, FuncPass pass)
{
	foreach (FunctionDeclNode* fun; context.mod.functions) pass(context, *fun.irData);
}

void apply_lir_func_pass(ref CompilationContext context, FuncPass pass)
{
	foreach (FunctionDeclNode* fun; context.mod.functions) pass(context, *fun.lirData);
}

void pass_optimize_ir(ref CompilationContext context)
{
	apply_ir_func_pass(context, &pass_optimize_ir_func);
}

void pass_optimize_ir_func(ref CompilationContext context, ref IrFunction ir)
{
	ir.assignSequentialBlockIndices();

	foreach (IrIndex blockIndex, ref IrBasicBlockInstr block; ir.blocks)
	{
		if (!block.lastInstr.isDefined) continue;

		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(block.lastInstr);
		ubyte invertedCond;

		switch(instrHeader.op) with(IrOpcode)
		{
			case block_exit_unary_branch:
				invertedCond = invertUnaryCond(cast(IrUnaryCondition)instrHeader.cond);
				break;
			case block_exit_binary_branch:
				invertedCond = invertBinaryCond(cast(IrBinaryCondition)instrHeader.cond);
				break;

			default: continue;
		}

		uint seqIndex0 = ir.getBlock(block.successors[0, ir]).seqIndex;
		uint seqIndex1 = ir.getBlock(block.successors[1, ir]).seqIndex;
		if (block.seqIndex + 1 == seqIndex0)
		{
			instrHeader.cond = invertedCond;
			IrIndex succIndex0 = block.successors[0, ir];
			IrIndex succIndex1 = block.successors[1, ir];
			block.successors[0, ir] = succIndex1;
			block.successors[1, ir] = succIndex0;
		}
	}
}

void pass_optimize_lir(ref CompilationContext context)
{
	apply_lir_func_pass(context, &pass_optimize_lir_func);
}

void pass_optimize_lir_func(ref CompilationContext context, ref IrFunction ir)
{
	ir.assignSequentialBlockIndices();

	foreach (IrIndex blockIndex, ref IrBasicBlockInstr block; ir.blocks)
	{
		if (!block.lastInstr.isDefined) continue;

		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(block.lastInstr);
		auto flags = context.machineInfo.instrInfo[instrHeader.op].flags;

		if (flags & LirInstrFlags.isJump)
		{
			uint seqIndex0 = ir.getBlock(block.successors[0, ir]).seqIndex;
			// successor is the next instruction after current block
			if (block.seqIndex + 1 == seqIndex0)
			{
				removeInstruction(ir, blockIndex, block.lastInstr);
			}
		}
	}
}
