/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module optimize;

import std.stdio;
import all;

alias FuncPass = void function(ref CompilationContext, ref IrFunction);

void apply_ir_func_pass(ref CompilationContext context, FuncPass pass)
{
	foreach (FunctionDeclNode* fun; context.mod.functions) {
		pass(context, *fun.irData);
		if (context.validateIr)
			validateIrFunction(context, *fun.irData);
	}
}

void apply_lir_func_pass(ref CompilationContext context, FuncPass pass)
{
	foreach (FunctionDeclNode* fun; context.mod.functions) {
		pass(context, *fun.lirData);
		if (context.validateIr)
			validateIrFunction(context, *fun.lirData);
	}
}

void pass_optimize_ir(ref CompilationContext context)
{
	apply_ir_func_pass(context, &func_pass_invert_conditions);
	apply_ir_func_pass(context, &func_pass_remove_dead_code);
}

void func_pass_invert_conditions(ref CompilationContext context, ref IrFunction ir)
{
	ir.assignSequentialBlockIndices();

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
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

void func_pass_remove_dead_code(ref CompilationContext context, ref IrFunction ir)
{
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(ir))
		{
			if (hasSideEffects(cast(IrOpcode)instrHeader.op)) continue;
			if (!instrHeader.hasResult) continue;

			context.assertf(instrHeader.result.isVirtReg, "instruction result must be virt reg");
			if (ir.getVirtReg(instrHeader.result).users.length > 0) continue;

			// we found some dead instruction, remove it
			foreach(ref IrIndex arg; instrHeader.args) {
				removeUser(ir, instrIndex, arg);
			}
			removeInstruction(ir, blockIndex, instrIndex);
			//writefln("remove dead %s", instrIndex);
		}
	}
}

/*
void lir_func_pass_simplify(ref CompilationContext context, ref IrFunction ir)
{
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(ir))
		{
			switch(cast(Amd64Opcode)instrHeader.op) with(Amd64Opcode)
			{
				case mov:
					static assert(LirAmd64Instr_xor.sizeof == LirAmd64Instr_mov.sizeof);
					// replace 'mov reg, 0' with xor reg reg
					IrIndex dst = instrHeader.result;
					IrIndex src = instrHeader.args[0];
					if (src.isConstant && context.getConstant(src).i64 == 0)
					{

					}
				default: break;
			}
		}
	}
}
*/
void pass_optimize_lir(ref CompilationContext context)
{
	apply_lir_func_pass(context, &pass_optimize_lir_func);
}

void pass_optimize_lir_func(ref CompilationContext context, ref IrFunction ir)
{
	ir.assignSequentialBlockIndices();

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		if (!block.lastInstr.isDefined) continue;

		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(block.lastInstr);
		auto isJump = context.machineInfo.instrInfo[instrHeader.op].isJump;

		if (isJump)
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
