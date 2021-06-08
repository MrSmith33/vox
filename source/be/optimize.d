/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module be.optimize;

import std.stdio;
import all;

alias FuncPassIr = void function(CompilationContext*, IrFunction*, IrIndex, ref IrBuilder);
alias FuncPass = void function(CompilationContext*, IrFunction*);

void apply_lir_func_pass(CompilationContext* context, FuncPass pass, string passName)
{
	foreach (ref SourceFileInfo file; context.files.data)
	foreach (IrFunction* lir; file.mod.lirModule.functions) {
		pass(context, lir);
		if (context.validateIr)
			validateIrFunction(context, lir, passName);
	}
}

void pass_optimize_ir(ref CompilationContext c, ref ModuleDeclNode mod, ref FunctionDeclNode func)
{
	if (func.isExternal) return;

	FuncPassIr dcePass = &func_pass_remove_dead_code;
	if (c.disableDCE) dcePass = null;

	FuncPassIr[3] passes = [
		c.disableInline ? null : &func_pass_inline,
		&func_pass_invert_conditions,
		dcePass,
	];

	IrBuilder builder;

	IrFunction* irData = c.getAst!IrFunction(func.backendData.irData);
	func.backendData.optimizedIrData = c.appendAst!IrFunction;
	IrFunction* optimizedIrData = c.getAst!IrFunction(func.backendData.optimizedIrData);
	*optimizedIrData = *irData; // copy

	builder.beginDup(optimizedIrData, &c);

	IrIndex funcIndex = func.getIrIndex(&c);

	foreach (FuncPassIr pass; passes) {
		if (pass is null) continue;
		pass(&c, optimizedIrData, funcIndex, builder);
		if (c.validateIr)
			validateIrFunction(&c, optimizedIrData);
		if (c.printIrOptEach && c.printDumpOf(&func)) dumpFunction(&c, optimizedIrData, "IR opt");
	}
	if (!c.printIrOptEach && c.printIrOpt && c.printDumpOf(&func)) dumpFunction(&c, optimizedIrData, "IR opt all");
	builder.finalizeIr;
}

void func_pass_inline(CompilationContext* c, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	IrIndex* inlineStack = cast(IrIndex*)c.tempBuffer.nextPtr;
	uint inlineStackLen = 0;
	void pushFunc(IrIndex index) {
		c.tempBuffer.put(index.asUint);
		++inlineStackLen;
	}
	void popFunc() {
		c.tempBuffer.unput(1);
		--inlineStackLen;
	}
	bool isOnStack(IrIndex index)
	{
		foreach(IrIndex slot; inlineStack[0..inlineStackLen])
			if (slot == index) return true;
		return false;
	}
	pushFunc(funcIndex);

	IrIndex blockIndex = ir.entryBasicBlock;
	while (blockIndex.isDefined)
	{
		IrBasicBlock* block = ir.getBlock(blockIndex);
		IrIndex instrIndex = block.firstInstr;

		while (instrIndex.isInstruction)
		{
			IrIndex nextInstr = ir.nextInstr(instrIndex);
			IrInstrHeader* instrHeader = ir.getInstr(instrIndex);

			void try_inline()
			{
				if (!instrHeader.alwaysInline) return; // inlining is not requested for this call

				IrIndex calleeIndex = instrHeader.arg(ir, 0);
				if (!calleeIndex.isFunction) return; // cannot inline indirect calls

				FunctionDeclNode* callee = c.getFunction(calleeIndex);
				if (callee.isExternal) return; // cannot inline external functions

				if (isOnStack(calleeIndex)) return; // recursive call

				IrFunction* calleeIr = c.getAst!IrFunction(callee.backendData.irData);

				// Inliner returns the next instruction to visit
				// We will visit inlined code next
				nextInstr = inline_call(&builder, calleeIr, instrIndex, blockIndex);
				pushFunc(calleeIndex);
			}

			switch(cast(IrOpcode)instrHeader.op)
			{
				case IrOpcode.call: try_inline(); break;
				case IrOpcode.inline_marker:
					removeInstruction(ir, instrIndex);
					popFunc;
					break;
				default:
					break;
			}

			instrIndex = nextInstr;
		}

		blockIndex = block.nextBlock;
	}
	popFunc;
}

void func_pass_invert_conditions(CompilationContext* context, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	ir.assignSequentialBlockIndices();

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		if (!block.lastInstr.isDefined) continue;

		IrInstrHeader* instrHeader = ir.getInstr(block.lastInstr);
		ubyte invertedCond;

		switch(instrHeader.op) with(IrOpcode)
		{
			case branch_unary:
				invertedCond = invertUnaryCond(cast(IrUnaryCondition)instrHeader.cond);
				break;
			case branch_binary:
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

void func_pass_remove_dead_code(CompilationContext* context, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(ir))
		{
			if (hasSideEffects(cast(IrOpcode)instrHeader.op)) continue;
			if (!instrHeader.hasResult) continue;

			if (!instrHeader.result(ir).isVirtReg) continue;
			if (ir.getVirtReg(instrHeader.result(ir)).users.length > 0) continue;

			// instruction's result is unused, remove that instruction
			foreach(ref IrIndex arg; instrHeader.args(ir)) {
				removeUser(context, ir, instrIndex, arg);
			}
			removeInstruction(ir, instrIndex);
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
					if (src.isConstant && context.constants.get(src).i64 == 0)
					{

					}
				default: break;
			}
		}
	}
}
*/
void pass_optimize_lir(CompilationContext* context)
{
	apply_lir_func_pass(context, &pass_optimize_lir_func, "Optimize LIR");
}

void pass_optimize_lir_func(CompilationContext* context, IrFunction* ir)
{
	ir.assignSequentialBlockIndices();

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		if (!block.lastInstr.isDefined) continue;

		IrInstrHeader* instrHeader = ir.getInstr(block.lastInstr);
		auto isJump = context.machineInfo.instrInfo[instrHeader.op].isJump;

		if (isJump)
		{
			uint seqIndex0 = ir.getBlock(block.successors[0, ir]).seqIndex;
			// successor is the next instruction after current block
			if (block.seqIndex + 1 == seqIndex0)
			{
				removeInstruction(ir, block.lastInstr);
			}
		}
	}
}
