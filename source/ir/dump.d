/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Convertion of function IR to textual representation
module ir.dump;

import std.stdio;

import all;

struct InstrPrintInfo
{
	CompilationContext* context;
	TextSink* sink;
	IrFunction* ir;
	IrIndex blockIndex;
	IrBasicBlock* block;
	IrIndex instrIndex;
	IrInstrHeader* instrHeader;
	FuncDumpSettings* settings;
	void dumpInstr() { settings.handlers.instrDumper(this); }
	void dumpIndex(IrIndex i) { settings.handlers.indexDumper(this, i); }
}

struct IrDumpHandlers
{
	InstructionDumper instrDumper;
	IrIndexDumper indexDumper;
}

alias InstructionDumper = void function(ref InstrPrintInfo p);
alias IrIndexDumper = void function(ref InstrPrintInfo, IrIndex);

struct FuncDumpSettings
{
	bool printVars = false;
	bool printBlockFlags = false;
	bool printBlockIns = true;
	bool printBlockOuts = false;
	bool printBlockRefs = false;
	bool printInstrIndexEnabled = true;
	bool printUses = true;
	bool printLive = true;
	IrDumpHandlers* handlers;
	void dumpInstr(ref InstrPrintInfo p) { handlers.instrDumper(p); }
	void dumpIndex(ref InstrPrintInfo p, IrIndex i) { handlers.indexDumper(p, i); }
}

IrDumpHandlers irDumpHandlers = IrDumpHandlers(&dumpIrInstr, &dumpIrIndex);

void dumpFunction_ir(ref IrFunction ir, ref CompilationContext ctx)
{
	FuncDumpSettings settings;
	settings.handlers = &irDumpHandlers;
	dumpFunction(ir, ctx, settings);
}

void dumpFunction(ref IrFunction ir, ref CompilationContext ctx, ref FuncDumpSettings settings)
{
	TextSink sink;
	dumpFunction(ir, sink, ctx, settings);
	writeln(sink.text);
}

void dumpFunction(ref IrFunction ir, ref TextSink sink, ref CompilationContext ctx, ref FuncDumpSettings settings)
{
	sink.put("function ");
	sink.put(ctx.idString(ir.name));
	sink.putfln("() %s bytes {", ir.storageLength * uint.sizeof);
	int indexPadding = numDigitsInNumber(ir.storageLength);

	void printInstrIndex(IrIndex someIndex) {
		if (!settings.printInstrIndexEnabled) return;
		sink.putf("%*s|", indexPadding, someIndex.storageUintIndex);
	}

	void printRegUses(IrIndex result) {
		if (result.kind == IrValueKind.physicalRegister) return;
		auto vreg = &ir.getVirtReg(result);
		sink.put(" users [");
		foreach (i, index; vreg.users.range(ir))
		{
			if (i > 0) sink.put(", ");
			sink.putf("%s", index);
		}
		sink.put("]");
	}

	InstrPrintInfo printer;
	printer.context = &ctx;
	printer.sink = &sink;
	printer.ir = &ir;
	printer.settings = &settings;

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		printer.blockIndex = blockIndex;
		printer.block = &block;

		printInstrIndex(blockIndex);
		sink.putf("  %s", blockIndex);

		if (settings.printBlockFlags)
		{
			if (block.isSealed) sink.put(" S");
			else sink.put(" .");

			if (block.isFinished) sink.put("F");
			else sink.put(".");
		}

		if (settings.printBlockIns && block.predecessors.length > 0)
		{
			sink.putf(" in(");
			foreach(i, predIndex; block.predecessors.range(ir)) {
				if (i > 0) sink.put(", ");
				sink.putf("%s", predIndex);
			}
			sink.put(")");
		}
		if (settings.printBlockOuts && block.successors.length > 0)
		{
			sink.putf(" out(");
			foreach(i, succIndex; block.successors.range(ir)) {
				if (i > 0) sink.put(", ");
				sink.putf("%s", succIndex);
			}
			sink.put(")");
		}
		sink.putln;

		// phis
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			printInstrIndex(phiIndex);
			sink.putf("    %s = %s(", phi.result, phiIndex);
			foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
			{
				if (arg_i > 0) sink.put(", ");
				sink.putf("%s %s", phiArg.value, phiArg.basicBlock);
			}
			sink.put(")");
			if (settings.printUses) printRegUses(phi.result);
			sink.putln;
		}

		// instrs
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			printInstrIndex(instrIndex);

			// print instr
			printer.instrIndex = instrIndex;
			printer.instrHeader = &instrHeader;

			printer.dumpInstr();

			if (settings.printUses && instrHeader.hasResult) printRegUses(instrHeader.result);
			sink.putln;
		}
	}

	sink.putln("}");
}

void dumpIrIndex(ref InstrPrintInfo p, IrIndex index)
{
	p.sink.putf("%s", index);
}

void dumpIrInstr(ref InstrPrintInfo p)
{
	switch(p.instrHeader.op)
	{
		case IrOpcode.call: dumpCall(p); break;
		case IrOpcode.block_exit_unary_branch: dumpUnBranch(p); break;
		case IrOpcode.block_exit_binary_branch: dumpBinBranch(p); break;

		case IrOpcode.block_exit_jump:
			p.sink.putf("    jmp %s", p.block.successors[0, *p.ir]);
			break;

		case IrOpcode.parameter:
			uint paramIndex = p.ir.get!IrInstr_parameter(p.instrIndex).index;
			p.sink.putf("    %s = parameter%s", p.instrHeader.result, paramIndex);
			break;

		case IrOpcode.block_exit_return_void:
			p.sink.put("     return");
			break;

		case IrOpcode.block_exit_return_value:
			p.sink.putf("    return %s", p.instrHeader.args[0]);
			break;

		default:
			if (p.instrHeader.hasResult)
				p.sink.putf("    %s = %s", p.instrHeader.result, cast(IrOpcode)p.instrHeader.op);
			else  p.sink.putf("    %s", cast(IrOpcode)p.instrHeader.op);
			dumpArgs(p);
			break;
	}
}

void dumpCall(ref InstrPrintInfo p)
{
	FunctionIndex calleeIndex = p.instrHeader.tail!IrInstrTail_call.callee;
	FunctionDeclNode* callee = p.context.mod.functions[calleeIndex];
	if (p.instrHeader.hasResult)
		p.sink.putf("    %s = call %s", p.instrHeader.result, callee.strId(p.context));
	else
		p.sink.putf("    call %s", callee.strId(p.context));
	dumpArgs(p);
}

void dumpArgs(ref InstrPrintInfo p)
{
	foreach (i, IrIndex arg; p.instrHeader.args)
	{
		if (i > 0) p.sink.put(",");
		p.sink.put(" ");
		p.dumpIndex(arg);
	}
}

void dumpUnBranch(ref InstrPrintInfo p)
{
	p.sink.putf("    if %s", unaryCondStrings[p.instrHeader.cond]);
	p.sink.put(" then ");
	p.dumpIndex(p.instrHeader.args[0]);
	dumpBranchTargets(p);
}

void dumpBinBranch(ref InstrPrintInfo p)
{
	p.sink.putf("    if ");
	p.dumpIndex(p.instrHeader.args[0]);
	p.sink.putf(" %s ", binaryCondStrings[p.instrHeader.cond]);
	p.dumpIndex(p.instrHeader.args[1]);
	p.sink.put(" then ");
	dumpBranchTargets(p);
}

void dumpBranchTargets(ref InstrPrintInfo p)
{
	switch (p.block.successors.length) {
		case 0:
			p.sink.put("<null> else <null>");
			break;
		case 1:
			p.dumpIndex(p.block.successors[0, *p.ir]);
			p.sink.put(" else <null>");
			break;
		default:
			p.dumpIndex(p.block.successors[0, *p.ir]);
			p.sink.put(" else ");
			p.dumpIndex(p.block.successors[1, *p.ir]);
			break;
	}
}
