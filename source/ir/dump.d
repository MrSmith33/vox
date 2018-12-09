/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Convertion of function IR to textual representation
module ir.dump;

import std.stdio;
import std.format : formattedWrite;

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
	IrDumpHandlers* handlers;
	void dumpInstr() { handlers.instrDumper(this); }
}

struct IrDumpHandlers
{
	InstructionDumper instrDumper;
	IrIndexDumper indexDumper;
}

struct IrIndexDump
{
	this(IrIndex index, ref InstrPrintInfo printInfo) {
		this.index = index;
		this.printInfo = &printInfo;
	}
	IrIndex index;
	InstrPrintInfo* printInfo;

	void toString(scope void delegate(const(char)[]) sink) {
		printInfo.handlers.indexDumper(sink, *printInfo, index);
	}
}

alias InstructionDumper = void function(ref InstrPrintInfo p);
alias IrIndexDumper = void function(scope void delegate(const(char)[]) sink, ref InstrPrintInfo, IrIndex);

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
	bool escapeForDot = false;
}

IrDumpHandlers[] instrSetDumpHandlers = [
	IrDumpHandlers(&dumpIrInstr, &dumpIrIndex),
	IrDumpHandlers(&dumpAmd64Instr, &dumpLirAmd64Index),
];

void dumpFunction(ref IrFunction ir, ref CompilationContext ctx)
{
	FuncDumpSettings settings;
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

	InstrPrintInfo printer;
	printer.context = &ctx;
	printer.sink = &sink;
	printer.ir = &ir;
	printer.handlers = &instrSetDumpHandlers[ir.instructionSet];
	printer.settings = &settings;

	void printInstrIndex(IrIndex someIndex) {
		if (!settings.printInstrIndexEnabled) return;
		sink.putf("%*s|", indexPadding, someIndex.storageUintIndex);
	}

	void printRegUses(IrIndex result) {
		if (result.isPhysReg) return;
		if (result.isStackSlot) return;

		auto vreg = &ir.getVirtReg(result);
		sink.put(" users [");
		foreach (i, index; vreg.users.range(ir))
		{
			if (i > 0) sink.put(", ");
			sink.putf("%s", IrIndexDump(index, printer));
		}
		sink.put("]");
	}

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		printer.blockIndex = blockIndex;
		printer.block = &block;

		printInstrIndex(blockIndex);
		sink.putf("  %s", IrIndexDump(blockIndex, printer));

		if (settings.printBlockFlags)
		{
			if (block.isSealed) sink.put(" S");
			else sink.put(" .");

			if (block.isFinished) sink.put("F");
			else sink.put(".");

			if (block.isLoopHeader) sink.put("L");
			else sink.put(".");
		}

		if (settings.printBlockIns && block.predecessors.length > 0)
		{
			sink.putf(" in(");
			foreach(i, predIndex; block.predecessors.range(ir)) {
				if (i > 0) sink.put(", ");
				sink.putf("%s", IrIndexDump(predIndex, printer));
			}
			sink.put(")");
		}
		if (settings.printBlockOuts && block.successors.length > 0)
		{
			sink.putf(" out(");
			foreach(i, succIndex; block.successors.range(ir)) {
				if (i > 0) sink.put(", ");
				sink.putf("%s", IrIndexDump(succIndex, printer));
			}
			sink.put(")");
		}
		sink.putln;

		// phis
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			printInstrIndex(phiIndex);
			sink.putf("    %s = %s(", IrIndexDump(phi.result, printer), IrIndexDump(phiIndex, printer));
			foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
			{
				if (arg_i > 0) sink.put(", ");
				sink.putf("%s %s", IrIndexDump(phiArg.value, printer), IrIndexDump(phiArg.basicBlock, printer));
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

void dumpFunctionCFG(ref IrFunction ir, ref TextSink sink, ref CompilationContext ctx, ref FuncDumpSettings settings)
{
	settings.escapeForDot = true;
	sink.put(`digraph "`);
	sink.put("function ");
	sink.put(ctx.idString(ir.name));
	sink.putfln(`() %s bytes" {`, ir.storageLength * uint.sizeof);
	int indexPadding = numDigitsInNumber(ir.storageLength);

	InstrPrintInfo p;
	p.context = &ctx;
	p.sink = &sink;
	p.ir = &ir;
	p.settings = &settings;

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		foreach(i, succIndex; block.successors.range(ir)) {
			sink.putfln("node_%s -> node_%s;",
				blockIndex.storageUintIndex, succIndex.storageUintIndex);
		}

		sink.putf(`node_%s [shape=record,label="{`, blockIndex.storageUintIndex);

		p.blockIndex = blockIndex;
		p.block = &block;

		sink.putf(`  %s`, IrIndexDump(blockIndex, p));
		sink.put(`\l`);

		// phis
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			sink.putf("    %s = %s(", IrIndexDump(phi.result, p), phiIndex);
			foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
			{
				if (arg_i > 0) sink.put(", ");
				sink.putf("%s %s", IrIndexDump(phiArg.value, p), IrIndexDump(phiArg.basicBlock, p));
			}
			sink.put(")");
			sink.put(`\l`);
		}

		// instrs
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			// print instr
			p.instrIndex = instrIndex;
			p.instrHeader = &instrHeader;
			p.dumpInstr();
			sink.put(`\l`);
		}
		sink.putfln(`}"];`);
	}

	sink.putln("}");
}

void dumpIrIndex(scope void delegate(const(char)[]) sink, ref InstrPrintInfo p, IrIndex index)
{
	switch(index.kind) with(IrValueKind) {
		default: sink.formattedWrite("0x%X", index.asUint); break;
		case listItem: sink.formattedWrite("l.%s", index.storageUintIndex); break;
		case instruction: sink.formattedWrite("i.%s", index.storageUintIndex); break;
		case basicBlock: sink.formattedWrite("@%s", index.storageUintIndex); break;
		case constant: sink.formattedWrite("%s", p.context.getConstant(index).i64); break;
		case phi: sink.formattedWrite("phi.%s", index.storageUintIndex); break;
		case memoryAddress: sink.formattedWrite("m.%s", index.storageUintIndex); break;
		case stackSlot: sink.formattedWrite("s.%s", index.storageUintIndex); break;
		case virtualRegister: sink.formattedWrite("v.%s", index.storageUintIndex); break;
		case physicalRegister: sink.formattedWrite("p.%s", index.storageUintIndex); break;
	}
}

void dumpIrInstr(ref InstrPrintInfo p)
{
	switch(p.instrHeader.op)
	{
		case IrOpcode.call: dumpCall(p); break;
		case IrOpcode.block_exit_unary_branch: dumpUnBranch(p); break;
		case IrOpcode.block_exit_binary_branch: dumpBinBranch(p); break;
		case IrOpcode.block_exit_jump: dumpJmp(p); break;

		case IrOpcode.parameter:
			uint paramIndex = p.ir.get!IrInstr_parameter(p.instrIndex).index;
			p.sink.putf("    %s = parameter%s", IrIndexDump(p.instrHeader.result, p), paramIndex);
			break;

		case IrOpcode.block_exit_return_void:
			p.sink.put("     return");
			break;

		case IrOpcode.block_exit_return_value:
			p.sink.putf("    return %s", IrIndexDump(p.instrHeader.args[0], p));
			break;

		default:
			if (p.instrHeader.hasResult)
				p.sink.putf("    %s = %s", IrIndexDump(p.instrHeader.result, p), cast(IrOpcode)p.instrHeader.op);
			else  p.sink.putf("    %s", cast(IrOpcode)p.instrHeader.op);
			dumpArgs(p);
			break;
	}
}

void dumpCall(ref InstrPrintInfo p)
{
	FunctionIndex calleeIndex = p.instrHeader.preheader!IrInstrPreheader_call.calleeIndex;
	p.context.assertf(calleeIndex < p.context.mod.functions.length,
		"Invalid callee index %s", calleeIndex);
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
		p.sink.putf(" %s", IrIndexDump(arg, p));
	}
}

void dumpJmp(ref InstrPrintInfo p)
{
	p.sink.put("    jmp ");
	if (p.block.successors.length > 0)
		p.sink.putf("%s", IrIndexDump(p.block.successors[0, *p.ir], p));
	else
		p.sink.put(p.settings.escapeForDot ? `\<null\>` : "<null>");
}

void dumpUnBranch(ref InstrPrintInfo p)
{
	p.sink.putf("    if %s then %s",
		unaryCondStrings[p.instrHeader.cond],
		IrIndexDump(p.instrHeader.args[0], p));
	dumpBranchTargets(p);
}

void dumpBinBranch(ref InstrPrintInfo p)
{
	string[] opStrings = p.settings.escapeForDot ? binaryCondStringsEscapedForDot : binaryCondStrings;
	p.sink.putf("    if %s %s %s then ",
		IrIndexDump(p.instrHeader.args[0], p),
		opStrings[p.instrHeader.cond],
		IrIndexDump(p.instrHeader.args[1], p));

	dumpBranchTargets(p);
}

void dumpBranchTargets(ref InstrPrintInfo p)
{
	switch (p.block.successors.length) {
		case 0:
			p.sink.put(p.settings.escapeForDot ? `\<null\> else \<null\>` : "<null> else <null>");
			break;
		case 1:
			p.sink.putf(p.settings.escapeForDot ? `%s else \<null\>` : "%s else <null>",
				IrIndexDump(p.block.successors[0, *p.ir], p));
			break;
		default:
			p.sink.putf("%s else %s",
				IrIndexDump(p.block.successors[0, *p.ir], p),
				IrIndexDump(p.block.successors[1, *p.ir], p));
			break;
	}
}
