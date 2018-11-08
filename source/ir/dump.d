/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Convertion of function IR to textual representation
module ir.dump;

import std.stdio;

import all;

alias InstructionDumper = void function(ref InstrPrintInfo p);

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
	InstructionDumper dumper;
}

void dumpFunction(IrFunction* ir, CompilationContext* ctx, ref FuncDumpSettings settings)
{
	TextSink sink;
	dumpFunction(ir, sink, ctx, settings);
	writeln(sink.text);
}

void dumpFunction(IrFunction* ir, ref TextSink sink, CompilationContext* ctx, ref FuncDumpSettings settings)
{
	sink.put("function ");
	sink.put(ctx.idString(ir.name));
	sink.putln("() {");

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
	printer.context = ctx;
	printer.sink = &sink;
	printer.ir = ir;
	printer.settings = &settings;

	foreach (IrIndex blockIndex, ref IrBasicBlockInstr block; ir.blocks)
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
		foreach(IrIndex phiIndex, ref IrPhiInstr phi; block.phis(ir))
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

			settings.dumper(printer);

			if (settings.printUses && instrHeader.hasResult) printRegUses(instrHeader.result);
			sink.putln;
		}
	}

	sink.putln("}");
	sink.putfln("IR size: %s uints | %s bytes",
		ir.storageLength, ir.storageLength * uint.sizeof);
}

struct InstrPrintInfo
{
	CompilationContext* context;
	TextSink* sink;
	IrFunction* ir;
	IrIndex blockIndex;
	IrBasicBlockInstr* block;
	IrIndex instrIndex;
	IrInstrHeader* instrHeader;
	FuncDumpSettings* settings;
}

void dumpIrInstr(ref InstrPrintInfo p)
{
	switch(p.instrHeader.op)
	{
		case IrOpcode.block_exit_jump:
			p.sink.putf("    jmp %s", p.block.successors[0, p.ir]);
			break;

		case IrOpcode.block_exit_unary_branch:
			p.sink.putf("    if %s %s then %s else %s",
				p.instrHeader.cond,
				p.instrHeader.args[0],
				p.block.successors[0, p.ir],
				p.block.successors[1, p.ir]);
			break;

		case IrOpcode.block_exit_binary_branch:
			p.sink.putf("    if %s %s %s then ",
				p.instrHeader.args[0],
				binaryCondStrings[p.instrHeader.cond],
				p.instrHeader.args[1]);
			switch (p.block.successors.length) {
				case 0:
					p.sink.put("<null> else <null>");
					break;
				case 1:
					p.sink.putf("%s else <null>",
						p.block.successors[0, p.ir]);
					break;
				default:
					p.sink.putf("%s else %s",
						p.block.successors[0, p.ir],
						p.block.successors[1, p.ir]);
					break;
			}
			break;

		case IrOpcode.parameter:
			uint paramIndex = p.ir.get!IrInstrParameter(p.instrIndex).index;
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
			foreach (i, IrIndex arg; p.instrHeader.args)
			{
				if (i > 0) p.sink.put(",");
				p.sink.putf(" %s", arg);
			}
			break;
	}
}
