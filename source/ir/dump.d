/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
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
		printInfo.handlers.indexDumper(sink, *printInfo.context, index);
	}
}

alias InstructionDumper = void function(ref InstrPrintInfo p);
alias IrIndexDumper = void function(scope void delegate(const(char)[]) sink, ref CompilationContext, IrIndex);

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

void dumpTypes(ref TextSink sink, ref CompilationContext ctx)
{
	IrIndex type = ctx.types.firstType;
	while (type.isDefined)
	{
		sink.putfln("%s", IrTypeDump(type, ctx));
		type = ctx.types.get!IrTypeHeader(type).nextType;
	}
}

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
	InstrPrintInfo printer;
	printer.context = &ctx;
	printer.sink = &sink;
	printer.ir = &ir;
	printer.handlers = &instrSetDumpHandlers[ir.instructionSet];
	printer.settings = &settings;

	sink.put("func ");
	// results
	auto funcType = &ctx.types.get!IrTypeFunction(ir.type);
	foreach(i, result; funcType.resultTypes) {
		if (i > 0) sink.put(", ");
		sink.putf("%s", IrIndexDump(result, printer));
	}
	if (funcType.numResults > 0) sink.put(" ");
	sink.put(ctx.idString(ir.backendData.name));

	// parameters
	sink.put("(");
	foreach(i, param; funcType.parameterTypes) {
		if (i > 0) sink.put(", ");
		sink.putf("%s", IrIndexDump(param, printer));
	}
	sink.put(")");
	sink.putfln(` %s bytes ir:"%s" {`, ir.storage.length * uint.sizeof, instr_set_names[ir.instructionSet]);
	int indexPadding = numDigitsInNumber10(ir.storage.length);

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

	IrIndex blockIndex = ir.entryBasicBlock;
	IrBasicBlock* block;
	while (blockIndex.isDefined)
	{
		if (!blockIndex.isBasicBlock)
		{
			sink.putfln("  invalid block %s", IrIndexDump(blockIndex, printer));
			break;
		}

		block = &ir.getBlock(blockIndex);
		scope(exit) blockIndex = block.nextBlock;

		printer.blockIndex = blockIndex;
		printer.block = block;

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
		IrIndex phiIndex = block.firstPhi;
		IrPhi* phi;
		while (phiIndex.isDefined)
		{
			if (!phiIndex.isPhi)
			{
				sink.putfln("  invalid phi %s", IrIndexDump(phiIndex, printer));
				break;
			}

			phi = &ir.getPhi(phiIndex);
			scope(exit) phiIndex = phi.nextPhi;

			printInstrIndex(phiIndex);
			sink.putf("    %s %s = %s(",
				IrIndexDump(phi.result, printer),
				IrIndexDump(ir.getVirtReg(phi.result).type, printer),
				IrIndexDump(phiIndex, printer));
			foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
			{
				if (arg_i > 0) sink.put(", ");
				sink.putf("%s", IrIndexDump(phiArg.basicBlock, printer));
				dumpArg(phiArg.value, printer);
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
	sink.put(ctx.idString(ir.backendData.name));
	sink.putfln(`() %s bytes" {`, ir.storage.length * uint.sizeof);
	int indexPadding = numDigitsInNumber10(ir.storage.length);

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
			sink.putf("    %s %s = %s(",
				IrIndexDump(phi.result, p),
				IrIndexDump(ir.getVirtReg(phi.result).type, p),
				IrIndexDump(phiIndex, p));
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

void dumpIrIndex(scope void delegate(const(char)[]) sink, ref CompilationContext context, IrIndex index)
{
	if (!index.isDefined) {
		sink("<null>");
		return;
	}

	final switch(index.kind) with(IrValueKind) {
		case none: sink.formattedWrite("0x%X", index.asUint); break;
		case listItem: sink.formattedWrite("l.%s", index.storageUintIndex); break;
		case instruction: sink.formattedWrite("i.%s", index.storageUintIndex); break;
		case basicBlock: sink.formattedWrite("@%s", index.storageUintIndex); break;
		case constant: sink.formattedWrite("%s", context.constants.get(index).i64); break;
		case global: sink.formattedWrite("g.%s", index.storageUintIndex); break;
		case phi: sink.formattedWrite("phi.%s", index.storageUintIndex); break;
		case stackSlot: sink.formattedWrite("s.%s", index.storageUintIndex); break;
		case virtualRegister: sink.formattedWrite("v.%s", index.storageUintIndex); break;
		case physicalRegister: sink.formattedWrite("p.%s", index.storageUintIndex); break;
		case type: dumpIrType(sink, context, index); break;
		case variable: assert(false);
		case func: sink.formattedWrite("m.%s", index.storageUintIndex); break;
	}
}

struct IrTypeDump
{
	this(IrIndex index, ref CompilationContext ctx) {
		this.index = index;
		this.ctx = &ctx;
	}
	IrIndex index;
	CompilationContext* ctx;

	void toString(scope void delegate(const(char)[]) sink) {
		dumpIrType(sink, *ctx, index);
	}
}

void dumpIrType(scope void delegate(const(char)[]) sink, ref CompilationContext ctx, IrIndex type)
{
	final switch(type.typeKind) with(IrTypeKind) {
		case basic: sink.formattedWrite("%s", cast(IrValueType)type.typeIndex); break;
		case pointer:
			dumpIrType(sink, ctx, ctx.types.get!IrTypePointer(type).baseType);
			sink("*");
			break;
		case array:
			auto array = ctx.types.get!IrTypeArray(type);
			sink.formattedWrite("[%s x ", array.size);
			dumpIrType(sink, ctx, array.elemType);
			sink("]");
			break;
		case struct_t:
			IrTypeStruct* struct_t = &ctx.types.get!IrTypeStruct(type);
			sink("{");
			foreach(i, IrTypeStructMember member; struct_t.members)
			{
				if (i > 0) sink(", ");
				dumpIrType(sink, ctx, member.type);
			}
			sink("}");
			break;
		case func_t:
			// results
			auto funcType = &ctx.types.get!IrTypeFunction(type);
			foreach(i, result; funcType.resultTypes) {
				if (i > 0) sink(", ");
				dumpIrType(sink, ctx, result);
			}

			// parameters
			sink("(");
			foreach(i, param; funcType.parameterTypes) {
				if (i > 0) sink(", ");
				dumpIrType(sink, ctx, param);
			}
			sink(")");
			break;
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
			dumpOptionalResult(p);
			p.sink.putf("parameter%s", paramIndex);
			break;

		case IrOpcode.block_exit_return_void:
			p.sink.put("     return");
			break;

		case IrOpcode.block_exit_return_value:
			p.sink.put("    return");
			dumpArg(p.instrHeader.args[0], p);
			break;

		default:
			dumpOptionalResult(p);
			p.sink.putf("%s", cast(IrOpcode)p.instrHeader.op);
			dumpArgs(p);
			break;
	}
}

void dumpOptionalResult(ref InstrPrintInfo p)
{
	if (p.instrHeader.hasResult)
	{
		if (p.instrHeader.result.isVirtReg)
		{
			p.sink.putf("    %s %s = ",
				IrIndexDump(p.instrHeader.result, p),
				IrIndexDump(p.ir.getVirtReg(p.instrHeader.result).type, p));
		}
		else
		{
			p.sink.putf("    %s = ", IrIndexDump(p.instrHeader.result, p));
		}
	}
	else
	{
		p.sink.put("    ");
	}
}

void dumpCall(ref InstrPrintInfo p)
{
	FunctionIndex calleeIndex = p.instrHeader.preheader!IrInstrPreheader_call.calleeIndex;
	FunctionDeclNode* callee = p.context.getFunction(calleeIndex);
	dumpOptionalResult(p);
	p.sink.putf("call %s", p.context.idString(callee.id));
	dumpArgs(p);
}

void dumpArgs(ref InstrPrintInfo p)
{
	foreach (i, IrIndex arg; p.instrHeader.args)
	{
		if (i > 0) p.sink.put(",");
		dumpArg(arg, p);
	}
}

void dumpArg(IrIndex arg, ref InstrPrintInfo p)
{
	if (arg.isPhysReg)
	{
		p.sink.putf(" %s", IrIndexDump(arg, p));

	}
	else
	{
		p.sink.putf(" %s %s",
			IrIndexDump(p.ir.getValueType(*p.context, arg), p),
			IrIndexDump(arg, p));
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
	p.sink.putf("    if %s", unaryCondStrings[p.instrHeader.cond]);
	dumpArg(p.instrHeader.args[0], p);
	p.sink.put(" then ");
	dumpBranchTargets(p);
}

void dumpBinBranch(ref InstrPrintInfo p)
{
	string[] opStrings = p.settings.escapeForDot ? binaryCondStringsEscapedForDot : binaryCondStrings;
	p.sink.put("    if");
	dumpArg(p.instrHeader.args[0], p);
	p.sink.putf(" %s", opStrings[p.instrHeader.cond]);
	dumpArg(p.instrHeader.args[1], p);
	p.sink.put(" then ");

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
