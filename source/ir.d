/// IR that implements idea of storing everything (almost)
/// inside a single array;
/// Stores instructions in a linked list
module ir;

import std.stdio;
import std.string : format;
import std.traits : getUDAs, Parameters;
import std.bitmanip : bitfields;
import std.format : formattedWrite, FormatSpec;

import compiler1;
import driver;
import utils;

//version = standalone;
version (standalone) void main()
{
	// function i32 $sign () {
	//    |  @start:0
	//   1|    %0 = i32 o_param
	//   2|    %1 = i1  o_icmp   l i32 %0, i64 0
	//         branch i1 %1 @then_1, @else_1
	//    |  @then_1:1 in[@start]
	//         jmp @blk_2
	//    |  @else_1:2 in[@start]
	//   7|    %2 = i1  o_icmp   g i32 %0, i64 0
	//         branch i1 %2 @then_2, @else_2
	//    |  @then_2:3 in[@else_1]
	//         jmp @blk_1
	//    |  @else_2:4 in[@else_1]
	//         jmp @blk_1
	//    |  @blk_1:5 in[@then_2, @else_2]
	//  13|    %4 = i32 phi.1(i64 1 @3, i64 0 @4)
	//         jmp @blk_2
	//    |  @blk_2:6 in[@then_1, @blk_1]
	//  15|    %3 = i32 phi.0(i32 -1 @1, i32 %4 @5)
	//         return i32 %3
	// }

	// i32 sign(i32 number) {
	//     i32 result;
	//     if (number < 0) result = 0-1;
	//     else if (number > 0) result = 1;
	//     else result = 0;
	//     return result;
	// }
	writefln("start");
	Driver driver;
	driver.initialize(null);
	scope(exit) driver.releaseMemory;

	IrBuilder builder;
	IrFunction ir;

	ir.returnType = IrValueType.i32;
	ir.name = driver.context.idMap.getOrReg("sign");

	//i32 sign(i32 number)
	builder.begin(&ir, &driver.context);
	IrIndex param0Index = builder.addInstruction!IrInstrParameter(ir.entryBasicBlock);
	ir.get!IrInstrParameter(param0Index).index = 0;
	IrIndex param0Value = ir.get!IrInstrHeader(param0Index).result;
	builder.addJump(ir.entryBasicBlock);
	//{
	IrIndex start_block = builder.addBasicBlock();
	builder.addBlockTarget(ir.entryBasicBlock, start_block);
	builder.sealBlock(start_block);
	//	i32 result;
	IrIndex zeroVal = driver.context.addConstant(IrConstant(0));
	IrVar resultVar = IrVar(Identifier(0), builder.newIrVarId());
	builder.writeVariable(start_block, resultVar, zeroVal);
	IrLabel scope1ExitLabel = IrLabel(start_block);
	IrIndex then_1_block = builder.addBasicBlock();
	IrIndex else_1_block = builder.addBasicBlock();
	//	if (number < 0)
	auto branch1 = builder.addBinBranch(start_block, IrBinaryCondition.l, param0Value, zeroVal);
	builder.addUser(branch1, param0Value);
	builder.addUser(branch1, zeroVal);

	builder.addBlockTarget(start_block, then_1_block);
	builder.sealBlock(then_1_block);
	builder.addBlockTarget(start_block, else_1_block);
	builder.sealBlock(else_1_block);
	//		result = 0-1;
	IrIndex minusOneVal = driver.context.addConstant(IrConstant(-1));
	builder.writeVariable(then_1_block, resultVar, minusOneVal);
	builder.addJumpToLabel(then_1_block, scope1ExitLabel);
	//	else
	//	{
	//		if (number > 0)
	auto branch2 = builder.addBinBranch(else_1_block, IrBinaryCondition.g, param0Value, zeroVal);
	builder.addUser(branch2, param0Value);
	builder.addUser(branch2, zeroVal);

	IrIndex then_2_block = builder.addBasicBlock();
	IrIndex else_2_block = builder.addBasicBlock();
	builder.addBlockTarget(else_1_block, then_2_block);
	builder.sealBlock(then_2_block);
	builder.addBlockTarget(else_1_block, else_2_block);
	builder.sealBlock(else_2_block);
	//			result = 1;
	IrIndex oneVal = driver.context.addConstant(IrConstant(1));
	builder.writeVariable(then_2_block, resultVar, oneVal);
	builder.addJumpToLabel(then_2_block, scope1ExitLabel);
	//		else
	//			result = 0;
	builder.writeVariable(else_2_block, resultVar, zeroVal);
	builder.addJumpToLabel(else_2_block, scope1ExitLabel);
	//	}
	IrIndex currentBlock = scope1ExitLabel.blockIndex;
	builder.sealBlock(currentBlock);
	//	return result;
	builder.addReturn(currentBlock, builder.readVariable(currentBlock, resultVar));
	//}

	builder.sealBlock(ir.exitBasicBlock);

	FuncDumpSettings dumpSettings;
	dumpSettings.dumper = &dumpIrInstr;
	dumpFunction(&ir, &driver.context, dumpSettings);
}

//version = IrPrint;

struct IrModule
{
	IrFunction*[] functions;

	ubyte[] codeBuffer;
	ubyte[] code;

	void addFunction(IrFunction* fun)
	{
		functions ~= fun;
	}

	void dump(ref TextSink sink, CompilationContext* context, ref FuncDumpSettings settings)
	{
		foreach (func; functions) dumpFunction(func, sink, context, settings);
	}
}

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
		auto vreg = &ir.get!IrVirtualRegister(result);
		sink.put(" users [");
		foreach (i, index; vreg.users.range(ir))
		{
			if (i > 0) sink.put(", ");
			sink.putf("%s", index);
		}
		sink.put("]");
	}

	InstrPrintInfo printer;
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

/// Describes what IrIndex is pointing at
/// Is used as UDA on instructions
enum IrValueKind : ubyte
{
	none, /// Used for undefined indicies
	listItem, /// Indicates items of linked list in SmallVector
	instruction,
	basicBlock,
	constant,
	phi,
	memoryAddress,
	virtualRegister,
	physicalRegister,
}

/// Represent index of any IR entity inside function's ir array
struct IrIndex
{
	this(uint _storageUintIndex, IrValueKind _kind)
	{
		storageUintIndex = _storageUintIndex;
		kind = _kind;
	}

	static IrIndex fromUint(uint data)
	{
		IrIndex res;
		res.asUint = data;
		return res;
	}

	union
	{
		mixin(bitfields!(
			uint,        "storageUintIndex", 28, // may be 0 for defined index
			IrValueKind, "kind",              4  // is never 0 for defined index
		));
		uint asUint; // is 0 for undefined index
	}
	static assert(IrValueKind.max <= 0b1111, "4 bits are reserved");
	bool isDefined() { return asUint != 0; }

	void toString(scope void delegate(const(char)[]) sink) const {
		final switch(kind) with(IrValueKind) {
			case none: sink("<null>"); break;
			case listItem: sink.formattedWrite("l.%s", storageUintIndex); break;
			case instruction: sink.formattedWrite("i.%s", storageUintIndex); break;
			case basicBlock: sink.formattedWrite("@%s", storageUintIndex); break;
			case constant: sink.formattedWrite("c.%s", storageUintIndex); break;
			case phi: sink.formattedWrite("phi.%s", storageUintIndex); break;
			case memoryAddress: sink.formattedWrite("m.%s", storageUintIndex); break;
			case virtualRegister: sink.formattedWrite("v.%s", storageUintIndex); break;
			case physicalRegister: sink.formattedWrite("p.%s", storageUintIndex); break;
		}
	}

	/// When this index represents index of 0's array item, produces
	/// index of this array items. Calling with 0 returns itself.
	IrIndex indexOf(T)(size_t offset)
	{
		static assert(T.alignof == 4, "Can only point to types aligned to 4 bytes");
		IrIndex result = this;
		result.storageUintIndex = cast(uint)(storageUintIndex + divCeil(T.sizeof, uint.sizeof) * offset);
		return result;
	}

	bool isConstant() { return kind == IrValueKind.constant; }
}

struct IrLabel
{
	/// If numPredecessors == 0, is null
	/// If numPredecessors == 1, points to first predecessor
	/// If numPredecessors > 1,  points to a new block
	IrIndex blockIndex;
	///
	uint numPredecessors;
}

enum HasResult : bool { no, yes }

struct InstrInfo
{
	ushort opcode;
	uint numArgs;
	HasResult hasResult;
}

enum getInstrInfo(T) = getUDAs!(T, InstrInfo)[0];
enum getIrValueKind(T) = getUDAs!(T, IrValueKind)[0];

/// Stores numeric constant data
@(IrValueKind.constant)
struct IrConstant
{
	this(long value) {
		this.i64 = value;
	}

	ubyte numSignedBytes() {
		if (cast(byte)(i64 & 0xFF) == i64)
			return 1;
		else if (cast(short)(i64 & 0xFFFF) == i64)
			return 2;
		else if (cast(int)(i64 & 0xFFFF_FFFF) == i64)
			return 4;
		else
			return 8;
	}

	ubyte numUnsignedBytes() {
		if (cast(ubyte)(i64 & 0xFF) == i64)
			return 1;
		else if (cast(ushort)(i64 & 0xFFFF) == i64)
			return 2;
		else if (cast(uint)(i64 & 0xFFFF_FFFF) == i64)
			return 4;
		else
			return 8;
	}

	union {
		bool i1;
		byte i8;
		short i16;
		int i32;
		long i64;
	}
}

///
@(IrValueKind.phi)
struct IrPhiInstr
{
	IrIndex blockIndex;
	IrIndex result;
	IrIndex nextPhi;
	IrIndex prevPhi;
	IrIndex firstArgListItem;

	PhiArgIterator args(IrFunction* ir) { return PhiArgIterator(ir, firstArgListItem); }
}

struct PhiArgIterator
{
	IrFunction* ir;
	IrIndex firstArgListItem;
	int opApply(scope int delegate(size_t, ref IrPhiArg) dg) {
		IrIndex next = firstArgListItem;
		size_t i = 0;
		while (next.isDefined)
		{
			IrPhiArg* arg = &ir.get!IrPhiArg(next);
			if (int res = dg(i, *arg))
				return res;
			++i;
			next = arg.nextListItem;
		}
		return 0;
	}
}

///
@(IrValueKind.listItem)
struct IrPhiArg
{
	IrIndex value;
	IrIndex basicBlock;
	IrIndex nextListItem;
}

/// Per Basic Block info for unresolved Phi functions, when CFG is incomplete.
/// Finished IR contains no such values
@(IrValueKind.listItem)
struct IrIncompletePhi
{
	IrVar var;
	IrIndex phi;
	IrIndex nextListItem;
}

enum IrOpcode : ushort
{
	// used as placeholder inside generic instructions. Must not remain in IR.
	invalid,

	block_exit_jump,
	block_exit_unary_branch,
	block_exit_binary_branch,
	block_exit_return_void,
	block_exit_return_value,

	parameter,

	set_binary_cond,
	set_unary_cond,

	store,
	load,

	add,
	sub
}

@(IrValueKind.virtualRegister)
struct IrVirtualRegister
{
	/// Index of instruction that defines this register
	IrIndex definition;
	/// List of instruction indicies that use this register
	SmallVector users;
}

/// Must end with one of block_exit_... instructions
@(IrValueKind.basicBlock)
struct IrBasicBlockInstr
{
	IrIndex firstInstr; // null or first instruction
	IrIndex lastInstr; // null or last instruction
	IrIndex prevBlock; // null only if this is entryBasicBlock
	IrIndex nextBlock; // null only if this is exitBasicBlock
	IrIndex firstPhi; // may be null

	PhiIterator phis(IrFunction* ir) { return PhiIterator(ir, &this); }
	InstrIterator instructions(IrFunction* ir) { return InstrIterator(ir, &this); }

	SmallVector predecessors;
	SmallVector successors;

	uint seqIndex;
	mixin(bitfields!(
		/// True if all predecessors was added
		bool, "isSealed",   1,
		/// True if block_exit instruction is in place
		bool, "isFinished", 1,
		uint, "",           6
	));

	IrName name;
}
//pragma(msg, "BB size: ", cast(int)IrBasicBlockInstr.sizeof, " bytes");

struct PhiIterator
{
	IrFunction* ir;
	IrBasicBlockInstr* block;
	int opApply(scope int delegate(IrIndex, ref IrPhiInstr) dg) {
		IrIndex next = block.firstPhi;
		while (next.isDefined)
		{
			IrPhiInstr* phi = &ir.get!IrPhiInstr(next);
			if (int res = dg(next, *phi))
				return res;
			next = phi.nextPhi;
		}
		return 0;
	}
}

struct InstrIterator
{
	IrFunction* ir;
	IrBasicBlockInstr* block;
	int opApply(scope int delegate(IrIndex, ref IrInstrHeader) dg) {
		IrIndex next = block.firstInstr;
		while (next.isDefined)
		{
			IrInstrHeader* header = &ir.get!IrInstrHeader(next);
			if (int res = dg(next, *header))
				return res;
			next = header.nextInstr;
		}
		return 0;
	}
}

/// Common prefix of all IR instruction structs
@(IrValueKind.instruction) @InstrInfo()
struct IrInstrHeader
{
	ushort op;
	ubyte numArgs;

	mixin(bitfields!(
		HasResult,  "hasResult", 1,
		ubyte,      "cond",      4,
		uint, "",                3
	));

	static assert(IrBinaryCondition.max <= 0b1111, "4 bits are reserved");
	static assert(IrUnaryCondition.max <= 0b1111, "4 bits are reserved");

	IrIndex prevInstr;
	IrIndex nextInstr;

	IrIndex[0] _payload;

	ref IrIndex result() {
		assert(hasResult);
		return _payload.ptr[0];
	}

	IrIndex[] args() {
		return _payload.ptr[cast(size_t)hasResult..cast(size_t)hasResult+numArgs];
	}
}

template IrGenericInstr(ushort opcode, uint numArgs, HasResult hasResult)
{
	@(IrValueKind.instruction) @InstrInfo(opcode, numArgs, hasResult)
	struct IrGenericInstr
	{
		IrInstrHeader header;
		static if (hasResult)   IrIndex result;
		static if (numArgs > 0) IrIndex[numArgs] args;

		/// takes result + all arguments
		void initialize(IrIndex[hasResult + numArgs] payload ...)
		{
			header.op = opcode;
			header.numArgs = numArgs;
			header.hasResult = hasResult;
			header._payload.ptr[0..hasResult + numArgs] = payload;
		}
	}
}

alias IrReturnValueInstr = IrGenericInstr!(IrOpcode.block_exit_return_value, 1, HasResult.no);
alias IrReturnVoidInstr = IrGenericInstr!(IrOpcode.block_exit_return_void, 0, HasResult.no);
alias IrStoreInstr = IrGenericInstr!(IrOpcode.store, 1, HasResult.no);
alias IrLoadInstr = IrGenericInstr!(IrOpcode.load, 1, HasResult.yes);
alias IrSetBinaryCondInstr = IrGenericInstr!(IrOpcode.set_binary_cond, 2, HasResult.yes);
alias IrBinaryExprInstr(ushort opcode) = IrGenericInstr!(opcode, 2, HasResult.yes);
alias IrInstrJump = IrGenericInstr!(IrOpcode.block_exit_jump, 0, HasResult.no);

enum IrBinaryCondition : ubyte {
	eq,
	ne,
	g,
	ge,
	l,
	le,
}

string[] binaryCondStrings = cast(string[IrBinaryCondition.max+1])["==", "!=", ">", ">=", "<", "<="];

/// Uses header.cond
alias IrInstrBinaryBranch = IrGenericInstr!(IrOpcode.block_exit_binary_branch, 2, HasResult.no);

enum IrUnaryCondition : ubyte {
	zero,
	not_zero
}

/// Uses header.cond
alias IrInstrUnaryBranch = IrGenericInstr!(IrOpcode.block_exit_unary_branch, 1, HasResult.no);

@(IrValueKind.instruction) @InstrInfo(IrOpcode.parameter, 0, HasResult.yes)
struct IrInstrParameter
{
	IrInstrHeader header;
	IrIndex result;
	uint index;
}

struct IrVarId { uint id; alias id this; }
struct IrVar { Identifier name; IrVarId id; IrValueType type; }

enum IrValueType : ubyte
{
	void_t,
	i32,
	i64,
	//f32,
	//f64,

	ptr,
}

struct BlockVarPair
{
	IrIndex blockId;
	IrVarId varId;
}

struct IrFunction
{
	uint[] storage;
	/// number of uints allocated from storage
	uint storageLength;

	/// Special block. Automatically created. Program start. Created first.
	IrIndex entryBasicBlock;

	/// Special block. Automatically created. All returns must jump to it.
	IrIndex exitBasicBlock;

	///
	IrValueType returnType;

	uint numBasicBlocks;

	Identifier name;

	BlockIterator blocks() { return BlockIterator(&this); }

	alias getBlock = get!IrBasicBlockInstr;
	alias getPhi = get!IrPhiInstr;

	ref T get(T)(IrIndex index)
	{
		assert(index.kind != IrValueKind.none, "null index");
		assert(index.kind == getIrValueKind!T, format("%s != %s", index.kind, getIrValueKind!T));
		return *cast(T*)(&storage[index.storageUintIndex]);
	}
}

struct BlockIterator
{
	IrFunction* ir;
	int opApply(scope int delegate(IrIndex, ref IrBasicBlockInstr) dg) {
		IrIndex next = ir.entryBasicBlock;
		while (next.isDefined)
		{
			IrBasicBlockInstr* block = &ir.getBlock(next);
			if (int res = dg(next, *block))
				return res;
			next = block.nextBlock;
		}
		return 0;
	}
}


// papers:
// 1. Simple and Efficient Construction of Static Single Assignment Form
struct IrBuilder
{
	CompilationContext* context;
	IrFunction* ir;

	// The last created basic block
	IrIndex lastBasicBlock;

	// Stores current definition of variable per block during SSA-form IR construction.
	private IrIndex[BlockVarPair] blockVarDef;
	private IrIndex[IrIndex] blockToIrIncompletePhi;

	private IrVarId nextIrVarId;

	IrVar returnVar;

	/// Must be called before compilation of each function. Allows reusing temp buffers.
	/// Sets up entry and exit basic blocks.
	void begin(IrFunction* ir, CompilationContext* context) {
		this.context = context;
		this.ir = ir;

		ir.storage = context.irBuffer.freePart;
		ir.storageLength = 0;

		blockVarDef.clear();

		setupEntryExitBlocks();

		if (ir.returnType != IrValueType.void_t)
		{
			IrIndex retIndex = addInstruction!IrReturnValueInstr(ir.exitBasicBlock);
			returnVar = IrVar(Identifier(0), newIrVarId());
			IrIndex retValue = readVariable(ir.exitBasicBlock, returnVar);
			ir.get!IrReturnValueInstr(retIndex).args[0] = retValue;
			addUser(retIndex, retValue);
		}
		else
		{
			addInstruction!IrReturnVoidInstr(ir.exitBasicBlock);
		}
		ir.getBlock(ir.exitBasicBlock).isFinished = true;
	}

	/// Must be called before IR to LIR pass
	void beginLir(IrFunction* lir, IrFunction* oldIr, CompilationContext* context) {
		this.context = context;
		this.ir = lir;

		ir.storage = context.irBuffer.freePart;
		ir.storageLength = 0;

		blockVarDef.clear();
	}

	void setupEntryExitBlocks()
	{
		assert(ir.numBasicBlocks == 0);
		// Canonical function CFG has entry block, and single exit block.
		ir.numBasicBlocks = 2;

		ir.entryBasicBlock = append!IrBasicBlockInstr;
		ir.exitBasicBlock = append!IrBasicBlockInstr;

		ir.getBlock(ir.entryBasicBlock).nextBlock = ir.exitBasicBlock;
		sealBlock(ir.entryBasicBlock);
		ir.getBlock(ir.exitBasicBlock).prevBlock = ir.entryBasicBlock;
		lastBasicBlock = ir.entryBasicBlock;
	}

	/// Returns index to allocated item
	/// Allocates howMany items. By default allocates single item.
	/// If howMany > 1 - returns index of first item, access other items via IrIndex.indexOf
	/// T must have UDA of IrValueKind value
	IrIndex append(T)(uint howMany = 1)
	{
		IrIndex index = appendVoid!T(howMany);
		(&ir.get!T(index))[0..howMany] = T.init;
		return index;
	}

	/// Returns index to uninitialized memory for all requested items.
	/// Allocates howMany items. By default allocates single item.
	/// If howMany > 1 - resultIndex has index of first item, access other items via IrIndex.indexOf
	/// T must have UDA of IrValueKind value
	IrIndex appendVoid(T)(uint howMany = 1)
	{
		static assert(T.alignof == 4, "Can only store types aligned to 4 bytes");

		IrIndex resultIndex = IrIndex(ir.storageLength, getIrValueKind!T);

		enum allocSize = divCeil(T.sizeof, uint.sizeof);
		size_t numAllocatedSlots = allocSize * howMany;
		ir.storageLength += numAllocatedSlots;
		context.irBuffer.length += numAllocatedSlots;

		return resultIndex;
	}

	/// appendVoid + appendBlockInstr
	IrIndex appendVoidToBlock(T)(IrIndex blockIndex, uint howMany = 1)
	{
		IrIndex instr = appendVoid!T(howMany);
		appendBlockInstr(blockIndex, instr);
		return instr;
	}

	/// Adds control-flow edge pointing `fromBlock` -> `toBlock`.
	void addBlockTarget(IrIndex fromBasicBlockIndex, IrIndex toBasicBlockIndex) {
		ir.getBlock(fromBasicBlockIndex).successors.append(&this, toBasicBlockIndex);
		ir.getBlock(toBasicBlockIndex).predecessors.append(&this, fromBasicBlockIndex);
	}

	/// Sets lastBasicBlock to this block
	IrIndex addBasicBlock() {
		assert(lastBasicBlock.isDefined);
		++ir.numBasicBlocks;
		IrIndex newBlock = append!IrBasicBlockInstr;
		ir.getBlock(newBlock).nextBlock = ir.getBlock(lastBasicBlock).nextBlock;
		ir.getBlock(newBlock).prevBlock = lastBasicBlock;
		ir.getBlock(ir.getBlock(lastBasicBlock).nextBlock).prevBlock = newBlock;
		ir.getBlock(lastBasicBlock).nextBlock = newBlock;
		lastBasicBlock = newBlock;
		return lastBasicBlock;
	}

	/// Does not remove its instructions/phis
	/*void removeBasicBlock(IrIndex basicBlockToRemove) {
		--numBasicBlocks;
		IrBasicBlockInstr* bb = &get!IrBasicBlockInstr(basicBlockToRemove);
		if (bb.prevBlock.isDefined)
			getBlock(bb.prevBlock).nextBlock = bb.nextBlock;
		if (bb.nextBlock.isDefined)
			getBlock(bb.nextBlock).prevBlock = bb.prevBlock;
	}*/

	// Algorithm 4: Handling incomplete CFGs
	/// Basic block is sealed if no further predecessors will be added to the block.
	/// Sealed block is not necessarily filled.
	/// Ignores already sealed blocks.
	void sealBlock(IrIndex basicBlockToSeal) {
		IrBasicBlockInstr* bb = &ir.getBlock(basicBlockToSeal);
		if (bb.isSealed) return;
		IrIndex index = blockToIrIncompletePhi.get(basicBlockToSeal, IrIndex());
		while (index.isDefined)
		{
			IrIncompletePhi ip = context.getTemp!IrIncompletePhi(index);
			addPhiOperands(basicBlockToSeal, ip.var, ip.phi);
			index = ip.nextListItem;
		}
		blockToIrIncompletePhi.remove(basicBlockToSeal);
		bb.isSealed = true;
	}

	/// Allocates new variable id for this function. It should be bound to a variable
	/// and used with writeVariable, readVariable functions
	IrVarId newIrVarId() {
		return IrVarId(nextIrVarId++);
	}

	// Algorithm 1: Implementation of local value numbering
	/// Redefines `variable` with `value`. Is used for assignment to variable
	void writeVariable(IrIndex blockIndex, IrVar variable, IrIndex value) {
		with(IrValueKind)
		{
			assert(
				value.kind == constant ||
				value.kind == virtualRegister ||
				value.kind == physicalRegister, format("%s", value));
		}
		blockVarDef[BlockVarPair(blockIndex, variable.id)] = value;
	}

	/// Returns the value that currently defines `variable` within `blockIndex`
	IrIndex readVariable(IrIndex blockIndex, IrVar variable) {
		if (auto irRef = BlockVarPair(blockIndex, variable.id) in blockVarDef)
			return *irRef;
		return readVariableRecursive(blockIndex, variable);
	}

	/// Puts `user` into a list of users of `used` value
	void addUser(IrIndex user, IrIndex used) {
		assert(user.isDefined, "user is undefined");
		assert(used.isDefined, "used is undefined");
		final switch (used.kind) with(IrValueKind) {
			case none: assert(false);
			case listItem: assert(false);
			case instruction: assert(false);
			case basicBlock: assert(false);
			case constant: break; // allowed, noop
			case phi: assert(false); // must be virt reg instead
			case memoryAddress: break; // allowed, noop
			case virtualRegister:
				ir.get!IrVirtualRegister(used).users.append(&this, user);
				break;
			case physicalRegister: break; // allowed, noop
		}
	}

	IrIndex addInstruction(I)(IrIndex blockIndex)
	{
		IrBasicBlockInstr* block = &ir.getBlock(blockIndex);
		IrIndex instr = append!I;
		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(instr);
		instrHeader.op = getInstrInfo!I.opcode;
		instrHeader.prevInstr = block.lastInstr; // points to prev instruction or to null
		instrHeader.numArgs = getInstrInfo!I.numArgs;
		instrHeader.hasResult = getInstrInfo!I.hasResult;

		if (instrHeader.hasResult)
		{
			instrHeader.result = addVirtualRegister(instr);
		}

		if (!block.firstInstr.isDefined) {
			block.firstInstr = instr;
			block.lastInstr = instr;
		} else {
			ir.get!IrInstrHeader(block.lastInstr).nextInstr = instr;
		}

		return instr;
	}

	/// Adds instruction to the end of basic block
	/// Doesn't set any instruction info except prevInstr index
	void appendBlockInstr(IrIndex blockIndex, IrIndex instr)
	{
		IrBasicBlockInstr* block = &ir.getBlock(blockIndex);

		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(instr);
		instrHeader.prevInstr = block.lastInstr; // points to prev instruction or to null

		if (!block.firstInstr.isDefined) {
			block.firstInstr = instr;
			block.lastInstr = instr;
		} else {
			ir.get!IrInstrHeader(block.lastInstr).nextInstr = instr;
		}
	}

	/// Returns virtual register of result
	IrIndex emitBinaryInstr(IrIndex blockIndex, IrBinaryCondition cond, IrIndex arg0, IrIndex arg1)
	{
		auto instr = addInstruction!IrSetBinaryCondInstr(blockIndex);
		IrIndex vreg = addVirtualRegister(instr);
		with(ir.get!IrSetBinaryCondInstr(instr)) {
			header.cond = cond;
			args = [arg0, arg1];
			result = vreg;
		}
		return vreg;
	}

	/// Returns virtual register of result
	IrIndex emitBinaryInstr(IrIndex blockIndex, IrOpcode opcode, IrIndex arg0, IrIndex arg1)
	{
		alias InstT = IrBinaryExprInstr!(IrOpcode.invalid);
		auto instr = addInstruction!InstT(blockIndex);
		IrIndex vreg = addVirtualRegister(instr);
		with(ir.get!InstT(instr)) {
			args = [arg0, arg1];
			header.op = opcode; // replace IrOpcode.invalid with actual opcode
			result = vreg;
		}
		return vreg;
	}

	IrIndex addBinBranch(IrIndex blockIndex, IrBinaryCondition cond, IrIndex arg0, IrIndex arg1, ref IrLabel trueExit, ref IrLabel falseExit)
	{
		auto res = addBinBranch(blockIndex, cond, arg0, arg1);
		forceAllocLabelBlock(trueExit, 1);
		forceAllocLabelBlock(falseExit, 1);
		addBlockTarget(blockIndex, trueExit.blockIndex);
		addBlockTarget(blockIndex, falseExit.blockIndex);
		return res;
	}

	IrIndex addBinBranch(IrIndex blockIndex, IrBinaryCondition cond, IrIndex arg0, IrIndex arg1)
	{
		IrBasicBlockInstr* block = &ir.getBlock(blockIndex);
		assert(!block.isFinished);
		block.isFinished = true;
		auto branch = addInstruction!IrInstrBinaryBranch(blockIndex);
		with(ir.get!IrInstrBinaryBranch(branch)) {
			header.cond = cond;
			args = [arg0, arg1];
		}
		return branch;
	}

	IrIndex addUnaryBranch(IrIndex blockIndex, IrUnaryCondition cond, IrIndex arg0)
	{
		IrBasicBlockInstr* block = &ir.getBlock(blockIndex);
		assert(!block.isFinished);
		block.isFinished = true;
		auto branch = addInstruction!IrInstrBinaryBranch(blockIndex);
		with(ir.get!IrInstrUnaryBranch(branch)) {
			header.cond = cond;
			args = [arg0];
		}
		return branch;
	}

	void addReturn(IrIndex blockIndex, IrIndex returnValue)
	{
		assert(ir.returnType != IrValueType.void_t);
		writeVariable(blockIndex, returnVar, returnValue);
		addJump(blockIndex);
		addBlockTarget(blockIndex, ir.exitBasicBlock);
	}

	void addReturn(IrIndex blockIndex)
	{
		assert(ir.returnType == IrValueType.void_t);
		addJump(blockIndex);
		addBlockTarget(blockIndex, ir.exitBasicBlock);
	}

	IrIndex addJump(IrIndex blockIndex)
	{
		IrBasicBlockInstr* block = &ir.getBlock(blockIndex);
		assert(!block.isFinished);
		block.isFinished = true;
		return addInstruction!IrInstrJump(blockIndex);
	}

	void addJumpToLabel(IrIndex blockIndex, ref IrLabel label)
	{
		switch (label.numPredecessors)
		{
			case 0:
				// label.blockIndex points to block that started the scope
				// no block was created for label yet
				label.numPredecessors = 1;
				label.blockIndex = blockIndex;
				break;
			case 1:
				// label.blockIndex points to the only predecessor of label block
				// no block was created for label yet
				label.numPredecessors = 2;
				IrIndex firstPred = label.blockIndex;
				label.blockIndex = addBasicBlock;
				addBlockTarget(firstPred, label.blockIndex);
				// block may be finished if binary branch targets label
				//if (!ir.getBlock(firstPred).isFinished)
				addJump(firstPred);
				goto default;
			default:
				// label.blockIndex points to label's own block
				++label.numPredecessors;
				addBlockTarget(blockIndex, label.blockIndex);
				addJump(blockIndex);
				break;
		}
	}

	private void forceAllocLabelBlock(ref IrLabel label, int newPredecessors)
	{
		switch (label.numPredecessors)
		{
			case 0:
				// label.blockIndex points to block that started the scope
				// no block was created for label yet
				label.numPredecessors = newPredecessors;
				label.blockIndex = addBasicBlock;
				break;
			case 1:
				// label.blockIndex points to the only predecessor of label block
				// no block was created for label yet
				label.numPredecessors = 1 + newPredecessors;
				IrIndex firstPred = label.blockIndex;
				label.blockIndex = addBasicBlock;
				addBlockTarget(firstPred, label.blockIndex);
				addJump(firstPred);
				break;
			default:
				// label.blockIndex points to label's own block
				label.numPredecessors += newPredecessors;
				break;
		}
	}


	private void incBlockRefcount(IrIndex basicBlock) { assert(false); }
	private void decBlockRefcount(IrIndex basicBlock) { assert(false); }

	// Creates operand for result of phi/instruction that is defined by `definition`
	IrIndex addVirtualRegister(IrIndex definition) {
		IrIndex virtRegIndex = append!IrVirtualRegister;
		IrVirtualRegister* virtReg = &ir.get!IrVirtualRegister(virtRegIndex);
		virtReg.definition = definition;
		return virtRegIndex;
	}

	// ignores null opdId
	private void removeVirtualRegister(IrIndex vreg) {
		// noop
		// TODO: freelist?
	}

	// Adds phi function to specified block
	IrIndex addPhi(IrIndex blockIndex) {
		IrIndex phiIndex = append!IrPhiInstr;
		IrIndex vreg = addVirtualRegister(phiIndex);
		ir.get!IrPhiInstr(phiIndex) = IrPhiInstr(blockIndex, vreg);
		IrBasicBlockInstr* block = &ir.getBlock(blockIndex);
		if (block.firstPhi.isDefined) {
			ir.get!IrPhiInstr(block.firstPhi).prevPhi = phiIndex;
			ir.get!IrPhiInstr(phiIndex).nextPhi = block.firstPhi;
		}
		block.firstPhi = phiIndex;
		return phiIndex;
	}

	private void removePhi(IrIndex phiIndex)
	{
		version(IrPrint) writefln("[IR] remove phi %s", phiIndex);
		IrPhiInstr* phi = &ir.get!IrPhiInstr(phiIndex);
		IrBasicBlockInstr* block = &ir.getBlock(phi.blockIndex);
		version(IrPrint) {
			foreach(IrIndex phiIndex, ref IrPhiInstr phi; block.phis(ir)) {
				writefln("[IR]   %s = %s", phi.result, phiIndex);
			}
		}
		// TODO: free list of phis
		if (block.firstPhi == phiIndex) block.firstPhi = phi.nextPhi;
		if (phi.nextPhi.isDefined) ir.get!IrPhiInstr(phi.nextPhi).prevPhi = phi.prevPhi;
		if (phi.prevPhi.isDefined) ir.get!IrPhiInstr(phi.prevPhi).nextPhi = phi.nextPhi;
		version(IrPrint) writefln("[IR] after remove phi %s", phiIndex);
		version(IrPrint) {
			IrBasicBlockInstr* block1 = &ir.getBlock(phi.blockIndex);
			foreach(IrIndex phiIndex, ref IrPhiInstr phi; block1.phis(ir)) {
				writefln("[IR]   %s = %s", phi.result, phiIndex);
			}
		}
	}

	// Algorithm 2: Implementation of global value numbering
	/// Returns the last value of the variable in basic block
	private IrIndex readVariableRecursive(IrIndex blockIndex, IrVar variable) {
		IrIndex value;
		if (!ir.getBlock(blockIndex).isSealed) {
			// Incomplete CFG
			IrIndex phiIndex = addPhi(blockIndex);
			value = ir.get!IrPhiInstr(phiIndex).result;
			blockToIrIncompletePhi.update(blockIndex,
				{
					IrIndex incompletePhi = context.appendTemp!IrIncompletePhi;
					context.getTemp!IrIncompletePhi(incompletePhi) = IrIncompletePhi(variable, phiIndex);
					return incompletePhi;
				},
				(ref IrIndex oldPhi)
				{
					IrIndex incompletePhi = context.appendTemp!IrIncompletePhi;
					context.getTemp!IrIncompletePhi(incompletePhi) = IrIncompletePhi(variable, phiIndex, oldPhi);
					return incompletePhi;
				});
		}
		else
		{
			SmallVector preds = ir.getBlock(blockIndex).predecessors;
			if (preds.length == 1) {
				// Optimize the common case of one predecessor: No phi needed
				value = readVariable(preds[0, ir], variable);
			}
			else
			{
				// Break potential cycles with operandless phi
				IrIndex phiIndex = addPhi(blockIndex);
				value = ir.get!IrPhiInstr(phiIndex).result;
				writeVariable(blockIndex, variable, value);
				value = addPhiOperands(blockIndex, variable, phiIndex);
			}
		}
		with(IrValueKind)
		{
			assert(
				value.kind == constant ||
				value.kind == virtualRegister ||
				value.kind == physicalRegister, format("%s", value));
		}
		writeVariable(blockIndex, variable, value);
		return value;
	}

	// Adds all values of variable as arguments of phi. Values are gathered from block's predecessors.
	// Returns either φ result virtual register or one of its arguments if φ is trivial
	private IrIndex addPhiOperands(IrIndex blockIndex, IrVar variable, IrIndex phi) {
		// Determine operands from predecessors
		foreach (i, predIndex; ir.getBlock(blockIndex).predecessors.range(ir))
		{
			IrIndex value = readVariable(predIndex, variable);
			version(IrPrint) writefln("[IR] phi operand %s", value);
			// Phi should not be cached before loop, since readVariable can add phi to phis, reallocating the array
			addPhiArg(phi, predIndex, value);
			addUser(phi, value);
		}
		return tryRemoveTrivialPhi(phi);
	}

	void addPhiArg(IrIndex phiIndex, IrIndex blockIndex, IrIndex value)
	{
		IrIndex phiArg = append!IrPhiArg;
		auto phi = &ir.get!IrPhiInstr(phiIndex);
		ir.get!IrPhiArg(phiArg) = IrPhiArg(value, blockIndex, phi.firstArgListItem);
		phi.firstArgListItem = phiArg;
	}

	// Algorithm 3: Detect and recursively remove a trivial φ function
	// Returns either φ result virtual register or one of its arguments if φ is trivial
	private IrIndex tryRemoveTrivialPhi(IrIndex phiIndex) {
		IrPhiArg same;
		foreach (size_t i, ref IrPhiArg phiArg; ir.get!IrPhiInstr(phiIndex).args(ir))
		{
			version(IrPrint) writefln("[IR] arg %s", phiArg.value);
			if (phiArg.value == same.value || phiArg.value == phiIndex) {
				version(IrPrint) writefln("[IR]   same");
				continue; // Unique value or self−reference
			}
			if (same != IrPhiArg()) {
				version(IrPrint) writefln("[IR]   non-trivial");
				return ir.get!IrPhiInstr(phiIndex).result; // The phi merges at least two values: not trivial
			}
			version(IrPrint) writefln("[IR]   same = %s", phiArg.value);
			same = phiArg;
		}
		version(IrPrint) writefln("[IR]   trivial");
		assert(same.value.isDefined, "Phi function got no arguments");

		// Remember all users except the phi itself
		IrIndex phiResultIndex = ir.get!IrPhiInstr(phiIndex).result;
		assert(phiResultIndex.kind == IrValueKind.virtualRegister, format("%s", phiResultIndex));

		SmallVector users = ir.get!IrVirtualRegister(phiResultIndex).users;

		// Reroute all uses of phi to same and remove phi
		replaceBy(users, phiResultIndex, same);
		removePhi(phiIndex);

		// Try to recursively remove all phi users, which might have become trivial
		foreach (i, index; users.range(ir))
			if (index.kind == IrValueKind.phi && index != phiIndex)
				tryRemoveTrivialPhi(index);

		removeVirtualRegister(phiResultIndex);
		return same.value;
	}

	IrIndex definitionOf(IrIndex someIndex)
	{
		final switch (someIndex.kind) with(IrValueKind) {
			case none: assert(false);
			case listItem: assert(false);
			case instruction: return someIndex;
			case basicBlock: assert(false);
			case constant: assert(false);
			case phi: return someIndex;
			case memoryAddress: assert(false); // TODO
			case virtualRegister: return ir.get!IrVirtualRegister(someIndex).definition;
			case physicalRegister: assert(false);
		}
	}

	// ditto
	/// Rewrites all users of phi to point to `byWhat` instead of its result `what`.
	/// `what` is the result of phi (vreg), `phiUsers` is users of `what`
	private void replaceBy(SmallVector phiUsers, IrIndex what, IrPhiArg byWhat) {
		foreach (size_t i, IrIndex userIndex; phiUsers.range(ir))
		{
			final switch (userIndex.kind) with(IrValueKind) {
				case none: assert(false);
				case listItem: assert(false);
				case instruction:
					foreach (ref IrIndex arg; ir.get!IrInstrHeader(userIndex).args)
						if (arg == what)
						{
							arg = byWhat.value;
							replaceUserWith(byWhat.value, definitionOf(what), userIndex);
						}
					break;
				case basicBlock: assert(false);
				case constant: assert(false);
				case phi:
					foreach (size_t i, ref IrPhiArg phiArg; ir.get!IrPhiInstr(userIndex).args(ir))
						if (phiArg.value == what)
						{
							phiArg = byWhat;
							replaceUserWith(byWhat.value, definitionOf(what), userIndex);
						}
					break;
				case memoryAddress: assert(false); // TODO
				case virtualRegister: assert(false);
				case physicalRegister: assert(false);
			}
		}
	}

	private void replaceUserWith(IrIndex used, IrIndex what, IrIndex byWhat) {
		final switch (used.kind) with(IrValueKind) {
			case none, listItem, basicBlock, physicalRegister: assert(false);
			case instruction: return ir.get!IrVirtualRegister(ir.get!IrInstrHeader(used).result).users.replaceAll(ir, what, byWhat);
			case constant: return; // constants dont track users
			case phi: return ir.get!IrVirtualRegister(ir.get!IrPhiInstr(used).result).users.replaceAll(ir, what, byWhat);
			case memoryAddress: assert(false); // TODO, has single user
			case virtualRegister: return ir.get!IrVirtualRegister(used).users.replaceAll(ir, what, byWhat);
		}
	}
}

/// Used for linked list
@(IrValueKind.listItem)
struct ListItem
{
	IrIndex itemIndex;
	IrIndex nextItem;
}

/// Allows storing 2 items inline, or linked list's info if more than 2 items are needed
struct SmallVector
{
	union {
		IrIndex[2] items; // .kind != IrValueKind.listItem
		struct {
			IrIndex firstListItem; // .kind == IrValueKind.listItem
			uint listLength;
		}
	}

	uint length()
	{
		if (items[0].kind == IrValueKind.listItem)
			return listLength;
		else if (items[1].isDefined)
			return 2;
		else if (items[0].isDefined)
			return 1;
		else
			return 0;
	}

	bool isBig()
	{
		return items[0].kind == IrValueKind.listItem;
	}

	SmallVectorRange range(IrFunction* ir)
	{
		return SmallVectorRange(&this, ir);
	}

	void replaceAll(IrFunction* ir, IrIndex what, IrIndex byWhat)
	{
		foreach (ref IrIndex item; range(ir))
		{
			if (item == what) item = byWhat;
		}
	}

	IrIndex opIndex(size_t index, IrFunction* ir)
	{
		size_t len = length;
		assert(index < len);
		if (len < 3) return items[index];

		foreach(i, val; range(ir))
			if (i == index)
				return val;
		assert(false);
	}

	void append(IrBuilder* builder, IrIndex itemData)
	{
		assert(itemData.kind != IrValueKind.listItem, "listItem is not storable inside SmallVector");
		assert(itemData.kind != IrValueKind.none, "IrValueKind.none is not storable inside SmallVector");
		if (isBig)
		{
			IrIndex newListItemIndex = builder.append!ListItem;
			ListItem* listItem = &builder.ir.get!ListItem(newListItemIndex);
			*listItem = ListItem(itemData, firstListItem);
			firstListItem = newListItemIndex;
			++listLength;
		}
		else
		{
			if (!items[0].isDefined)
			{
				items[0] = itemData;
			}
			else if (!items[1].isDefined)
			{
				items[1] = itemData;
			}
			else
			{
				IrIndex arrayIndex = builder.append!ListItem(3);
				ListItem* itemArray = &builder.ir.get!ListItem(arrayIndex);
				itemArray[2] = ListItem(itemData);
				itemArray[1] = ListItem(items[1], arrayIndex.indexOf!ListItem(2));
				itemArray[0] = ListItem(items[0], arrayIndex.indexOf!ListItem(1));
				firstListItem = arrayIndex;
				listLength = 3;
			}
		}
	}

	void toString(scope void delegate(const(char)[]) sink)
	{
		if (isBig)
			sink.formattedWrite("[%s items]", listLength);
		else if (items[1].isDefined)
			sink.formattedWrite("[%s, %s]", items[0], items[1]);
		else if (items[0].isDefined)
			sink.formattedWrite("[%s]", items[0]);
		else sink("[]");
	}
}

unittest
{
	IrFunction ir;
	ir.storage.setBuffer(new uint[1024]);
	ir.temp.setBuffer(new uint[1024]);
	SmallVector vec;

	assert(vec.length == 0);
	assert(!vec.isBig);

	vec.append(&ir, IrIndex(1, IrValueKind.instruction));
	assert(vec.length == 1);
	assert(!vec.isBig);

	vec.append(&ir, IrIndex(2, IrValueKind.instruction));
	assert(vec.length == 2);
	assert(!vec.isBig);

	vec.append(&ir, IrIndex(3, IrValueKind.instruction));
	assert(vec.length == 3);
	assert(vec.isBig);

	vec.append(&ir, IrIndex(4, IrValueKind.instruction));
	assert(vec.length == 4);
	assert(vec.isBig);
}

struct SmallVectorRange
{
	SmallVector* vector;
	IrFunction* ir;

	int opApply(scope int delegate(size_t, ref IrIndex) dg)
	{
		return opApplyImpl!2(dg);
	}

	int opApply(scope int delegate(ref IrIndex) dg)
	{
		return opApplyImpl!1(dg);
	}

	int opApplyImpl(uint size, Del)(scope Del dg)
	{
		if (vector.isBig) // length > 2
		{
			IrIndex next = vector.firstListItem;
			size_t seqIndex = 0;
			while (next.isDefined)
			{
				ListItem* listItem = &ir.get!ListItem(next);
				next = listItem.nextItem;
				static if (size == 2) {
					if (int res = dg(seqIndex, listItem.itemIndex))
						return res;
					++seqIndex;
				} else {
					if (int res = dg(listItem.itemIndex))
						return res;
				}
			}
		}
		else // 0, 1, 2
		{
			if (vector.items[0].isDefined)
			{
				static if (size == 2) {
					if (int res = dg(0, vector.items[0])) return res;
					if (vector.items[1].isDefined)
					{
						if (int res = dg(1, vector.items[1])) return res;
					}
				} else {
					if (int res = dg(vector.items[0])) return res;
					if (vector.items[1].isDefined)
					{
						if (int res = dg(vector.items[1])) return res;
					}
				}
			}
		}
		return 0;
	}
}
