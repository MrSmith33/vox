/// IR that implements idea of storing everything (almost)
/// inside a single array;
/// Stores instructions in a linked list
module new_ir;

import std.stdio;
import std.string : format;
import std.traits : getUDAs;
import std.bitmanip : bitfields;
import std.format : formattedWrite, FormatSpec;
import utils;

void main()
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

	Win32Allocator allocator;
	size_t memSize = 1024UL*1024*1024*10;
	size_t arrSizes = 1024UL*1024*1024*5;
	bool success = allocator.reserve(memSize);
	assert(success, "allocator failed");

	IrBuilder builder;
	IrFunction ir;
	ir.storage.setBuffer(cast(uint[])allocator.allocate(arrSizes));
	ir.temp.setBuffer(cast(uint[])allocator.allocate(arrSizes));

	ir.returnType = IrValueType.i32;

	//i32 sign(i32 number)
	builder.begin(&ir);
	IrIndex param0Index = builder.addInstruction!IrInstrParameter(ir.entryBasicBlock, IrOpcode.parameter);
	ir.get!IrInstrParameter(param0Index).index = 0;
	IrIndex param0Value = ir.get!IrInstrHeader(param0Index).result;
	builder.addJump(ir.entryBasicBlock);
	//{
	IrIndex start_block = builder.addBasicBlock();
	ir.addBlockTarget(ir.entryBasicBlock, start_block);
	builder.sealBlock(start_block);
	//	i32 result;
	IrIndex zeroVal = builder.addConstant(IrConstant(0));
	IrVar resultVar = IrVar(Identifier(0), builder.newIrVarId());
	builder.writeVariable(start_block, resultVar, zeroVal);
	IrLabel scope1ExitLabel = IrLabel(start_block);
	IrIndex then_1_block = builder.addBasicBlock();
	IrIndex else_1_block = builder.addBasicBlock();
	//	if (number < 0)
	builder.addBinBranch(start_block, IrBinaryCondition.l, param0Value, zeroVal);
	ir.addBlockTarget(start_block, then_1_block);
	builder.sealBlock(then_1_block);
	ir.addBlockTarget(start_block, else_1_block);
	builder.sealBlock(else_1_block);
	//		result = 0-1;
	IrIndex minusOneVal = builder.addConstant(IrConstant(-1));
	builder.writeVariable(then_1_block, resultVar, minusOneVal);
	builder.addJumpToLabel(then_1_block, scope1ExitLabel);
	//	else
	//	{
	//		if (number > 0)
	builder.addBinBranch(else_1_block, IrBinaryCondition.g, param0Value, zeroVal);
	IrIndex then_2_block = builder.addBasicBlock();
	IrIndex else_2_block = builder.addBasicBlock();
	ir.addBlockTarget(else_1_block, then_2_block);
	builder.sealBlock(then_2_block);
	ir.addBlockTarget(else_1_block, else_2_block);
	builder.sealBlock(else_2_block);
	//			result = 1;
	IrIndex oneVal = builder.addConstant(IrConstant(1));
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
	builder.addReturnValue(currentBlock, builder.readVariable(currentBlock, resultVar));
	//}

	builder.sealBlock(ir.exitBasicBlock);

	TextSink sink;
	dumpFunction(&ir, sink);
	writeln(sink.text);
	writefln("IR size: %s bytes", ir.storage[].length * uint.sizeof);
}

void dumpFunction(IrFunction* ir, ref TextSink sink)
{
	sink.putfln("function () {");
	bool printVars = false;
	bool printBlockIns =  true;
	bool printBlockRefs = false;
	bool printInstrIndexEnabled = true;
	bool printUses = true;
	bool printLive = true;
	int indexPadding = numDigitsInNumber(ir.storage.length);

	void printInstrIndex(IrIndex someIndex) {
		if (!printInstrIndexEnabled) return;
		sink.putf("%*s|", indexPadding, someIndex.storageUintIndex);
	}

	void printRegUses(IrIndex result) {
		auto vreg = &ir.get!IrVirtualRegister(result);
		sink.put(" uses [");
		foreach (i, index; vreg.users.range(ir))
		{
			if (i > 0) sink.put(", ");
			sink.putf("%s", index.printer(ir));
		}
		sink.put("]");
	}

	foreach (IrIndex blockIndex, ref IrBasicBlockInstr block; ir.blocks)
	{
		printInstrIndex(blockIndex);
		sink.putf("  %s", blockIndex.printer(ir));
		if (printBlockIns && block.predecessors.length > 0)
		{
			sink.putf(" in(");
			foreach(i, predIndex; block.predecessors.range(ir)) {
				if (i > 0) sink.put(", ");
				sink.putf("%s", predIndex.printer(ir));
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
				sink.putf("%s %s", phiArg.value.printer(ir), phiArg.basicBlock.printer(ir));
			}
			sink.put(")");
			if (printUses) printRegUses(phi.result);
			sink.putln;
		}

		// instrs
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			printInstrIndex(instrIndex);

			switch(instrHeader.op)
			{
				case IrOpcode.block_exit_jump:
					sink.putf("    jmp %s", block.successors[0, ir].printer(ir));
					break;

				case IrOpcode.block_exit_unary_branch:
					sink.putf("    if %s %s then %s else %s",
						instrHeader.unaryCond,
						instrHeader.args[0].printer(ir),
						block.successors[0, ir].printer(ir),
						block.successors[1, ir].printer(ir));
					break;

				case IrOpcode.block_exit_binary_branch:
					sink.putf("    if %s %s %s then ",
						instrHeader.args[0].printer(ir),
						binaryCondStrings[instrHeader.binaryCond],
						instrHeader.args[1].printer(ir));
					switch (block.successors.length) {
						case 0:
							sink.put("<null> else <null>");
							break;
						case 1:
							sink.putf("%s else <null>",
								block.successors[0, ir].printer(ir));
							break;
						default:
							sink.putf("%s else %s",
								block.successors[0, ir].printer(ir),
								block.successors[1, ir].printer(ir));
							break;
					}
					break;

				case IrOpcode.parameter:
					uint paramIndex = ir.get!IrInstrParameter(instrIndex).index;
					sink.putf("    %s = parameter%s", instrHeader.result.printer(ir), paramIndex);
					break;

				case IrOpcode.block_exit_return_void:
					sink.put("     return");
					break;

				case IrOpcode.block_exit_return_value:
					sink.putf("    return %s", instrHeader.args[0].printer(ir));
					break;

				default:
					if (instrHeader.hasResult)
						sink.putf("    %s = %s", instrHeader.result.printer(ir), cast(IrOpcode)instrHeader.op);
					else  sink.putf("    %s", cast(IrOpcode)instrHeader.op);
					foreach (i, IrIndex arg; instrHeader.args)
					{
						if (i > 0) sink.put(",");
						sink.putf(" %s", arg.printer(ir));
					}
					break;
			}

			if (printUses && instrHeader.hasResult) printRegUses(instrHeader.result);
			sink.putln;
		}
	}

	sink.putln("}");
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

	union
	{
		mixin(bitfields!(
			uint,        "storageUintIndex", 28, // is never 0 for defined index
			IrValueKind, "kind",              4  // is never 0 for defined index
		));
		uint asUint; // is never 0 for defined index
	}
	static assert(IrValueKind.max <= 0b1111, "4 bits are reserved");
	bool isDefined() { return asUint != 0; }

	void toString(scope void delegate(const(char)[]) sink) const {
		final switch(kind) with(IrValueKind) {
			case none: sink("<null>"); break;
			case listItem: sink.formattedWrite("l%s", storageUintIndex); break;
			case instruction: sink.formattedWrite("i%s", storageUintIndex); break;
			case basicBlock: sink.formattedWrite("@%s", storageUintIndex); break;
			case constant: sink.formattedWrite("c%s", storageUintIndex); break;
			case phi: sink.formattedWrite("φ%s", storageUintIndex); break;
			case memoryAddress: sink.formattedWrite("m%s", storageUintIndex); break;
			case virtualRegister: sink.formattedWrite("v%s", storageUintIndex); break;
			case physicalRegister: sink.formattedWrite("p%s", storageUintIndex); break;
		}
	}

	IrIndexPrinter printer(IrFunction* func) { return IrIndexPrinter(this, func); }

	/// When this index represents index of 0's array item, produces
	/// index of this array items. Calling with 0 returns itself.
	IrIndex indexOf(T)(size_t offset)
	{
		static assert(T.alignof == 4, "Can only point to types aligned to 4 bytes");
		IrIndex result = this;
		result.storageUintIndex = cast(uint)(storageUintIndex + divCeil(T.sizeof, uint.sizeof) * offset);
		return result;
	}
}

struct IrIndexPrinter
{
	IrIndex index;
	IrFunction* ir;

	void toString(scope void delegate(const(char)[]) sink) {
		final switch(index.kind) with(IrValueKind) {
			case none: sink("<null>"); break;
			case listItem: sink.formattedWrite("l%s", index.storageUintIndex); break;
			case instruction: sink.formattedWrite("i%s", index.storageUintIndex); break;
			case basicBlock: sink.formattedWrite("@%s", index.storageUintIndex); break;
			case constant: sink.formattedWrite("%s", ir.get!IrConstant(index).i64); break;
			case phi: sink.formattedWrite("φ%s", index.storageUintIndex); break;
			case memoryAddress: sink.formattedWrite("m%s", index.storageUintIndex); break;
			case virtualRegister: sink.formattedWrite("v%s", index.storageUintIndex); break;
			case physicalRegister: sink.formattedWrite("p%s", index.storageUintIndex); break;
		}
	}
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
	IrValueKind kind;
	uint numArgs;
	HasResult hasResult;
}

enum getInstrInfo(T) = getUDAs!(T, InstrInfo)[0];

/// Stores numeric constant data
@InstrInfo(IrValueKind.constant)
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
@InstrInfo(IrValueKind.phi)
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
@InstrInfo(IrValueKind.listItem)
struct IrPhiArg
{
	IrIndex value;
	IrIndex basicBlock;
	IrIndex nextListItem;
}

/// Per Basic Block info for unresolved Phi functions, when CFG is incomplete.
/// Finished IR contains no such values
@InstrInfo(IrValueKind.listItem)
struct IrIncompletePhi
{
	IrVar var;
	IrIndex phi;
	IrIndex nextListItem;
}

enum IrOpcode : ubyte
{
	parameter,
	block_header,
	block_exit_jump,
	block_exit_unary_branch,
	block_exit_binary_branch,
	block_exit_return_void,
	block_exit_return_value,
}

@InstrInfo(IrValueKind.virtualRegister)
struct IrVirtualRegister
{
	/// Index of instruction that defines this register
	IrIndex definition;
	/// List of instruction indicies that use this register
	SmallVector users;
}

/// Must end with one of block_exit_... instructions
@InstrInfo(IrValueKind.basicBlock)
struct IrBasicBlockInstr
{
	IrIndex firstInstr; // points to itself if no instructions
	IrIndex lastInstr; // points to itself if no instructions
	IrIndex prevBlock; // null only if this is entryBasicBlock
	IrIndex nextBlock; // null only if this is exitBasicBlock
	IrIndex firstPhi; // may be null

	PhiIterator phis(IrFunction* ir) { return PhiIterator(ir, &this); }
	InstrIterator instructions(IrFunction* ir) { return InstrIterator(ir, &this); }

	SmallVector predecessors;
	SmallVector successors;

	uint seqIndex;
	bool isSealed;
}
pragma(msg, "BB size: ", cast(int)IrBasicBlockInstr.sizeof, " bytes");

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
@InstrInfo(IrValueKind.instruction)
struct IrInstrHeader
{
	ushort op;
	ubyte numArgs;
	union
	{
		mixin(bitfields!(
			HasResult,         "hasResult",  1,
			IrBinaryCondition, "binaryCond", 3,
			uint, "",                        4
		));
		mixin(bitfields!(
			uint,             "",          1,
			IrUnaryCondition, "unaryCond", 3,
			uint, "",                      4
		));
	}
	static assert(IrBinaryCondition.max <= 0b111, "3 bits are reserved");
	static assert(IrUnaryCondition.max <= 0b111, "3 bits are reserved");
	//HasResult hasResult;
	IrIndex prevInstr;
	IrIndex nextInstr;

	IrIndex[0] _payload;
	ref IrIndex result() {
		return _payload.ptr[0];
	}
	IrIndex[] args() {
		return _payload.ptr[cast(size_t)hasResult..cast(size_t)hasResult+numArgs];
	}
}

template IrGenericInstr(uint numArgs, HasResult hasResult)
{
	@InstrInfo(IrValueKind.instruction, numArgs, hasResult)
	struct IrGenericInstr
	{
		IrInstrHeader header;
		static if (hasResult)   IrIndex result;
		static if (numArgs > 0) IrIndex[numArgs] args;
	}
}

alias IrReturnValueInstr = IrGenericInstr!(1, HasResult.no);

enum IrBinaryCondition : ubyte {
	eq,
	ne,
	l,
	le,
	g,
	ge
}

string[] binaryCondStrings = cast(string[IrBinaryCondition.max+1])["==", "!=", "<", "<=", ">", ">="];

/// Uses header.binaryCond
@InstrInfo(IrValueKind.instruction, 2, HasResult.no)
struct IrInstrBinaryBranch
{
	IrInstrHeader header;
	IrIndex[2] args;
}

enum IrUnaryCondition : ubyte {
	zero,
	not_zero
}

/// Uses header.unaryCond
@InstrInfo(IrValueKind.instruction, 1, HasResult.no)
struct IrInstrUnaryBranch
{
	IrInstrHeader header;
	IrIndex arg;
}

@InstrInfo(IrValueKind.instruction, 0, HasResult.yes)
struct IrInstrParameter
{
	IrInstrHeader header;
	IrIndex result;
	uint index;
}

struct IrVarId { uint id; alias id this; }
struct IrVar { Identifier name; IrVarId id; IrValueType type; }
struct Identifier { uint index; }

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
	FixedBuffer!uint storage;
	FixedBuffer!uint temp;

	uint numBasicBlocks;

	/// Special block. Automatically created. Program start. Created first.
	IrIndex entryBasicBlock;

	/// Special block. Automatically created. All returns must jump to it.
	IrIndex exitBasicBlock;

	// The last created basic block
	IrIndex lastBasicBlock;

	///
	IrValueType returnType;

	BlockIterator blocks() { return BlockIterator(&this); }

	alias getBlock = get!IrBasicBlockInstr;

	ref T get(T)(IrIndex index)
	{
		assert(index.kind != IrValueKind.none, "null index");
		assert(index.kind == getInstrInfo!T.kind, format("%s != %s", index.kind, getInstrInfo!T.kind));
		return *cast(T*)(&storage.bufPtr[index.storageUintIndex]);
	}

	ref T getTemp(T)(IrIndex index)
	{
		assert(index.kind != IrValueKind.none, "null index");
		assert(index.kind == getInstrInfo!T.kind, format("%s != %s", index.kind, getInstrInfo!T.kind));
		return *cast(T*)(&temp.bufPtr[index.storageUintIndex]);
	}

	/// Returns index to allocated item
	/// Allocates howMany items. By default allocates single item.
	/// If howMany > 1 - returns index of first item, access other items via IrIndex.indexOf
	/// T must have UDA of InstrInfo() value
	IrIndex append(T)(uint howMany = 1)
	{
		static assert(T.alignof == 4 || is(T == IrConstant), "Can only store types aligned to 4 bytes");

		IrIndex result;
		result.storageUintIndex = storage.length;
		result.kind = getInstrInfo!T.kind;

		storage.voidPut(divCeil(T.sizeof, uint.sizeof)*howMany);
		(&get!T(result))[0..howMany] = T.init;
		return result;
	}

	/// ditto
	IrIndex appendTemp(T)(uint howMany = 1)
	{
		static assert(T.alignof == 4 || is(T == IrConstant), "Can only store types aligned to 4 bytes");

		IrIndex result;
		result.storageUintIndex = temp.length;
		result.kind = getInstrInfo!T.kind;

		temp.voidPut(divCeil(T.sizeof, uint.sizeof)*howMany);
		(&getTemp!T(result))[0..howMany] = T.init;
		return result;
	}

	/// Adds control-flow edge pointing `fromBlock` -> `toBlock`.
	void addBlockTarget(IrIndex fromBasicBlockIndex, IrIndex toBasicBlockIndex) {
		getBlock(fromBasicBlockIndex).successors.append(&this, toBasicBlockIndex);
		getBlock(toBasicBlockIndex).predecessors.append(&this, fromBasicBlockIndex);
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

	void computeBlockOrder()
	{
		uint[] orderArray = temp.voidPut(numBasicBlocks);
		scope(exit) temp.clear;
		// todo
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
	IrFunction* ir;

	IrIndex currentBB;

	// Stores current definition of variable per block during SSA-form IR construction.
	private IrIndex[BlockVarPair] blockVarDef;
	private IrIndex[IrIndex] blockToIrIncompletePhi;

	private IrVarId nextIrVarId;

	IrVar returnVar;

	/// Must be called before compilation of each function. Allows reusing temp buffers.
	/// Sets up entry and exit basic blocks.
	void begin(IrFunction* ir) {
		this.ir = ir;
		blockVarDef.clear();

		// Add dummy item into storage, since 0 index represents null
		ir.storage.put(0);

		// Canonical function CFG has entry block, and single exit block.
		ir.numBasicBlocks = 2;

		ir.entryBasicBlock = ir.append!IrBasicBlockInstr;
		ir.exitBasicBlock = ir.append!IrBasicBlockInstr;

		ir.getBlock(ir.entryBasicBlock).nextBlock = ir.exitBasicBlock;
		ir.getBlock(ir.exitBasicBlock).prevBlock = ir.entryBasicBlock;
		currentBB = ir.entryBasicBlock;

		if (ir.returnType != IrValueType.void_t)
		{
			IrIndex retIndex = addInstruction!IrReturnValueInstr(ir.exitBasicBlock, IrOpcode.block_exit_return_value);
			returnVar = IrVar(Identifier(0), newIrVarId());
			IrIndex retValue = readVariable(ir.exitBasicBlock, returnVar);
			ir.get!IrReturnValueInstr(retIndex).args[0] = retValue;
			addUser(retIndex, retValue);
		}
	}

	/// Sets currentBB to this block
	IrIndex addBasicBlock() {
		assert(currentBB.isDefined);
		++ir.numBasicBlocks;
		IrIndex newBlock = ir.append!IrBasicBlockInstr;
		ir.getBlock(newBlock).nextBlock = ir.getBlock(currentBB).nextBlock;
		ir.getBlock(newBlock).prevBlock = currentBB;
		ir.getBlock(ir.getBlock(currentBB).nextBlock).prevBlock = newBlock;
		ir.getBlock(currentBB).nextBlock = newBlock;
		currentBB = newBlock;
		return currentBB;
	}

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
			IrIncompletePhi ip = ir.getTemp!IrIncompletePhi(index);
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
				ir.get!IrVirtualRegister(used).users.append(ir, user);
				break;
			case physicalRegister: break; // allowed, noop
		}
	}

	IrIndex addInstruction(I)(IrIndex blockIndex, ushort op) {
		IrBasicBlockInstr* block = &ir.getBlock(blockIndex);
		IrIndex instr = ir.append!I;
		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(instr);
		instrHeader.op = op;
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
		// TODO add users


		return instr;
	}

	IrIndex addBinBranch(IrIndex blockIndex, IrBinaryCondition cond, IrIndex arg0, IrIndex arg1)
	{
		auto branch = addInstruction!IrInstrBinaryBranch(blockIndex, IrOpcode.block_exit_binary_branch);
		with(ir.get!IrInstrBinaryBranch(branch)) {
			header.binaryCond = cond;
			args = [arg0, arg1];
		}
		addUser(branch, arg0);
		addUser(branch, arg1);
		return branch;
	}

	void addReturnValue(IrIndex blockIndex, IrIndex returnValue)
	{
		assert(ir.returnType != IrValueType.void_t);
		writeVariable(blockIndex, returnVar, returnValue);
		addJump(blockIndex);
		ir.addBlockTarget(blockIndex, ir.exitBasicBlock);
	}

	IrIndex addJump(IrIndex blockIndex)
	{
		return addInstruction!(IrGenericInstr!(0, HasResult.no))(blockIndex, IrOpcode.block_exit_jump);
	}

	void addJumpToLabel(IrIndex blockIndex, ref IrLabel label)
	{
		switch (label.numPredecessors)
		{
			case 0:
				label.numPredecessors = 1;
				label.blockIndex = blockIndex;
				break;
			case 1:
				label.numPredecessors = 1;
				IrIndex firstPred = label.blockIndex;
				label.blockIndex = addBasicBlock;
				ir.addBlockTarget(firstPred, label.blockIndex);
				addJump(firstPred);
				goto default;
			default:
				++label.numPredecessors;
				ir.addBlockTarget(blockIndex, label.blockIndex);
				addJump(blockIndex);
				break;
		}
	}

	IrIndex addConstant(IrConstant con) {
		IrIndex conIndex = ir.append!IrConstant;
		ir.get!IrConstant(conIndex) = con;
		return conIndex;
	}


	private void incBlockRefcount(IrIndex basicBlock) { assert(false); }
	private void decBlockRefcount(IrIndex basicBlock) { assert(false); }

	// Creates operand for result of phi/instruction that is defined by `definition`
	private IrIndex addVirtualRegister(IrIndex definition) {
		IrIndex virtRegIndex = ir.append!IrVirtualRegister;
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
	private IrIndex addPhi(IrIndex blockIndex) {
		IrIndex phiIndex = ir.append!IrPhiInstr;
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
		// TODO: free list of phis
		IrPhiInstr* phi = &ir.get!IrPhiInstr(phiIndex);
		IrBasicBlockInstr* block = &ir.getBlock(phi.blockIndex);
		if (block.firstPhi == phiIndex) block.firstPhi = phi.nextPhi;
		if (phi.nextPhi.isDefined) ir.get!IrPhiInstr(phi.nextPhi).prevPhi = phi.prevPhi;
		if (phi.prevPhi.isDefined) ir.get!IrPhiInstr(phi.prevPhi).nextPhi = phi.nextPhi;
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
					IrIndex incompletePhi = ir.appendTemp!IrIncompletePhi;
					ir.getTemp!IrIncompletePhi(incompletePhi) = IrIncompletePhi(variable, phiIndex);
					return incompletePhi;
				},
				(ref IrIndex oldPhi)
				{
					IrIndex incompletePhi = ir.appendTemp!IrIncompletePhi;
					ir.getTemp!IrIncompletePhi(incompletePhi) = IrIncompletePhi(variable, phiIndex, oldPhi);
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
			// Phi should not be cached before loop, since readVariable can add phi to phis, reallocating the array
			addPhiArg(phi, predIndex, value);
			addUser(phi, value);
		}
		return tryRemoveTrivialPhi(phi);
	}

	private void addPhiArg(IrIndex phiIndex, IrIndex blockIndex, IrIndex value)
	{
		IrIndex phiArg = ir.append!IrPhiArg;
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
			if (phiArg.value == same.value || phiArg.value == phiIndex) {
				continue; // Unique value or self−reference
			}
			if (same != IrPhiArg()) {
				return ir.get!IrPhiInstr(phiIndex).result; // The phi merges at least two values: not trivial
			}
			same = phiArg;
		}

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

	// ditto
	/// Rewrites all users of phi to point to `byWhat` instead of its result `what`.
	private void replaceBy(SmallVector phiUsers, IrIndex what, IrPhiArg byWhat) {
		foreach (size_t i, IrIndex userIndex; phiUsers.range(ir))
		{
			final switch (userIndex.kind) with(IrValueKind) {
				case none: assert(false);
				case listItem: assert(false);
				case instruction:
					foreach (ref IrIndex arg; ir.get!IrInstrHeader(userIndex).args)
						if (arg == what)
							arg = byWhat.value;
					break;
				case basicBlock: assert(false);
				case constant: assert(false);
				case phi:
					foreach (size_t i, ref IrPhiArg phiArg; ir.get!IrPhiInstr(userIndex).args(ir))
						if (phiArg.value == what)
							phiArg = byWhat;
					break;
				case memoryAddress: assert(false); // TODO
				case virtualRegister: assert(false);
				case physicalRegister: assert(false);
			}
		}
	}
}

/// Used for linked list
@InstrInfo(IrValueKind.listItem)
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
		return SmallVectorRange(this, ir);
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

	void append(IrFunction* ir, IrIndex itemData)
	{
		assert(itemData.kind != IrValueKind.listItem, "listItem is not storable inside SmallVector");
		assert(itemData.kind != IrValueKind.none, "IrValueKind.none is not storable inside SmallVector");
		if (isBig)
		{
			IrIndex newListItemIndex = ir.append!ListItem;
			ListItem* listItem = &ir.get!ListItem(newListItemIndex);
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
				IrIndex arrayIndex = ir.append!ListItem(3);
				ListItem* itemArray = &ir.get!ListItem(arrayIndex);
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
	SmallVector vector;
	IrFunction* ir;

	int opApply(scope int delegate(size_t, IrIndex) dg)
	{
		if (vector.isBig) // length > 2
		{
			IrIndex next = vector.firstListItem;
			size_t seqIndex = 0;
			while (next.isDefined)
			{
				ListItem* listItem = &ir.get!ListItem(next);
				next = listItem.nextItem;
				if (int res = dg(seqIndex, listItem.itemIndex))
					return res;
				++seqIndex;
			}
		}
		else // 0, 1, 2
		{
			if (vector.items[0].isDefined)
			{
				if (int res = dg(0, vector.items[0])) return res;
				if (vector.items[1].isDefined)
				{
					if (int res = dg(1, vector.items[1])) return res;
				}
			}
		}
		return 0;
	}
}
