/// IR that implements idea of storing everything (almost)
/// inside a single array;
/// Stores instructions in a linked list
module new_ir;

import std.stdio;
import std.string : format;
import std.traits : getUDAs;
import std.bitmanip : bitfields;
import std.format : formattedWrite, FormatSpec;

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
	IrBuilder builder;
	IrFunction ir;
	builder.begin(&ir);
	IrIndex param0Index = builder.addInstruction!IrInstrParameter(ir.entryBasicBlock, IrOpcode.parameter);
	ir.get!IrInstrParameter(param0Index).index = 0;

	IrIndex block0 = builder.addBasicBlock();
	ir.addBlockTarget(ir.entryBasicBlock, block0);
	builder.addInstruction!(IrGenericInstr!(0, HasResult.no))(ir.entryBasicBlock, IrOpcode.block_exit_jump);
	ir.addBlockTarget(block0, ir.exitBasicBlock);
	IrIndex zeroVal = builder.addConstant(IrConstant(0));

	auto br = builder.addInstruction!IrInstrBinaryBranch(block0, IrOpcode.block_exit_binary_branch);
	with(ir.get!IrInstrBinaryBranch(br)) {
		header.binaryCond = IrBinaryCondition.l;
		args = [param0Index, zeroVal];
	}


	TextSink sink;
	dumpFunction(ir, sink);
	writeln(sink.text);
	writefln("IR size: %s bytes", ir.storage[].length * uint.sizeof);
}

void dumpFunction(ref IrFunction func, ref TextSink sink)
{
	sink.putf("function (");
	//foreach (i, param; func.parameters)
	//{
	//	sink.putf("%s %%%s", param.type, IrNameProxy(ctx, func.variableNames[i]));
	//	if (i+1 < func.parameters.length) sink.put(", ");
	//}
	sink.putln(") {");

	bool printVars = false;
	bool printBlockIns =  true;
	bool printBlockRefs = false;
	bool printBlockInstrRange = false;
	bool printInstrIndex = true;
	bool printUses = true;
	bool printLive = true;


	void printRegUses(IrIndex result) {
		auto vreg = &func.get!IrVirtualRegister(result);
		sink.put(" uses [");
		foreach (i, index; vreg.users.range(&func))
		{
			if (i > 0) sink.put(", ");
			sink.putf(" %s", index);
		}
		sink.put("]");
	}

	foreach (IrIndex blockIndex, ref IrBasicBlockInstr block; func.blocks)
	{
		if (printInstrIndex) sink.put("   |");
		sink.putf("  %s", blockIndex);
		if (printBlockIns && block.predecessors.length > 0)
		{
			sink.putf(" in [");
			foreach(i, predIndex; block.predecessors.range(&func)) {
				if (i > 0) sink.put(", ");
				sink.putf("%s", predIndex);
			}
			sink.put("]");
		}
		sink.putln;
		//sink.putfln("%s", block.predecessors.items);
		// phis
		// instrs

		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(&func))
		{
			if (printInstrIndex) sink.putf("% 3s|", instrIndex.storageUintIndex);

			switch(instrHeader.op)
			{
				case IrOpcode.block_exit_jump:
					sink.putf("    jmp %s", block.successors[0, &func]); break;
				case IrOpcode.block_exit_unary_branch:
					sink.putf("    branch %s %s", instrHeader.unaryCond, instrHeader.args[0]);
					break;
				case IrOpcode.block_exit_binary_branch:
					sink.putf("    branch %s %s, %s", instrHeader.binaryCond, instrHeader.args[0], instrHeader.args[1]);
					break;
				case IrOpcode.parameter:
					uint paramIndex = func.get!IrInstrParameter(instrIndex).index;
					sink.putf("    %s = parameter%s", instrHeader.result, paramIndex);
					break;
				case IrOpcode.block_exit_return_void: sink.put("        return"); break;
				case IrOpcode.block_exit_return_value: sink.putf("        return %s", instrHeader.args[0]); break;
				default:
					//if (printInstrIndex) sink.putf("% 3s|", i);
					if (instrHeader.hasResult) {
						sink.putf("    %s = %s", instrHeader.result, cast(IrOpcode)instrHeader.op);
					}
					else  sink.putf("    %s", cast(IrOpcode)instrHeader.op);

					foreach (i, IrIndex arg; instrHeader.args)
					{
						if (i > 0) sink.put(",");
						sink.putf(" %s", arg);
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
			case none: sink("null"); break;
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

	auto instructions(IrFunction* ir) { return InstrIterator(ir, &this); }

	SmallVector predecessors;
	SmallVector successors;

	uint seqIndex;
}

pragma(msg, "BB size: ", cast(int)IrBasicBlockInstr.sizeof, " bytes");

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

enum IrBinaryCondition : ubyte {
	eq,
	ne,
	l,
	le,
	g,
	ge
}

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
struct IrVar { string name; IrVarId id; IrValueType type; }

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

struct IrPhiArg
{
	IrIndex arg;
	IrIndex basicBlock;
}

struct IrFunction
{
	Buffer!uint storage;
	Buffer!IrVirtualRegister virtualRegisters;
	Buffer!uint temp;

	uint numBasicBlocks;

	/// Special block. Automatically created. Program start. Created first.
	IrIndex entryBasicBlock;

	/// Special block. Automatically created. All returns must jump to it.
	IrIndex exitBasicBlock;

	// The last created basic block
	IrIndex lastBasicBlock;

	BlockIterator blocks() { return BlockIterator(&this); }

	alias getBlock = get!IrBasicBlockInstr;

	ref T get(T)(IrIndex index)
	{
		assert(index.kind == getInstrInfo!T.kind, format("%s != %s", index.kind, getInstrInfo!T.kind));
		assert(index.kind != IrValueKind.none, "null index");
		return *cast(T*)(&storage.bufPtr[index.storageUintIndex]);
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

	/// Adds control-flow edge pointing `fromBlock` -> `toBlock`.
	void addBlockTarget(IrIndex fromBasicBlockIndex, IrIndex toBasicBlockIndex) {
		getBlock(fromBasicBlockIndex).successors.append(&this, toBasicBlockIndex);
		getBlock(toBasicBlockIndex).predecessors.append(&this, fromBasicBlockIndex);
	}

	/// Does not remove its instructions/phis
	void removeBasicBlock(IrIndex basicBlockToRemove) {
		--numBasicBlocks;
		IrBasicBlockInstr* bb = &get!IrBasicBlockInstr(basicBlockToRemove);
		if (bb.prevBlock.isDefined)
			getBlock(bb.prevBlock).nextBlock = bb.nextBlock;
		if (bb.nextBlock.isDefined)
			getBlock(bb.nextBlock).prevBlock = bb.prevBlock;
	}

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


// papers:
// 1. Simple and Efficient Construction of Static Single Assignment Form
struct IrBuilder
{
	IrFunction* ir;

	IrIndex currentBB;

	// Stores current definition of variable per block during SSA-form IR construction.
	private IrIndex[BlockVarPair] blockVarDef;

	private IrVarId nextIrVarId;

	/// Must be called before compilation of each function. Allows reusing temp buffers.
	/// Sets up entry and exit basic blocks.
	void begin(IrFunction* ir) {
		this.ir = ir;

		// Add dummy item into storage, since 0 index represents null
		ir.storage.put(0);

		// Canonical function CFG has entry block, and single exit block.
		ir.numBasicBlocks = 2;

		ir.entryBasicBlock = ir.append!IrBasicBlockInstr;
		ir.exitBasicBlock = ir.append!IrBasicBlockInstr;

		ir.getBlock(ir.entryBasicBlock).nextBlock = ir.exitBasicBlock;
		ir.getBlock(ir.exitBasicBlock).prevBlock = ir.entryBasicBlock;
		currentBB = ir.entryBasicBlock;
	}

	/// Sets currentBB to this block
	IrIndex addBasicBlock() {
		++ir.numBasicBlocks;
		IrIndex newBlock = ir.append!IrBasicBlockInstr;
		ir.getBlock(newBlock).nextBlock = ir.getBlock(currentBB).nextBlock;
		ir.getBlock(newBlock).prevBlock = currentBB;
		ir.getBlock(ir.getBlock(currentBB).nextBlock).prevBlock = newBlock;
		ir.getBlock(currentBB).nextBlock = newBlock;
		currentBB = newBlock;
		return currentBB;
	}

	/// Places ending instruction of current Basic Block
	void finishBasicBlock() { }

	// Algorithm 4: Handling incomplete CFGs
	/// Basic block is sealed if no further predecessors will be added to the block.
	/// Sealed block is not necessarily filled.
	/// Ignores already sealed blocks.
	void sealBlock(IrIndex basicBlockToSeal) { }




	/// Allocates new variable id for this function. It should be bound to a variable
	/// and used with writeVariable, readVariable functions
	IrVarId newIrVarId() {
		return IrVarId(nextIrVarId++);
	}

	// Algorithm 1: Implementation of local value numbering
	/// Redefines `variable` with `value` within `basicBlock`. Is used for assignment to variable
	void writeVariable(IrVar variable, IrIndex basicBlock, IrIndex value) { assert(false); }

	/// Returns the value that currently defines `variable` within `basicBlock`
	IrIndex readVariable(IrVar variable, IrIndex basicBlock) { assert(false); }


	/// Puts `user` into a list of users of `used` value
	void addUser(IrIndex user, IrIndex used) { assert(false); }


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

		return instr;
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
	private void removeVirtualRegister(IrIndex vreg) { assert(false); }

	// Adds phi function to specified block
	private IrIndex addPhi(IrIndex basicBlock) { assert(false); }

	// Algorithm 2: Implementation of global value numbering
	private IrIndex readVariableRecursive(IrVar variable, IrIndex basicBlock) { assert(false); }

	// ditto
	private IrIndex addPhiOperands(IrVar variable, IrIndex phi, IrIndex basicBlock) { assert(false); }

	// Algorithm 3: Detect and recursively remove a trivial φ function
	private IrIndex tryRemoveTrivialPhi(IrIndex phi) { assert(false); }

	// ditto
	/// Rewrites all users of phi `phi` to point to `byWhat` instead.
	private void replaceBy(uint firstUser, IrIndex phi, IrPhiArg byWhat) { assert(false); }
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

struct TextSink
{
	import std.format : formattedWrite;
	import std.string : stripRight;

	Buffer!char data;

	void clear() { data.clear(); }
	string text() { return stripRight(cast(string)data.data); }

	void put(in char[] str)
	{
		if (str.length == 0) return;
		data.put(str);
		data.stealthPut('\0');
	}

	void putf(Args...)(const(char)[] fmt, Args args) { formattedWrite(&this, fmt, args); }
	void putfln(Args...)(const(char)[] fmt, Args args) { formattedWrite(&this, fmt, args); put("\n"); }
	void putln(const(char)[] str = null) { put(str); put("\n"); }
}

struct ChunkedBuffer
{

}

struct Buffer(T)
{
	import std.experimental.allocator.gc_allocator;
	alias allocator = GCAllocator.instance;

	T* bufPtr;
	uint capacity;
	T[] buf() { return bufPtr[0..capacity]; }
	// Must be kept private since it can be used to check for avaliable space
	// when used as output range
	private uint length;

	// postblit
	this(this)
	{
		import core.memory;
		void[] tmp = allocator.allocate(capacity*T.sizeof);
		T* newBufPtr = cast(T*)tmp.ptr;
		newBufPtr[0..length] = bufPtr[0..length];
		bufPtr = newBufPtr;
		GC.addRange(bufPtr, capacity * T.sizeof, typeid(T));
	}

	bool empty() { return length == 0; }

	void put(T[] items ...)
	{
		reserve(items.length);
		bufPtr[length..length+items.length] = items;
		length += cast(uint)items.length;
	}

	void put(R)(R itemRange)
	{
		foreach(item; itemRange)
			put(item);
	}

	void stealthPut(T item)
	{
		reserve(1);
		bufPtr[length] = item;
	}

	/// Increases length and returns void-initialized slice to be filled by user
	T[] voidPut(size_t howMany)
	{
		reserve(howMany);
		length += howMany;
		return buf[length-howMany..length];
	}

	ref T opIndex(size_t at)
	{
		return bufPtr[at];
	}

	ref T back() { return bufPtr[length-1]; }

	inout(T[]) data() inout {
		return bufPtr[0..length];
	}

	alias opSlice = data;

	void clear() nothrow {
		length = 0;
	}

	void reserve(size_t items)
	{
		if (capacity - length < items)
		{
			import core.memory;
			GC.removeRange(bufPtr);
			size_t newCapacity = nextPOT(capacity + items);
			void[] tmp = buf;
			allocator.reallocate(tmp, newCapacity*T.sizeof);
			bufPtr = cast(T*)tmp.ptr;
			capacity = cast(uint)(tmp.length / T.sizeof);
			GC.addRange(bufPtr, capacity * T.sizeof, typeid(T));
		}
	}

	void removeInPlace(size_t index)
	{
		if (index+1 != length)
		{
			bufPtr[index] = bufPtr[length-1];
		}
		--length;
	}

	void unput(size_t numItems)
	{
		length = cast(uint)(length - numItems);
	}
}

T divCeil(T)(T a, T b)
{
	return a / b + (a % b > 0);
}

T nextPOT(T)(T x)
{
	--x;
	x |= x >> 1;  // handle  2 bit numbers
	x |= x >> 2;  // handle  4 bit numbers
	x |= x >> 4;  // handle  8 bit numbers
	static if (T.sizeof >= 2) x |= x >> 8;  // handle 16 bit numbers
	static if (T.sizeof >= 4) x |= x >> 16; // handle 32 bit numbers
	static if (T.sizeof >= 8) x |= x >> 32; // handle 64 bit numbers
	++x;

	return x;
}
