/// IR that implements idea of storing everything (almost)
/// inside a single array;
/// Stores instructions in a linked list
module new_ir;

import std.stdio;
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
	IrIndex block0 = builder.addBasicBlock();
	ir.addBlockTarget(ir.entryBasicBlock, block0);
	ir.addBlockTarget(block0, ir.exitBasicBlock);

	writeln(ir.storage[]);
	writeln(ir);
	foreach(i, b; ir.blocks) writefln("%s %s", i, b);
	writefln("IR size: %s bytes", ir.storage[].length * uint.sizeof);
}

struct IrIndex
{
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
		sink.formattedWrite("%s.%s", storageUintIndex, kind);
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

enum IrValueKind : ubyte
{
	none, /// Used for undefined indicies
	listItem,
	instruction,
	basicBlock,
	constant,
	phi,
	memoryAddress,
	virtualRegister,
	physicalRegister,
}

enum IrOpcode : ubyte
{
	undefined, // placed in 0 position of storage
	block_header,
	block_exit_jump,
	block_exit_branch_unary,
	block_exit_branch_binary,
	block_exit_return_void,
	block_exit_return_value,
}

/// Must end with one of block_exit_... instructions
@(IrValueKind.basicBlock)
struct IrBasicBlockInstr
{
	IrIndex prevBlock; // null only if this is entryBasicBlock
	IrIndex nextBlock; // null only if this is exitBasicBlock
	IrIndex firstInstr; // not null
	IrIndex firstPhi; // may be null

	SmallVector predecessors;
	SmallVector successors;

	uint seqIndex;
}

struct IrInstructionHeader
{
	uint op;
	IrIndex prevInstr;
	IrIndex nextInstr;
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
	Buffer!uint temp;

	uint numBasicBlocks;

	/// Special block. Automatically created. Program start. Created first.
	IrIndex entryBasicBlock;

	/// Special block. Automatically created. All returns must jump to it.
	IrIndex exitBasicBlock;

	// The last created basic block
	IrIndex lastBasicBlock;

	BasicBlockRange blocks() { return BasicBlockRange(&this); }

	alias getBlock = get!IrBasicBlockInstr;

	T* get(T)(IrIndex index)
	{
		return cast(T*)(&storage.bufPtr[index.storageUintIndex]);
	}

	/// Returns index to allocated item
	/// Allocates howMany items. By default allocates single item.
	/// If howMany > 1 - returns index of first item, access other items via IrIndex.indexOf
	IrIndex append(T)(uint howMany = 1)
	{
		static assert(T.alignof == 4, "Can only store types aligned to 4 bytes");
		alias udas = getUDAs!(T, IrValueKind);

		IrIndex result;
		result.storageUintIndex = storage.length;

		static if (udas.length == 0)
			result.kind = IrValueKind.none;
		else static if (udas.length == 1)
			result.kind = udas[0];
		else static assert(false, "T must have 1 or 0 IrValueKind UDAs attached");

		storage.voidPut(divCeil(T.sizeof, uint.sizeof)*howMany);
		get!T(result)[0..howMany] = T.init;
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
		IrBasicBlockInstr* bb = get!IrBasicBlockInstr(basicBlockToRemove);
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

struct BasicBlockRange
{
	IrFunction* ir;

	int opApply(scope int delegate(IrIndex, ref IrBasicBlockInstr) dg)
	{
		IrIndex next = ir.entryBasicBlock;
		while (next.isDefined)
		{
			IrBasicBlockInstr* block = ir.getBlock(next);
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


	void addInstruction(IrIndex basicBlock) { }


	private void incBlockRefcount(IrIndex basicBlock) { assert(false); }
	private void decBlockRefcount(IrIndex basicBlock) { assert(false); }

	// Creates operand for result of phi/instruction that is defined by `definition`
	private IrIndex addVirtualRegister(IrIndex definition) { assert(false); }

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
		else if (items[0].isDefined)
			return 0;
		else if (items[1].isDefined)
			return 1;
		else
			return 2;
	}

	bool isBig()
	{
		return items[0].kind == IrValueKind.listItem;
	}

	SmallVectorRange range(IrFunction* ir)
	{
		return SmallVectorRange(this, ir);
	}

	void append(IrFunction* ir, IrIndex itemData)
	{
		if (isBig)
		{
			IrIndex newListItemIndex = ir.append!ListItem;
			ListItem* listItem = ir.get!ListItem(newListItemIndex);
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
				ListItem* itemArray = ir.get!ListItem(arrayIndex);
				itemArray[2] = ListItem(itemData);
				itemArray[1] = ListItem(items[1], arrayIndex.indexOf!ListItem(2));
				itemArray[0] = ListItem(items[0], arrayIndex.indexOf!ListItem(1));
				firstListItem = arrayIndex;
				listLength = 3;
			}
		}
	}

	void toString(scope void delegate(const(char)[]) sink) {
		if (isBig)
			sink.formattedWrite("[%s items]", listLength);
		else if (items[1].isDefined)
 			sink.formattedWrite("[%s, %s]", items[0], items[1]);
		else if (items[0].isDefined)
 			sink.formattedWrite("[%s]", items[0]);
		else sink("[]");
	}
}

struct SmallVectorRange
{
	SmallVector vector;
	IrFunction* ir;

	int opApply(scope int delegate(IrIndex) dg)
	{
		if (vector.isBig) // length > 2
		{
			IrIndex next = vector.firstListItem;
			while (next.isDefined)
			{
				ListItem* listItem = ir.get!ListItem(next);
				next = listItem.nextItem;
				if (int res = dg(listItem.itemIndex))
					return res;
			}
		}
		else // 0, 1, 2
		{
			if (vector.items[0].isDefined)
			{
				if (int res = dg(vector.items[0])) return res;
				if (vector.items[1].isDefined)
				{
					if (int res = dg(vector.items[1])) return res;
				}
			}
		}
		return 0;
	}
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
