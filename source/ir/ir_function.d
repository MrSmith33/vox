/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR Function
module ir.ir_function;

import std.string : format;

import all;

/// Stores info about single function in IR
/// All data of a function is stored in a number of arenas
/// Every function has its data stored sequentially in each arena, and items in each function have separate indexing
/// Indices are relative to the pointers.
/// All this implies that in order to modify the function's IR it needs to be at the end of each arena.
/// Then we can freely append new items to the end of arenas.
struct IrFunction
{
	IrInstrHeader* instrPtr;
	IrIndex* instrPayloadPtr;
	IrIndex* instrNextPtr;
	IrIndex* instrPrevPtr;
	IrPhi* phiPtr;
	uint* arrayPtr;
	IrVirtualRegister* vregPtr;
	// index 0 must be always entry block
	// index 1 must be always exit block
	IrBasicBlock* basicBlockPtr;
	StackSlot* stackSlotPtr;

	// Optional. Used for IR interpretation
	uint* vmSlotOffsets;
	// Optional. Used for IR interpretation
	uint frameSize;

	/// Used for instrPtr, instrNextPtr, instrPrevPtr
	uint numInstructions;
	uint numPayloadSlots;     /// instrPayloadPtr
	uint numPhis;             /// phiPtr
	uint arrayLength;         /// arrayPtr
	uint numVirtualRegisters; /// vregPtr
	uint numBasicBlocks;      /// basicBlockPtr
	uint numStackSlots;       /// stackSlotPtr


	/// Special block. Automatically created. Program entry. Created first.
	enum IrIndex entryBasicBlock = IrIndex(0, IrValueKind.basicBlock);
	/// Special block. Automatically created. All returns must jump to it.
	enum IrIndex exitBasicBlock = IrIndex(1, IrValueKind.basicBlock);

	/// IrTypeFunction index
	IrIndex type;
	///
	IrInstructionSet instructionSet;

	/// How much bytes we need to allocate in prolog and deallocate in epilog
	int stackFrameSize;
	// number of calls in the function
	// collected in abi lowering pass
	// if 0 or 1, we can merge stack allocation with function's stack
	// if 0, we can omit stack alignment
	uint numCalls;

	VregIterator virtualRegisters() return { return VregIterator(&this); }

	///
	Identifier name;

	CallConvention getCallConvEnum(CompilationContext* c) {
		return c.types.get!IrTypeFunction(type).callConv;
	}
	CallConv* getCallConv(CompilationContext* c) {
		return callConventions[c.types.get!IrTypeFunction(type).callConv];
	}

	IrBasicBlock[] blocksArray() {
		return basicBlockPtr[0..numBasicBlocks];
	}

	StackSlot[] stackSlots() {
		return stackSlotPtr[0..numStackSlots];
	}

	IrIndex lastBasicBlock() {
		if (numBasicBlocks < 2) return IrIndex();
		return getBlock(exitBasicBlock).prevBlock;
	}

	IrIndex firstVirtReg() {
		if (numVirtualRegisters == 0) return IrIndex();
		return IrIndex(0, IrValueKind.virtualRegister);
	}

	IrIndex lastVirtReg() {
		if (numVirtualRegisters == 0) return IrIndex();
		return IrIndex(numVirtualRegisters - 1, IrValueKind.virtualRegister);
	}

	BlockIterator blocks() return { return BlockIterator(&this); }
	BlockReverseIterator blocksReverse() return { return BlockReverseIterator(&this); }

	alias getBlock = get!IrBasicBlock;
	alias getPhi = get!IrPhi;
	alias getVirtReg = get!IrVirtualRegister;
	alias getInstr = get!IrInstrHeader;
	alias getStackSlot = get!StackSlot;

	T* get(T)(IrIndex index)
	{
		enum IrValueKind kind = getIrValueKind!T;
		assert(index.kind != IrValueKind.none, "get!"~T.stringof~" null index");
		assert(index.kind == kind, format("expected %s, got %s", kind, index.kind));
		static if (kind == IrValueKind.instruction)
			return cast(T*)(&instrPtr[index.storageUintIndex]);
		else static if (kind == IrValueKind.basicBlock)
			return &basicBlockPtr[index.storageUintIndex];
		else static if (kind == IrValueKind.phi)
			return &phiPtr[index.storageUintIndex];
		else static if (kind == IrValueKind.virtualRegister)
			return &vregPtr[index.storageUintIndex];
		else static if (kind == IrValueKind.stackSlot)
			return &stackSlotPtr[index.storageUintIndex];
		else
			static assert(false, format("Cannot get %s from IrFunction", T.stringof));
	}

	IrIndex* getArray(IrIndex index)
	{
		assert(index.kind != IrValueKind.none, "null index");
		assert(index.kind == IrValueKind.array, format("expected %s, got %s", IrValueKind.array, index.kind));
		return cast(IrIndex*)(&arrayPtr[index.storageUintIndex]);
	}

	// In correct code there must be no dangling basic blocks left
	// But if there were, they would appear after exit block
	void orderBlocks()
	{
		IrIndex first;
		IrIndex firstLink;

		void walk(IrIndex node)
		{
			IrBasicBlock* block = getBlock(node);
			block.visitFlag = true;
			foreach(ref IrIndex succ; block.successors.range(&this))
				if (!getBlock(succ).visitFlag)
					walk(succ);
			if (first.isDefined)
				linkSingleBlockBefore(&this, node, first);
			else
				firstLink = node;
			first = node;
		}

		walk(entryBasicBlock);

		// find last block by iterating forward
		IrIndex last;

		// clear all flags
		foreach (idx, ref IrBasicBlock block; blocks)
		{
			last = idx;
			block.visitFlag = false;
		}

		// bring exit block to the end of function
		if (last != exitBasicBlock) {
			moveBlockAfter(&this, exitBasicBlock, last);
		}
	}

	void removeAllPhis() {
		foreach (IrIndex index, ref IrBasicBlock block; blocks)
			block.removeAllPhis;
	}

	void freeIrArray(IrIndex offset, uint capacity) {
		// noop for now
	}

	void assignSequentialBlockIndices()
	{
		uint index;
		foreach (idx, ref IrBasicBlock block; blocks)
		{
			block.seqIndex = index++;
		}
	}

	IrIndex getValueType(CompilationContext* context, IrIndex someIndex)
	{
		return .getValueType(someIndex, &this, context);
	}

	ref IrIndex prevInstr(IrIndex instrIndex) {
		return instrPrevPtr[instrIndex.storageUintIndex];
	}

	ref IrIndex nextInstr(IrIndex instrIndex) {
		return instrNextPtr[instrIndex.storageUintIndex];
	}

	size_t byteLength() {
		return
			(IrInstrHeader.sizeof + uint.sizeof + uint.sizeof) * numInstructions +
			IrIndex.sizeof * numPayloadSlots +
			IrPhi.sizeof * numPhis +
			uint.sizeof * arrayLength +
			IrVirtualRegister.sizeof * numVirtualRegisters +
			IrBasicBlock.sizeof * numBasicBlocks;
	}

	void dumpStackSlots(CompilationContext* context)
	{
		writefln("Slots %s, size 0x%X", numStackSlots, stackFrameSize);
		foreach (i, ref slot; stackSlots)
		{
			writefln("% 2s size 0x%X align %s disp 0x%X", i, slot.sizealign.size, slot.sizealign.alignment, slot.displacement);
		}
	}
}

void dupSingleIrStorage(T)(ref Arena!T arena, ref T* ptr, uint length) {
	//writefln("arena %s %s..%s %s", arena.length, ptr, ptr + length, length);
	T[] buf = ptr[0..length];
	ptr = arena.nextPtr;
	arena.put(buf);
	//writefln("arena %s %s..%s %s", arena.length, ptr, ptr + length, length);
}

void dupIrStorage(IrFunction* ir, CompilationContext* c)
{
	dupSingleIrStorage(c.irStorage.instrHeaderBuffer, ir.instrPtr, ir.numInstructions);
	dupSingleIrStorage(c.irStorage.instrPayloadBuffer, ir.instrPayloadPtr, ir.numPayloadSlots);
	dupSingleIrStorage(c.irStorage.instrNextBuffer, ir.instrNextPtr, ir.numInstructions);
	dupSingleIrStorage(c.irStorage.instrPrevBuffer, ir.instrPrevPtr, ir.numInstructions);
	dupSingleIrStorage(c.irStorage.phiBuffer, ir.phiPtr, ir.numPhis);
	dupSingleIrStorage(c.irStorage.vregBuffer, ir.vregPtr, ir.numVirtualRegisters);
	dupSingleIrStorage(c.irStorage.arrayBuffer, ir.arrayPtr, ir.arrayLength);
	dupSingleIrStorage(c.irStorage.basicBlockBuffer, ir.basicBlockPtr, ir.numBasicBlocks);
	dupSingleIrStorage(c.irStorage.stackSlotBuffer, ir.stackSlotPtr, ir.numStackSlots);
}

// mainIr must be in editable state, at the end of all arenas
// irToCopy is appended after mainIr and mainIr length is updated.
// Appended slots are iterated and all references are updated
void appendIrStorage(IrFunction* mainIr, const IrFunction* irToCopy, CompilationContext* c)
{
	// create table of offsets per IrValueKind
	// align to cache line
	align(64) uint[16] offsets;
	offsets[IrValueKind.instruction]     = mainIr.numInstructions;
	offsets[IrValueKind.basicBlock]      = mainIr.numBasicBlocks;
	offsets[IrValueKind.phi]             = mainIr.numPhis;
	offsets[IrValueKind.virtualRegister] = mainIr.numVirtualRegisters;
	offsets[IrValueKind.array]           = mainIr.arrayLength;
	offsets[IrValueKind.stackSlot]       = mainIr.numStackSlots;
	// others remain 0 and do not affect IrIndex being fixed

	void dupAndFixStorage(T)(ref Arena!T arena, const T* ptr, uint length) {
		const(IrIndex)[] oldData = cast(const(IrIndex)[])ptr[0..length];
		IrIndex[] newDataBuf = cast(IrIndex[])arena.voidPut(length);

		foreach(i, ref IrIndex index; newDataBuf) {
			IrIndex oldIndex = oldData[i];
			// Fix each IrIndex. Only affects IrIndex when offset is non-zero. Otherwise copies without modification
			// Incrementing the whole uint is safe as long as `storageUintIndex` part doesn't overflow
			index.asUint = oldIndex.asUint + offsets[oldIndex.kind];
		}
	}

	IrInstrHeader[] instrs = c.irStorage.instrHeaderBuffer.put(irToCopy.instrPtr[0..irToCopy.numInstructions]); // dup IrInstrHeader
	foreach(ref IrInstrHeader instr; instrs) instr._payloadOffset += mainIr.numPayloadSlots; // fix
	// phis, vregs and basic block consist out of IrIndex entries or have integer data of type IrValueKind.none.
	// The bitflags are designed so that 4 bits are 0 at the time of this operation
	dupAndFixStorage(c.irStorage.instrPayloadBuffer, irToCopy.instrPayloadPtr, irToCopy.numPayloadSlots);
	dupAndFixStorage(c.irStorage.instrNextBuffer, irToCopy.instrNextPtr, irToCopy.numInstructions);
	dupAndFixStorage(c.irStorage.instrPrevBuffer, irToCopy.instrPrevPtr, irToCopy.numInstructions);
	dupAndFixStorage(c.irStorage.phiBuffer, irToCopy.phiPtr, irToCopy.numPhis);
	dupAndFixStorage(c.irStorage.vregBuffer, irToCopy.vregPtr, irToCopy.numVirtualRegisters);
	dupAndFixStorage(c.irStorage.arrayBuffer, irToCopy.arrayPtr, irToCopy.arrayLength);
	dupAndFixStorage(c.irStorage.basicBlockBuffer, irToCopy.basicBlockPtr, irToCopy.numBasicBlocks);
	// dup StackSlot. Fixes are not needed because StackSlot.type and StackSlot.baseReg are of type and physical register kind.
	StackSlot[] stackSlots = c.irStorage.stackSlotBuffer.put(irToCopy.stackSlotPtr[0..irToCopy.numStackSlots]);

	// make sure numInstructions is incremented once
	mainIr.numInstructions += irToCopy.numInstructions;
	mainIr.numPayloadSlots += irToCopy.numPayloadSlots;
	mainIr.numPhis += irToCopy.numPhis;
	mainIr.numVirtualRegisters += irToCopy.numVirtualRegisters;
	mainIr.arrayLength += irToCopy.arrayLength;
	mainIr.numBasicBlocks += irToCopy.numBasicBlocks;
	mainIr.numStackSlots += irToCopy.numStackSlots;
}

struct IrFuncStorage
{
	Arena!IrInstrHeader instrHeaderBuffer;
	Arena!IrIndex instrPayloadBuffer; // stores variadic results + arguments per instruction
	Arena!IrIndex instrNextBuffer; // index of next instruction
	Arena!IrIndex instrPrevBuffer; // index of previous instruction
	Arena!IrPhi phiBuffer;
	Arena!IrVirtualRegister vregBuffer;
	Arena!uint arrayBuffer; // stores data of IrSmallArray
	Arena!IrBasicBlock basicBlockBuffer;
	Arena!StackSlot stackSlotBuffer;

	void printMemSize(ref TextSink sink)
	{
		size_t byteLength;
		size_t committedBytes;
		size_t reservedBytes;
		void collect(T)(ref Arena!T arena) {
			byteLength += arena.byteLength;
			committedBytes += arena.committedBytes;
			reservedBytes += arena.reservedBytes;
		}
		collect(instrHeaderBuffer);
		collect(instrPayloadBuffer);
		collect(instrNextBuffer);
		collect(instrPrevBuffer);
		collect(phiBuffer);
		collect(vregBuffer);
		collect(arrayBuffer);
		collect(basicBlockBuffer);
		collect(stackSlotBuffer);
		sink.putfln("  %-16s%-6iB    %-6iB   %-6iB",
			"  IR total",
			scaledNumberFmt(byteLength),
			scaledNumberFmt(committedBytes),
			scaledNumberFmt(reservedBytes));
	}
}

// phi iterators are aware of this
// only safe to delete current phi while iterating
void removePhi(CompilationContext* context, IrFunction* ir, IrIndex phiIndex)
{
	version(IrPrint) writefln("[IR] remove phi %s", phiIndex);
	IrPhi* phi = ir.getPhi(phiIndex);
	IrBasicBlock* block = ir.getBlock(phi.blockIndex);
	version(IrPrint) {
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir)) {
			writefln("[IR]   %s = %s", phi.result, phiIndex);
		}
	}
	// TODO: free list of phis
	if (block.firstPhi == phiIndex) block.firstPhi = phi.nextPhi;
	if (phi.nextPhi.isDefined) ir.getPhi(phi.nextPhi).prevPhi = phi.prevPhi;
	if (phi.prevPhi.isDefined) ir.getPhi(phi.prevPhi).nextPhi = phi.nextPhi;
	version(IrPrint) writefln("[IR] after remove phi %s", phiIndex);
	version(IrPrint) {
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir)) {
			writefln("[IR]   %s = %s", phi.result, phiIndex);
		}
	}

	// mark as removed
	phi.blockIndex = IrIndex();

	// remove args
	foreach(IrIndex arg; phi.args(ir)) {
		removeUser(context, ir, phiIndex, arg);
	}
	phi.args.free(ir);
}

// instruction iterators are aware of this
// only safe to delete current instruction while iterating
void removeInstruction(IrFunction* ir, IrIndex instrIndex)
{
	if (ir.prevInstr(instrIndex).isInstruction)
		ir.nextInstr(ir.prevInstr(instrIndex)) = ir.nextInstr(instrIndex);
	else if (ir.prevInstr(instrIndex).isBasicBlock)
		ir.getBlock(ir.prevInstr(instrIndex)).firstInstr = ir.nextInstr(instrIndex);
	else assert(false);

	if (ir.nextInstr(instrIndex).isInstruction)
		ir.prevInstr(ir.nextInstr(instrIndex)) = ir.prevInstr(instrIndex);
	else if (ir.nextInstr(instrIndex).isBasicBlock)
		ir.getBlock(ir.nextInstr(instrIndex)).lastInstr = ir.prevInstr(instrIndex);
	else assert(false);
}

// ditto
void replaceInstruction(IrFunction* ir, IrIndex instrIndex, IrIndex replaceBy)
{
	ir.prevInstr(replaceBy) = ir.prevInstr(instrIndex);
	ir.nextInstr(replaceBy) = ir.nextInstr(instrIndex);

	if (ir.prevInstr(instrIndex).isInstruction)
		ir.nextInstr(ir.prevInstr(instrIndex)) = replaceBy;
	else if (ir.prevInstr(instrIndex).isBasicBlock)
		ir.getBlock(ir.prevInstr(instrIndex)).firstInstr = replaceBy;
	else assert(false);

	if (ir.nextInstr(instrIndex).isInstruction)
		ir.prevInstr(ir.nextInstr(instrIndex)) = replaceBy;
	else if (ir.nextInstr(instrIndex).isBasicBlock)
		ir.getBlock(ir.nextInstr(instrIndex)).lastInstr = replaceBy;
	else assert(false);
}

void removeUser(CompilationContext* context, IrFunction* ir, IrIndex user, IrIndex used) {
	assert(used.isDefined, "used is undefined");
	final switch (used.kind) with(IrValueKind) {
		case none: assert(false, "removeUser none");
		case array: assert(false, "removeUser array");
		case instruction: assert(false, "removeUser instruction");
		case basicBlock: break; // allowed. As argument of jmp jcc
		case constant, constantAggregate, constantZero: break; // allowed, noop
		case global:
			context.globals.get(used).removeUser(user);
			break;
		case phi: assert(false, "removeUser phi"); // must be virt reg instead
		case stackSlot: break; // allowed, noop
		case virtualRegister:
			ir.getVirtReg(used).users.remove(ir, user);
			break;
		case physicalRegister: break; // allowed, noop
		case type: break; // no user tracking
		case variable: assert(false);
		case func: break; // allowed, noop
	}
}

struct BlockIterator
{
	IrFunction* ir;
	int opApply(scope int delegate(IrIndex, ref IrBasicBlock) dg) {
		IrIndex next = ir.entryBasicBlock;
		while (next.isDefined)
		{
			IrBasicBlock* block = ir.getBlock(next);
			if (int res = dg(next, *block))
				return res;
			next = block.nextBlock;
		}
		return 0;
	}
}

struct VregIterator
{
	IrFunction* ir;
	int opApply(scope int delegate(IrIndex, ref IrVirtualRegister) dg) {
		foreach(size_t i, ref IrVirtualRegister vreg; ir.vregPtr[0..ir.numVirtualRegisters])
			if (int res = dg(IrIndex(cast(uint)i, IrValueKind.virtualRegister), vreg))
				return res;
		return 0;
	}
}

struct BlockReverseIterator
{
	IrFunction* ir;
	int opApply(scope int delegate(IrIndex, ref IrBasicBlock) dg) {
		IrIndex prev = ir.exitBasicBlock;
		while (prev.isDefined)
		{
			IrBasicBlock* block = ir.getBlock(prev);
			if (int res = dg(prev, *block))
				return res;
			prev = block.prevBlock;
		}
		return 0;
	}
}
