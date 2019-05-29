/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

///
module be.stack_layout;

import std.stdio;
import std.string : format;
import all;

// Stack Allocation
// https://docs.microsoft.com/en-us/cpp/build/stack-allocation?view=vs-2017

// Before prolog our layout is this
// ++             slot index
// ???? paramN    PN-1 rsp +  8 + N*8
// ???? ...
// XXZ0 param5    P5   rsp + 40
// XXY8 param4    S3   rsp + 32                     \
// XXY0 param3    S2   rsp + 24                      \ shadow space
// XXX8 param2    S1   rsp + 16                      /
// XXX0 param1    S0   rsp + 8                      / aligned to 16 bytes
// XXW8 ret addr       rsp + 0     <-- RSP
// --

// Shadow space is prioritized for first 4 arguments
// After allocating local stack space
// ++             slot index
// ???? paramN    PN-1 rsp +  8 + N*8              N parameters
// ???? ...
// XXZ0 param5    P5   rsp + 72
// XXY8 shadow3   S3   rsp + 64                     \
// XXY0 shadow2   S2   rsp + 56                      \ shadow space
// XXX8 shadow1   S1   rsp + 48                      /
// XXX0 shadow0   S0   rsp + 40                     / aligned to 16 bytes
// XXW8 ret addr       rsp + 32
// XXW0 local0    L0   rsp + 24      \
// XXV8 local1    L1   rsp + 16       \ numLocals = 4 (example)
// XXV0 local2    L2   rsp +  8       /
// XXU8 local3    L3   rsp +  0      /  <-- RSP
// optional padding                     <-- RSP
// --
// all space after ret addr is of size 'layout.reservedBytes'

enum STACK_ITEM_SIZE = 8; // x86_64

/// Arranges items on the stack according to calling convention
void pass_stack_layout(ref CompilationContext context, ref ModuleDeclNode mod, ref FunctionDeclNode func)
{
	if (func.isExternal) return;

	auto layout = &func.backendData.stackLayout;
	// TODO: Win64 calling convention hardcoded

	context.assertf(layout.maxAlignment <= 16, "Big alingments aren't implemented");
	/*
	if (context.useFramePointer)
	{
		// ++        varIndex
		// shadow4                                  \
		// shadow3                                   \ shadow space
		// param2    1              \                /
		// param1    0  rbp + 2     / numParams = 2 / this address is aligned to 16 bytes
		// ret addr     rbp + 1
		// rbp      <-- rbp + 0
		// local1    2  rbp - 1     \
		// local2    3  rbp - 2     / numLocals = 2
		// --
		if (isParameter) // parameter
		{
			index = 2 + varIndex;
		}
		else // local variable
		{
			index = -(varIndex - numParams + 1);
		}
		baseReg = func.callingConvention.framePointer;
	}*/

	IrIndex baseReg = func.backendData.callingConvention.stackPointer;

	// 1, 2, 4, 8, 16
	uint[5] numAlignments;
	uint[5] alignmentSizes;
	uint[5] alignmentOffsets;

	uint alignToIndex(uint alignment) {
		switch(alignment) {
			case 1: return 0;
			case 2: return 1;
			case 4: return 2;
			case 8: return 3;
			case 16: return 4;
			default: assert(false);
		}
	}

	layout.reservedBytes = 0;

	foreach (i, ref slot; layout.slots)
	{
		if (slot.isParameter) continue;

		uint index = alignToIndex(slot.alignment);
		++numAlignments[index];
		alignmentSizes[index] += slot.size;
		layout.reservedBytes += slot.size;
	}
	//writefln("reservedBytes1 0x%X", layout.reservedBytes);

	if (context.useFramePointer)
	{
		baseReg = func.backendData.callingConvention.framePointer;
		// frame pointer is stored together with locals
		layout.reservedBytes += STACK_ITEM_SIZE;
	}
	//writefln("reservedBytes2 0x%X", layout.reservedBytes);

	alignmentOffsets[4] = 0;
	foreach_reverse (i; 0..4)
	{
		alignmentOffsets[i] = alignmentOffsets[i+1] + alignmentSizes[i+1];
	}

	// align to 8 bytes first
	layout.reservedBytes = alignValue(layout.reservedBytes, STACK_ITEM_SIZE);
	//writefln("reservedBytes3 0x%X", layout.reservedBytes);

	// Align to 16 bytes
	// Before we are called, the stack is aligned to 16 bytes, after call return address is pushed
	// We take into account the return address (extra 8 bytes)
	if ((layout.reservedBytes + STACK_ITEM_SIZE) % 16 != 0) {
		layout.reservedBytes += STACK_ITEM_SIZE;
	}
	//writefln("reservedBytes4 0x%X", layout.reservedBytes);

	uint paramsSize = STACK_ITEM_SIZE * layout.numParamSlots;
	uint paramsOffset = layout.reservedBytes + STACK_ITEM_SIZE; // locals size + ret addr

	// TODO utilize shadow space

	int nextLocalIndex = 0;
	foreach (i, ref slot; layout.slots)
	{
		if (slot.isParameter)
		{
			slot.displacement = paramsOffset + slot.paramIndex * STACK_ITEM_SIZE;
		}
		else
		{
			uint index = alignToIndex(slot.alignment);
			alignmentSizes[index] -= slot.size;
			slot.displacement = alignmentOffsets[index] + alignmentSizes[index];
		}
		slot.baseReg = baseReg;
	}

	if (context.printStackLayout) layout.dump(&context);
}

struct StackLayout
{
	/// How much bytes we need to allocate in prolog and deallocate in epilog
	int reservedBytes;
	int numParamSlots;
	uint maxAlignment = 1;
	// number of calls in the function
	// TODO: if 0 or 1, we can merge stack allocation with function's stack
	// TODO: if 0, we can omit stack alignment
	uint numCalls;
	Array!StackSlot slots;

	ref StackSlot opIndex(IrIndex slotIndex) {
		assert(slotIndex.kind == IrValueKind.stackSlot);
		return slots[slotIndex.storageUintIndex];
	}

	/// paramIndex == -1 for non-params
	IrIndex addStackItem(CompilationContext* context, IrIndex type, bool isParameter, ushort paramIndex)
	{
		uint id = cast(uint)(slots.length);
		StackSlot slot = StackSlot(context.types.typeSize(type), context.types.typeAlignment(type), isParameter, paramIndex);
		slot.type = context.types.appendPtr(type);

		assert(slot.alignment.isPowerOfTwo, format("alignment is not power of 2 (%s)", slot.alignment));
		assert((slot.size / slot.alignment) * slot.alignment == slot.size,
			"size is not multiple of alignment");

		maxAlignment = max(maxAlignment, slot.alignment);

		if(isParameter) ++numParamSlots;

		slots.put(context.arrayArena, slot);
		return IrIndex(id, IrValueKind.stackSlot);
	}

	void dump(CompilationContext* context)
	{
		writefln("Slots %s, size 0x%X", slots.length, reservedBytes);
		foreach (i, ref slot; slots)
		{
			writefln("% 2s size 0x%X align %s disp 0x%X", i, slot.size, slot.alignment, slot.displacement);
		}
	}
}

struct StackSlot
{
	uint size;
	uint alignment;
	bool isParameter;
	ushort paramIndex;
	ushort numUses;
	/// Signed offset from base register
	int displacement;
	/// Base register (stack or frame pointer)
	IrIndex baseReg;
	/// Must be a pointer type
	IrIndex type;
	void addUser() { ++numUses; }
}
