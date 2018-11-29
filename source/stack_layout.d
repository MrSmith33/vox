/**
Copyright: Copyright (c) 2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

///
module stack_layout;

import all;

/// Arranges items on the stack according to calling convention
void pass_stack_layout(ref CompilationContext ctx)
{
	foreach (FunctionDeclNode* func; ctx.mod.functions)
	{
		if (func.isExternal) continue;

		enum STACK_ITEM_SIZE = 8; // x86_64
		auto layout = &func.stackLayout;
		// TODO: Win64 calling convention hardcoded

		// Before prolog our layout is this
		// ++             slot index
		// ???? paramN    PN-1 rsp +  8 + N*8
		// ???? ...
		// XXY8 param5    P5   rsp + 40
		// XXY0 param4    S3   rsp + 32                     \
		// XXY8 param3    S2   rsp + 24                      \ shadow space
		// XXY0 param2    S1   rsp + 16                      /
		// XXX8 param1    S0   rsp + 8                      /
		// XXX0 ret addr       rsp + 0     <-- RSP is aligned to 16 bytes
		// --

		// Shadow space is prioritized for first 4 arguments
		// After allocating local stack space
		// ++             slot index
		// ???? paramN    PN-1 rsp +  8 + N*8              N parameters
		// ???? ...
		// XXZ8 param5    P5   rsp + 40
		// XXZ0 shadow3   S3   rsp + 32                     \
		// XXY8 shadow2   S2   rsp + 24                      \ shadow space
		// XXY0 shadow1   S1   rsp + 16                      /
		// XXX8 shadow0   S0   rsp + 8                      /
		// XXX0 ret addr       rsp + 0       aligned to 16 bytes
		// XXW8 local0    L0   rsp +  10     \
		// XXW0 local1    L1   rsp +  8       \ numLocals = 4 (example)
		// XXV8 local2    L2   rsp +  0       /
		// XXV0 local3    L2   rsp +  0      /  <-- RSP
		// --

		int numParams = cast(int)func.parameters.length;
		uint freeShadowSlots = 4 - min(numParams, 4); // 0 - 4 free shadow slots for locals
		uint requiredLocalSlots = 0;
		// allocate local slots for all locals not fitting into shadow slots
		if (layout.numLocalSlots > freeShadowSlots)
			requiredLocalSlots = layout.numLocalSlots - freeShadowSlots;
		layout.reservedBytes = requiredLocalSlots * STACK_ITEM_SIZE;

		/*
		if (ctx.useFramePointer)
		{
			// ++        varIndex
			// shadow4                                  \
			// shadow3                                   \ shadow space
			// param2    1              \                /
			// param1    0  rbp + 2     / numParams = 2 /
			// ret addr     rbp + 1     this address is aligned to 16 bytes
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

		// TODO: We are assuming all stots to require 8 bytes

		IrIndex baseReg = func.callingConvention.stackPointer;

		if (ctx.useFramePointer)
		{
			baseReg = func.callingConvention.framePointer;
		}

		int paramSlotDisplacement(uint paramIndex) {
			return (layout.numLocalSlots + paramIndex + 1/*ret addr*/) * STACK_ITEM_SIZE;
		}

		int nextLocalIndex = 0;
		foreach (ref slot; layout.slots)
		{
			if (slot.isParameter)
			{
				// alloc shadow slot
				slot.displacement = paramSlotDisplacement(slot.paramIndex);
			}
			else
			{
				if (freeShadowSlots > 0)
				{
					// alloc shadow slot
					slot.displacement = paramSlotDisplacement(4 - freeShadowSlots);
					--freeShadowSlots;
				}
				else
				{
					// alloc local slot
					slot.displacement = (layout.numLocalSlots - nextLocalIndex - 1) * STACK_ITEM_SIZE;
					++nextLocalIndex;
				}
			}
			slot.baseReg = baseReg;
		}
	}
}

struct StackLayout
{
	/// How much bytes we need to allocate in prolog and deallocate in epilog
	int reservedBytes;
	int numParamSlots() { return cast(uint)slots.length - numLocalSlots; }
	int numLocalSlots;
	uint maxAlignment = 1;
	StackSlot[] slots;

	ref StackSlot opIndex(IrIndex slotIndex) {
		assert(slotIndex.kind == IrValueKind.stackSlot);
		return slots[slotIndex.storageUintIndex];
	}

	/// paramIndex == -1 for non-params
	IrIndex addStackItem(uint size, uint alignment, bool isParameter, ushort paramIndex)
	{
		assert(size > 0);
		assert(alignment > 0);

		uint id = cast(uint)(slots.length);
		StackSlot slot = StackSlot(size, alignment, isParameter, paramIndex);

		if (!isParameter) ++numLocalSlots;

		slots ~= slot;
		return IrIndex(id, IrValueKind.stackSlot);
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
	void addUser() { ++numUses; }
}
