/**
Copyright: Copyright (c) 2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// Linear scan register allocation pass
module stack_layout;

import all;
/+
/// Arranges items on the stack according to calling convention
void pass_stack_layout(ref CompilationContext ctx) {
	IrModule* mod = &ctx.mod.irModule;
	foreach (IrFunction* func; mod.functions)
	{
		enum STACK_ITEM_SIZE = 8; // x86_64
		int numParams = cast(int)func.numParameters;

		auto layout = &func.stackLayout;
		layout.reservedBytes = layout.numLocals * STACK_ITEM_SIZE;
		//writefln("%s", layout.numLocals);
		//writefln("%s", layout.numParams);

		// ++        slot index
		// param2    1  rsp + 20     \
		// param1    0  rsp + 18     / numParams = 2
		// ret addr     rsp + 10
		// local1    2  rsp +  8     \
		// local2    3  rsp +  0     / numLocals = 2   <-- RSP
		// --

		//writefln("numSlots %s numLocals %s numParams %s", numSlots, numLocals, numParams);
		//writefln("layout %s", layout.reservedBytes);

		/*
		if (USE_FRAME_POINTER)
		{
			// ++        varIndex
			// param2    1              \
			// param1    0  rbp + 2     / numParams = 2
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
			baseReg = Register.BP;
		}*/

		int localIndex = 0;
		foreach (ref slot; layout.slots)
		{
			if (slot.isParameter)
			{
				slot.offset = (layout.numLocals + slot.paramIndex + 1) * STACK_ITEM_SIZE;
			}
			else
			{
				slot.offset = (layout.numLocals - localIndex - 1) * STACK_ITEM_SIZE;
				++localIndex;
			}
		}
	}
}

struct StackLayout
{
	int reservedBytes;
	int numParams() { return cast(uint)slots.length - numLocals; }
	int numLocals;
	StackSlot[] slots;

	this(this)
	{
		slots = slots.dup;
	}

	/// paramIndex == -1 for non-params
	IrRef addStackItem(ulong size, ulong alignment, bool isParameter, ushort paramIndex)
	{
		assert(size > 0);
		assert(alignment > 0);

		auto id = StackSlotId(cast(uint)(slots.length));
		auto slot = StackSlot(size, alignment, isParameter, paramIndex);

		if (!isParameter) ++numLocals;

		slots ~= slot;
		return IrRef(id);
	}
}

struct StackSlot
{
	uint size;
	uint alignment;
	bool isParameter;
	ushort paramIndex;
	ushort numUses;
	int offset;
	void addUser() { ++numUses; }
}
+/
