/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// IR Stack Slot
module ir.ir_stack_slot;

import all;

///
enum StackSlotKind : ubyte {
	local,
	parameter,
	argument
}

@(IrValueKind.stackSlot)
struct StackSlot
{
	SizeAndAlignment sizealign;
	StackSlotKind kind;
	ushort numUses;
	/// Signed offset from base register
	int displacement;
	/// Base register (stack or frame pointer)
	IrIndex baseReg;
	/// Must be a pointer type
	IrIndex type;
	void addUser() { ++numUses; }
	bool isParameter() { return kind == StackSlotKind.parameter; }
}
