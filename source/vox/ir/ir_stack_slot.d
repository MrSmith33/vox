/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// IR Stack Slot
module vox.ir.ir_stack_slot;

import vox.all;

///
enum StackSlotKind : ubyte {
	local,
	// same allocation as local
	// used in register allocator
	// during move solving it denotes that stack slot is used as an address
	spillSlot,
	// Slot for callee saved register
	regSaveSlot,
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
	bool isRegSaveSlot() { return kind == StackSlotKind.regSaveSlot; }
}
