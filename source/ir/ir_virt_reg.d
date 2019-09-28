/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
///
module ir.ir_virt_reg;

import all;

@(IrValueKind.virtualRegister)
struct IrVirtualRegister
{
	/// Index of instruction or phi that defines this register
	// When vreg is removed, this field is used for linked list of removed registers
	IrIndex definition;
	///
	IrIndex type;
	/// List of instruction indicies that use this register
	SmallVector users;

	bool isRemoved() {
		return type.kind == IrValueKind.virtualRegister;
	}
}
