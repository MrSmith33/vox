/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
///
module vox.ir.ir_virt_reg;

import vox.all;

@(IrValueKind.virtualRegister)
struct IrVirtualRegister
{
	/// Index of instruction or phi that defines this register
	// When vreg is removed, this field is used for linked list of removed registers
	IrIndex definition;
	///
	IrIndex type;
	/// Instruction or phi indices that use this register
	/// This cannot be regular array because deletion is O(n), and it can cause O(n^2) time in DCE pass when lots of instructions are dead.
	/// This must store precise count of users, because this number is used for liveness info allocation
	/// If number is inacurate then it will result in insufficient storage allocated when user occurs multiple times
	/// As a result this must be multiset, not regular set.
	IrSmallSet users;

	bool isRemoved() {
		return type.kind == IrValueKind.virtualRegister;
	}
}
