/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR Phi function entity
module vox.ir.ir_phi;

import vox.all;

///
@(IrValueKind.phi)
struct IrPhi
{
	IrIndex blockIndex; // null if removed
	IrIndex result;
	IrIndex var;
	IrIndex nextPhi;
	IrIndex prevPhi;
	// order of arguments is the same as blockIndex.predecessors
	IrSmallArray args;

	bool isRemoved() {
		return blockIndex.isUndefined;
	}
}
