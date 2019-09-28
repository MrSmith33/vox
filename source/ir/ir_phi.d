/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR Phi function entity
module ir.ir_phi;

import all;

///
@(IrValueKind.phi)
struct IrPhi
{
	IrIndex blockIndex;
	IrIndex result;
	IrIndex var;
	IrIndex nextPhi; // if incomplete, points to next incomplete phi
	IrIndex prevPhi;
	IrIndex firstArgListItem;

	PhiArgIterator args(IrFunction* ir) { return PhiArgIterator(ir, firstArgListItem); }
}

struct PhiArgIterator
{
	IrFunction* ir;
	IrIndex firstArgListItem;
	int opApply(scope int delegate(size_t, ref IrPhiArg) dg) {
		IrIndex next = firstArgListItem;
		size_t i = 0;
		while (next.isDefined)
		{
			IrPhiArg* arg = &ir.get!IrPhiArg(next);
			if (int res = dg(i, *arg))
				return res;
			++i;
			next = arg.nextListItem;
		}
		return 0;
	}
}

///
@(IrValueKind.listItem)
struct IrPhiArg
{
	IrIndex value;
	/// Immediate predecessor that provides the value
	IrIndex basicBlock;
	IrIndex nextListItem;
}

/// Per Basic Block info for unresolved Phi functions, when CFG is incomplete.
/// Finished IR contains no such values
@(IrValueKind.listItem)
struct IrIncompletePhi
{
	IrIndex var;
	IrIndex phi;
	IrIndex nextListItem;
}
