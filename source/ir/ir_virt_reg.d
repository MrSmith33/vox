/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
///
module ir.ir_virt_reg;

import all;

@(IrValueKind.virtualRegister)
struct IrVirtualRegister
{
	/// Index of instruction that defines this register
	IrIndex definition;
	/// List of instruction indicies that use this register
	SmallVector users;
	IrIndex prevVirtReg; /// null only if this is firstVirtualReg
	IrIndex nextVirtReg; /// null only if this is lastVirtualReg
	/// Sequential index for random access
	uint seqIndex;
}

/// Generates temporary array of all virtual register indicies
IrIndex[] virtualRegArray(CompilationContext* context, IrFunction* ir)
{
	IrIndex[] result = cast(IrIndex[])context.tempBuffer.voidPut(ir.numVirtualRegisters);
	for (IrIndex vreg = ir.firstVirtualReg; vreg.isDefined; vreg = ir.getVirtReg(vreg).nextVirtReg)
	{
		result[ir.getVirtReg(vreg).seqIndex] = vreg;
	}
	return result;
}
