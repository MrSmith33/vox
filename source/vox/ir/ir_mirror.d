/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
/// IR Function
module vox.ir.ir_mirror;

import std.string : format;

import vox.all;

/// Allows associating a single uint sized item with any object in original IR
/// IR must be immutable (no new items must added)
/// Mirror is stored in temp memory of context
struct IrMirror(T)
{
	static assert(T.sizeof == uint.sizeof, "T size must be equal to uint.sizeof");

	// Mirror of original IR
	private T[] virtRegMirror;
	private T[] basicBlockMirror;
	private T[] phiMirror;
	private T[] instrMirror;

	void createVirtRegMirror(CompilationContext* context, IrFunction* ir) {
		virtRegMirror = makeParallelArray!T(context, ir.numVirtualRegisters);
	}

	void createBasicBlockMirror(CompilationContext* context, IrFunction* ir) {
		basicBlockMirror = makeParallelArray!T(context, ir.numBasicBlocks);
	}

	void createPhiMirror(CompilationContext* context, IrFunction* ir) {
		phiMirror = makeParallelArray!T(context, ir.numPhis);
	}

	void createInstrMirror(CompilationContext* context, IrFunction* ir) {
		instrMirror = makeParallelArray!T(context, ir.numInstructions);
	}

	void createAll(CompilationContext* context, IrFunction* ir)
	{
		createVirtRegMirror(context, ir);
		createBasicBlockMirror(context, ir);
		createPhiMirror(context, ir);
		createInstrMirror(context, ir);
	}

	ref T opIndex(IrIndex index)
	{
		switch (index.kind) with(IrValueKind) {
			case basicBlock: return basicBlockMirror[index.storageUintIndex];
			case phi: return phiMirror[index.storageUintIndex];
			case virtualRegister: return virtRegMirror[index.storageUintIndex];
			case instruction: return instrMirror[index.storageUintIndex];
			default: assert(false, format("%s", index));
		}
	}

	ref T instr(IrIndex index) {
		assert(index.isInstruction);
		return instrMirror[index.storageUintIndex];
	}
	ref T basicBlock(IrIndex index) {
		assert(index.isBasicBlock);
		return basicBlockMirror[index.storageUintIndex];
	}
	ref T phi(IrIndex index) {
		assert(index.isPhi);
		return phiMirror[index.storageUintIndex];
	}
	ref T vreg(IrIndex index) {
		assert(index.isVirtReg);
		return virtRegMirror[index.storageUintIndex];
	}
}

T[] makeParallelArray(T)(CompilationContext* context, uint size)
{
	static assert(T.sizeof % uint.sizeof == 0, "T.sizeof is not multiple of uint.sizeof");
	enum size_t numSlotsPerItem = divCeil(T.sizeof, uint.sizeof);
	auto result = cast(T[])context.tempBuffer.voidPut(size * numSlotsPerItem);
	result[] = T.init;
	return result;
}
