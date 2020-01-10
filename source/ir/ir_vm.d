/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

/// Virtual machine for IR interpretation
module ir.ir_vm;

import all;

/// Offset and length into CompilationContext.vmBuffer arena
struct IrVmSlotInfo
{
	uint offset;
	uint length;
}

struct IrVm
{
	/// CompilationContext.vmBuffer arena holds slots
	/// Memory that holds stack slots and virtual registers
	CompilationContext* c;
	/// Function being interpreted
	IrFunction* func;
	/// Maps each vreg to frame slot. Relative to frameOffset
	/// extra item is inserted that points past last item
	uint* vregSlotOffsets;
	/// Maps each stack slot to frame slot
	uint* stackSlotOffsets;
	uint frameOffset;
	uint frameSize;

	void pushFrame()
	{
		if (!func.vregSlotOffsets)
		{
			// allocate space for virt regs and stack slots
			func.vregSlotOffsets = c.allocateTempArray!uint(func.numVirtualRegisters+1).ptr;
			uint offset = 0;
			func.vregSlotOffsets[0] = offset;
			//writefln("vreg %s %s", 0, offset);
			foreach(i; 0..func.numVirtualRegisters)
			{
				offset += c.types.typeSize(func.vregPtr[i].type);
				//writefln("vreg %s %s", i+1, offset);
				func.vregSlotOffsets[i+1] = offset;
			}
			func.frameSize = offset;
		}
		vregSlotOffsets = func.vregSlotOffsets;
		frameSize = func.frameSize;

		frameOffset = c.pushVmStack(frameSize).offset;
		//writefln("pushFrame %s %s %s", frameSize, frameOffset, c.vmBuffer.uintLength);
	}

	void popFrame()
	{
		c.popVmStack(IrVmSlotInfo(frameOffset, frameSize));
	}

	ParameterSlotIterator parameters() {
		return ParameterSlotIterator(&this);
	}

	IrVmSlotInfo vregSlot(IrIndex vreg) {
		uint from = frameOffset + vregSlotOffsets[vreg.storageUintIndex];
		uint to = frameOffset + vregSlotOffsets[vreg.storageUintIndex+1];
		return IrVmSlotInfo(from, to - from);
	}

	ubyte[] slotToSlice(IrVmSlotInfo slot)
	{
		return c.vmBuffer.bufPtr[slot.offset..slot.offset+slot.length];
	}

	/// outputMem : Memory that holds the return value
	void run(IrVmSlotInfo outputMem)
	{
		// skip first block as it only contains IrOpcode.parameter instructions
		IrIndex curBlock = func.getBlock(func.entryBasicBlock).successors[0, func];
		IrIndex prevBlock;
		IrBasicBlock* block = &func.getBlock(curBlock);

		block_loop:
		while (true)
		{
			foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(func))
			{
				// TODO: need array for args for O(1) indexing
				foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(func))
				{
					if (phiArg.basicBlock == prevBlock)
					{
						copyToVreg(phi.result, phiArg.value);
						break;
					}
				}
			}

			instr_loop:
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(func))
			{
				switch(instrHeader.op)
				{
					case IrOpcode.parameter:
						uint paramIndex = func.get!IrInstrHeader(instrIndex).extraPayload(func, 1)[0].asUint;
						//writefln("param %s", paramIndex);
						break;

					case IrOpcode.jump:
						prevBlock = curBlock;
						curBlock = block.successors[0, func];
						//writefln("jump %s -> %s", prevBlock, curBlock);
						block = &func.getBlock(curBlock);
						break instr_loop;

					case IrOpcode.ret_val:
						//writefln("ret %s", instrHeader.arg(func, 0));
						copyToMem(outputMem, instrHeader.arg(func, 0));
						break block_loop;

					case IrOpcode.add:  binop!"+"(instrHeader); break;
					case IrOpcode.sub:  binop!"-"(instrHeader); break;
					case IrOpcode.and:  binop!"&"(instrHeader); break;
					case IrOpcode.or:   binop!"|"(instrHeader); break;
					case IrOpcode.xor:  binop!"^"(instrHeader); break;
					case IrOpcode.umul: binop!"*"(instrHeader); break;
					case IrOpcode.smul: binop!"*"(instrHeader); break;
					case IrOpcode.udiv: binop!"/"(instrHeader); break;
					case IrOpcode.sdiv: binop!"/"(instrHeader); break;
					case IrOpcode.shl:  binop!"<<"(instrHeader); break;
					case IrOpcode.lshr: binop!">>>"(instrHeader); break;
					case IrOpcode.ashr: binop!">>"(instrHeader); break;

					case IrOpcode.not:
						long a = readInt(instrHeader.arg(func, 0));
						long result = !a;
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.neg:
						long a = readInt(instrHeader.arg(func, 0));
						long result = -a;
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.conv:
						long result = readInt(instrHeader.arg(func, 0));
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.zext:
						long result = readInt(instrHeader.arg(func, 0));
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.sext:
						long result = readInt(instrHeader.arg(func, 0));
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.trunc:
						long result = readInt(instrHeader.arg(func, 0));
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.branch_binary:
						long a = readInt(instrHeader.arg(func, 0));
						long b = readInt(instrHeader.arg(func, 1));
						//writefln("br %s %s : %s %s", a, b, block.successors[0, func], block.successors[1, func]);
						bool result;
						final switch(cast(IrBinaryCondition)instrHeader.cond)
						{
							case IrBinaryCondition.eq: result = a == b; break;
							case IrBinaryCondition.ne: result = a != b; break;
							case IrBinaryCondition.g:  result = a > b;  break;
							case IrBinaryCondition.ge: result = a >= b; break;
							case IrBinaryCondition.l:  result = a < b;  break;
							case IrBinaryCondition.le: result = a <= b; break;
						}
						prevBlock = curBlock;
						if (result)
							curBlock = block.successors[0, func];
						else
							curBlock = block.successors[1, func];
						//writefln("jump %s -> %s", prevBlock, curBlock);
						block = &func.getBlock(curBlock);
						break instr_loop;

					case IrOpcode.branch_unary:
						long a = readInt(instrHeader.arg(func, 0));
						//writefln("br %s : %s %s", a, block.successors[0, func], block.successors[1, func]);
						bool result;
						final switch(cast(IrUnaryCondition)instrHeader.cond)
						{
							case IrUnaryCondition.zero:     result = a == 0; break;
							case IrUnaryCondition.not_zero: result = a != 0; break;
						}
						prevBlock = curBlock;
						if (result)
							curBlock = block.successors[0, func];
						else
							curBlock = block.successors[1, func];
						//writefln("jump %s -> %s", prevBlock, curBlock);
						block = &func.getBlock(curBlock);
						break instr_loop;

					default:
						c.internal_error("interpret %s", cast(IrOpcode)instrHeader.op);
				}
			}
		}
	}

	void binop(string op)(ref IrInstrHeader instrHeader)
	{
		long a = readInt(instrHeader.arg(func, 0));
		long b = readInt(instrHeader.arg(func, 1));
		mixin(`long result = a `~op~` b;`);
		writeInt(instrHeader.result(func), result);
	}

	long readInt(IrIndex index)
	{
		switch (index.kind) with(IrValueKind) {
			case constant, constantZero: return c.constants.get(index).i64;
			case virtualRegister: return readInt(vregSlot(index));
			default: c.internal_error("readInt %s", index); assert(false);
		}
	}

	long readInt(IrVmSlotInfo mem)
	{
		switch(mem.length)
		{
			case 1: return *cast( byte*)slotToSlice(mem).ptr;
			case 2: return *cast(short*)slotToSlice(mem).ptr;
			case 4: return *cast(  int*)slotToSlice(mem).ptr;
			case 8: return *cast( long*)slotToSlice(mem).ptr;
			default: c.internal_error("readInt %s", mem); assert(false);
		}
	}

	void writeInt(IrIndex index, long value)
	{
		IrVmSlotInfo mem = vregSlot(index);
		switch(mem.length)
		{
			case 1: *cast( byte*)slotToSlice(mem).ptr = cast( byte)value; break;
			case 2: *cast(short*)slotToSlice(mem).ptr = cast(short)value; break;
			case 4: *cast(  int*)slotToSlice(mem).ptr = cast(  int)value; break;
			case 8: *cast( long*)slotToSlice(mem).ptr = cast( long)value; break;
			default: c.internal_error("writeInt %s", mem); assert(false);
		}
	}

	void copyToVreg(IrIndex vreg, IrIndex value)
	{
		copyToMem(vregSlot(vreg), value);
	}

	void copyToMem(IrVmSlotInfo mem, IrIndex value)
	{
		//writefln("copyToMem %s %s", mem.length, value);
		if (value.isSomeConstant)
		{
			constantToMem(slotToSlice(mem), value, c);
		}
		else if (value.isVirtReg)
		{
			slotToSlice(mem)[] = slotToSlice(vregSlot(value));
		}
		else
		{
			writefln("%s", value);
			assert(false);
		}
	}
}

struct ParameterSlotIterator
{
	IrVm* vm;
	int opApply(scope int delegate(uint, IrVmSlotInfo) dg)
	{
		IrBasicBlock* block = &vm.func.getBlock(vm.func.entryBasicBlock);
		uint index;
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(vm.func))
		{
			if (instrHeader.op == IrOpcode.parameter)
			{
				IrIndex vreg = instrHeader.result(vm.func);
				auto slot = vm.vregSlot(vreg);
				if (int res = dg(index, slot))
					return res;
			}
			++index;
		}
		return 0;
	}
}
