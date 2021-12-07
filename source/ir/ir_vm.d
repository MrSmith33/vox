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
	uint* vmSlotOffsets;
	uint frameOffset;
	uint frameSize;

	void pushFrame()
	{
		if (!func.vmSlotOffsets)
		{
			// allocate space for virt regs and stack slots
			func.vmSlotOffsets = c.allocateTempArray!uint(func.numVirtualRegisters+func.numStackSlots+1).ptr;
			uint offset = 0;
			func.vmSlotOffsets[0] = offset;
			//writefln("vreg %s %s", 0, offset);
			foreach(i; 0..func.numVirtualRegisters)
			{
				offset += c.types.typeSize(func.vregPtr[i].type);
				//writefln("vreg %s %s", i+1, offset);
				func.vmSlotOffsets[i+1] = offset;
			}
			foreach(i; 0..func.numStackSlots)
			{
				offset += c.types.typeSize(c.types.getPointerBaseType(func.stackSlotPtr[i].type));
				//writefln("slot %s %s", i+1, offset);
				func.vmSlotOffsets[func.numVirtualRegisters + i+1] = offset;
			}
			func.frameSize = offset;
		}
		vmSlotOffsets = func.vmSlotOffsets;
		frameSize = func.frameSize;

		frameOffset = c.pushVmStack(frameSize).offset;
		//writefln("pushFrame %s %s %s", frameSize, frameOffset, c.vmBuffer.uintLength);
	}

	void popFrame()
	{
		c.popVmStack(IrVmSlotInfo(frameOffset, frameSize));
	}

	ParameterSlotIterator parameters() return {
		return ParameterSlotIterator(&this);
	}

	IrVmSlotInfo vregSlot(IrIndex vreg) {
		assert(vreg.isVirtReg);
		uint from = frameOffset + vmSlotOffsets[vreg.storageUintIndex];
		uint to = frameOffset + vmSlotOffsets[vreg.storageUintIndex+1];
		return IrVmSlotInfo(from, to - from);
	}

	IrVmSlotInfo stackSlotSlot(IrIndex slot) {
		assert(slot.isStackSlot);
		uint from = frameOffset + vmSlotOffsets[func.numVirtualRegisters + slot.storageUintIndex];
		uint to = frameOffset + vmSlotOffsets[func.numVirtualRegisters + slot.storageUintIndex+1];
		return IrVmSlotInfo(from, to - from);
	}

	ubyte[] slotToSlice(IrVmSlotInfo slot)
	{
		return c.vmBuffer.bufPtr[slot.offset..slot.offset+slot.length];
	}

	/// outputMem : Memory that holds the return value
	void run(IrVmSlotInfo outputMem)
	{
		++c.numCtfeRuns;
		IrIndex curBlock = func.entryBasicBlock;
		IrIndex prevBlock;
		IrBasicBlock* block = func.getBlock(curBlock);

		block_loop:
		while (true)
		{
			if (prevBlock.isDefined) {
				uint predIndex = block.predecessors.findFirst(func, prevBlock);
				foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(func))
				{
					IrIndex phiArg = phi.args[predIndex, func];
					copyToVreg(phi.result, phiArg);
				}
			}

			instr_loop:
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(func))
			{
				switch(instrHeader.op)
				{
					case IrOpcode.parameter:
						uint paramIndex = func.getInstr(instrIndex).extraPayload(func, 1)[0].asUint;
						//writefln("param %s", paramIndex);
						break;

					case IrOpcode.jump:
						prevBlock = curBlock;
						curBlock = block.successors[0, func];
						//writefln("jump %s -> %s", prevBlock, curBlock);
						block = func.getBlock(curBlock);
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
						long a = readValue(instrHeader.arg(func, 0)).i64;
						long result = !a;
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.neg:
						long a = readValue(instrHeader.arg(func, 0)).i64;
						long result = -a;
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.conv:
						long result = readValue(instrHeader.arg(func, 0)).i64;
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.zext:
						long result = readValue(instrHeader.arg(func, 0)).i64;
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.sext:
						long result = readValue(instrHeader.arg(func, 0)).i64;
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.trunc:
						long result = readValue(instrHeader.arg(func, 0)).i64;
						writeInt(instrHeader.result(func), result);
						break;

					case IrOpcode.store:
						IrVmSlotInfo memSlot = ptrToSlice(instrHeader.arg(func, 0));
						copyToMem(memSlot, instrHeader.arg(func, 1));
						break;

					case IrOpcode.load:
						IrVmSlotInfo sourceSlot = ptrToSlice(instrHeader.arg(func, 0));
						IrVmSlotInfo resultSlot = vregSlot(instrHeader.result(func));
						slotToSlice(resultSlot)[] = slotToSlice(sourceSlot);
						break;

					case IrOpcode.branch_binary:
						IrIndex type = getValueType(instrHeader.arg(func, 0), func, c);
						IrConstVal a = readValue(instrHeader.arg(func, 0));
						IrConstVal b = readValue(instrHeader.arg(func, 1));
						//writefln("br %s %s : %s %s", a, b, block.successors[0, func], block.successors[1, func]);
						bool result;
						final switch(cast(IrBinaryCondition)instrHeader.cond)
						{
							case IrBinaryCondition.eq:  result = a.u64 == b.u64; break;
							case IrBinaryCondition.ne:  result = a.u64 != b.u64; break;

							case IrBinaryCondition.ugt:
								switch(type.basicType(c)) {
									case IrBasicType.i8:  result = a.u8  > b.u8;  break;
									case IrBasicType.i16: result = a.u16 > b.u16; break;
									case IrBasicType.i32: result = a.u32 > b.u32; break;
									case IrBasicType.i64: result = a.u64 > b.u64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.uge:
								switch(type.basicType(c)) {
									case IrBasicType.i8:  result = a.u8  >= b.u8;  break;
									case IrBasicType.i16: result = a.u16 >= b.u16; break;
									case IrBasicType.i32: result = a.u32 >= b.u32; break;
									case IrBasicType.i64: result = a.u64 >= b.u64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.ult:
								switch(type.basicType(c)) {
									case IrBasicType.i8:  result = a.u8  < b.u8;  break;
									case IrBasicType.i16: result = a.u16 < b.u16; break;
									case IrBasicType.i32: result = a.u32 < b.u32; break;
									case IrBasicType.i64: result = a.u64 < b.u64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.ule:
								switch(type.basicType(c)) {
									case IrBasicType.i8:  result = a.u8  <= b.u8;  break;
									case IrBasicType.i16: result = a.u16 <= b.u16; break;
									case IrBasicType.i32: result = a.u32 <= b.u32; break;
									case IrBasicType.i64: result = a.u64 <= b.u64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;

							case IrBinaryCondition.sgt:
								switch(type.basicType(c)) {
									case IrBasicType.i8:  result = a.i8  > b.i8;  break;
									case IrBasicType.i16: result = a.i16 > b.i16; break;
									case IrBasicType.i32: result = a.i32 > b.i32; break;
									case IrBasicType.i64: result = a.i64 > b.i64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.sge:
								switch(type.basicType(c)) {
									case IrBasicType.i8:  result = a.i8  >= b.i8;  break;
									case IrBasicType.i16: result = a.i16 >= b.i16; break;
									case IrBasicType.i32: result = a.i32 >= b.i32; break;
									case IrBasicType.i64: result = a.i64 >= b.i64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.slt:
								switch(type.basicType(c)) {
									case IrBasicType.i8:  result = a.i8  < b.i8;  break;
									case IrBasicType.i16: result = a.i16 < b.i16; break;
									case IrBasicType.i32: result = a.i32 < b.i32; break;
									case IrBasicType.i64: result = a.i64 < b.i64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.sle:
								switch(type.basicType(c)) {
									case IrBasicType.i8:  result = a.i8  <= b.i8;  break;
									case IrBasicType.i16: result = a.i16 <= b.i16; break;
									case IrBasicType.i32: result = a.i32 <= b.i32; break;
									case IrBasicType.i64: result = a.i64 <= b.i64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;

							case IrBinaryCondition.fgt:
								switch(type.basicType(c)) {
									case IrBasicType.f32: result = a.f32 > b.f32; break;
									case IrBasicType.f64: result = a.f64 > b.f64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.fge:
								switch(type.basicType(c)) {
									case IrBasicType.f32: result = a.f32 >= b.f32; break;
									case IrBasicType.f64: result = a.f64 >= b.f64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.flt:
								switch(type.basicType(c)) {
									case IrBasicType.f32: result = a.f32 < b.f32; break;
									case IrBasicType.f64: result = a.f64 < b.f64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
							case IrBinaryCondition.fle:
								switch(type.basicType(c)) {
									case IrBasicType.f32: result = a.f32 <= b.f32; break;
									case IrBasicType.f64: result = a.f64 <= b.f64; break;
									default: c.internal_error("%s", type.basicType(c));
								}
								break;
						}
						prevBlock = curBlock;
						if (result)
							curBlock = block.successors[0, func];
						else
							curBlock = block.successors[1, func];
						//writefln("jump %s -> %s", prevBlock, curBlock);
						block = func.getBlock(curBlock);
						break instr_loop;

					case IrOpcode.branch_unary:
						long a = readValue(instrHeader.arg(func, 0)).i64;
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
						block = func.getBlock(curBlock);
						break instr_loop;

					case IrOpcode.call:
						IrIndex callee = instrHeader.arg(func, 0);
						if (!callee.isFunction)
						{
							writefln("call of %s is not implemented", callee);
							assert(false);
						}

						FunctionDeclNode* calleeFunc = c.getFunction(callee);
						AstIndex calleeIndex = AstIndex(callee.storageUintIndex);
						force_callee_ir_gen(calleeFunc, calleeIndex, c);
						if (calleeFunc.state != AstNodeState.ir_gen_done)
							c.internal_error(calleeFunc.loc,
								"Function's IR is not yet generated");

						IrVmSlotInfo returnMem;
						if (instrHeader.hasResult) {
							IrIndex result = instrHeader.result(func);
							returnMem = vregSlot(result);
						}

						if (calleeFunc.isBuiltin) {
							IrIndex result = eval_call_builtin(TokenIndex(), &this, calleeIndex, instrHeader.args(func)[1..$], c);
							constantToMem(slotToSlice(returnMem), result, c);
							break;
						}

						IrFunction* irData = c.getAst!IrFunction(calleeFunc.backendData.irData);
						IrVm vm = IrVm(c, irData);
						vm.pushFrame;
						foreach(uint index, IrVmSlotInfo slot; vm.parameters)
						{
							copyToMem(slot, instrHeader.arg(func, index+1)); // Account for callee in 0 arg
						}
						vm.run(returnMem);
						vm.popFrame;
						break;

					default:
						c.internal_error("interpret %s", cast(IrOpcode)instrHeader.op);
				}
			}
		}
	}

	void binop(string op)(ref IrInstrHeader instrHeader)
	{
		long a = readValue(instrHeader.arg(func, 0)).i64;
		long b = readValue(instrHeader.arg(func, 1)).i64;
		mixin(`long result = a `~op~` b;`);
		writeInt(instrHeader.result(func), result);
	}

	IrConstVal readValue(IrIndex index)
	{
		switch (index.kind) with(IrValueKind) {
			case constant, constantZero: return c.constants.get(index).value;
			case virtualRegister: return readValue(vregSlot(index));
			default: c.internal_error("readValue %s", index);
		}
	}

	IrConstVal readValue(IrVmSlotInfo mem)
	{
		switch(mem.length)
		{
			case 1: return IrConstVal(*cast( ubyte*)slotToSlice(mem).ptr);
			case 2: return IrConstVal(*cast(ushort*)slotToSlice(mem).ptr);
			case 4: return IrConstVal(*cast(  uint*)slotToSlice(mem).ptr);
			case 8: return IrConstVal(*cast( ulong*)slotToSlice(mem).ptr);
			default: c.internal_error("readValue %s", mem);
		}
	}

	IrVmSlotInfo ptrToSlice(IrIndex ptr)
	{
		IrIndex ptrType = getValueType(ptr, func, c);
		IrIndex ptrMemType = c.types.getPointerBaseType(ptrType);
		uint targetSize = c.types.typeSize(ptrMemType);
		switch (ptr.kind) with(IrValueKind) {
			case constant, constantZero: return IrVmSlotInfo(c.constants.get(ptr).u32, targetSize);
			case virtualRegister: return IrVmSlotInfo(readValue(vregSlot(ptr)).u32, targetSize);
			case stackSlot: return stackSlotSlot(ptr);
			default: c.internal_error("ptrToSlice %s", ptr);
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
			default: c.internal_error("writeInt %s", mem);
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
		IrBasicBlock* block = vm.func.getBlock(vm.func.entryBasicBlock);
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
