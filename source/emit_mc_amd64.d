/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module emit_mc_amd64;

import std.stdio;

import all;
import amd64asm;

/// Emits machine code for amd64 architecture
void pass_emit_mc_amd64(ref CompilationContext context)
{
	context.assertf(context.codeBuffer.length > 0, "Code buffer is empty");
	auto emitter = CodeEmitter(&context);
	emitter.compileModule;
}

//version = emit_mc_print;

struct CodeEmitter
{
	CompilationContext* context;

	FunctionDeclNode* fun;
	IrFunction* lir;
	CodeGen_x86_64 gen;
	PC[] blockStarts;
	PC[2][] jumpFixups;

	static struct CallFixup {
		PC address; // address after call instruction
		FunctionIndex calleeIndex;
	}

	private Buffer!CallFixup callFixups;

	void compileModule()
	{
		// copy static data into buffer and set offsets
		foreach(ref IrGlobal global; context.globals.array)
		{
			if (global.isInBuffer) continue; // already in buffer
			if (global.numUsers == 0) continue; // no users

			// alignment
			uint padding = paddingSize!uint(context.staticDataBuffer.length, global.alignment);
			context.staticDataBuffer.voidPut(padding)[] = 0;

			// offset
			global.staticBufferOffset = context.staticDataBuffer.length;
			context.staticDataBuffer.put(global.initializer);

			// zero termination
			if (global.needsZeroTermination)
				context.staticDataBuffer.put(0);

			global.flags |= IrGlobalFlags.isInBuffer;
			//writefln("Global %s, size %s, zero %s, offset %s, buf size %s",
			//	global.initializer, global.length, global.needsZeroTermination, global.staticBufferOffset, context.staticDataBuffer.length);
		}

		gen.encoder.setBuffer(context.codeBuffer);
		//writefln("code buf %s", context.codeBuffer.ptr);
		foreach(f; context.mod.functions) {
			if (f.isExternal) continue;
			compileFunction(f);
		}

		fixCalls();
		context.mod.code = gen.encoder.code;
	}

	void compileFunction(FunctionDeclNode* f)
	{
		fun = f;
		lir = fun.backendData.lirData;
		fun.backendData.funcPtr = gen.pc;
		blockStarts = cast(PC[])context.tempBuffer.voidPut(lir.numBasicBlocks * (PC.sizeof / uint.sizeof));
		uint[] buf = context.tempBuffer.voidPut(lir.numBasicBlocks * 2 * (PC.sizeof / uint.sizeof));
		buf[] = 0;
		jumpFixups = cast(PC[2][])buf;
		compileBody();
		fixJumps();
	}

	void compileBody()
	{
		lir.assignSequentialBlockIndices();

		foreach (IrIndex lirBlockIndex, ref IrBasicBlock lirBlock; lir.blocks)
		{
			blockStarts[lirBlock.seqIndex] = gen.pc;
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; lirBlock.instructions(*lir))
			{
				switch(cast(Amd64Opcode)instrHeader.op)
				{
					case Amd64Opcode.mov:
						genMove(instrHeader.result, instrHeader.args[0], ArgType.QWORD);
						break;
					case Amd64Opcode.load:
						genLoad(instrHeader.result, instrHeader.args[0], ArgType.QWORD);
						break;
					case Amd64Opcode.store:
						genStore(instrHeader.args[0], instrHeader.args[1], ArgType.QWORD);
						break;
					case Amd64Opcode.add:
						genRegular(instrHeader.args[0], instrHeader.args[1], AMD64OpRegular.add, ArgType.QWORD);
						break;
					case Amd64Opcode.sub:
						genRegular(instrHeader.args[0], instrHeader.args[1], AMD64OpRegular.sub, ArgType.QWORD);
						break;
					case Amd64Opcode.call:
						gen.call(Imm32(0));
						FunctionIndex calleeIndex = instrHeader.preheader!IrInstrPreheader_call.calleeIndex;
						callFixups.put(CallFixup(gen.pc, calleeIndex));
						break;
					case Amd64Opcode.jmp:
						if (lirBlock.seqIndex + 1 != lir.getBlock(lirBlock.successors[0, *lir]).seqIndex)
						{
							gen.jmp(Imm32(0));
							jumpFixups[lirBlock.seqIndex][0] = gen.pc;
						}
						break;
					case Amd64Opcode.bin_branch:
						genRegular(instrHeader.args[0], instrHeader.args[1], AMD64OpRegular.cmp, ArgType.DWORD);
						Condition cond = IrBinCondToAmd64Condition[instrHeader.cond];
						gen.jcc(cond, Imm32(0));
						jumpFixups[lirBlock.seqIndex][0] = gen.pc;
						if (lirBlock.seqIndex + 1 != lir.getBlock(lirBlock.successors[1, *lir]).seqIndex)
						{
							gen.jmp(Imm32(0));
							jumpFixups[lirBlock.seqIndex][1] = gen.pc;
						}
						break;
					case Amd64Opcode.un_branch:
						Register reg = cast(Register)instrHeader.args[0].storageUintIndex;
						gen.testd(reg, reg);
						Condition cond = IrUnCondToAmd64Condition[instrHeader.cond];
						gen.jcc(cond, Imm32(0));
						jumpFixups[lirBlock.seqIndex][0] = gen.pc;
						if (lirBlock.seqIndex + 1 != lir.getBlock(lirBlock.successors[1, *lir]).seqIndex)
						{
							gen.jmp(Imm32(0));
							jumpFixups[lirBlock.seqIndex][1] = gen.pc;
						}
						break;
					case Amd64Opcode.ret:
						gen.ret();
						break;
					default:
						context.internal_error("Unimplemented instruction %s", cast(Amd64Opcode)instrHeader.op);
						break;
				}
			}
		}
	}

	void fixCalls()
	{
		foreach (CallFixup fixup; callFixups.data) {
			FunctionDeclNode* callee = context.mod.functions[fixup.calleeIndex];
			//writefln("fix call to '%s' @%s", callee.strId(context), cast(void*)callee.funcPtr);
			fix_PC_REL_32(fixup.address, cast(PC)callee.backendData.funcPtr);
		}
		callFixups.clear();
	}

	void fixJump(PC fixup, lazy IrIndex targetBlock)
	{
		PC succPC = blockStarts[lir.getBlock(targetBlock).seqIndex];
		fix_PC_REL_32(fixup, succPC);
	}

	void fixJumps()
	{
		foreach (IrIndex lirBlockIndex, ref IrBasicBlock lirBlock; lir.blocks)
		{
			PC[2] fixups = jumpFixups[lirBlock.seqIndex];
			if (fixups[0] !is null) fixJump(fixups[0], lirBlock.successors[0, *lir]);
			if (fixups[1] !is null) fixJump(fixups[1], lirBlock.successors[1, *lir]);
		}
	}

	MemAddress localVarMemAddress(IrIndex stackSlotIndex) {
		context.assertf(stackSlotIndex.isStackSlot, "Index is not stack slot, but %s", stackSlotIndex.kind);
		auto stackSlot = &fun.backendData.stackLayout[stackSlotIndex];
		Register baseReg = indexToRegister(stackSlot.baseReg);
		return minMemAddrBaseDisp(baseReg, stackSlot.displacement);
	}

	Register indexToRegister(IrIndex regIndex) {
		context.assertf(regIndex.isPhysReg, "Index is not register, but %s", regIndex.kind);
		return cast(Register)regIndex.storageUintIndex;
	}

	void genRegular(IrIndex dst, IrIndex src, AMD64OpRegular op, ArgType argType)
	{
		AsmArg argDst;
		AsmArg argSrc;
		AsmOpParam param;
		param.op = op;
		param.argType = argType;

		argDst.reg = indexToRegister(dst);

		// HACK, TODO: ESP is generated instead of RSP. Need to store types in instructions / more instruction types
		if (argDst.reg == Register.SP) param.argType = ArgType.QWORD;

		param.dstKind = AsmArgKind.REG;

		//writefln("%s.%s %s %s", op, argType, dst.type, src.type);

		final switch (src.kind) with(IrValueKind)
		{
			case none, listItem, instruction, basicBlock, phi, type: assert(false);
			case constant:
				IrConstant con = context.constants.get(src);
				if (con.numSignedBytes == 1) {
					param.immType = ArgType.BYTE;
					argSrc.imm8 = Imm8(con.i8);
				}
				else {
					param.immType = ArgType.DWORD;
					argSrc.imm32 = Imm32(con.i32);
				}
				param.srcKind = AsmArgKind.IMM;
				break;

			case global:
				assert(false); // TODO

			case virtualRegister: context.unreachable; assert(false);
			case memoryAddress: context.unreachable; assert(false);
			case physicalRegister:
				argSrc.reg = indexToRegister(src);
				param.srcKind = AsmArgKind.REG;
				break;

			case stackSlot: context.unreachable; assert(false); // gen.mov(reg0, localVarMemAddress(valueRef), argType);
			case variable: assert(false);
		}
		gen.encodeRegular(argDst, argSrc, param);
	}

	/// Generate move from src operand to dst operand. argType describes the size of operands.
	void genMove(IrIndex dst, IrIndex src, ArgType argType)
	{
		version(emit_mc_print) writefln("genMove %s %s", dst, src);
		MoveType moveType = calcMoveType(dst.kind, src.kind);

		if (moveType != MoveType.invalid && dst == src) return;

		Register srcReg = cast(Register)src.storageUintIndex;
		Register dstReg = cast(Register)dst.storageUintIndex;

		switch(moveType)
		{
			default:
				context.internal_error("Invalid move to %s from %s", dst.kind, src.kind);
				assert(false);

			case MoveType.const_to_reg:
				int con = context.constants.get(src).i32;
				version(emit_mc_print) writefln("  move.%s reg:%s, con:%s", argType, dstReg, con);
				if (con == 0) // xor
				{
					AsmArg argDst = {reg : dstReg};
					AsmArg argSrc = {reg : dstReg};
					AsmOpParam param = AsmOpParam(AsmArgKind.REG, AsmArgKind.REG, AMD64OpRegular.xor, argType);
					gen.encodeRegular(argDst, argSrc, param);
				}
				else
					gen.mov(dstReg, Imm32(con), argType);
				break;

			case MoveType.global_to_reg:
				// HACK, TODO: 32bit version of reg is incoming, while for ptr 64bits are needed
				IrGlobal* global = &context.globals.get(src);
				context.assertf(global.isInBuffer, "Global is not in static data buffer");

				PC globalAddress = cast(PC)context.staticDataBuffer.bufPtr + global.staticBufferOffset;
				MemAddress addr = memAddrRipDisp32(0);
				gen.lea(dstReg, addr, ArgType.QWORD);
				PC fixupAddr = gen.pc;
				fix_PC_REL_32(fixupAddr, globalAddress);
				break;

			case MoveType.reg_to_reg:
				version(emit_mc_print) writefln("  move.%s reg:%s, reg:%s", argType, dstReg, srcReg);
				gen.mov(dstReg, srcReg, argType);
				break;

			case MoveType.stack_to_reg:
				gen.lea(dstReg, localVarMemAddress(src), ArgType.QWORD);
				break;
		}
	}

	void fix_PC_REL_32(PC fixup, PC target)
	{
		*cast(Imm32*)(fixup-4) = jumpOffset(fixup, target);
	}

	/// Generate move from src operand to dst operand. argType describes the size of operands.
	// If src is phys reg the it is used as address base.
	// dst must be phys reg
	void genLoad(IrIndex dst, IrIndex src, ArgType argType)
	{
		bool valid = dst.isPhysReg && (src.isPhysReg || src.isStackSlot);
		context.assertf(valid, "Invalid load %s -> %s", src.kind, dst.kind);

		Register dstReg = indexToRegister(dst);

		switch(src.kind) with(IrValueKind)
		{
			case physicalRegister:
				Register srcReg = indexToRegister(src);
				gen.mov(dstReg, memAddrBase(srcReg), argType);
				break;

			case stackSlot:
				gen.movd(dstReg, localVarMemAddress(src));
				break;

			default:
				context.internal_error("invalid source of load %s", src.kind);
				break;
		}
	}

	void genStore(IrIndex dst, IrIndex src, ArgType argType)
	{
		MemAddress dstMem;
		switch (dst.kind) with(IrValueKind)
		{
			case physicalRegister: // store address is in register
				Register dstReg = indexToRegister(dst);
				dstMem = memAddrBase(dstReg);
				break;
			case stackSlot:
				dstMem = localVarMemAddress(dst);
				break;
			default:
				context.internal_error("store %s <- %s is not implemented", dst.kind, src.kind);
				break;
		}

		switch (src.kind) with(IrValueKind)
		{
			case constant:
				uint con = context.constants.get(src).i32;
				gen.mov(dstMem, Imm32(con), argType);
				break;
			case physicalRegister:
				Register srcReg = indexToRegister(src);
				gen.mov(dstMem, srcReg, argType);
				break;
			case global:
				context.internal_error("store %s <- %s, must go through intermediate register", dst.kind, src.kind);
				break;
			default:
				context.internal_error("store %s <- %s is not implemented", dst.kind, src.kind);
				break;
		}
	}
}

MoveType calcMoveType(IrValueKind dst, IrValueKind src)
{
	switch(dst) with(IrValueKind) {
		case none, listItem, constant: return MoveType.invalid;
		case virtualRegister: return MoveType.invalid;
		case physicalRegister:
			switch(src) with(IrValueKind) {
				case constant: return MoveType.const_to_reg;
				case global: return MoveType.global_to_reg;
				case physicalRegister: return MoveType.reg_to_reg;
				case memoryAddress: return MoveType.mem_to_reg;
				case stackSlot: return MoveType.stack_to_reg;
				default: return MoveType.invalid;
			}
		case memoryAddress:
			switch(src) with(IrValueKind) {
				case constant: return MoveType.const_to_mem;
				case physicalRegister: return MoveType.reg_to_mem;
				default: return MoveType.invalid;
			}
		case stackSlot:
			switch(src) with(IrValueKind) {
				case constant: return MoveType.const_to_stack;
				case physicalRegister: return MoveType.reg_to_stack;
				default: return MoveType.invalid;
			}
		default: return MoveType.invalid;
	}
}

enum MoveType
{
	invalid,
	const_to_reg,
	global_to_reg,
	const_to_stack,
	reg_to_reg,
	reg_to_stack,
	stack_to_reg,
	const_to_mem,
	reg_to_mem,
	mem_to_reg,
}
