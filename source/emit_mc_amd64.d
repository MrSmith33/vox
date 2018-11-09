/**
Copyright: Copyright (c) 2018 Andrey Penechko.
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

	void compileModule()
	{
		gen.encoder.setBuffer(context.codeBuffer);
		//writefln("code buf %s", context.codeBuffer.ptr);
		foreach(f; context.mod.functions) compileFunction(f);

		context.mod.code = gen.encoder.code;
	}

	void compileFunction(FunctionDeclNode* f)
	{
		fun = f;
		lir = fun.lirData;
		fun.funcPtr = gen.pc;
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

		foreach (IrIndex lirBlockIndex, ref IrBasicBlockInstr lirBlock; lir.blocks)
		{
			blockStarts[lirBlock.seqIndex] = gen.pc;
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; lirBlock.instructions(*lir))
			{
				switch(cast(Amd64Opcode)instrHeader.op)
				{
					case Amd64Opcode.mov:
						genMove(instrHeader.result, instrHeader.args[0]);
						break;
					case Amd64Opcode.jmp:
						resolve(lirBlockIndex, lirBlock);
						if (lirBlock.seqIndex + 1 != lir.getBlock(lirBlock.successors[0, *lir]).seqIndex)
						{
							gen.jmp(Imm32(0));
							jumpFixups[lirBlock.seqIndex][0] = gen.pc;
						}
						break;
					case Amd64Opcode.bin_branch:
						genRegular(instrHeader.args[0], instrHeader.args[1], AMD64OpRegular.cmp);
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
						// TODO missing phi resolution
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

	void fixJump(PC fixup, lazy IrIndex targetBlock)
	{
		PC succPC = blockStarts[lir.getBlock(targetBlock).seqIndex];
		*cast(Imm32*)(fixup-4) = jumpOffset(fixup, succPC);
	}

	void fixJumps()
	{
		foreach (IrIndex lirBlockIndex, ref IrBasicBlockInstr lirBlock; lir.blocks)
		{
			PC[2] fixups = jumpFixups[lirBlock.seqIndex];
			if (fixups[0] !is null) fixJump(fixups[0], lirBlock.successors[0, *lir]);
			if (fixups[1] !is null) fixJump(fixups[1], lirBlock.successors[1, *lir]);
		}
	}

	void resolve(IrIndex lirBlockIndex, ref IrBasicBlockInstr lirBlock)
	{
		version(emit_mc_print) writefln("resolve %s", lirBlockIndex);
		foreach(IrIndex succIndex; lirBlock.successors.range(*lir))
		{
			version(emit_mc_print) writefln("  succ %s", succIndex);
			IrBasicBlockInstr* successor = &lir.getBlock(succIndex);
			foreach(IrIndex phiIndex, ref IrPhiInstr phi; successor.phis(*lir))
			{
				version(emit_mc_print) writefln("    phi %s", phiIndex);
				foreach (size_t arg_i, ref IrPhiArg arg; phi.args(*lir))
				{
					version(emit_mc_print) writefln("      arg %s %s", arg.basicBlock, arg.value);
					if (arg.basicBlock == lirBlockIndex)
						genMove(phi.result, arg.value);
				}
			}
		}
	}

	void genRegular(IrIndex dst, IrIndex src, AMD64OpRegular op)
	{
		AsmArg argDst;
		AsmArg argSrc;
		AsmOpParam param;
		param.op = op;
		param.argType = ArgType.DWORD; // TODO: remove hardcode

		context.assertf(dst.isPhysReg, "Destination must be register");
		argDst.reg = cast(Register)dst.storageUintIndex;
		param.dstKind = AsmArgKind.REG;

		//writefln("%s.%s %s %s", op, argType, dst.type, src.type);

		final switch (src.kind) with(IrValueKind)
		{
			case none, listItem, instruction, basicBlock, phi: assert(false);
			case constant:
				IrConstant con = context.constants[src.storageUintIndex];
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

			case virtualRegister: context.unreachable; assert(false);
			case memoryAddress: context.unreachable; assert(false);
			case physicalRegister:
				argSrc.reg = cast(Register)src.storageUintIndex;
				param.srcKind = AsmArgKind.REG;
				break;

			case stackSlot: context.unreachable; assert(false); // gen.mov(reg0, localVarMemAddress(valueRef), argType);
		}
		gen.encodeRegular(argDst, argSrc, param);
	}

	/// Generate move from src operand to dst operand. argType describes the size of operands.
	void genMove(IrIndex dst, IrIndex src)
	{
		version(emit_mc_print) writefln("genMove %s %s", dst, src);
		MoveType moveType = calcMoveType(dst.kind, src.kind);
		if (moveType != MoveType.invalid && dst == src) return;

		Register srcReg = cast(Register)src.storageUintIndex;
		Register dstReg = cast(Register)dst.storageUintIndex;

		final switch(moveType)
		{
			case MoveType.invalid:
				context.internal_error("Invalid move to %s from %s",
					dst.kind, src.kind);
				context.unreachable;
				assert(false);

			case MoveType.const_to_reg:
				long con = context.constants[src.storageUintIndex].i64;
				//writefln("move.%s reg:%s, con:%s", argType, dstReg, con);
				gen.movd(dstReg, Imm32(cast(uint)con));
				break;

			case MoveType.const_to_stack:
			//	long con = curFunc.constants[src.storageUintIndex.i64;
			//	gen.movd(localVarMemAddress(dst.irRef.constIndex), Imm32(cast(uint)con));
				context.internal_error("const_to_stack is not implemented");
				break;

			case MoveType.reg_to_reg:
				//writefln("move.%s reg:%s, reg:%s", argType, dstReg, srcReg);
				gen.movd(dstReg, srcReg);
				break;

			case MoveType.reg_to_stack:
			//	gen.mov(localVarMemAddress(dst.irRef.constIndex), srcReg, argType);
				context.internal_error("reg_to_stack is not implemented");
				break;

			case MoveType.stack_to_reg:
			//	gen.mov(dstReg, localVarMemAddress(src.irRef.constIndex), argType);
				context.internal_error("stack_to_reg is not implemented");
				break;

			case MoveType.const_to_mem:
				context.internal_error("const_to_mem is not implemented");
				break;

			case MoveType.reg_to_mem:
				context.internal_error("reg_to_mem is not implemented");
				break;

			case MoveType.mem_to_reg:
				context.internal_error("mem_to_reg is not implemented");
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
	const_to_stack,
	reg_to_reg,
	reg_to_stack,
	stack_to_reg,
	const_to_mem,
	reg_to_mem,
	mem_to_reg,
}
