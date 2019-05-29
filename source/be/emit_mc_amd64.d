/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module be.emit_mc_amd64;

import std.stdio;

import all;
import be.amd64asm;

/// Emits machine code for amd64 architecture
void pass_emit_mc_amd64(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	auto emitter = CodeEmitter(&context);

	// Add all symbols first, so they can be referenced during code emission
	addStaticDataSymbols(context);
	finalizeStaticData(context);

	if (context.printStaticData) {
		writefln("// Data: addr 0x%X, %s bytes",
			context.staticDataBuffer.bufPtr,
			context.staticDataBuffer.length);
		printHex(context.staticDataBuffer.data, 16);
	}

	foreach (ref SourceFileInfo file; context.files.data) {
		addFunctionSymbols(context, *file.mod);
	}
	if (context.hasErrors) return;

	// emit code
	foreach (ref SourceFileInfo file; context.files.data) {
		emitter.compileModule(file.mod);
	}
}

void finalizeStaticData(ref CompilationContext context)
{
	// copy static data into buffer and set offsets
	foreach(size_t i, ref IrGlobal global; context.globals.buffer.data)
	{
		IrIndex globalIndex = IrIndex(cast(uint)i, IrValueKind.global);
		global.validate(globalIndex, &context);

		if (global.isInBuffer) continue; // already in buffer
		if (global.numUsers == 0) continue; // no users

		// alignment
		uint padding = paddingSize!uint(cast(uint)context.staticDataBuffer.length, global.alignment);
		context.staticDataBuffer.pad(padding);

		// offset
		global.staticBufferOffset = cast(uint)context.staticDataBuffer.length;

		ObjectSymbol* globalSym = &context.objSymTab.getSymbol(global.objectSymIndex);
		globalSym.sectionOffset = global.staticBufferOffset;

		// copy
		if (global.initializerPtr !is null) {
			context.staticDataBuffer.put(global.initializer);
		} else {
			context.staticDataBuffer.voidPut(global.length)[] = 0;
		}

		// zero termination
		if (global.needsZeroTermination)
			context.staticDataBuffer.put(0);

		global.flags |= IrGlobalFlags.isInBuffer;
		//writefln("Global %s, size %s, zero %s, offset %s, buf size %s",
		//	global.initializer, global.length, global.needsZeroTermination, global.staticBufferOffset, context.staticDataBuffer.length);
	}
}

void addStaticDataSymbols(ref CompilationContext context)
{
	Identifier dataId = context.idMap.getOrRegNoDup(":data");
	foreach(size_t i, ref IrGlobal global; context.globals.buffer.data)
	{
		IrIndex globalIndex = IrIndex(cast(uint)i, IrValueKind.global);
		global.validate(globalIndex, &context);

		ushort symFlags;

		if (global.isMutable) symFlags |= ObjectSymbolFlags.isMutable;
		if (global.isAllZero) symFlags |= ObjectSymbolFlags.isAllZero;
		if (global.needsZeroTermination) symFlags |= ObjectSymbolFlags.needsZeroTermination;
		if (global.isString) symFlags |= ObjectSymbolFlags.isString;

		ObjectSymbol sym = {
			kind : ObjectSymbolKind.isLocal,
			flags : symFlags,
			sectionOffset : global.staticBufferOffset,
			dataPtr : global.initializerPtr,
			sectionIndex : context.dataSectionIndex,
			moduleIndex : global.moduleSymIndex,
			length : global.length,
			alignment : global.alignment,
			id : dataId,
		};

		global.objectSymIndex = context.objSymTab.addSymbol(sym);
	}
}

void addFunctionSymbols(ref CompilationContext context, ref ModuleDeclNode mod)
{
	foreach(FunctionDeclNode* f; mod.functions)
	{
		LinkIndex symbolIndex;

		if (f.isExternal)
		{
			// When JIT-compiling, host can provide a set of external functions
			// we will use provided function pointer
			symbolIndex = context.externalSymbols.get(f.id, LinkIndex());

			if (!symbolIndex.isDefined)
			{
				context.error(f.loc, "Unresolved external function %s", f.strId(&context));
				continue;
			}
		}
		else
		{
			ObjectSymbol sym = {
				kind : ObjectSymbolKind.isLocal,
				sectionIndex : context.textSectionIndex,
				moduleIndex : mod.objectSymIndex,
				alignment : 1,
				id : f.id,
			};
			symbolIndex = context.objSymTab.addSymbol(sym);
		}

		// TODO: check that parameters match
		f.backendData.objectSymIndex = symbolIndex;
	}
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
	int stackPointerExtraOffset;
	IrIndex stackPointer;

	void compileModule(ModuleDeclNode* mod)
	{
		ubyte* codeStart = context.codeBuffer.nextPtr;
		gen.encoder.setBuffer(&context.codeBuffer);

		foreach(f; mod.functions) {
			if (f.isExternal) continue;
			compileFunction(f);
		}

		ubyte[] code = codeStart[0..context.codeBuffer.nextPtr-codeStart];

		if (context.printCodeHex) {
			writefln("// Amd64 code: addr 0x%X, %s bytes", code.ptr, code.length);
			printHex(code, 16);
			writeln;
		}
	}

	void compileFunction(FunctionDeclNode* f)
	{
		fun = f;
		lir = fun.backendData.lirData;
		fun.backendData.funcPtr = gen.pc;

		ObjectSymbol* funcSym = &context.objSymTab.getSymbol(fun.backendData.objectSymIndex);
		funcSym.dataPtr = gen.pc;
		funcSym.sectionOffset = cast(ulong)(gen.pc - context.codeBuffer.bufPtr);

		Identifier mainId = context.idMap.getOrRegNoDup("main");
		if (fun.backendData.name == mainId)
		{
			if (context.entryPoint !is null)
			{
				context.unrecoverable_error(fun.loc, "Multiple entry points: %s, %s", fun.loc, context.entryPoint.loc);
			}

			context.entryPoint = fun;
		}

		stackPointer = fun.backendData.callingConvention.stackPointer;

		blockStarts = cast(PC[])context.tempBuffer.voidPut(lir.numBasicBlocks * (PC.sizeof / uint.sizeof));

		uint[] buf = context.tempBuffer.voidPut(lir.numBasicBlocks * 2 * (PC.sizeof / uint.sizeof));
		buf[] = 0;
		jumpFixups = cast(PC[2][])buf;

		compileFuncProlog();
		compileBody();
		fixJumps();
	}

	void compileFuncProlog()
	{
		// Establish frame pointer
		if (context.useFramePointer)
		{
			gen.pushq(Register.BP);
			gen.movq(Register.BP, Register.SP);
		}

		uint reservedBytes = fun.backendData.stackLayout.reservedBytes;
		if (reservedBytes) // Reserve space for locals
		{
			if (reservedBytes > byte.max) gen.subq(Register.SP, Imm32(reservedBytes));
			else gen.subq(Register.SP, Imm8(cast(byte)reservedBytes));
		}
	}

	void compileFuncEpilog()
	{
		uint reservedBytes = fun.backendData.stackLayout.reservedBytes;
		if (reservedBytes)
		{
			if (reservedBytes > byte.max) gen.addq(Register.SP, Imm32(reservedBytes));
			else gen.addq(Register.SP, Imm8(cast(byte)reservedBytes));
		}

		if (context.useFramePointer)
		{
			// Restore frame pointer
			gen.popq(Register.BP);
		}

		gen.ret();
	}

	uint referenceOffset()
	{
		ptrdiff_t diff = cast(void*)gen.pc - fun.backendData.funcPtr;
		assert(diff >= 0, "Negative buffer position");
		assert(diff <= uint.max, "Function is bigger than uint.max");
		return cast(uint)diff;
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
					case Amd64Opcode.xchg:
						context.assertf(instrHeader.args[0].isPhysReg, "%s is not phys reg", instrHeader.args[0]);
						context.assertf(instrHeader.args[1].isPhysReg, "%s is not phys reg", instrHeader.args[1]);
						context.assertf(instrHeader.args[0].physRegSize == instrHeader.args[1].physRegSize,
							"reg size mismatch %s != %s",
							instrHeader.args[0].physRegSize,
							instrHeader.args[1].physRegSize);
						Register dst = indexToRegister(instrHeader.args[0]);
						Register src = indexToRegister(instrHeader.args[1]);
						gen.xchg(dst, src, cast(ArgType)instrHeader.args[0].physRegSize);
						break;
					case Amd64Opcode.load:
						genLoad(instrHeader.result, instrHeader.args[0]);
						break;
					case Amd64Opcode.store:
						genStore(instrHeader.args[0], instrHeader.args[1], cast(ArgType)instrHeader.argSize);
						break;
					case Amd64Opcode.add:
						genRegular(instrHeader.args[0], instrHeader.args[1], AMD64OpRegular.add, cast(ArgType)instrHeader.args[0].physRegSize);
						if (instrHeader.args[0] == stackPointer && instrHeader.args[1].isConstant)
						{
							stackPointerExtraOffset -= context.constants.get(instrHeader.args[1]).i64;
						}
						break;
					case Amd64Opcode.sub:
						genRegular(instrHeader.args[0], instrHeader.args[1], AMD64OpRegular.sub, cast(ArgType)instrHeader.args[0].physRegSize);
						if (instrHeader.args[0] == stackPointer && instrHeader.args[1].isConstant)
						{
							stackPointerExtraOffset += context.constants.get(instrHeader.args[1]).i64;
						}
						break;
					case Amd64Opcode.imul:
						context.assertf(instrHeader.args[0].isPhysReg, "%s is not phys reg", instrHeader.args[0]);
						Register dst = indexToRegister(instrHeader.args[0]);
						switch(instrHeader.args[1].kind) with(IrValueKind) {
							case constant:
								IrConstant con = context.constants.get(instrHeader.args[1]);
								gen.imulq(dst, dst, Imm32(con.i32));
								break;
							case physicalRegister:
								Register src = indexToRegister(instrHeader.args[1]);
								gen.imul(dst, src, cast(ArgType)instrHeader.args[0].physRegSize);
								break;
							default:
								context.internal_error("imul %s not implemented", instrHeader.args);
								assert(false);
						}
						break;
					case Amd64Opcode.call:
						FunctionIndex calleeIndex = instrHeader.preheader!IrInstrPreheader_call.calleeIndex;
						FunctionDeclNode* callee = context.getFunction(calleeIndex);
						ObjectSymbol* sym = &context.objSymTab.getSymbol(callee.backendData.objectSymIndex);

						if (sym.isIndirect)
							gen.call(memAddrRipDisp32(0));
						else
							gen.call(Imm32(0));

						ObjectSymbolReference r = {
							fromSymbol : fun.backendData.objectSymIndex,
							referencedSymbol : callee.backendData.objectSymIndex,
							refOffset : referenceOffset() - 4,
							4,
							ObjectSymbolRefKind.relative32,
						};
						context.objSymTab.addReference(r);
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
						Register reg = indexToRegister(instrHeader.args[0]);
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
						compileFuncEpilog();
						break;
					case Amd64Opcode.push:
						IrIndex src = instrHeader.args[0];
						switch (src.kind) with(IrValueKind)
						{
							case constant:
								IrConstant con = context.constants.get(src);
								gen.pushd(Imm32(con.i32));
								break;

							case physicalRegister:
								Register reg = indexToRegister(src);
								gen.pushq(reg);
								break;
							default: context.unreachable; assert(false);
						}
						stackPointerExtraOffset += STACK_ITEM_SIZE;
						break;
					default:
						context.internal_error("Unimplemented instruction `%s`", cast(Amd64Opcode)instrHeader.op);
						break;
				}
			}

			context.assertf(stackPointerExtraOffset == 0, "Unmatched stack size modification");
		}
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
		return minMemAddrBaseDisp(baseReg, stackSlot.displacement + stackPointerExtraOffset);
	}

	Register indexToRegister(IrIndex regIndex) {
		context.assertf(regIndex.isPhysReg, "Index is not register, but %s", regIndex.kind);
		return cast(Register)regIndex.physRegIndex;
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
			case none, listItem, instruction, basicBlock, phi, type, virtualRegister, variable, func: assert(false);
			case constant:
				IrConstant con = context.constants.get(src);
				if (con.i64.argSizeIntSigned == IrArgSize.size8) {
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

			case physicalRegister:
				argSrc.reg = indexToRegister(src);
				param.srcKind = AsmArgKind.REG;
				break;

			case stackSlot: context.unreachable; assert(false); // gen.mov(reg0, localVarMemAddress(valueRef), argType);
		}
		gen.encodeRegular(argDst, argSrc, param);
	}

	/// Generate move from src operand to dst operand. argType describes the size of operands.
	void genMove(IrIndex dst, IrIndex src, ArgType argType)
	{
		version(emit_mc_print) writefln("genMove %s %s", dst, src);
		MoveType moveType = calcMoveType(dst.kind, src.kind);

		if (moveType != MoveType.invalid && dst == src) return;

		Register srcReg = cast(Register)src.physRegIndex;
		Register dstReg = cast(Register)dst.physRegIndex;

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

			// copy address of global into register
			case MoveType.global_to_reg:
				// HACK, TODO: 32bit version of reg is incoming, while for ptr 64bits are needed
				MemAddress addr = memAddrRipDisp32(0);
				gen.lea(dstReg, addr, ArgType.QWORD);

				IrGlobal* global = &context.globals.get(src);
				ObjectSymbolReference r = {
					fromSymbol : fun.backendData.objectSymIndex,
					referencedSymbol : global.objectSymIndex,
					refOffset : referenceOffset() - 4,
					4,
					ObjectSymbolRefKind.relative32,
				};
				context.objSymTab.addReference(r);
				break;

			case MoveType.reg_to_reg:
				version(emit_mc_print) writefln("  move.%s reg:%s, reg:%s", argType, dstReg, srcReg);
				gen.mov(dstReg, srcReg, argType);
				break;

			// copy address of stack slot into register
			case MoveType.stack_to_reg:
				gen.lea(dstReg, localVarMemAddress(src), ArgType.QWORD);
				break;
		}
	}

	void fix_PC_REL_32(PC fixup, PC target)
	{
		*cast(Imm32*)(fixup-4) = jumpOffset(fixup, target);
	}

	// nextInstr is address
	void fix_PC_REL_CUSTOM(Imm32* offset, PC nextInstr, PC target)
	{
		*offset = jumpOffset(nextInstr, target);
	}

	/// Generate move from src operand to dst operand. argType describes the size of operands.
	// If src is phys reg then it is used as address base.
	// dst must be phys reg
	void genLoad(IrIndex dst, IrIndex src)
	{
		bool valid = dst.isPhysReg && (src.isPhysReg || src.isStackSlot);
		context.assertf(valid, "Invalid load %s -> %s", src.kind, dst.kind);

		ArgType argType = cast(ArgType)dst.physRegSize;
		Register dstReg = indexToRegister(dst);

		switch(src.kind) with(IrValueKind)
		{
			case physicalRegister:
				Register srcReg = indexToRegister(src);
				gen.mov(dstReg, memAddrBase(srcReg), argType);
				break;

			case stackSlot:
				gen.mov(dstReg, localVarMemAddress(src), argType);
				break;

			default:
				context.internal_error("invalid source of load %s", src.kind);
				break;
		}
	}

	// dst must be of pointer type
	void genStore(IrIndex dst, IrIndex src, ArgType argType)
	{
		context.assertf(!src.isGlobal,
			"store %s <- %s, must go through intermediate register",
			dst.kind, src.kind);

		MoveType moveType = calcMoveType(dst.kind, src.kind);
		switch (moveType) with(MoveType)
		{
			case const_to_stack:
				uint con = context.constants.get(src).i32;
				MemAddress dstMem = localVarMemAddress(dst);
				gen.mov(dstMem, Imm32(con), argType);
				break;
			case const_to_reg:
				Register dstReg = indexToRegister(dst);
				MemAddress dstMem = memAddrBase(dstReg);
				uint con = context.constants.get(src).i32;
				gen.mov(dstMem, Imm32(con), argType);
				break;
			case reg_to_stack:
				Register srcReg = indexToRegister(src);
				MemAddress dstMem = localVarMemAddress(dst);
				gen.mov(dstMem, srcReg, argType);
				break;
			case reg_to_reg:
				Register dstReg = indexToRegister(dst);
				MemAddress dstMem = memAddrBase(dstReg);
				Register srcReg = indexToRegister(src);
				gen.mov(dstMem, srcReg, argType);
				break;
			case const_to_global:
				uint con = context.constants.get(src).i32;
				IrGlobal* global = &context.globals.get(dst);
				context.assertf(global.isInBuffer, "Global is not in static data buffer");

				MemAddress addr = memAddrRipDisp32(0);
				gen.mov(addr, Imm32(con), argType);

				ObjectSymbolReference r = {
					fromSymbol : fun.backendData.objectSymIndex,
					referencedSymbol : global.objectSymIndex,
					refOffset : referenceOffset() - 8,
					8,
					ObjectSymbolRefKind.relative32,
				};
				context.objSymTab.addReference(r);
				break;
			case reg_to_global:
				Register srcReg = indexToRegister(src);
				IrGlobal* global = &context.globals.get(dst);
				context.assertf(global.isInBuffer, "Global is not in static data buffer");

				MemAddress addr = memAddrRipDisp32(0);
				gen.mov(addr, srcReg, argType);

				ObjectSymbolReference r = {
					fromSymbol : fun.backendData.objectSymIndex,
					referencedSymbol : global.objectSymIndex,
					refOffset : referenceOffset() - 4,
					4,
					ObjectSymbolRefKind.relative32,
				};
				context.objSymTab.addReference(r);
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
				case stackSlot: return MoveType.stack_to_reg;
				default: return MoveType.invalid;
			}
		case stackSlot:
			switch(src) with(IrValueKind) {
				case constant: return MoveType.const_to_stack;
				case physicalRegister: return MoveType.reg_to_stack;
				default: return MoveType.invalid;
			}
		case global:
			switch(src) with(IrValueKind) {
				case constant: return MoveType.const_to_global;
				case physicalRegister: return MoveType.reg_to_global;
				default: return MoveType.invalid;
			}
		default: return MoveType.invalid;
	}
}

enum MoveType
{
	invalid,
	const_to_reg,
	const_to_global,
	global_to_reg,
	const_to_stack,
	reg_to_reg,
	reg_to_stack,
	reg_to_global,
	stack_to_reg,
	const_to_mem,
	reg_to_mem,
	mem_to_reg,
}
