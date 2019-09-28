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
	foreach(AstIndex funcIndex; mod.functions)
	{
		FunctionDeclNode* f = context.getAst!FunctionDeclNode(funcIndex);
		LinkIndex symbolIndex;

		if (f.isExternal)
		{
			// When JIT-compiling, host can provide a set of external functions
			// we will use provided function pointer
			symbolIndex = context.externalSymbols.get(f.id, LinkIndex());

			if (!symbolIndex.isDefined)
			{
				context.error(f.loc, "Unresolved external function %s", context.idString(f.id));
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

		foreach(funcIndex; mod.functions) {
			FunctionDeclNode* f = context.getAst!FunctionDeclNode(funcIndex);

			if (f.isExternal) continue;
			compileFunction(f);
		}

		ubyte[] code = codeStart[0..context.codeBuffer.nextPtr-codeStart];

		if (context.printCodeHex && context.printDumpOfAll) {
			writefln("// Amd64 code: addr 0x%X, %s bytes", code.ptr, code.length);
			printHex(code, 16);
			writeln;
		}
	}

	void compileFunction(FunctionDeclNode* f)
	{
		fun = f;
		lir = context.getAst!IrFunction(fun.backendData.lirData);

		ObjectSymbol* funcSym = &context.objSymTab.getSymbol(fun.backendData.objectSymIndex);
		funcSym.dataPtr = gen.pc;
		funcSym.sectionOffset = cast(ulong)(gen.pc - context.codeBuffer.bufPtr);

		Identifier mainId = context.idMap.getOrRegNoDup("main");
		if (fun.id == mainId)
		{
			if (context.entryPoint !is null)
			{
				context.unrecoverable_error(fun.loc, "Multiple entry points: %s, %s", fun.loc, context.entryPoint.loc);
			}

			context.entryPoint = fun;
		}

		stackPointer = fun.backendData.getCallConv(context).stackPointer;

		blockStarts = cast(PC[])context.tempBuffer.voidPut(lir.numBasicBlocks * (PC.sizeof / uint.sizeof));

		uint[] buf = context.tempBuffer.voidPut(lir.numBasicBlocks * 2 * (PC.sizeof / uint.sizeof)); // TODO: free mem
		buf[] = 0;
		jumpFixups = cast(PC[2][])buf;

		compileFuncProlog();
		compileBody();
		fixJumps();

		funcSym.length = cast(uint)(gen.pc - funcSym.dataPtr);

		if (context.printCodeHex && context.printDumpOnlyOf(f)) {
			writefln("// Amd64 code: %s addr 0x%X, %s bytes", context.idString(f.id), funcSym.dataPtr, funcSym.length);
			printHex(funcSym.dataPtr[0..funcSym.length], 16);
		}
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
		ObjectSymbol* funcSym = &context.objSymTab.getSymbol(fun.backendData.objectSymIndex);
		ptrdiff_t diff = gen.pc - funcSym.dataPtr;
		assert(diff >= 0, "Negative buffer position");
		assert(diff <= uint.max, "Function is bigger than uint.max");
		return cast(uint)diff;
	}

	void genJumpToSuccessor(ref IrBasicBlock fromBlock, ubyte successorIndex)
	{
		if (fromBlock.seqIndex + 1 != lir.getBlock(fromBlock.successors[successorIndex, lir]).seqIndex)
		{
			gen.jmp(Imm32(0));
			jumpFixups[fromBlock.seqIndex][successorIndex] = gen.pc;
		}
	}

	void compileBody()
	{
		lir.assignSequentialBlockIndices();

		foreach (IrIndex lirBlockIndex, ref IrBasicBlock lirBlock; lir.blocks)
		{
			blockStarts[lirBlock.seqIndex] = gen.pc;
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; lirBlock.instructions(lir))
			{
				switch(cast(Amd64Opcode)instrHeader.op)
				{
					case Amd64Opcode.mov:
						genMove(instrHeader.result(lir), instrHeader.arg(lir, 0));
						break;
					case Amd64Opcode.xchg:
						context.assertf(instrHeader.arg(lir, 0).isPhysReg, "%s is not phys reg", instrHeader.arg(lir, 0));
						context.assertf(instrHeader.arg(lir, 1).isPhysReg, "%s is not phys reg", instrHeader.arg(lir, 1));
						context.assertf(instrHeader.arg(lir, 0).physRegSize == instrHeader.arg(lir, 1).physRegSize,
							"reg size mismatch %s != %s",
							instrHeader.arg(lir, 0).physRegSize,
							instrHeader.arg(lir, 1).physRegSize);
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						Register src = indexToRegister(instrHeader.arg(lir, 1));
						gen.xchg(dst, src, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.load:
						genLoad(instrHeader.result(lir), instrHeader.arg(lir, 0));
						break;
					case Amd64Opcode.store:
						genStore(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), cast(ArgType)instrHeader.argSize);
						break;
					case Amd64Opcode.add:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.add, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						if (instrHeader.arg(lir, 0) == stackPointer && instrHeader.arg(lir, 1).isConstant)
						{
							stackPointerExtraOffset -= context.constants.get(instrHeader.arg(lir, 1)).i64;
						}
						break;
					case Amd64Opcode.sub:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.sub, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						if (instrHeader.arg(lir, 0) == stackPointer && instrHeader.arg(lir, 1).isConstant)
						{
							stackPointerExtraOffset += context.constants.get(instrHeader.arg(lir, 1)).i64;
						}
						break;
					case Amd64Opcode.xor:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.xor, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.or:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.or, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.and:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.and, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.imul:
						context.assertf(instrHeader.arg(lir, 0).isPhysReg, "%s is not phys reg", instrHeader.arg(lir, 0));
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						switch(instrHeader.arg(lir, 1).kind) with(IrValueKind) {
							case constant:
								IrConstant con = context.constants.get(instrHeader.arg(lir, 1));
								gen.imulq(dst, dst, Imm32(con.i32));
								break;
							case physicalRegister:
								Register src = indexToRegister(instrHeader.arg(lir, 1));
								gen.imul(dst, src, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
								break;
							default:
								context.internal_error("imul %s not implemented", instrHeader.args(lir));
								assert(false);
						}
						break;
					case Amd64Opcode.div:
						Register divisor = indexToRegister(instrHeader.arg(lir, 2));
						gen.div(divisor, cast(ArgType)instrHeader.arg(lir, 2).physRegSize);
						break;
					case Amd64Opcode.idiv:
						Register divisor = indexToRegister(instrHeader.arg(lir, 2));
						gen.idiv(divisor, cast(ArgType)instrHeader.arg(lir, 2).physRegSize);
						break;
					case Amd64Opcode.divsx:
						final switch(instrHeader.argSize) {
							case IrArgSize.size8: gen.movsx_btow(Register.AX, Register.AX); break;
							case IrArgSize.size16: gen.cwd; break;
							case IrArgSize.size32: gen.cdq; break;
							case IrArgSize.size64: gen.cqo; break;
						}
						break;
					case Amd64Opcode.shl:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						IrIndex src = instrHeader.arg(lir, 1);
						if (src.isConstant) {
							IrConstant con = context.constants.get(instrHeader.arg(lir, 1));
							if (con.i8 == 1)
								gen.shl1(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
							else
								gen.shli(dst, Imm8(con.i8), cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						}
						else
							gen.shl(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.shr:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						IrIndex src = instrHeader.arg(lir, 1);
						if (src.isConstant) {
							IrConstant con = context.constants.get(instrHeader.arg(lir, 1));
							if (con.i8 == 1)
								gen.shr1(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
							else
								gen.shri(dst, Imm8(con.i8), cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						}
						else
							gen.shr(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.sar:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						IrIndex src = instrHeader.arg(lir, 1);
						if (src.isConstant) {
							IrConstant con = context.constants.get(instrHeader.arg(lir, 1));
							if (con.i8 == 1)
								gen.sar1(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
							else
								gen.sari(dst, Imm8(con.i8), cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						}
						else
							gen.sar(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.not:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						gen.not(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.neg:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						gen.neg(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.call:
						IrIndex calleeIndex = instrHeader.arg(lir, 0);

						if (calleeIndex.isFunction)
						{
							// direct call by name
							FunctionDeclNode* callee = context.getFunction(calleeIndex);
							ObjectSymbol* sym = &context.objSymTab.getSymbol(callee.backendData.objectSymIndex);

							if (sym.isIndirect)
								gen.call(memAddrRipDisp32(0)); // read address from import section
							else
								gen.call(Imm32(0)); // call relative to next instruction

							ObjectSymbolReference r = {
								fromSymbol : fun.backendData.objectSymIndex,
								referencedSymbol : callee.backendData.objectSymIndex,
								refOffset : referenceOffset() - 4,
								4,
								ObjectSymbolRefKind.relative32,
							};
							context.objSymTab.addReference(r);
						}
						else
						{
							// call by ptr
							Register calleePtr = indexToRegister(calleeIndex);
							gen.call(calleePtr);
						}
						break;

					case Amd64Opcode.jmp:
						genJumpToSuccessor(lirBlock, 0);
						break;
					case Amd64Opcode.bin_branch:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						auto cond = cast(IrBinaryCondition)instrHeader.cond;

						if (arg0.isConstant)
						{
							if (arg1.isConstant)
							{
								if (evalBinCondition(*context, cond, arg0, arg1))
									genJumpToSuccessor(lirBlock, 0);
								else
									genJumpToSuccessor(lirBlock, 1);
								break;
							}

							// move const to the right
							// TODO: perform canonicalization in middle-end
							swap(arg0, arg1);
							cond = invertBinaryCond(cond);
						}

						genRegular(arg0, arg1, AMD64OpRegular.cmp, cast(ArgType)instrHeader.argSize);
						Condition mach_cond = IrBinCondToAmd64Condition[cond];
						gen.jcc(mach_cond, Imm32(0));
						jumpFixups[lirBlock.seqIndex][0] = gen.pc;
						genJumpToSuccessor(lirBlock, 1);
						break;
					case Amd64Opcode.un_branch:
						if (instrHeader.arg(lir, 0).isConstant)
						{
							IrConstant con = context.constants.get(instrHeader.arg(lir, 0)).i64;
							if (con.i64 && instrHeader.cond == IrUnaryCondition.not_zero ||
								(!con.i64) && instrHeader.cond == IrUnaryCondition.zero)
								genJumpToSuccessor(lirBlock, 0);
							else
								genJumpToSuccessor(lirBlock, 1);
							break;
						}
						Register reg = indexToRegister(instrHeader.arg(lir, 0));
						gen.test(reg, reg, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						Condition cond = IrUnCondToAmd64Condition[instrHeader.cond];
						gen.jcc(cond, Imm32(0));
						jumpFixups[lirBlock.seqIndex][0] = gen.pc;
						genJumpToSuccessor(lirBlock, 1);
						break;
					case Amd64Opcode.set_unary_cond:
						Register reg = indexToRegister(instrHeader.arg(lir, 0));
						gen.test(reg, reg, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						Condition cond = IrUnCondToAmd64Condition[instrHeader.cond];
						Register dst = indexToRegister(instrHeader.result(lir));
						gen.setcc(cond, dst);
						break;
					case Amd64Opcode.set_binary_cond:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.cmp, cast(ArgType)instrHeader.argSize);
						Condition cond = IrBinCondToAmd64Condition[instrHeader.cond];
						Register dst = indexToRegister(instrHeader.result(lir));
						gen.setcc(cond, dst);
						break;
					case Amd64Opcode.ret:
						compileFuncEpilog();
						break;
					case Amd64Opcode.push:
						IrIndex src = instrHeader.arg(lir, 0);
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
			if (fixups[0] !is null) fixJump(fixups[0], lirBlock.successors[0, lir]);
			if (fixups[1] !is null) fixJump(fixups[1], lirBlock.successors[1, lir]);
		}
	}

	MemAddress localVarMemAddress(IrIndex stackSlotIndex) {
		context.assertf(stackSlotIndex.isStackSlot, "Index is not stack slot, but %s", stackSlotIndex.kind);
		auto stackSlot = &fun.backendData.stackLayout[stackSlotIndex];
		Register baseReg = indexToRegister(stackSlot.baseReg);
		return minMemAddrBaseDisp(baseReg, stackSlot.displacement + stackPointerExtraOffset);
	}

	Register indexToRegister(IrIndex regIndex) {
		context.assertf(regIndex.isPhysReg, "Index is not register, but %s %s", regIndex.kind, regIndex);
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

		//writefln("%s.%s %s %s", op, argType, dst.kind, src.kind);

		final switch (src.kind) with(IrValueKind)
		{
			case none, listItem, instruction, basicBlock, phi, type, virtualRegister, variable, func, constantAggregate: assert(false);
			case constant:
				IrConstant con = context.constants.get(src);
				if (con.i64.argSizeIntSigned == IrArgSize.size8) {
					param.immType = ArgType.BYTE;
					argSrc.imm8 = Imm8(con.i8);
				}
				else if (argType == ArgType.WORD) {
					param.immType = ArgType.WORD;
					argSrc.imm16 = Imm16(con.i16);
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

	/// Generate move from src operand to dst operand. Size of destination is used
	void genMove(IrIndex dst, IrIndex src)
	{
		ArgType argType = cast(ArgType)dst.physRegSize;
		version(emit_mc_print) writefln("genMove %s %s %s", dst, src, argType);
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
				IrConstant con = context.constants.get(src);
				version(emit_mc_print) writefln("  move.%s reg:%s, con:%s", argType, dstReg, con.i64);
				if (con.i64 == 0) // xor
				{
					AsmArg argDst = {reg : dstReg};
					AsmArg argSrc = {reg : dstReg};
					AsmOpParam param = AsmOpParam(AsmArgKind.REG, AsmArgKind.REG, AMD64OpRegular.xor, argType);
					gen.encodeRegular(argDst, argSrc, param);
				}
				else
				{
					final switch(argType) {
						case ArgType.BYTE: gen.movb(dstReg, Imm8(con.i8)); break;
						case ArgType.WORD: gen.movw(dstReg, Imm16(con.i16)); break;
						case ArgType.DWORD: gen.movd(dstReg, Imm32(con.i32)); break;
						case ArgType.QWORD:
							if (con.payloadSize(src) == IrArgSize.size64)
								gen.movq(dstReg, Imm64(con.i64));
							else
								gen.movd(dstReg, Imm32(con.i32));
							break;
					}
				}
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

			// copy address of function into register
			case MoveType.func_to_reg:
				// HACK, TODO: 32bit version of reg is incoming, while for ptr 64bits are needed
				MemAddress addr = memAddrRipDisp32(0);
				gen.lea(dstReg, addr, ArgType.QWORD);

				FunctionDeclNode* func = context.getFunction(src);
				ObjectSymbolReference r = {
					fromSymbol : fun.backendData.objectSymIndex,
					referencedSymbol : func.backendData.objectSymIndex,
					refOffset : referenceOffset() - 4,
					4,
					ObjectSymbolRefKind.relative32,
				};
				context.objSymTab.addReference(r);
				break;

			case MoveType.reg_to_reg:
				version(emit_mc_print) writefln("  move.%s reg:%s, reg:%s", argType, dstReg, srcReg);
				if (dstReg == srcReg) return; // skip if same register
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
		bool valid = dst.isPhysReg && (src.isPhysReg || src.isStackSlot || src.isGlobal);
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

			case global:
				IrGlobal* global = &context.globals.get(src);
				context.assertf(global.isInBuffer, "Global is not in static data buffer");

				MemAddress addr = memAddrRipDisp32(0);
				gen.mov(dstReg, addr, argType);

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
				context.internal_error("invalid source of load %s", src.kind);
				break;
		}
	}

	// dst must be of pointer type
	// dst is pointer of unknown type (that's why we need explicit argType)
	void genStore(IrIndex dst, IrIndex src, ArgType argType)
	{
		context.assertf(!src.isGlobal,
			"store %s <- %s, must go through intermediate register",
			dst.kind, src.kind);

		MoveType moveType = calcMoveType(dst.kind, src.kind);
		switch (moveType) with(MoveType)
		{
			case const_to_stack:
				IrConstant con = context.constants.get(src);
				MemAddress dstMem = localVarMemAddress(dst);
				final switch(argType) with(ArgType)
				{
					case BYTE: gen.movb(dstMem, Imm8(con.i8)); break;
					case WORD: gen.movw(dstMem, Imm16(con.i16)); break;
					case DWORD: gen.movd(dstMem, Imm32(con.i32)); break;
					case QWORD:
						IrArgSize dataSize = con.payloadSize(src);
						context.assertf(dataSize != IrArgSize.size64, "Constant is too big");
						gen.movq(dstMem, Imm32(con.i32));
						break;
				}
				break;
			case const_to_reg:
				IrConstant con = context.constants.get(src);
				Register dstReg = indexToRegister(dst);
				MemAddress dstMem = memAddrBase(dstReg);
				final switch(argType) with(ArgType)
				{
					case BYTE: gen.movb(dstMem, Imm8(con.i8)); break;
					case WORD: gen.movw(dstMem, Imm16(con.i16)); break;
					case DWORD: gen.movd(dstMem, Imm32(con.i32)); break;
					case QWORD:
						IrArgSize dataSize = con.payloadSize(src);
						context.assertf(dataSize != IrArgSize.size64, "Constant is too big");
						gen.movq(dstMem, Imm32(con.i32));
						break;
				}
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
				case func: return MoveType.func_to_reg;
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
	func_to_reg,
}
