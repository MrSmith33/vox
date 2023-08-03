/**
Copyright: Copyright (c) 2018-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module vox.be.emit_mc_amd64;

import std.stdio;

import vox.all;
import vox.be.amd64asm;

/// Emits machine code for amd64 architecture
void pass_emit_mc_amd64(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	auto emitter = CodeEmitter(&context);

	// emit code
	foreach (ref SourceFileInfo file; context.files.data) {
		emitter.compileModule(file.mod);
	}

	fillStaticDataSections(&context);

	if (context.printStaticData) {
		writefln("// RW data: addr 0x%X, %s bytes",
			context.staticDataBuffer.bufPtr,
			context.staticDataBuffer.length);
		printHex(context.staticDataBuffer.data, 16);
		writefln("// RO data: addr 0x%X, %s bytes",
			context.roStaticDataBuffer.bufPtr,
			context.roStaticDataBuffer.length);
		printHex(context.roStaticDataBuffer.data, 16);
	}
}

// Arranges static data inside static data sections
void fillStaticDataSections(CompilationContext* c)
{
	// copy initialized static data into buffer and set offsets
	foreach(size_t i, ref IrGlobal global; c.globals.buffer.data)
	{
		ObjectSymbol* globalSym = c.objSymTab.getSymbol(global.objectSymIndex);
		if (globalSym.isAllZero) continue;

		ObjectSection* symSection = c.objSymTab.getSection(globalSym.sectionIndex);

		if (symSection.buffer.contains(globalSym.initializer.ptr)) {
			// If data is in we assume that zero termination was handled as needed
			globalSym.sectionOffset = cast(uint)(globalSym.initializer.ptr - symSection.buffer.bufPtr);
			continue;
		}

		// alignment
		uint padding = paddingSize!uint(cast(uint)symSection.buffer.length, globalSym.alignment);
		symSection.buffer.pad(padding);

		// offset
		globalSym.sectionOffset = cast(uint)symSection.buffer.length;

		// copy data
		c.assertf(globalSym.dataPtr !is null, "null initializer");
		globalSym.dataPtr = symSection.buffer.put(globalSym.initializer).ptr;

		// zero termination
		if (globalSym.needsZeroTermination) symSection.buffer.put(0);
		//writefln("Global %s, size %s, zero %s, offset %s, buf size %s",
		//	globalSym.initializer, globalSym.length, globalSym.needsZeroTermination, globalSym.sectionOffset, symSection.buffer.length);
	}

	uint zeroDataOffset = cast(uint)c.staticDataBuffer.length;
	LinkIndex rwSectionIndex = c.builtinSections[ObjectSectionType.rw_data];

	// second pass for zero initialized data
	foreach(size_t i, ref IrGlobal global; c.globals.buffer.data)
	{
		ObjectSymbol* globalSym = c.objSymTab.getSymbol(global.objectSymIndex);
		if (!globalSym.isAllZero) continue;

		c.assertf(globalSym.sectionIndex == rwSectionIndex, "Cannot have zero-initialized data in sections other than RW");

		// alignment
		uint padding = paddingSize!uint(zeroDataOffset, globalSym.alignment);
		zeroDataOffset += padding;

		// offset
		globalSym.sectionOffset = zeroDataOffset;

		// copy data
		zeroDataOffset += globalSym.length;

		// zero termination
		if (globalSym.needsZeroTermination) ++zeroDataOffset;
	}

	ObjectSection* rwSection = c.objSymTab.getSection(c.builtinSections[ObjectSectionType.rw_data]);
	c.zeroDataLength = zeroDataOffset - cast(uint)c.staticDataBuffer.length;
	rwSection.zeroDataLength = c.zeroDataLength;
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
		context.currentFunction = f;
		scope(exit) context.currentFunction = null;

		fun = f;
		lir = context.getAst!IrFunction(fun.backendData.lirData);

		ObjectSymbol* funcSym = context.objSymTab.getSymbol(fun.backendData.objectSymIndex);
		funcSym.dataPtr = gen.pc;
		funcSym.sectionOffset = cast(ulong)(gen.pc - context.codeBuffer.bufPtr);

		if (context.buildType == BuildType.exe && fun.id == CommonIds.id_main)
		{
			if (context.entryPoint !is null)
			{
				context.unrecoverable_error(fun.loc, "Multiple entry points: %s, %s", fun.loc, context.entryPoint.loc);
			}

			context.entryPoint = fun;

			if (context.targetOs == TargetOs.linux) {
				// On Linux entry point is aligned to 16 bytes, but we assume 16 byte alignment + 8 bytes from call istruction
				// section 3.4.1 of AMD64 ABI 1.0 says:
				//  `rsp`: The stack pointer holds the address of the byte with lowest address which is part of
				//         the stack. It is guaranteed to be 16-byte aligned at process entry
				// https://stackoverflow.com/questions/26866723/main-and-stack-alignment
				gen.subq(Register.SP, Imm8(8));
			}
		}

		stackPointer = IrIndex(lir.getCallConv(context).stackPointer, ArgType.QWORD);

		blockStarts = cast(PC[])context.tempBuffer.voidPut(lir.numBasicBlocks * (PC.sizeof / uint.sizeof));

		uint[] buf = context.tempBuffer.voidPut(lir.numBasicBlocks * 2 * (PC.sizeof / uint.sizeof)); // TODO: free mem
		// buf[] = 0; //zeroing is not needed, because both slots are correctly filled by jump instruction emitters
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
		uint reservedBytes = lir.stackFrameSize;

		// frame pointer is stored with a push, so don't allocate space for it
		if (context.useFramePointer) {
			context.assertf(reservedBytes >= STACK_ITEM_SIZE, "bug");
			reservedBytes -= STACK_ITEM_SIZE;
		}

		// Establish frame pointer
		if (context.useFramePointer)
		{
			gen.pushq(Register.BP);
			gen.movq(Register.BP, Register.SP);
		}

		if (reservedBytes) // Reserve space for locals
		{
			if (reservedBytes > byte.max) gen.subq(Register.SP, Imm32(reservedBytes));
			else gen.subq(Register.SP, Imm8(cast(byte)reservedBytes));
		}
	}

	void compileFuncEpilog()
	{
		uint reservedBytes = lir.stackFrameSize;

		// frame pointer is stored with a push, so don't allocate space for it
		if (context.useFramePointer) {
			context.assertf(reservedBytes >= STACK_ITEM_SIZE, "bug");
			reservedBytes -= STACK_ITEM_SIZE;
		}

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
		ObjectSymbol* funcSym = context.objSymTab.getSymbol(fun.backendData.objectSymIndex);
		ptrdiff_t diff = gen.pc - funcSym.dataPtr;
		context.assertf(diff >= 0, "Negative buffer position");
		context.assertf(diff <= uint.max, "Function is bigger than uint.max");
		return cast(uint)diff;
	}

	// successorBIndex is 0 or 1
	void genJumpToSuccessors(ref IrBasicBlock fromBlock, ubyte successorBIndex, PC successorA = null)
	{
		if (fromBlock.seqIndex + 1 != lir.getBlock(fromBlock.successors[successorBIndex, lir]).seqIndex) {
			gen.jmp(Imm32(0));
			jumpFixups[fromBlock.seqIndex][successorBIndex] = gen.pc;
		} else {
			// zero out the successor fixup
			jumpFixups[fromBlock.seqIndex][successorBIndex] = null;
		}
		// zero out the other fixup
		jumpFixups[fromBlock.seqIndex][1 - successorBIndex] = successorA;
	}

	void compileBody()
	{
		lir.assignSequentialBlockIndices();

		foreach (IrIndex lirBlockIndex, ref IrBasicBlock lirBlock; lir.blocks)
		{
			blockStarts[lirBlock.seqIndex] = gen.pc;
			stackPointerExtraOffset = 0;
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; lirBlock.instructions(lir))
			{
				switch(cast(Amd64Opcode)instrHeader.op)
				{
					case Amd64Opcode.mov:
						genMove(instrHeader.result(lir), instrHeader.arg(lir, 0));
						break;
					case Amd64Opcode.xchg:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						context.assertf(arg1.isPhysReg, "%s is not phys reg", arg1);
						context.assertf(arg0.isPhysReg, "%s is not phys reg", arg0);
						context.assertf(arg0.physRegSize == arg1.physRegSize,
							"%s:%s reg size mismatch %s != %s", lirBlockIndex, instrIndex, arg0.physRegSize, arg1.physRegSize);
						context.assertf(arg0.physRegClass == arg1.physRegClass && arg0.physRegClass == AMD64_REG_CLASS.GPR, "Only GPR xchg is implemented");
						Register dst = indexToRegister(arg0);
						Register src = indexToRegister(arg1);
						gen.xchg(dst, src, cast(ArgType)arg0.physRegSize);
						break;
					case Amd64Opcode.load:
						genLoad(instrHeader.result(lir), instrHeader.arg(lir, 0));
						break;
					case Amd64Opcode.store:
						genStore(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), instrHeader.argSize);
						break;
					case Amd64Opcode.add:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						if (arg1.isStackSlot)
						{
							// this was generated from GEP
							//   reg += rsp + disp8/32
							// convert it into
							//   lea reg, rsp + reg + disp8/32
							Register dst = indexToRegister(arg0);
							MemAddress addr = localVarMemAddress(arg1);
							switch(addr.type) {
								case MemAddrType.baseDisp8:
									MemAddress newAddr = memAddrBaseIndexDisp8(addr.baseReg, dst, SibScale(0), addr.disp8.value);
									gen.lea(dst, newAddr, ArgType.QWORD);
									break;
								case MemAddrType.baseDisp32:
									MemAddress newAddr = memAddrBaseIndexDisp32(addr.baseReg, dst, SibScale(0), addr.disp32.value);
									gen.lea(dst, newAddr, ArgType.QWORD);
									break;
								default:
									context.internal_error("Invalid memory operand %s", addr);
							}
						}
						else
						{
							genRegular(arg0, arg1, AMD64OpRegular.add, instrIndex);
						}

						if (arg0 == stackPointer) {
							if (arg1.isSimpleConstant) {
								stackPointerExtraOffset -= context.constants.get(arg1).i64;
							} else {
								dumpFunction(context, lir, "Code gen");
								context.internal_error("Cannot decrement stack pointer by non-constant in %s", instrIndex);
							}
						}
						break;
					case Amd64Opcode.sub:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						genRegular(arg0, arg1, AMD64OpRegular.sub, instrIndex);
						if (arg0 == stackPointer) {
							if (arg1.isSimpleConstant) {
								stackPointerExtraOffset += context.constants.get(arg1).i64;
							} else {
								dumpFunction(context, lir, "Code gen");
								context.internal_error("Cannot increment stack pointer by non-constant in %s", instrIndex);
							}
						}
						break;
					case Amd64Opcode.xor:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.xor, instrIndex);
						break;
					case Amd64Opcode.or:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.or, instrIndex);
						break;
					case Amd64Opcode.and:
						genRegular(instrHeader.arg(lir, 0), instrHeader.arg(lir, 1), AMD64OpRegular.and, instrIndex);
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

					case Amd64Opcode.fadd:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						context.assertf(arg0.physRegClass == AMD64_REG_CLASS.XMM, "fadd reg class %s != XMM", arg0.physRegClass);
						context.assertf(arg1.physRegClass == AMD64_REG_CLASS.XMM, "fadd reg class %s != XMM", arg1.physRegClass);
						final switch(instrHeader.argSize) with(IrArgSize) {
							case size32: gen.addss(indexToRegister(arg0), indexToRegister(arg1)); break;
							case size64: gen.addsd(indexToRegister(arg0), indexToRegister(arg1)); break;
							case size8, size16, size128, size256, size512: context.internal_error("fadd %s", instrHeader.argSize);
						}
						break;
					case Amd64Opcode.fsub:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						context.assertf(arg0.physRegClass == AMD64_REG_CLASS.XMM, "fsub reg class %s != XMM", arg0.physRegClass);
						context.assertf(arg1.physRegClass == AMD64_REG_CLASS.XMM, "fsub reg class %s != XMM", arg1.physRegClass);
						final switch(instrHeader.argSize) with(IrArgSize) {
							case size32: gen.subss(indexToRegister(arg0), indexToRegister(arg1)); break;
							case size64: gen.subsd(indexToRegister(arg0), indexToRegister(arg1)); break;
							case size8, size16, size128, size256, size512: context.internal_error("fsub %s", instrHeader.argSize);
						}
						break;
					case Amd64Opcode.fdiv:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						context.assertf(arg0.physRegClass == AMD64_REG_CLASS.XMM, "fdiv reg class %s != XMM", arg0.physRegClass);
						context.assertf(arg1.physRegClass == AMD64_REG_CLASS.XMM, "fdiv reg class %s != XMM", arg1.physRegClass);
						final switch(instrHeader.argSize) with(IrArgSize) {
							case size32: gen.divss(indexToRegister(arg0), indexToRegister(arg1)); break;
							case size64: gen.divsd(indexToRegister(arg0), indexToRegister(arg1)); break;
							case size8, size16, size128, size256, size512: context.internal_error("fdiv %s", instrHeader.argSize);
						}
						break;
					case Amd64Opcode.fmul:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						context.assertf(arg0.physRegClass == AMD64_REG_CLASS.XMM, "fmul reg class %s != XMM", arg0.physRegClass);
						context.assertf(arg1.physRegClass == AMD64_REG_CLASS.XMM, "fmul reg class %s != XMM", arg1.physRegClass);
						final switch(instrHeader.argSize) with(IrArgSize) {
							case size32: gen.mulss(indexToRegister(arg0), indexToRegister(arg1)); break;
							case size64: gen.mulsd(indexToRegister(arg0), indexToRegister(arg1)); break;
							case size8, size16, size128, size256, size512: context.internal_error("fmul %s", instrHeader.argSize);
						}
						break;

					case Amd64Opcode.movzx_btow: gen.movzx_btow(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movzx_btod: gen.movzx_btod(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movzx_btoq: gen.movzx_btoq(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movzx_wtod: gen.movzx_wtod(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movzx_wtoq: gen.movzx_wtoq(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movsx_btow: gen.movsx_btow(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movsx_btod: gen.movsx_btod(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movsx_btoq: gen.movsx_btoq(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movsx_wtod: gen.movsx_wtod(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movsx_wtoq: gen.movsx_wtoq(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.movsx_dtoq: gen.movsx_dtoq(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.f32_to_f64: gen.cvtss2sd(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.f64_to_f32: gen.cvtsd2ss(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;

					case Amd64Opcode.i32_to_f32: gen.cvtsid2ss(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.i64_to_f32: gen.cvtsiq2ss(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.i32_to_f64: gen.cvtsid2sd(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.i64_to_f64: gen.cvtsiq2sd(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;

					case Amd64Opcode.f32_to_i32_trunc: gen.cvttss2sid(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.f32_to_i64_trunc: gen.cvttss2siq(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.f64_to_i32_trunc: gen.cvttsd2sid(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;
					case Amd64Opcode.f64_to_i64_trunc: gen.cvttsd2siq(indexToRegister(instrHeader.result(lir)), indexToRegister(instrHeader.arg(lir, 0))); break;

					case Amd64Opcode.rep_stos: gen.rep_prefix; gen.stos; break;
					case Amd64Opcode.divsx:
						final switch(instrHeader.argSize) {
							case IrArgSize.size8: gen.movsx_btow(Register.AX, Register.AX); break;
							case IrArgSize.size16: gen.cwd; break;
							case IrArgSize.size32: gen.cdq; break;
							case IrArgSize.size64: gen.cqo; break;
							case IrArgSize.size128, IrArgSize.size256, IrArgSize.size512: context.internal_error("divsx %s", instrHeader.argSize);
						}
						break;
					case Amd64Opcode.shl:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						IrIndex src = instrHeader.arg(lir, 1);
						if (src.isSimpleConstant) {
							IrConstant con = context.constants.get(instrHeader.arg(lir, 1));
							if (con.i8 == 1)
								gen.shl1(dst, cast(ArgType)instrHeader.argSize);
							else
								gen.shli(dst, Imm8(con.i8), cast(ArgType)instrHeader.argSize);
						}
						else
							gen.shl(dst, cast(ArgType)instrHeader.argSize);
						break;
					case Amd64Opcode.shr:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						IrIndex src = instrHeader.arg(lir, 1);
						if (src.isSimpleConstant) {
							IrConstant con = context.constants.get(instrHeader.arg(lir, 1));
							if (con.i8 == 1)
								gen.shr1(dst, cast(ArgType)instrHeader.argSize);
							else
								gen.shri(dst, Imm8(con.i8), cast(ArgType)instrHeader.argSize);
						}
						else
							gen.shr(dst, cast(ArgType)instrHeader.argSize);
						break;
					case Amd64Opcode.sar:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						IrIndex src = instrHeader.arg(lir, 1);
						if (src.isSimpleConstant) {
							IrConstant con = context.constants.get(instrHeader.arg(lir, 1));
							if (con.i8 == 1)
								gen.sar1(dst, cast(ArgType)instrHeader.argSize);
							else
								gen.sari(dst, Imm8(con.i8), cast(ArgType)instrHeader.argSize);
						}
						else
							gen.sar(dst, cast(ArgType)instrHeader.argSize);
						break;
					case Amd64Opcode.not:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						gen.not(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.neg:
						Register dst = indexToRegister(instrHeader.arg(lir, 0));
						gen.neg(dst, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						break;
					case Amd64Opcode.fneg:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						context.assertf(arg0.physRegClass == AMD64_REG_CLASS.XMM, "incorrect class %s, xmm expected", arg0.physRegClass);
						switch(arg0.physRegSize) {
							case IrArgSize.size32:
								MemAddress addr = memAddrRipDisp32(0);
								gen.xorps(indexToRegister(arg0), addr);
								IrIndex sign_bit_global = context.globals.get_or_add_f32_sign_bit_constant(context);
								addRefTo(sign_bit_global);
								break;
							case IrArgSize.size64:
								MemAddress addr = memAddrRipDisp32(0);
								gen.xorpd(indexToRegister(arg0), addr);
								IrIndex sign_bit_global = context.globals.get_or_add_f64_sign_bit_constant(context);
								addRefTo(sign_bit_global);
								break;
							default: context.internal_error("fneg %s", arg0.physRegSize);
						}
						break;
					case Amd64Opcode.call:
						IrIndex calleeIndex = instrHeader.arg(lir, 0);

						if (calleeIndex.isFunction)
						{
							// direct call by name
							FunctionDeclNode* callee = context.getFunction(calleeIndex);
							ObjectSymbol* sym = context.objSymTab.getSymbol(callee.backendData.objectSymIndex);

							if (sym.isIndirect)
								gen.call(memAddrRipDisp32(0)); // read address from import section
							else
								gen.call(Imm32(0)); // call relative to next instruction

							addRefTo(calleeIndex);
						}
						else
						{
							// call by ptr
							if (calleeIndex.isStackSlot)
							{
								MemAddress addr = localVarMemAddress(calleeIndex);
								gen.call(addr);
							}
							else
							{
								Register calleePtr = indexToRegister(calleeIndex);
								gen.call(calleePtr);
							}
						}
						break;
					case Amd64Opcode.syscall:
						gen.syscall();
						break;
					case Amd64Opcode.jmp:
						genJumpToSuccessors(lirBlock, 0);
						break;
					case Amd64Opcode.bin_branch:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						auto cond = cast(IrBinaryCondition)instrHeader.cond;

						if (arg0.isSimpleConstant)
						{
							if (arg1.isSimpleConstant)
							{
								if (evalBinCondition(*context, cond, arg0, arg1))
									genJumpToSuccessors(lirBlock, 0);
								else
									genJumpToSuccessors(lirBlock, 1);
								break;
							}

							// move const to the right
							// TODO: perform canonicalization in middle-end
							swap(arg0, arg1);
							cond = invertBinaryCond(cond);
						}

						if (arg0.physRegClass == AMD64_REG_CLASS.XMM) {
							context.assertf(arg1.isPhysReg, "%s: arg1.isPhysReg %s", instrIndex, arg1.isPhysReg);
							context.assertf(arg1.physRegClass == AMD64_REG_CLASS.XMM, "%s: arg1.physRegClass %s", instrIndex, arg1.physRegClass);
							final switch(instrHeader.argSize) with(IrArgSize) {
								case size32: gen.ucomiss(indexToRegister(arg0), indexToRegister(arg1)); break;
								case size64: gen.ucomisd(indexToRegister(arg0), indexToRegister(arg1)); break;
								case size8, size16, size128, size256, size512: context.internal_error("bin_branch %s", instrHeader.argSize);
							}
						} else {
							genRegular(arg0, arg1, AMD64OpRegular.cmp, instrIndex);
						}

						Condition mach_cond = IrBinCondToAmd64Condition[cond];
						gen.jcc(mach_cond, Imm32(0));
						genJumpToSuccessors(lirBlock, 1, gen.pc);
						break;
					case Amd64Opcode.un_branch:
						if (instrHeader.arg(lir, 0).isSimpleConstant)
						{
							IrConstant con = context.constants.get(instrHeader.arg(lir, 0));
							if (con.i64 && instrHeader.cond == IrUnaryCondition.not_zero ||
								(!con.i64) && instrHeader.cond == IrUnaryCondition.zero)
								genJumpToSuccessors(lirBlock, 0);
							else
								genJumpToSuccessors(lirBlock, 1);
							break;
						}
						Register reg = indexToRegister(instrHeader.arg(lir, 0));
						gen.test(reg, reg, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						Condition cond = IrUnCondToAmd64Condition[instrHeader.cond];
						gen.jcc(cond, Imm32(0));
						genJumpToSuccessors(lirBlock, 1, gen.pc);
						break;
					case Amd64Opcode.set_unary_cond:
						Register reg = indexToRegister(instrHeader.arg(lir, 0));
						gen.test(reg, reg, cast(ArgType)instrHeader.arg(lir, 0).physRegSize);
						Condition cond = IrUnCondToAmd64Condition[instrHeader.cond];
						Register dst = indexToRegister(instrHeader.result(lir));
						gen.setcc(cond, dst);
						break;
					case Amd64Opcode.set_binary_cond:
						IrIndex arg0 = instrHeader.arg(lir, 0);
						IrIndex arg1 = instrHeader.arg(lir, 1);
						Condition cond = IrBinCondToAmd64Condition[instrHeader.cond];
						if (arg0.physRegClass == AMD64_REG_CLASS.XMM) {
							assert(arg1.physRegClass == AMD64_REG_CLASS.XMM);
							switch(arg0.physRegSize) {
								case IrArgSize.size32: gen.ucomiss(indexToRegister(arg0), indexToRegister(arg1)); break;
								case IrArgSize.size64: gen.ucomisd(indexToRegister(arg0), indexToRegister(arg1)); break;
								default: context.internal_error("set_binary_cond %s", arg0.physRegSize);
							}
						} else {
							genRegular(arg0, arg1, AMD64OpRegular.cmp, instrIndex);
						}
						Register dst = indexToRegister(instrHeader.result(lir));
						gen.setcc(cond, dst);
						break;
					case Amd64Opcode.ret:
						jumpFixups[lirBlock.seqIndex][0] = null;
						jumpFixups[lirBlock.seqIndex][1] = null;
						compileFuncEpilog();
						break;
					case Amd64Opcode.ud2:
						jumpFixups[lirBlock.seqIndex][0] = null;
						jumpFixups[lirBlock.seqIndex][1] = null;
						gen.ud2;
						break;
					case Amd64Opcode.push:
						IrIndex src = instrHeader.arg(lir, 0);
						switch (src.kind) with(IrValueKind)
						{
							case constant, constantZero:
								IrConstant con = context.constants.get(src);
								gen.pushd(Imm32(con.i32));
								break;

							case physicalRegister:
								Register reg = indexToRegister(src);
								gen.pushq(reg);
								break;

							// those wont push the address itself, but memory contents
							/*case stackSlot:
								MemAddress addr = localVarMemAddress(src);
								gen.pushq(addr);
								break;

							case global, func:
								MemAddress addr = memAddrRipDisp32(0);
								gen.pushq(addr);
								addRefTo(src);
								break;*/

							default:
								context.internal_error("Cannot encode %s %s in %s %s",
									cast(Amd64Opcode)instrHeader.op, src, context.idString(lir.name), instrIndex);
						}
						stackPointerExtraOffset += STACK_ITEM_SIZE;
						break;
					default:
						context.internal_error("Unimplemented instruction `%s`", cast(Amd64Opcode)instrHeader.op);
				}
			}

			if (stackPointerExtraOffset != 0) {
				// When we call noreturn function stack cleanup is omitted
				// After such calls we expect unreachable
				if (lir.getInstr(lirBlock.lastInstr).op != Amd64Opcode.ud2)
					context.internal_error("Unmatched stack size modification");
			}
		}
	}

	void addRefTo(IrIndex target, short offset = 4)
	{
		context.objSymTab.addReferenceTo(
			context,
			fun.backendData.objectSymIndex,
			target,
			referenceOffset() - offset,
			offset,
			ObjectSymbolRefKind.relative32);
	}

	void addRefTo(LinkIndex entityIndex, short offset = 4)
	{
		ObjectSymbolReference r = {
			fromSymbol : fun.backendData.objectSymIndex,
			referencedSymbol : entityIndex,
			refOffset : referenceOffset() - offset,
			offset,
			ObjectSymbolRefKind.relative32,
		};
		context.objSymTab.addReference(r);
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
		auto stackSlot = lir.getStackSlot(stackSlotIndex);
		Register baseReg = indexToRegister(stackSlot.baseReg);
		return minMemAddrBaseDisp(baseReg, stackSlot.displacement + stackPointerExtraOffset);
	}

	Register indexToRegister(IrIndex regIndex) {
		context.assertf(regIndex.isPhysReg, "Index is not register, but %s %s", regIndex.kind, regIndex);
		return cast(Register)regIndex.physRegIndex;
	}

	void genRegular(IrIndex dst, IrIndex src, AMD64OpRegular op, IrIndex instrIndex)
	{
		context.assertf(dst.isPhysReg, "dst must be physical register %s", dst.kind);

		IrArgSize argSize = cast(IrArgSize)dst.physRegSize;

		AsmArg argDst;
		AsmArg argSrc;
		AsmOpParam param;
		param.op = op;
		param.argType = cast(ArgType)argSize;

		argDst.reg = indexToRegister(dst);

		// HACK, TODO: ESP is generated instead of RSP. Need to store types in instructions / more instruction types
		if (argDst.reg == Register.SP) param.argType = ArgType.QWORD;

		param.dstKind = AsmArgKind.REG;

		//writefln("%s.%s %s %s", op, param.argType, dst.kind, src.kind);

		final switch (src.kind) with(IrValueKind)
		{
			case none, array, instruction, basicBlock, phi, type, virtualRegister, variable, func, constantAggregate: context.internal_error("divsx src %s", src.kind);
			case constantZero:
			case constant:
				IrConstant con = context.constants.get(src);
				if (con.i64.argSizeIntSigned == IrArgSize.size8) {
					param.immType = ArgType.BYTE;
					argSrc.imm8 = Imm8(con.i8);
				}
				else if (argSize == IrArgSize.size16) {
					param.immType = ArgType.WORD;
					argSrc.imm16 = Imm16(con.i16);
				}
				else {
					param.immType = ArgType.DWORD;
					argSrc.imm32 = Imm32(con.i32);
				}
				param.srcKind = AsmArgKind.IMM;
				break;

			case physicalRegister:
				argSrc.reg = indexToRegister(src);
				param.srcKind = AsmArgKind.REG;
				break;

			case global, stackSlot:
				// This should not happen. Stack slot or global must go through mov or load instruction.
				context.internal_error("Cannot encode %s %s %s in %s %s", op, dst, src, context.idString(lir.name), instrIndex);
		}
		gen.encodeRegular(argDst, argSrc, param);
	}

	/// Generate move from src operand to dst operand. Size of destination is used
	void genMove(IrIndex dst, IrIndex src)
	{
		// i64 <- i32 must be 32bit move if both sides are registers.
		IrArgSize argSize;
		if (src.isPhysReg)
			argSize = cast(IrArgSize)min(dst.physRegSize, src.physRegSize);
		else
			argSize = cast(IrArgSize)dst.physRegSize;

		version(emit_mc_print) writefln("genMove %s %s %s", dst, src, argSize);
		MoveType moveType = calcMoveType(dst.kind, src.kind);

		if (moveType != MoveType.invalid && dst == src) return;

		Register srcReg = cast(Register)src.physRegIndex;
		Register dstReg = cast(Register)dst.physRegIndex;

		switch(moveType)
		{
			default:
				context.internal_error("Invalid move from %s to %s", IrIndexDump(dst, context, lir), IrIndexDump(src, context, lir));

			case MoveType.const_to_reg:
				IrConstant con = context.constants.get(src);
				version(emit_mc_print) writefln("  move.%s reg:%s, con:%s", argSize, dstReg, con.i64);
				if (con.i64 == 0) // xor
				{
					if (dst.physRegClass == AMD64_REG_CLASS.GPR) {
						AsmArg argDst = {reg : dstReg};
						AsmArg argSrc = {reg : dstReg};
						AsmOpParam param = AsmOpParam(AsmArgKind.REG, AsmArgKind.REG, AMD64OpRegular.xor, cast(ArgType)IrArgSize.size32);
						gen.encodeRegular(argDst, argSrc, param);
					} else if (dst.physRegClass == AMD64_REG_CLASS.XMM) {
						// TODO: replace with pxor
						// See: https://stackoverflow.com/questions/33666617/what-is-the-best-way-to-set-a-register-to-zero-in-x86-assembly-xor-mov-or-and
						gen.xorps(dstReg, dstReg);
					}
				}
				else
				{
					if (dst.physRegClass == AMD64_REG_CLASS.GPR) {
						final switch(argSize) with(IrArgSize) {
							case size8: gen.movb(dstReg, Imm8(con.i8)); break;
							case size16: gen.movw(dstReg, Imm16(con.i16)); break;
							case size32: gen.movd(dstReg, Imm32(con.i32)); break;
							case size64:
								if (!con.intFitsIn32Bits)
									gen.movq(dstReg, Imm64(con.i64));
								else {
									if (con.u32_top == uint.max) {
										gen.movq(dstReg, Imm32(con.i32)); // sign-extend 32bit constant to 64bit register
									} else {
										gen.movd(dstReg, Imm32(con.i32)); // zero-extend 32bit constant to 64bit register
									}
								}
								break;
							case size128, size256, size512:
								context.internal_error("Not implemented: const_to_reg %s %s", dst, src);
						}
					} else if (dst.physRegClass == AMD64_REG_CLASS.XMM) {
						LinkIndex roSectionIndex = context.builtinSections[ObjectSectionType.ro_data];
						ObjectSymbol* funcSym = context.objSymTab.getSymbol(fun.backendData.objectSymIndex);
						ObjectSymbol sym = {
							kind : ObjectSymbolKind.isLocal,
							sectionIndex : roSectionIndex,
							moduleIndex : funcSym.moduleIndex,
							id : context.idMap.getOrReg(context, ":float"),
						};
						LinkIndex symIndex = context.objSymTab.addSymbol(sym);
						ObjectSymbol* globalSym = context.objSymTab.getSymbol(symIndex);
						ObjectSection* roSection = context.objSymTab.getSection(roSectionIndex);
						globalSym.sectionOffset = cast(uint)roSection.buffer.length;

						final switch(argSize) with(IrArgSize) {
							case size32:
								globalSym.setInitializer(context.roStaticDataBuffer.nextPtr[0..4]);
								context.roStaticDataBuffer.put(con.i32);
								gen.movd_xr(dstReg, memAddrRipDisp32(0));
								break;
							case size64:
								globalSym.setInitializer(context.roStaticDataBuffer.nextPtr[0..8]);
								context.roStaticDataBuffer.put(con.i64);
								gen.movq_xr(dstReg, memAddrRipDisp32(0));
								break;
							case size8, size16, size128, size256, size512: context.internal_error("genMove XMM <- const %s", argSize);
						}
						addRefTo(symIndex);
					}
				}
				break;

			// copy address of global into register
			case MoveType.global_to_reg:
				context.assertf(dst.physRegClass == AMD64_REG_CLASS.GPR, "global_to_reg %s", dst);
				// HACK, TODO: 32bit version of reg is incoming, while for ptr 64bits are needed
				MemAddress addr = memAddrRipDisp32(0);
				gen.lea(dstReg, addr, cast(ArgType)IrArgSize.size64);
				addRefTo(src);
				break;

			// copy address of function into register
			case MoveType.func_to_reg:
				context.assertf(dst.physRegClass == AMD64_REG_CLASS.GPR, "func_to_reg %s", dst);
				FunctionDeclNode* func = context.getFunction(src);
				LinkIndex entityIndex = func.backendData.objectSymIndex;
				ObjectSymbol* sym = context.objSymTab.getSymbol(entityIndex);
				if (sym.isIndirect) {
					// read address from the import section
					MemAddress addr = memAddrRipDisp32(0);
					gen.mov(dstReg, addr, cast(ArgType)IrArgSize.size64);
				} else {
					// take address of the symbol
					MemAddress addr = memAddrRipDisp32(0);
					gen.lea(dstReg, addr, cast(ArgType)IrArgSize.size64);
				}
				addRefTo(entityIndex);
				break;

			case MoveType.reg_to_reg:
				version(emit_mc_print) writefln("  move.%s reg:%s, reg:%s", argSize, dstReg, srcReg);
				if (src.physRegClass == AMD64_REG_CLASS.XMM && dst.physRegClass == AMD64_REG_CLASS.XMM) {
					final switch(argSize) with(IrArgSize) {
						case size8, size16:
							context.internal_error("Not implemented: reg_to_reg %s %s", dst, src);
						case size32: gen.movss(dstReg, srcReg); break;
						case size64: gen.movsd(dstReg, srcReg); break;
						case size128: gen.movups(dstReg, srcReg); break;
						case size256, size512:
							context.internal_error("Not implemented: reg_to_reg %s %s", dst, src);
					}
				} else if (src.physRegClass == AMD64_REG_CLASS.XMM) {
					final switch(argSize) with(IrArgSize) {
						case size32: gen.movd_rx(dstReg, srcReg); break;
						case size64: gen.movq_rx(dstReg, srcReg); break;
						case size8, size16, size128, size256, size512:
							context.internal_error("Not implemented: reg_to_reg %s %s", dst, src);
					}
				} else if (dst.physRegClass == AMD64_REG_CLASS.XMM) {
					final switch(argSize) with(IrArgSize) {
						case size32: gen.movd_xr(dstReg, srcReg); break;
						case size64: gen.movq_xr(dstReg, srcReg); break;
						case size8, size16, size128, size256, size512:
							context.internal_error("Not implemented: reg_to_reg %s %s", dst, src);
					}
				} else {
					if (dstReg != srcReg) {
						gen.mov(dstReg, srcReg, cast(ArgType)argSize);
					}
				}
				break;

			// copy address of stack slot into register
			case MoveType.stack_to_reg:
				context.assertf(dst.physRegClass == AMD64_REG_CLASS.GPR, "stack_to_reg %s", dst);
				gen.lea(dstReg, localVarMemAddress(src), cast(ArgType)IrArgSize.size64);
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

	void doMemToReg(IrIndex dst, MemAddress srcMem, IrArgSize argSize) {
		Register dstReg = indexToRegister(dst);
		if (dst.physRegClass == AMD64_REG_CLASS.XMM) {
			final switch(argSize) with(IrArgSize) {
				case size32: gen.movd_xr(dstReg, srcMem); break;
				case size64: gen.movq_xr(dstReg, srcMem); break;
				case size128: gen.movups(dstReg, srcMem); break;
				case size8, size16, size256, size512: context.internal_error("doMemToReg %s", argSize);
			}
		} else {
			gen.mov(dstReg, srcMem, cast(ArgType)argSize);
		}
	}

	/// Generate move from src operand to dst operand. argType describes the size of operands.
	// If src is phys reg then it is used as address base.
	// dst must be phys reg
	void genLoad(IrIndex dst, IrIndex src)
	{
		IrArgSize argSize = cast(IrArgSize)dst.physRegSize;
		bool valid = dst.isPhysReg && (src.isPhysReg || src.isStackSlot || src.isGlobal);
		context.assertf(valid, "Invalid load %s -> %s", src.kind, dst.kind);

		switch(src.kind) with(IrValueKind) {
			case physicalRegister: doMemToReg(dst, memAddrBase(indexToRegister(src)), argSize); break;
			case stackSlot: doMemToReg(dst, localVarMemAddress(src), argSize); break;
 			case global:
				doMemToReg(dst, memAddrRipDisp32(0), argSize);
				addRefTo(src);
				break;

			default:
				context.internal_error("invalid source of load %s", src.kind);
		}
	}

	// dst must be of pointer type
	// dst is pointer of unknown type (that's why we need explicit argType)
	void genStore(IrIndex dst, IrIndex src, IrArgSize argSize)
	{
		context.assertf(!src.isGlobal,
			"store %s <- %s, must go through intermediate register",
			dst.kind, src.kind);

		void doRegToMem(MemAddress dstMem) {
			if (src.physRegClass == AMD64_REG_CLASS.XMM) {
				Register srcReg = indexToRegister(src);
				final switch(argSize) with(IrArgSize) {
					case size32: gen.movd_rx(dstMem, srcReg); break;
					case size64: gen.movq_rx(dstMem, srcReg); break;
					case size128: gen.movups(dstMem, srcReg); break;
					case size8, size16, size256, size512: context.internal_error("doRegToMem %s", argSize);
				}
			} else {
				Register srcReg = indexToRegister(src);
				gen.mov(dstMem, srcReg, cast(ArgType)argSize);
			}
		}
		void doConToMem(MemAddress dstMem, IrConstant con) {
			final switch(argSize) with(IrArgSize) {
				case size8: gen.movb(dstMem, Imm8(con.i8)); break;
				case size16: gen.movw(dstMem, Imm16(con.i16)); break;
				case size32: gen.movd(dstMem, Imm32(con.i32)); break;
				case size64:
					context.assertf(con.intFitsIn32Bits, "Constant 0x%X is too big", con.i64);
					gen.movq(dstMem, Imm32(con.i32));
					break;
				case size128, size256, size512: context.internal_error("doConToMem %s", argSize);
			}
		}

		MoveType moveType = calcMoveType(dst.kind, src.kind);
		switch (moveType) with(MoveType)
		{
			case const_to_stack:
				IrConstant con = context.constants.get(src);
				MemAddress dstMem = localVarMemAddress(dst);
				doConToMem(dstMem, con);
				break;
			case const_to_reg:
				IrConstant con = context.constants.get(src);
				Register dstReg = indexToRegister(dst);
				MemAddress dstMem = memAddrBase(dstReg);
				doConToMem(dstMem, con);
				break;
			case reg_to_stack:
				MemAddress dstMem = localVarMemAddress(dst);
				doRegToMem(dstMem);
				break;
			case reg_to_reg:
				Register dstReg = indexToRegister(dst);
				MemAddress dstMem = memAddrBase(dstReg);
				doRegToMem(dstMem);
				break;
			case const_to_global:
				IrConstant con = context.constants.get(src);
				MemAddress dstMem = memAddrRipDisp32(0);
				doConToMem(dstMem, con);
				addRefTo(dst, 8);
				break;
			case reg_to_global:
				MemAddress dstMem = memAddrRipDisp32(0);
				doRegToMem(dstMem);
				addRefTo(dst);
				break;
			default:
				context.internal_error("store %s <- %s is not implemented", dst.kind, src.kind);
		}
	}
}

MoveType calcMoveType(IrValueKind dst, IrValueKind src)
{
	switch(dst) with(IrValueKind) {
		case none, array, constant: return MoveType.invalid;
		case virtualRegister: return MoveType.invalid;
		case physicalRegister:
			switch(src) with(IrValueKind) {
				case constant, constantZero: return MoveType.const_to_reg;
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
