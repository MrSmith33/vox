/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module vox.be.ir_to_lir_amd64;

import std.stdio;
import vox.all;


void pass_ir_to_lir_amd64(CompilationContext* context, IrBuilder* builder, ModuleDeclNode* mod, FunctionDeclNode* func)
{
	assert(!func.isExternal);

	func.backendData.lirData = context.appendAst!IrFunction;
	IrFunction* lirData = context.getAst!IrFunction(func.backendData.lirData);

	mod.lirModule.addFunction(context, lirData);
	IrFunction* irData = context.getAst!IrFunction(func.backendData.loweredIrData);

	lirData.name = irData.name;

	builder.beginLir(lirData, irData, context);
	processFunc(context, builder, mod, irData, lirData);
	builder.finalizeIr;

	if (context.validateIr) validateIrFunction(context, lirData, "IR -> LIR");
	if (context.printLir && context.printDumpOf(func)) dumpFunction(context, lirData, "IR -> LIR");
}

void processFunc(CompilationContext* context, IrBuilder* builder, ModuleDeclNode* mod, IrFunction* ir, IrFunction* lir)
{
	string funName = context.idString(ir.name);
	//writefln("IR to LIR %s", funName);
	lir.instructionSet = IrInstructionSet.lir_amd64;

	// Mirror of original IR, we will put the new IrIndex of copied entities there
	// and later use this info to rewire all connections between basic blocks
	IrMirror!IrIndex mirror;
	mirror.createVirtRegMirror(context, ir);

	enum MAX_ARGS = 16; // only used for physical registers in calls
	IrIndex[MAX_ARGS] argBuffer = void;

	lir.type = ir.type;

	// copy stack slots
	lir.stackSlotPtr = ir.stackSlotPtr;
	lir.numStackSlots = ir.numStackSlots;
	dupSingleIrStorage(context.irStorage.stackSlotBuffer, lir.stackSlotPtr, lir.numStackSlots);

	// save map from old index to new index
	void recordIndex(IrIndex oldIndex, IrIndex newIndex)
	{
		//writefln("rec %s -> %s", oldIndex, newIndex);
		assert(oldIndex.isDefined);
		assert(newIndex.isDefined);
		assert(oldIndex.isVirtReg);
		mirror[oldIndex] = newIndex;
	}

	IrIndex getFixedIndex(IrIndex index)
	{
		assert(index.isDefined);
		assert(!(index.isBasicBlock || index.isPhi || index.isInstruction), format("%s", index));
		if (index.isVirtReg) {
			return mirror[index];
		}
		return index;
	}

	void fixIndex(ref IrIndex index)
	{
		assert(index.isDefined);
		assert(!(index.isBasicBlock || index.isPhi || index.isInstruction));
		if (index.isVirtReg) {
			//writefln("%s -> %s", index, mirror[index]);
			assert(mirror[index].isDefined);
			index = mirror[index];
		}
	}

	// Fixes virtual registers
	// Legalizes complex arguments into a separate instruction
	IrIndex getFixedLegalIndex(IrIndex index, IrIndex lirBlockIndex)
	{
		final switch (index.kind) with(IrValueKind) {
			case none, array, instruction, basicBlock, phi, type, variable: context.internal_error("getFixedLegalIndex %s", index.kind);
			case physicalRegister:
			case constantAggregate:
			case constantZero:
				return index;
			case constant:
				final switch(index.constantKind) with(IrConstantKind) {
					case smallZx, smallSx: return index;
					case big:
						auto con = context.constants.get(index);
						if (con.type.isTypeFloat) break;
						if (con.type.isTypeInteger && !con.intFitsIn32Bits) break;
						return index;
				}
				goto case; // big constant
			case global:
			case stackSlot:
			case func:
				// copy to temp register
				IrIndex type = ir.getValueType(context, index);
				ExtraInstrArgs extra = { type : type };
				return builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, index).result;

			case virtualRegister:
				return mirror[index];
		}
	}

	void makeMov(IrIndex to, IrIndex from, IrArgSize argSize, IrIndex block) {
		ExtraInstrArgs extra = { addUsers : false, result : to, argSize : argSize };
		builder.emitInstr!(Amd64Opcode.mov)(block, extra, from);
	}

	IrIndex genAddressOffset(IrIndex lirPtr, uint offset, IrIndex ptrType, IrIndex lirBlockIndex) {
		IrIndex ptr;
		if (offset == 0) {
			ExtraInstrArgs extra = { addUsers : false, type : ptrType };
			InstrWithResult movInstr = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, lirPtr);
			ptr = movInstr.result;
		} else {
			IrIndex offsetIndex = context.constants.add(makeIrType(IrBasicType.i32), offset);
			ExtraInstrArgs extra = { addUsers : false, type : ptrType };
			InstrWithResult addressInstr = builder.emitInstr!(Amd64Opcode.add)(lirBlockIndex, extra, lirPtr, offsetIndex);
			ptr = addressInstr.result;
		}
		return ptr;
	}

	IrIndex genLoad(IrIndex lirPtr, uint offset, IrIndex ptrType, IrIndex lirBlockIndex) {
		IrIndex ptr = genAddressOffset(lirPtr, offset, ptrType, lirBlockIndex);
		IrIndex valType = context.types.getPointerBaseType(ptrType);
		IrArgSize argSize = typeToIrArgSize(valType, context);
		ExtraInstrArgs extra = { addUsers : false, type : valType, argSize : argSize };
		InstrWithResult instr = builder.emitInstr!(Amd64Opcode.load)(lirBlockIndex, extra, ptr);
		return instr.result;
	}

	void genMemZero(IrIndex lirPtr, ulong numBytes, IrIndex lirBlockIndex) {
		// generate rep stos rax, rcx, rdi
		IrIndex dataReg = IrIndex(amd64_reg.ax, IrArgSize.size64);
		IrIndex sizeReg = IrIndex(amd64_reg.cx, IrArgSize.size64);
		IrIndex ptrReg = IrIndex(amd64_reg.di, IrArgSize.size64);
		// data
		makeMov(dataReg, context.constants.addZeroConstant(makeIrType(IrBasicType.i64)), IrArgSize.size64, lirBlockIndex);
		// size
		makeMov(sizeReg, context.constants.add(makeIrType(IrBasicType.i64), numBytes), IrArgSize.size64, lirBlockIndex);
		// ptr
		makeMov(ptrReg, lirPtr, IrArgSize.size64, lirBlockIndex);

		ExtraInstrArgs extra = { addUsers : false };
		builder.emitInstr!(Amd64Opcode.rep_stos)(lirBlockIndex, extra, dataReg, sizeReg, ptrReg);
	}

	// fromOffset is used when irValue is pointer that needs deferencing
	void genStore(IrIndex lirPtr, uint offset, IrIndex irValue, uint fromOffset, IrIndex valueType, IrIndex lirBlockIndex, IrFunction* ir)
	{
		//writefln("genStore %s %s %s %s %s", lirPtr, offset, irValue, fromOffset, IrIndexDump(valueType, context, ir));
		IrIndex dstType = getValueType(lirPtr, lir, context);
		IrIndex srcType = getValueType(irValue, ir, context);
		//writefln("  %s %s <- %s %s", IrTypeDump(dstType, *context), dstType, IrTypeDump(srcType, *context), srcType);
		context.assertf(dstType.isTypePointer, "%s", IrIndexDump(dstType, context, lir));
		IrIndex ptrType = context.types.appendPtr(valueType);

		switch(valueType.typeKind) with(IrTypeKind) {
			case basic, pointer:
				IrArgSize argSize = typeToIrArgSize(valueType, context);

				// check if irValue is l-value
				if (context.types.isSameType(dstType, srcType)) {
					// load from irValue ptr. loadedVal is already fixed
					IrIndex loadedVal = genLoad(getFixedIndex(irValue), fromOffset, ptrType, lirBlockIndex);

					IrIndex ptr = genAddressOffset(lirPtr, offset, ptrType, lirBlockIndex);
					ExtraInstrArgs extra = { addUsers : false, argSize : argSize };
					builder.emitInstr!(Amd64Opcode.store)(lirBlockIndex, extra, ptr, loadedVal);
					break;
				}

				IrIndex rvalue = getFixedLegalIndex(irValue, lirBlockIndex);
				ExtraInstrArgs extra = { addUsers : false, argSize : argSize };
				IrIndex ptr = genAddressOffset(lirPtr, offset, ptrType, lirBlockIndex);
				builder.emitInstr!(Amd64Opcode.store)(lirBlockIndex, extra, ptr, rvalue);
				break;

			case struct_t:
				IrTypeStruct* structType = &context.types.get!IrTypeStruct(valueType);
				IrIndex[] members;

				switch(irValue.kind) with(IrValueKind)
				{
					case virtualRegister:
						IrIndex origin = ir.getVirtReg(irValue).definition;
						IrInstrHeader* instr = ir.getInstr(origin);

						switch (instr.op)
						{
							case IrOpcode.load_aggregate, IrOpcode.load:
								foreach (i, IrTypeStructMember member; structType.members)
								{
									IrIndex ptr = genAddressOffset(lirPtr, offset + member.offset, ptrType, lirBlockIndex);
									genStore(ptr, 0, instr.arg(ir, 0), fromOffset + member.offset, member.type, lirBlockIndex, ir);
								}
								return;

							case IrOpcode.create_aggregate:
								members = instr.args(ir);
								break;

							case IrOpcode.call:
								// register fixup for return stack slot
								IrIndex resultSlot = getFixedIndex(irValue);
								//context.internal_error("call . todo %s", resultSlot);
								foreach (i, IrTypeStructMember member; structType.members)
								{
									IrIndex ptr = genAddressOffset(lirPtr, offset + member.offset, ptrType, lirBlockIndex);
									genStore(ptr, 0, resultSlot, fromOffset + member.offset, member.type, lirBlockIndex, ir);
								}
								return;

							default:
								// value fits into register, store as is
								IrIndex rvalue = getFixedIndex(irValue);
								IrArgSize argSize = typeToIrArgSize(valueType, context);
								ExtraInstrArgs extra = { addUsers : false, argSize : argSize };
								IrIndex ptr = genAddressOffset(lirPtr, offset, ptrType, lirBlockIndex);
								builder.emitInstr!(Amd64Opcode.store)(lirBlockIndex, extra, ptr, rvalue);
								return;
						}
						break;

					case constantZero:
						IrIndex ptr = genAddressOffset(lirPtr, offset, context.i8PtrType, lirBlockIndex);
						genMemZero(ptr, structType.sizealign.size, lirBlockIndex);
						return;

					case constantAggregate:
						members = context.constants.getAggregate(irValue).members;
						break;

					default: context.internal_error("%s", irValue.kind);
				}

				context.assertf(members.length == structType.numMembers, "%s != %s", members.length, structType.numMembers);

				foreach (i, IrTypeStructMember member; structType.members)
				{
					genStore(lirPtr, offset + member.offset, members[i], fromOffset, member.type, lirBlockIndex, ir);
				}
				break;

			case array:
				IrTypeArray* arrayType = &context.types.get!IrTypeArray(valueType);
				uint elemSize = context.types.typeSize(arrayType.elemType);
				IrIndex[] members;

				switch(irValue.kind) with(IrValueKind)
				{
					case virtualRegister:
						IrIndex origin = ir.getVirtReg(irValue).definition;
						IrInstrHeader* instr = ir.getInstr(origin);

						switch (instr.op)
						{
							case IrOpcode.load_aggregate, IrOpcode.load:
								foreach (i; 0..arrayType.numElements)
								{
									IrIndex ptr = genAddressOffset(lirPtr, offset + elemSize * i, ptrType, lirBlockIndex);
									genStore(ptr, 0, instr.arg(ir, 0), fromOffset + elemSize * i, arrayType.elemType, lirBlockIndex, ir);
								}
								return;

							case IrOpcode.create_aggregate:
								members = instr.args(ir);
								break;

							case IrOpcode.call:
								// register fixup for return stack slot
								IrIndex resultSlot = getFixedIndex(irValue);
								//context.internal_error("call . todo %s", resultSlot);
								foreach (i; 0..arrayType.numElements)
								{
									IrIndex ptr = genAddressOffset(lirPtr, offset + elemSize * i, ptrType, lirBlockIndex);
									genStore(ptr, 0, resultSlot, fromOffset + elemSize * i, arrayType.elemType, lirBlockIndex, ir);
								}
								return;

							default:
								context.internal_error("%s", cast(IrOpcode)instr.op);
						}
						break;

					case constantZero:
						IrIndex ptr = genAddressOffset(lirPtr, offset, context.i8PtrType, lirBlockIndex);
						genMemZero(ptr, elemSize * arrayType.numElements, lirBlockIndex);
						return;

					case constantAggregate:
						members = context.constants.getAggregate(irValue).members;
						break;

					default: context.internal_error("%s", irValue.kind);
				}

				context.assertf(members.length == arrayType.numElements, "%s != %s", members.length, arrayType.numElements);

				foreach (i; 0..arrayType.numElements)
				{
					genStore(lirPtr, offset + elemSize * i, members[i], fromOffset, arrayType.elemType, lirBlockIndex, ir);
				}
				break;

			default:
				context.internal_error("%s", valueType.typeKind);
		}
	}

	// copy single integer
	void genCopyInt(IrIndex dst, IrIndex src, IrIndex valType, IrArgSize argSize, IrIndex lirBlockIndex)
	{
		ExtraInstrArgs loadExtra = { addUsers : false, type : valType, argSize : argSize };
		InstrWithResult loadInstr = builder.emitInstr!(Amd64Opcode.load)(lirBlockIndex, loadExtra, src);
		ExtraInstrArgs storeExtra = { addUsers : false, argSize : argSize };
		IrIndex storeInstr = builder.emitInstr!(Amd64Opcode.store)(lirBlockIndex, storeExtra, dst, loadInstr.result);
	}

	// copy single item of any type
	void genCopy(IrIndex dst, IrIndex src, IrIndex lirBlockIndex)
	{
		//writefln("genCopy %s %s", dst, src);
		IrIndex ptrType = getValueType(dst, lir, context);
		//writefln("ptrType %s %s", ptrType, getValueType(src, lir, context));
		IrIndex valType = context.types.getPointerBaseType(ptrType);
		uint typeSize = context.types.typeSize(valType);

		uint offset = 0;
		IrIndex elemType = makeIrType(IrBasicType.i64); // i64, i32, i16, i8
		IrArgSize elemArgSize = IrArgSize.size64; // size64, size32, size16, size8
		uint elemSize = 8; // 8, 4, 2, 1
		while (elemSize > 0)
		{
			IrIndex elemPtrType = context.types.appendPtr(elemType);
			while (typeSize >= elemSize)
			{
				IrIndex ptrFrom = genAddressOffset(src, offset, elemPtrType, lirBlockIndex);
				IrIndex ptrTo = genAddressOffset(dst, offset, elemPtrType, lirBlockIndex);
				//writefln("from %s to %s", ptrFrom, ptrTo);
				genCopyInt(ptrTo, ptrFrom, elemType, elemArgSize, lirBlockIndex);

				offset += elemSize;
				typeSize -= elemSize;
			}
			elemSize /= 2;
			elemType.typeIndex = elemType.typeIndex - 1;
			--elemArgSize;
		}
	}

	// dup basic blocks
	// old and new blocks have the same indices
	foreach (size_t i, ref IrBasicBlock irBlock; ir.blocksArray)
	{
		IrIndex blockIndex = IrIndex(cast(uint)i, IrValueKind.basicBlock);
		IrIndex lirBlock = builder.appendBasicBlockSlot;

		lir.getBlock(lirBlock).isLoopHeader = irBlock.isLoopHeader;

		foreach(IrIndex pred; irBlock.predecessors.range(ir)) {
			lir.getBlock(lirBlock).predecessors.append(builder, pred);
		}
		foreach(IrIndex succ; irBlock.successors.range(ir)) {
			lir.getBlock(lirBlock).successors.append(builder, succ);
		}

		lir.getBlock(lirBlock).prevBlock = irBlock.prevBlock;
		lir.getBlock(lirBlock).nextBlock = irBlock.nextBlock;

		// Add phis with old args
		foreach(IrIndex phiIndex, ref IrPhi phi; irBlock.phis(ir))
		{
			IrVirtualRegister* oldReg = ir.getVirtReg(phi.result);
			IrIndex newPhi = builder.addPhi(lirBlock, oldReg.type, phi.var);
			IrIndex newResult = lir.getPhi(newPhi).result;
			IrVirtualRegister* newReg = lir.getVirtReg(newResult);
			newReg.type = oldReg.type;

			recordIndex(phi.result, lir.getPhi(newPhi).result);
			foreach(size_t arg_i, ref IrIndex phiArg; phi.args(ir))
			{
				builder.addPhiArg(newPhi, phiArg);
			}
		}
	}

	//writefln("%s", IrIndexDump(lir.type, context, lir));

	// fix successors predecessors links
	foreach (IrIndex lirBlockIndex, ref IrBasicBlock lirBlock; lir.blocks)
	{
		lirBlock.isSealed = true;
		lirBlock.isFinished = true;

		// old and new blocks have the same indices
		IrIndex irBlockIndex = lirBlockIndex;

		// Add instructions with old args
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; ir.getBlock(irBlockIndex).instructions(ir))
		{
			void emitLirInstr(alias I)()
			{
				static assert(!getInstrInfo!I.hasVariadicArgs);
				static assert(!getInstrInfo!I.hasVariadicResult);

				IrIndex[getInstrInfo!I.numArgs] fixedArgs = instrHeader.args(ir);
				foreach(ref arg; fixedArgs) arg = getFixedLegalIndex(arg, lirBlockIndex);

				static if (getInstrInfo!I.hasResult)
				{
					ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize };
					IrIndex result = instrHeader.result(ir);
					if (result.isVirtReg)
						extra.type = ir.getVirtReg(result).type;
					InstrWithResult res = builder.emitInstr!I(lirBlockIndex, extra, fixedArgs);
					recordIndex(result, res.result);
				}
				else
				{
					ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize };
					IrIndex res = builder.emitInstr!I(lirBlockIndex, extra, fixedArgs);
				}
			}

			// for movsx/movzx
			// arg is not fixed
			IrIndex emit_nonfinal(alias I)(IrIndex resType, IrIndex arg) {
				arg = getFixedLegalIndex(arg, lirBlockIndex);
				ExtraInstrArgs extra = { addUsers : false, type : resType };
				return builder.emitInstr!I(lirBlockIndex, extra, arg).result;
			}

			IrIndex legalizeFloatArg(IrIndex arg, IrIndex type) {
				switch (arg.kind) {
					case IrValueKind.constant:
						IrIndex globalIndex = context.globals.add();
						IrGlobal* global = context.globals.get(globalIndex);
						global.type = context.types.appendPtr(type);

						ObjectSymbol sym = {
							kind : ObjectSymbolKind.isLocal,
							sectionIndex : context.builtinSections[ObjectSectionType.ro_data],
							moduleIndex : mod.objectSymIndex,
							flags : ObjectSymbolFlags.isFloat,
							id : context.idMap.getOrReg(context, ":float"),
						};
						global.objectSymIndex = context.objSymTab.addSymbol(sym);

						ObjectSymbol* globalSym = context.objSymTab.getSymbol(global.objectSymIndex);

						SizeAndAlignment valueSizealign = context.types.typeSizeAndAlignment(type);
						ubyte[] buffer = context.globals.allocateInitializer(valueSizealign.size);
						constantToMem(buffer, arg, context);
						//writefln("%s %s", arg, buffer);
						globalSym.setInitializer(buffer);

						ExtraInstrArgs extra = { addUsers : false, type : type, argSize : instrHeader.argSize };
						return builder.emitInstr!(Amd64Opcode.load)(lirBlockIndex, extra, globalIndex).result;

					case IrValueKind.constantZero:
						ExtraInstrArgs extra = { addUsers : false, type : type, argSize : instrHeader.argSize };
						return builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, arg).result;

					default: return getFixedLegalIndex(arg, lirBlockIndex);
				}
			}

			// args are fixed
			void emit_final(alias I)(IrIndex resType, IrIndex[] args...) {
				ExtraInstrArgs extra = { addUsers : false, type : resType, argSize : instrHeader.argSize };
				InstrWithResult res = builder.emitInstr!I(lirBlockIndex, extra, args);
				IrIndex result = instrHeader.result(ir);
				recordIndex(result, res.result);
			}

			switch_instr:
			switch(instrHeader.op)
			{
				case IrOpcode.call:
					lir.numCalls += 1;
					foreach(i; 0..instrHeader.numArgs) {
						argBuffer[i] = getFixedIndex(instrHeader.arg(ir, i));
					}
					IrIndex returnReg;
					if (instrHeader.hasResult) returnReg = instrHeader.result(ir);
					ExtraInstrArgs callExtra = {
						addUsers : false,
						hasResult : instrHeader.hasResult,
						result : returnReg // will be used if function has result
					};
					InstrWithResult callInstr = builder.emitInstr!(Amd64Opcode.call)(
						lirBlockIndex, callExtra, argBuffer[0..instrHeader.numArgs]);
					lir.getInstr(callInstr.instruction).extendFixedArgRange = instrHeader.extendFixedArgRange;
					lir.getInstr(callInstr.instruction).extendFixedResultRange = instrHeader.extendFixedResultRange;
					break;

				case IrOpcode.syscall:
					lir.numCalls += 1;
					foreach(i; 0..instrHeader.numArgs) {
						argBuffer[i] = getFixedIndex(instrHeader.arg(ir, i));
					}
					IrIndex returnReg;
					if (instrHeader.hasResult) returnReg = instrHeader.result(ir);
					ExtraInstrArgs callExtra = {
						addUsers : false,
						hasResult : instrHeader.hasResult,
						result : returnReg // will be used if function has result
					};
					InstrWithResult callInstr = builder.emitInstr!(Amd64Opcode.syscall)(
						lirBlockIndex, callExtra, argBuffer[0..instrHeader.numArgs]);
					lir.getInstr(callInstr.instruction).extendFixedArgRange = instrHeader.extendFixedArgRange;
					lir.getInstr(callInstr.instruction).extendFixedResultRange = instrHeader.extendFixedResultRange;
					break;

				case IrOpcode.add: emitLirInstr!(Amd64Opcode.add); break;
				case IrOpcode.sub: emitLirInstr!(Amd64Opcode.sub); break;
				case IrOpcode.umul, IrOpcode.smul: emitLirInstr!(Amd64Opcode.imul); break;
				case IrOpcode.not: emitLirInstr!(Amd64Opcode.not); break;
				case IrOpcode.neg: emitLirInstr!(Amd64Opcode.neg); break;
				case IrOpcode.fneg: emitLirInstr!(Amd64Opcode.fneg); break;
				case IrOpcode.and: emitLirInstr!(Amd64Opcode.and); break;
				case IrOpcode.or: emitLirInstr!(Amd64Opcode.or); break;
				case IrOpcode.xor: emitLirInstr!(Amd64Opcode.xor); break;

				case IrOpcode.fadd:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					emit_final!(Amd64Opcode.fadd)(type, legalizeFloatArg(instrHeader.arg(ir, 0), type), legalizeFloatArg(instrHeader.arg(ir, 1), type));
					break;
				case IrOpcode.fsub:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					emit_final!(Amd64Opcode.fsub)(type, legalizeFloatArg(instrHeader.arg(ir, 0), type), legalizeFloatArg(instrHeader.arg(ir, 1), type));
					break;
				case IrOpcode.fmul:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					emit_final!(Amd64Opcode.fmul)(type, legalizeFloatArg(instrHeader.arg(ir, 0), type), legalizeFloatArg(instrHeader.arg(ir, 1), type));
					break;
				case IrOpcode.fdiv:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					emit_final!(Amd64Opcode.fdiv)(type, legalizeFloatArg(instrHeader.arg(ir, 0), type), legalizeFloatArg(instrHeader.arg(ir, 1), type));
					break;

				case IrOpcode.udiv, IrOpcode.sdiv, IrOpcode.urem, IrOpcode.srem:
					//   v1 = div v2, v3
					// is converted into:
					//   mov ax, v2
					//   zx/sx dx:ax
					//   ax = div ax, dx, v3
					//   mov v1, ax
					// since 8 bit division doesn't follow dx:ax pattern and uses ax instead, we use bigger division there

					bool isSigned = instrHeader.op == IrOpcode.sdiv || instrHeader.op == IrOpcode.srem;
					bool isDivision = instrHeader.op == IrOpcode.udiv || instrHeader.op == IrOpcode.sdiv;

					IrIndex dividendBottom;

					IrArgSize argSize = instrHeader.argSize;
					switch(argSize) with(IrArgSize)
					{
						case size8:
							// we transform 8 bit div/rem into 16 bit, so we don't need to deal with ah register
							argSize = size16;
							dividendBottom = IrIndex(amd64_reg.ax, argSize);
							ExtraInstrArgs extra = { addUsers : false, result : dividendBottom };
							if (isSigned) builder.emitInstr!(Amd64Opcode.movsx_btod)(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)));
							else builder.emitInstr!(Amd64Opcode.movzx_btod)(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)));
							break;
						case size16:
						case size32:
						case size64:
							// copy bottom half of dividend
							dividendBottom = IrIndex(amd64_reg.ax, argSize);
							makeMov(dividendBottom, getFixedIndex(instrHeader.arg(ir, 0)), argSize, lirBlockIndex);
							break;
						default: context.internal_error("%s:%s: Invalid target size %s", funName, instrIndex, argSize);
					}


					// divisor must be in register
					IrIndex divisor = instrHeader.arg(ir, 1);
					if (divisor.isSimpleConstant) {
						auto con = context.constants.get(divisor);
						if (instrHeader.argSize == IrArgSize.size8) {
							divisor = context.constants.add(makeIrType(IrBasicType.i16), con.i16);
						}
						ExtraInstrArgs extra = { addUsers : false, type : getValueType(divisor, ir, context) };
						divisor = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, divisor).result;
					}
					else
					{
						fixIndex(divisor);
						if (instrHeader.argSize == IrArgSize.size8) {
							ExtraInstrArgs extra = { addUsers : false, type : makeIrType(IrBasicType.i16) };
							if (isSigned) divisor = builder.emitInstr!(Amd64Opcode.movsx_btod)(lirBlockIndex, extra, divisor).result;
							else divisor = builder.emitInstr!(Amd64Opcode.movzx_btod)(lirBlockIndex, extra, divisor).result;
						}
					}

					IrIndex dividendTop = IrIndex(amd64_reg.dx, argSize);

					if (isSigned) {
						IrIndex divsxResult = IrIndex(amd64_reg.dx, argSize);
						// sign-extend top half of dividend
						ExtraInstrArgs extra2 = { argSize : argSize, result : divsxResult };
						builder.emitInstr!(Amd64Opcode.divsx)(lirBlockIndex, extra2);
					} else {
						// zero top half of dividend
						makeMov(dividendTop, context.constants.addZeroConstant(makeIrType(IrBasicType.i32)), IrArgSize.size32, lirBlockIndex);
					}

					// choose result
					IrIndex resultReg = dividendTop; // remainder
					if (isDivision) {
						resultReg = dividendBottom; // dividend
					}

					// divide
					ExtraInstrArgs extra3 = { addUsers : false, argSize : argSize, result : resultReg };
					InstrWithResult res;
					if (isSigned)
						res = builder.emitInstr!(Amd64Opcode.idiv)(lirBlockIndex, extra3, dividendBottom, dividendTop, divisor);
					else
						res = builder.emitInstr!(Amd64Opcode.div)(lirBlockIndex, extra3, dividendBottom, dividendTop, divisor);

					// fix size for size8
					resultReg.physRegSize = instrHeader.argSize;

					// copy result (quotient) with truncation in case of 8/16 bits
					ExtraInstrArgs extra4 = { addUsers : false, argSize : instrHeader.argSize, type : ir.getVirtReg(instrHeader.result(ir)).type };
					InstrWithResult movResult = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra4, resultReg);
					recordIndex(instrHeader.result(ir), movResult.result);
					break;

				case IrOpcode.shl, IrOpcode.lshr, IrOpcode.ashr:
					IrIndex rightArg;
					if (instrHeader.arg(ir, 1).isSimpleConstant)
						rightArg = instrHeader.arg(ir, 1);
					else
					{
						rightArg = IrIndex(amd64_reg.cx, IrArgSize.size8);
						makeMov(rightArg, getFixedIndex(instrHeader.arg(ir, 1)), instrHeader.argSize, lirBlockIndex);
					}
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize, type : type };
					InstrWithResult res;
					switch(instrHeader.op) {
						case IrOpcode.shl:
							res = builder.emitInstr!(Amd64Opcode.shl)(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), rightArg);
							break;
						case IrOpcode.lshr:
							res = builder.emitInstr!(Amd64Opcode.shr)(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), rightArg);
							break;
						case IrOpcode.ashr:
							res = builder.emitInstr!(Amd64Opcode.sar)(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), rightArg);
							break;
						default: assert(false);
					}
					recordIndex(instrHeader.result(ir), res.result);
					break;
				case IrOpcode.grow_stack:
					IrIndex stackPtrReg = IrIndex(lir.getCallConv(context).stackPointer, IrArgSize.size64);
					ExtraInstrArgs extra = { addUsers : false, result : stackPtrReg };
					builder.emitInstr!(Amd64Opcode.sub)(
						lirBlockIndex, extra, stackPtrReg, getFixedIndex(instrHeader.arg(ir, 0)));
					break;
				case IrOpcode.shrink_stack:
					IrIndex stackPtrReg = IrIndex(lir.getCallConv(context).stackPointer, IrArgSize.size64);
					ExtraInstrArgs extra = { addUsers : false, result : stackPtrReg };
					builder.emitInstr!(Amd64Opcode.add)(
						lirBlockIndex, extra, stackPtrReg, getFixedIndex(instrHeader.arg(ir, 0)));
					break;
				case IrOpcode.push:
					IrIndex fixedArg = getFixedLegalIndex(instrHeader.arg(ir, 0), lirBlockIndex);
					ExtraInstrArgs extra = { addUsers : false };
					builder.emitInstr!(Amd64Opcode.push)(lirBlockIndex, extra, fixedArg);
					break;
				case IrOpcode.move:
					if (instrHeader.result(ir).isVirtReg)
						emitLirInstr!(Amd64Opcode.mov);
					else
						makeMov(instrHeader.result(ir), getFixedIndex(instrHeader.arg(ir, 0)), instrHeader.argSize, lirBlockIndex);
					break;
				case IrOpcode.copy:
					genCopy(getFixedIndex(instrHeader.arg(ir, 0)), getFixedIndex(instrHeader.arg(ir, 1)), lirBlockIndex);
					break;
				case IrOpcode.load: emitLirInstr!(Amd64Opcode.load); break;
				case IrOpcode.store:
					IrIndex srcType = ir.getValueType(context, instrHeader.arg(ir, 1));
					genStore(getFixedIndex(instrHeader.arg(ir, 0)), 0, instrHeader.arg(ir, 1), 0, srcType, lirBlockIndex, ir);
					break;
				case IrOpcode.conv:
					// Incomplete implementation
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);
					context.assertf(typeSizeTo <= typeSizeFrom,
						"Can't cast from %s bytes to %s bytes", typeSizeFrom, typeSizeTo);
					emitLirInstr!(Amd64Opcode.mov);
					break;
				case IrOpcode.sext:
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrArgSize sizeFrom = getTypeArgSize(typeFrom, context);
					IrArgSize sizeTo = instrHeader.argSize;
					final switch(sizeFrom) with(IrArgSize)
					{
						case size8:
							switch(sizeTo) with(IrArgSize)
							{
								case size16: emitLirInstr!(Amd64Opcode.movsx_btow); break;
								case size32: emitLirInstr!(Amd64Opcode.movsx_btod); break;
								case size64: emitLirInstr!(Amd64Opcode.movsx_btoq); break;
								default: context.internal_error("%s:%s: Invalid target size %s", funName, instrIndex, sizeTo);
							}
							break;
						case size16:
							switch(sizeTo) with(IrArgSize)
							{
								case size32: emitLirInstr!(Amd64Opcode.movsx_wtod); break;
								case size64: emitLirInstr!(Amd64Opcode.movsx_wtoq); break;
								default: context.internal_error("%s:%s: Invalid target size %s", funName, instrIndex, sizeTo);
							}
							break;
						case size32:
							context.assertf(sizeTo == size64, "%s:%s: Invalid target size %s", funName, instrIndex, sizeTo);
							emitLirInstr!(Amd64Opcode.movsx_dtoq);
							break;
						case size64, size128, size256, size512: context.internal_error("%s:%s: Invalid source size %s", funName, instrIndex, sizeFrom);
					}
					break;
				case IrOpcode.zext:
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrArgSize sizeFrom = getTypeArgSize(typeFrom, context);
					IrArgSize sizeTo = instrHeader.argSize;
					final switch(sizeFrom) with(IrArgSize)
					{
						case size8:
							switch(sizeTo) with(IrArgSize)
							{
								case size16: emitLirInstr!(Amd64Opcode.movzx_btow); break;
								case size32: emitLirInstr!(Amd64Opcode.movzx_btod); break;
								case size64: emitLirInstr!(Amd64Opcode.movzx_btoq); break; // TODO use movzx_btod
								default: context.internal_error("%s:%s: Invalid target size %s", funName, instrIndex, sizeTo);
							}
							break;
						case size16:
							switch(sizeTo) with(IrArgSize)
							{
								case size32: emitLirInstr!(Amd64Opcode.movzx_wtod); break;
								case size64: emitLirInstr!(Amd64Opcode.movzx_wtoq); break; // TODO use movzx_wtod
								default: context.internal_error("%s:%s: Invalid target size %s", funName, instrIndex, sizeTo);
							}
							break;
						case size32:
							context.assertf(sizeTo == size64, "Invalid target size %s", sizeTo);
							emitLirInstr!(Amd64Opcode.mov);
							break;
						case size64, size128, size256, size512: context.internal_error("%s:%s: Invalid source size %s", funName, instrIndex, sizeFrom);
					}
					break;
				case IrOpcode.trunc:
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);
					context.assertf(typeSizeTo < typeSizeFrom,
						"Can't cast from %s bytes to %s bytes", typeSizeFrom, typeSizeTo);
					emitLirInstr!(Amd64Opcode.mov);
					break;

				case IrOpcode.fpext:
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);
					context.assertf(typeSizeTo > typeSizeFrom,
						"Can't cast from %s bytes to %s bytes", typeSizeFrom, typeSizeTo);
					emitLirInstr!(Amd64Opcode.f32_to_f64);
					break;

				case IrOpcode.fptrunc:
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);
					context.assertf(typeSizeTo < typeSizeFrom,
						"Can't cast from %s bytes to %s bytes", typeSizeFrom, typeSizeTo);
					emitLirInstr!(Amd64Opcode.f64_to_f32);
					break;

				case IrOpcode.uitofp:
					IrIndex arg0 = instrHeader.arg(ir, 0);
					IrIndex typeFrom = getValueType(arg0, ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);

					switch(typeSizeTo) {
					case 4:
						switch(typeSizeFrom) {
						case 1:
							IrIndex u32 = emit_nonfinal!(Amd64Opcode.movzx_btod)(makeIrType(IrBasicType.i32), arg0);
							emit_final!(Amd64Opcode.i32_to_f32)(typeTo, u32);
							break switch_instr;
						case 2:
							IrIndex u32 = emit_nonfinal!(Amd64Opcode.movzx_wtod)(makeIrType(IrBasicType.i32), arg0);
							emit_final!(Amd64Opcode.i32_to_f32)(typeTo, u32);
							break switch_instr;
						case 4:
							IrIndex i64 = emit_nonfinal!(Amd64Opcode.mov)(makeIrType(IrBasicType.i64), arg0);
							emit_final!(Amd64Opcode.i64_to_f32)(typeTo, i64);
							break switch_instr;
						case 8: emitLirInstr!(Amd64Opcode.i64_to_f32); break switch_instr; // u64 -> f32
						default: context.internal_error("Unexpected source type size %s", typeSizeFrom);
						}

					case 8:
						switch(typeSizeFrom) {
						case 1:
							IrIndex i32 = emit_nonfinal!(Amd64Opcode.movzx_btod)(makeIrType(IrBasicType.i32), arg0);
							emit_final!(Amd64Opcode.i32_to_f64)(typeTo, i32);
							break switch_instr;
						case 2:
							IrIndex i32 = emit_nonfinal!(Amd64Opcode.movzx_wtod)(makeIrType(IrBasicType.i32), arg0);
							emit_final!(Amd64Opcode.i32_to_f64)(typeTo, i32);
							break switch_instr;
						case 4:
							IrIndex i64 = emit_nonfinal!(Amd64Opcode.mov)(makeIrType(IrBasicType.i64), arg0);
							emit_final!(Amd64Opcode.i64_to_f64)(typeTo, i64);
							break switch_instr;
						case 8: emitLirInstr!(Amd64Opcode.i64_to_f64); break switch_instr; // u64 -> f64
						default: context.internal_error("Unexpected source type size %s", typeSizeFrom);
						}
					default: context.internal_error("Unexpected target type size %s", typeSizeTo);
					}

				case IrOpcode.sitofp:
					IrIndex arg0 = instrHeader.arg(ir, 0);
					IrIndex typeFrom = getValueType(arg0, ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);

					switch(typeSizeTo) {
					case 4:
						switch(typeSizeFrom) {
						case 1:
							IrIndex i32 = emit_nonfinal!(Amd64Opcode.movsx_btod)(makeIrType(IrBasicType.i32), arg0);
							emit_final!(Amd64Opcode.i32_to_f32)(typeTo, i32);
							break switch_instr;
						case 2:
							IrIndex i32 = emit_nonfinal!(Amd64Opcode.movsx_wtod)(makeIrType(IrBasicType.i32), arg0);
							emit_final!(Amd64Opcode.i32_to_f32)(typeTo, i32);
							break switch_instr;
						case 4: emitLirInstr!(Amd64Opcode.i32_to_f32); break switch_instr;
						case 8: emitLirInstr!(Amd64Opcode.i64_to_f32); break switch_instr;
						default: context.internal_error("Unexpected source type size %s", typeSizeFrom);
						}

					case 8:
						switch(typeSizeFrom) {
						case 1:
							IrIndex i32 = emit_nonfinal!(Amd64Opcode.movsx_btod)(makeIrType(IrBasicType.i32), arg0);
							emit_final!(Amd64Opcode.i32_to_f64)(typeTo, i32);
							break switch_instr;
						case 2:
							IrIndex i32 = emit_nonfinal!(Amd64Opcode.movsx_wtod)(makeIrType(IrBasicType.i32), arg0);
							emit_final!(Amd64Opcode.i32_to_f64)(typeTo, i32);
							break switch_instr;
						case 4: emitLirInstr!(Amd64Opcode.i32_to_f64); break switch_instr;
						case 8: emitLirInstr!(Amd64Opcode.i64_to_f64); break switch_instr;
						default: context.internal_error("Unexpected source type size %s", typeSizeFrom);
						}
					default: context.internal_error("Unexpected target type size %s", typeSizeTo);
					}

				case IrOpcode.fptoui:
					IrIndex arg0 = instrHeader.arg(ir, 0);
					IrIndex typeFrom = getValueType(arg0, ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);

					switch(typeSizeFrom) {
						case 4:
						switch(typeSizeTo) {
							case 1:
							case 2:
								IrIndex i32 = emit_nonfinal!(Amd64Opcode.f32_to_i32_trunc)(makeIrType(IrBasicType.i32), arg0);
								emit_final!(Amd64Opcode.mov)(typeTo, i32);
								break switch_instr;
							case 4: emitLirInstr!(Amd64Opcode.f32_to_i64_trunc); break switch_instr; // f32 -> u32
							case 8: emitLirInstr!(Amd64Opcode.f32_to_i64_trunc); break switch_instr; // f32 -> u64
							default: context.internal_error("Unexpected source type size %s", typeSizeTo);
						}

						case 8:
						switch(typeSizeTo) {
							case 1:
							case 2:
								IrIndex i32 = emit_nonfinal!(Amd64Opcode.f64_to_i32_trunc)(makeIrType(IrBasicType.i32), arg0);
								emit_final!(Amd64Opcode.mov)(typeTo, i32);
								break switch_instr;
							case 4:
								IrIndex i64 = emit_nonfinal!(Amd64Opcode.f64_to_i64_trunc)(makeIrType(IrBasicType.i64), arg0);
								emit_final!(Amd64Opcode.mov)(typeTo, i64);
								break switch_instr;
							case 8: emitLirInstr!(Amd64Opcode.f64_to_i64_trunc); break switch_instr; // f64 -> u64
							default: context.internal_error("Unexpected source type size %s", typeSizeTo);
						}
						default: context.internal_error("Unexpected target type size %s", typeSizeFrom);
					}

				case IrOpcode.fptosi:
					IrIndex arg0 = instrHeader.arg(ir, 0);
					IrIndex typeFrom = getValueType(arg0, ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);

					switch(typeSizeFrom) {
						case 4:
						switch(typeSizeTo) {
							case 1:
							case 2:
								IrIndex i32 = emit_nonfinal!(Amd64Opcode.f32_to_i32_trunc)(makeIrType(IrBasicType.i32), arg0);
								emit_final!(Amd64Opcode.mov)(typeTo, i32);
								break switch_instr;
							case 4: emitLirInstr!(Amd64Opcode.f32_to_i32_trunc); break switch_instr;
							case 8: emitLirInstr!(Amd64Opcode.f32_to_i64_trunc); break switch_instr;
							default: context.internal_error("Unexpected source type size %s", typeSizeTo);
						}

						case 8:
						switch(typeSizeTo) {
							case 1:
							case 2:
								IrIndex i32 = emit_nonfinal!(Amd64Opcode.f64_to_i32_trunc)(makeIrType(IrBasicType.i32), arg0);
								emit_final!(Amd64Opcode.mov)(typeTo, i32);
								break switch_instr;
							case 4: emitLirInstr!(Amd64Opcode.f64_to_i32_trunc); break switch_instr;
							case 8: emitLirInstr!(Amd64Opcode.f64_to_i64_trunc); break switch_instr;
							default: context.internal_error("Unexpected source type size %s", typeSizeTo);
						}
						default: context.internal_error("Unexpected target type size %s", typeSizeFrom);
					}

				case IrOpcode.set_unary_cond:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize, type : type };
					InstrWithResult res = builder.emitInstr!(Amd64Opcode.set_unary_cond)(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)));
					recordIndex(instrHeader.result(ir), res.result);
					break;

				case IrOpcode.set_binary_cond:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize, type : type };

					IrIndex argType = getValueType(instrHeader.arg(ir, 0), ir, context);
					// if both floats are constants the branch will disappear in backend
					if (argType.isTypeFloat && !(instrHeader.arg(ir, 0).isSimpleConstant && instrHeader.arg(ir, 1).isSimpleConstant)) {
						InstrWithResult res = builder.emitInstr!(Amd64Opcode.set_binary_cond)(
							lirBlockIndex, extra, legalizeFloatArg(instrHeader.arg(ir, 0), argType), legalizeFloatArg(instrHeader.arg(ir, 1), argType));
						recordIndex(instrHeader.result(ir), res.result);
					} else {
						InstrWithResult res = builder.emitInstr!(Amd64Opcode.set_binary_cond)(
							lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), getFixedIndex(instrHeader.arg(ir, 1)));
						recordIndex(instrHeader.result(ir), res.result);
					}
					break;

				case IrOpcode.jump:
					builder.emitInstr!(Amd64Opcode.jmp)(lirBlockIndex);
					break;

				case IrOpcode.branch_unary:
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize };
					IrIndex instruction = builder.emitInstr!(Amd64Opcode.un_branch)(
						lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)));
					break;

				case IrOpcode.branch_binary:
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize };
					IrIndex argType = getValueType(instrHeader.arg(ir, 0), ir, context);
					// if both floats are constants the branch will disappear in backend
					if (argType.isTypeFloat && !(instrHeader.arg(ir, 0).isSimpleConstant && instrHeader.arg(ir, 1).isSimpleConstant)) {
						IrIndex instruction = builder.emitInstr!(Amd64Opcode.bin_branch)(
							lirBlockIndex, extra, legalizeFloatArg(instrHeader.arg(ir, 0), argType), legalizeFloatArg(instrHeader.arg(ir, 1), argType));
					} else {
						IrIndex instruction = builder.emitInstr!(Amd64Opcode.bin_branch)(
							lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), getFixedIndex(instrHeader.arg(ir, 1)));
					}
					break;

				case IrOpcode.ret:
					builder.emitInstr!(Amd64Opcode.ret)(lirBlockIndex);
					break;

				case IrOpcode.unreachable:
					builder.emitInstr!(Amd64Opcode.ud2)(lirBlockIndex);
					break;

				default:
					context.internal_error("IrToLir unimplemented IR instr %s", cast(IrOpcode)instrHeader.op);
			}
		}
	}

	void fixInstrs(IrIndex blockIndex, ref IrBasicBlock lirBlock)
	{
		// all instructions already contain final arguments, add users here
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; lirBlock.instructions(lir))
		{
			foreach(ref IrIndex arg; instrHeader.args(lir))
			{
				builder.addUser(instrIndex, arg);
			}
		}
	}

	void fixPhis(IrIndex blockIndex, ref IrBasicBlock lirBlock)
	{
		// fix phi args and add users
		foreach(IrIndex phiIndex, ref IrPhi phi; lirBlock.phis(lir))
		{
			foreach(size_t arg_i, ref IrIndex phiArg; phi.args(lir))
			{
				fixIndex(phiArg);
				builder.addUser(phiIndex, phiArg);
			}
		}
	}

	//dumpFunction(context, lir, "IR -> LIR end"); // uncomment to see generated LIR before fixing

	foreach (IrIndex blockIndex, ref IrBasicBlock lirBlock; lir.blocks)
	{
		fixInstrs(blockIndex, lirBlock);
		fixPhis(blockIndex, lirBlock);
	}
}
