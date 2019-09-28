/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.ir_to_lir_amd64;

import std.stdio;
import all;


void pass_ir_to_lir_amd64(CompilationContext* context, IrBuilder* builder, ModuleDeclNode* mod, FunctionDeclNode* func)
{
	assert(!func.isExternal);

	func.backendData.lirData = context.appendAst!IrFunction;
	IrFunction* lirData = context.getAst!IrFunction(func.backendData.lirData);

	lirData.backendData = &func.backendData;

	mod.lirModule.addFunction(*context, lirData);
	IrFunction* irData = context.getAst!IrFunction(func.backendData.irData);

	builder.beginLir(lirData, irData, context);
	processFunc(context, builder, irData, lirData);
	builder.finalizeIr;

	if (context.validateIr) validateIrFunction(context, lirData);
	if (context.printLir && context.printDumpOf(func)) dumpFunction(context, lirData);
}

void processFunc(CompilationContext* context, IrBuilder* builder, IrFunction* ir, IrFunction* lir)
{
	//writefln("IR to LIR %s", context.idString(ir.name));
	lir.instructionSet = IrInstructionSet.lir_amd64;

	// Mirror of original IR, we will put the new IrIndex of copied entities there
	// and later use this info to rewire all connections between basic blocks
	IrMirror!IrIndex mirror;
	mirror.createVirtRegMirror(context, ir);

	// save map from old index to new index
	void recordIndex(IrIndex oldIndex, IrIndex newIndex)
	{
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

	IrIndex genAddressOffset(IrIndex lirPtr, uint offset, IrIndex ptrType, IrIndex lirBlockIndex) {
		IrIndex ptr;
		if (offset == 0) {
			ptr = lirPtr;
		} else {
			IrIndex offsetIndex = context.constants.add(offset, IsSigned.no);
			ExtraInstrArgs extra = { addUsers : false, type : ptrType };
			InstrWithResult addressInstr = builder.emitInstr!LirAmd64Instr_add(lirBlockIndex, extra, lirPtr, offsetIndex);
			ptr = addressInstr.result;
		}
		return ptr;
	}

	IrIndex genLoad(IrIndex lirPtr, uint offset, IrIndex ptrType, IrIndex lirBlockIndex) {
		IrIndex ptr = genAddressOffset(lirPtr, offset, ptrType, lirBlockIndex);
		IrIndex valType = context.types.getPointerBaseType(ptrType);
		ExtraInstrArgs extra = { addUsers : false, type : valType };
		auto instr = builder.emitInstr!LirAmd64Instr_load(lirBlockIndex, extra, ptr);
		return instr.result;
	}

	// fromOffset is used when irValue is pointer that needs deferencing
	void genStore(IrIndex lirPtr, uint offset, IrIndex irValue, uint fromOffset, IrIndex valueType, IrIndex lirBlockIndex, IrFunction* ir)
	{
		//writefln("genStore %s %s %s %s %s", lirPtr, offset, irValue, fromOffset, IrIndexDump(valueType, context, ir));
		IrIndex dstType = getValueType(lirPtr, lir, context);
		IrIndex srcType = getValueType(irValue, ir, context);
		//writefln("  %s %s <- %s %s", IrTypeDump(dstType, context), dstType, IrTypeDump(srcType, context), srcType);
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
					builder.emitInstr!LirAmd64Instr_store(lirBlockIndex, extra, ptr, loadedVal);
					break;
				}

				IrIndex rvalue = getFixedIndex(irValue);
				bool isBigConstant = rvalue.isConstant && context.constants.get(rvalue).payloadSize(rvalue) == IrArgSize.size64;

				/// Cannot obtain address and store it in another address in one step
				if (rvalue.isGlobal || rvalue.isStackSlot || isBigConstant)
				{
					// copy to temp register
					ExtraInstrArgs extra = { addUsers : false, type : srcType };
					InstrWithResult movInstr = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, rvalue);
					rvalue = movInstr.result;
				}

				ExtraInstrArgs extra = { addUsers : false, argSize : argSize };
				IrIndex ptr = genAddressOffset(lirPtr, offset, ptrType, lirBlockIndex);
				builder.emitInstr!LirAmd64Instr_store(lirBlockIndex, extra, ptr, rvalue);
				break;

			case struct_t:
				IrTypeStruct* structType = &context.types.get!IrTypeStruct(valueType);
				IrIndex[] members;

				switch(irValue.kind) with(IrValueKind)
				{
					case virtualRegister:
						IrIndex origin = ir.getVirtReg(irValue).definition;
						IrInstrHeader* instr = &ir.get!IrInstrHeader(origin);

						switch (instr.op)
						{
							case IrOpcode.load_aggregate:
								foreach (i, IrTypeStructMember member; structType.members)
								{
									genStore(lirPtr, offset + member.offset, instr.arg(ir, 0), fromOffset + member.offset, member.type, lirBlockIndex, ir);
								}
								return;

							case IrOpcode.create_aggregate:
								members = instr.args(ir);
								break;
							default:
								context.internal_error("%s", cast(IrOpcode)instr.op);
						}
						break;

					case constantAggregate:
						members = context.constants.getAggregate(irValue).members;
						break;

					default: context.internal_error("%s", irValue.kind); assert(false);
				}

				context.assertf(members.length == structType.numMembers, "%s != %s", members.length, structType.numMembers);

				foreach (i, IrTypeStructMember member; structType.members)
				{
					genStore(lirPtr, offset + member.offset, members[i], fromOffset, member.type, lirBlockIndex, ir);
				}
				break;

			default:
				context.internal_error("%s", valueType.typeKind); assert(false);
		}
	}

	// dup basic blocks
	// old and new blocks have the same indicies
	foreach (size_t i, ref IrBasicBlock irBlock; ir.blocksArray)
	{
		IrIndex blockIndex = IrIndex(cast(uint)i, IrValueKind.basicBlock);
		IrIndex lirBlock = builder.appendBasicBlockSlot;

		lir.getBlock(lirBlock).name = irBlock.name;
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
			IrVirtualRegister* oldReg = &ir.getVirtReg(phi.result);
			IrIndex newPhi = builder.addPhi(lirBlock, oldReg.type, phi.var);
			IrIndex newResult = lir.getPhi(newPhi).result;
			IrVirtualRegister* newReg = &lir.getVirtReg(newResult);
			newReg.type = oldReg.type;

			recordIndex(phi.result, lir.getPhi(newPhi).result);
			foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
			{
				builder.addPhiArg(newPhi, phiArg.basicBlock, phiArg.value);
			}
		}
	}

	bool isPassByValue(IrIndex type) {
		if (type.isTypeStruct) {
			IrTypeStruct* structRes = &context.types.get!IrTypeStruct(type);
			switch(structRes.size) {
				case 1: return true;
				case 2: return true;
				case 4: return true;
				case 8: return true;
				default: return false;
			}
		}
		return true;
	}

	// Handle ABI
	auto irFuncType = &context.types.get!IrTypeFunction(ir.type);
	IrIndex[] paramTypes;
	uint numHiddenParams = 0;
	IrIndex hiddenParameter;

	if (irFuncType.numResults == 0)
	{
		lir.type = context.types.appendFuncSignature(0, irFuncType.numParameters, irFuncType.callConv);
		auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
		paramTypes = lirFuncType.parameterTypes;
	}
	else if (irFuncType.numResults == 1)
	{
		IrIndex resType = irFuncType.resultTypes[0];
		if (isPassByValue(resType))
		{
			lir.type = context.types.appendFuncSignature(1, irFuncType.numParameters, irFuncType.callConv);
			auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
			lirFuncType.resultTypes[0] = resType;
			paramTypes = lirFuncType.parameterTypes;
		}
		else
		{
			numHiddenParams = 1;
			lir.type = context.types.appendFuncSignature(1, irFuncType.numParameters + 1, irFuncType.callConv);
			auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
			paramTypes = lirFuncType.parameterTypes[1..$];
			IrIndex retType = context.types.appendPtr(resType);
			lirFuncType.parameterTypes[0] = retType;
			lirFuncType.resultTypes[0] = retType;
		}
	}
	else
	{
		context.internal_error("%s results is not implemented", irFuncType.numResults);
	}

	foreach (i, ref IrIndex irParamType; irFuncType.parameterTypes)
	{
		if (isPassByValue(irParamType))
			paramTypes[i] = irParamType;
		else
			paramTypes[i] = context.types.appendPtr(irParamType);
	}

	//writefln("%s", IrIndexDump(lir.type, context, lir));

	// buffer for call/instruction arguments
	enum MAX_ARGS = 255;
	IrIndex[MAX_ARGS] argBuffer;

	// fix successors predecessors links
	foreach (IrIndex lirBlockIndex, ref IrBasicBlock lirBlock; lir.blocks)
	{
		lirBlock.isSealed = true;
		lirBlock.isFinished = true;

		// old and new blocks have the same indicies
		IrIndex irBlockIndex = lirBlockIndex;

		// Add hidden parameter(s) to first block
		if (irBlockIndex == ir.entryBasicBlock && numHiddenParams == 1)
		{
			auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
			IrIndex type = lirFuncType.parameterTypes[0];
			IrIndex paramReg = lir.backendData.getCallConv(context).paramsInRegs[0];
			paramReg.physRegSize = typeToRegSize(type, context);
			ExtraInstrArgs extra = { type : type };
			InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, paramReg);
			hiddenParameter = instr.result;
		}

		// Add instructions with old args
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; ir.getBlock(irBlockIndex).instructions(ir))
		{
			auto emitLirInstr(I)()
			{
				static assert(!getInstrInfo!I.hasVariadicArgs);
				static assert(!getInstrInfo!I.hasVariadicResult);

				IrIndex[getInstrInfo!I.numArgs] fixedArgs = instrHeader.args(ir);
				foreach(ref arg; fixedArgs) fixIndex(arg);

				static if (getInstrInfo!I.hasResult)
				{
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize, type : type };
					InstrWithResult res = builder.emitInstr!I(lirBlockIndex, extra, fixedArgs);
					recordIndex(instrHeader.result(ir), res.result);
				}
				else
				{
					ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize };
					IrIndex res = builder.emitInstr!I(lirBlockIndex, extra, fixedArgs);
				}
			}

			void makeMov(IrIndex to, IrIndex from, IrArgSize argSize) {
				ExtraInstrArgs extra = { addUsers : false, result : to, argSize : argSize };
				builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, getFixedIndex(from));
			}

			switch(instrHeader.op)
			{
				case IrOpcode.parameter:
					uint paramIndex = ir.get!IrInstrHeader(instrIndex).extraPayload(ir, 1)[0].asUint + numHiddenParams;
					context.assertf(paramIndex < lir.backendData.getCallConv(context).paramsInRegs.length,
						"Only parameters passed through registers are implemented");

					IrIndex paramReg = lir.backendData.getCallConv(context).paramsInRegs[paramIndex];
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					paramReg.physRegSize = typeToRegSize(type, context);
					ExtraInstrArgs extra = { type : type, argSize : instrHeader.argSize };
					InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, paramReg);
					recordIndex(instrHeader.result(ir), instr.result);
					break;

				case IrOpcode.create_aggregate:
					// skip
					break;

				case IrOpcode.load_aggregate:
					// skip
					break;

				// TODO. Win64 call conv is hardcoded here
				case IrOpcode.call:

					lir.backendData.stackLayout.numCalls += 1;

					IrIndex callee = instrHeader.arg(ir, 0);
					CallConv* callConv = context.types.getCalleeCallConv(callee, ir, context);
					argBuffer[0] = getFixedIndex(callee);
					IrIndex[] args = instrHeader.args(ir)[1..$];
					IrIndex[] localArgBuffer = argBuffer[1..$];

					enum STACK_ITEM_SIZE = 8;
					size_t numArgs = args.length;
					size_t numParamsInRegs = callConv.paramsInRegs.length;
					// how many bytes are allocated on the stack before func call
					size_t stackReserve = max(numArgs, numParamsInRegs) * STACK_ITEM_SIZE;
					IrIndex stackPtrReg = callConv.stackPointer;

					// Copy args to stack if necessary (big structs or doesn't fit into regs)
					foreach (i, IrIndex irArg; args)
					{
						IrIndex type = ir.getValueType(context, irArg);

						if (isPassByValue(type)) {
							localArgBuffer[i] = getFixedIndex(irArg);
						} else {
							//allocate stack slot, store value there and use slot pointer as argument
							localArgBuffer[i] = lir.backendData.stackLayout.addStackItem(context, type, StackSlotKind.local, 0);
							genStore(localArgBuffer[i], 0, irArg, 0, type, lirBlockIndex, ir);
						}
					}

					// align stack and push args that didn't fit into registers (register size args)
					if (numArgs > numParamsInRegs)
					{
						if (numArgs % 2 == 1)
						{	// align stack to 16 bytes
							stackReserve += STACK_ITEM_SIZE;
							IrIndex paddingSize = context.constants.add(STACK_ITEM_SIZE, IsSigned.no);
							ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
							builder.emitInstr!LirAmd64Instr_sub(
								lirBlockIndex, extra, stackPtrReg, paddingSize);
						}

						// push args to stack
						foreach_reverse (IrIndex lirArg; localArgBuffer[numParamsInRegs..numArgs])
						{
							// TODO: push can use mem address
							if (lirArg.isStackSlot) {
								ExtraInstrArgs extra = { addUsers : false, type : lir.getValueType(context, lirArg) };
								InstrWithResult movInstr = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, lirArg);
								lirArg = movInstr.result;
							}

							ExtraInstrArgs extra = { addUsers : false };
							builder.emitInstr!LirAmd64Instr_push(lirBlockIndex, extra, lirArg);
						}
					}

					// move args to registers
					size_t numPhysRegs = min(numParamsInRegs, numArgs);
					foreach (i, IrIndex lirArg; localArgBuffer[0..numPhysRegs])
					{
						IrIndex type = lir.getValueType(context, lirArg);
						IrIndex argRegister = callConv.paramsInRegs[i];
						argRegister.physRegSize = typeToRegSize(type, context);
						localArgBuffer[i] = argRegister;
						ExtraInstrArgs extra = {addUsers : false, result : argRegister};
						builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, lirArg);
					}

					{	// Allocate shadow space for 4 physical registers
						IrIndex const_32 = context.constants.add(32, IsSigned.no);
						ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
						builder.emitInstr!LirAmd64Instr_sub(
							lirBlockIndex, extra, stackPtrReg, const_32);
					}

					{
						IrIndex returnReg = callConv.returnReg;
						if (instrHeader.hasResult) {
							returnReg.physRegSize = typeToIrArgSize(ir.getVirtReg(instrHeader.result(ir)).type, context);
						}
						// call
						ExtraInstrArgs callExtra = {
							addUsers : false,
							hasResult : instrHeader.hasResult,
							result : returnReg // will be used if function has result
						};

						InstrWithResult callInstr = builder.emitInstr!LirAmd64Instr_call(
							lirBlockIndex, callExtra, argBuffer[0..numPhysRegs+1]); // include callee
						lir.get!IrInstrHeader(callInstr.instruction).extendFixedArgRange = true;
						if (callExtra.hasResult) lir.get!IrInstrHeader(callInstr.instruction).extendFixedResultRange = true;

						{	// Deallocate stack after call
							IrIndex conReservedBytes = context.constants.add(stackReserve, IsSigned.no);
							ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
							builder.emitInstr!LirAmd64Instr_add(
								lirBlockIndex, extra, stackPtrReg, conReservedBytes);
						}

						if (callExtra.hasResult) {
							// mov result to virt reg
							ExtraInstrArgs extra = { type : ir.getVirtReg(instrHeader.result(ir)).type };
							InstrWithResult movInstr = builder.emitInstr!LirAmd64Instr_mov(
								lirBlockIndex, extra, returnReg);
							recordIndex(instrHeader.result(ir), movInstr.result);
						}
					}

					break;

				case IrOpcode.add: emitLirInstr!LirAmd64Instr_add; break;
				case IrOpcode.sub: emitLirInstr!LirAmd64Instr_sub; break;
				case IrOpcode.umul, IrOpcode.smul: emitLirInstr!LirAmd64Instr_imul; break;
				case IrOpcode.not: emitLirInstr!LirAmd64Instr_not; break;
				case IrOpcode.neg: emitLirInstr!LirAmd64Instr_neg; break;
				case IrOpcode.and: emitLirInstr!LirAmd64Instr_and; break;
				case IrOpcode.or: emitLirInstr!LirAmd64Instr_or; break;
				case IrOpcode.xor: emitLirInstr!LirAmd64Instr_xor; break;

				case IrOpcode.udiv, IrOpcode.sdiv, IrOpcode.urem, IrOpcode.srem:
					//   v1 = div v2, v3
					// is converted into:
					//   mov ax, v2
					//   zx/sx dx:ax
					//   ax = div dx:ax, v3
					//   mov v1, ax

					bool isSigned = instrHeader.op == IrOpcode.sdiv || instrHeader.op == IrOpcode.srem;
					bool isDivision = instrHeader.op == IrOpcode.udiv || instrHeader.op == IrOpcode.sdiv;

					// copy bottom half of dividend
					IrIndex dividendBottom = amd64_reg.ax;
					dividendBottom.physRegSize = instrHeader.argSize;
					makeMov(dividendBottom, instrHeader.arg(ir, 0), instrHeader.argSize);

					IrIndex dividendTop = amd64_reg.dx;
					dividendTop.physRegSize = instrHeader.argSize;

					if (isSigned) {
						// sign-extend top half of dividend
						ExtraInstrArgs extra2 = { argSize : instrHeader.argSize };
						builder.emitInstr!LirAmd64Instr_divsx(lirBlockIndex, extra2);
					} else {
						// zero top half of dividend
						makeMov(dividendTop, context.constants.add(0, IsSigned.no), IrArgSize.size32);
					}

					// choose result
					IrIndex resultReg = dividendTop; // remainder
					if (isDivision) {
						resultReg = dividendBottom; // dividend
					}

					// divisor must be in register
					IrIndex divisor = instrHeader.arg(ir, 1);
					if (instrHeader.arg(ir, 1).isConstant) {
						ExtraInstrArgs extra = { addUsers : false, type : getValueType(divisor, ir, context) };
						divisor = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, divisor).result;
					}
					else fixIndex(divisor);

					// divide
					ExtraInstrArgs extra3 = { addUsers : false, argSize : instrHeader.argSize, result : resultReg };
					InstrWithResult res;
					if (isSigned)
						res = builder.emitInstr!LirAmd64Instr_idiv(lirBlockIndex, extra3, dividendTop, dividendBottom, divisor);
					else
						res = builder.emitInstr!LirAmd64Instr_div(lirBlockIndex, extra3, dividendTop, dividendBottom, divisor);

					// copy result (quotient)
					ExtraInstrArgs extra4 = { addUsers : false, argSize : instrHeader.argSize, type : ir.getVirtReg(instrHeader.result(ir)).type };
					InstrWithResult movResult = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra4, resultReg);
					recordIndex(instrHeader.result(ir), movResult.result);
					break;

				case IrOpcode.shl, IrOpcode.lshr, IrOpcode.ashr:
					IrIndex rightArg;
					if (instrHeader.arg(ir, 1).isConstant)
						rightArg = instrHeader.arg(ir, 1);
					else
					{
						rightArg = amd64_reg.cx;
						rightArg.physRegSize = ArgType.BYTE;
						makeMov(rightArg, instrHeader.arg(ir, 1), instrHeader.argSize);
					}
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize, type : type };
					InstrWithResult res;
					switch(instrHeader.op) {
						case IrOpcode.shl:
							res = builder.emitInstr!LirAmd64Instr_shl(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), rightArg);
							break;
						case IrOpcode.lshr:
							res = builder.emitInstr!LirAmd64Instr_shr(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), rightArg);
							break;
						case IrOpcode.ashr:
							res = builder.emitInstr!LirAmd64Instr_sar(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), rightArg);
							break;
						default: assert(false);
					}
					recordIndex(instrHeader.result(ir), res.result);
					break;
				case IrOpcode.load: emitLirInstr!LirAmd64Instr_load; break;
				case IrOpcode.store:
					IrIndex type = ir.getValueType(context, instrHeader.arg(ir, 1));
					genStore(getFixedIndex(instrHeader.arg(ir, 0)), 0, instrHeader.arg(ir, 1), 0, type, lirBlockIndex, ir);
					break;
				case IrOpcode.conv:
					// Incomplete implementation
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrIndex typeTo = ir.getVirtReg(instrHeader.result(ir)).type;
					uint typeSizeFrom = context.types.typeSize(typeFrom);
					uint typeSizeTo = context.types.typeSize(typeTo);
					context.assertf(typeSizeTo <= typeSizeFrom,
						"Can't cast from %s bytes to %s bytes", typeSizeFrom, typeSizeTo);
					emitLirInstr!LirAmd64Instr_mov;
					break;

				case IrOpcode.set_unary_cond:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize, type : type };
					InstrWithResult res = builder.emitInstr!LirAmd64Instr_set_unary_cond(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)));
					recordIndex(instrHeader.result(ir), res.result);
					break;

				case IrOpcode.set_binary_cond:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize, type : type };
					InstrWithResult res = builder.emitInstr!LirAmd64Instr_set_binary_cond(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), getFixedIndex(instrHeader.arg(ir, 1)));
					recordIndex(instrHeader.result(ir), res.result);
					break;

				case IrOpcode.block_exit_jump:
					builder.emitInstr!LirAmd64Instr_jmp(lirBlockIndex);
					break;

				case IrOpcode.block_exit_unary_branch:
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize };
					IrIndex instruction = builder.emitInstr!LirAmd64Instr_un_branch(
						lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)));
					break;

				case IrOpcode.block_exit_binary_branch:
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize };
					IrIndex instruction = builder.emitInstr!LirAmd64Instr_bin_branch(
						lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), getFixedIndex(instrHeader.arg(ir, 1)));
					break;

				case IrOpcode.block_exit_return_void:
					builder.emitInstr!LirAmd64Instr_return(lirBlockIndex);
					break;

				case IrOpcode.block_exit_return_value:
					IrIndex result = lir.backendData.getCallConv(context).returnReg;
					auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
					IrIndex type = lirFuncType.resultTypes[0];
					result.physRegSize = typeToRegSize(type, context);
					ExtraInstrArgs extra = { addUsers : false, result : result };

					// simple way of returning multi-member structs. Store to memory, then load into register
					IrIndex tryMovComplexStruct()
					{
						if (type.isTypeStruct)
						{
							IrTypeStruct* structRes = &context.types.get!IrTypeStruct(type);
							if (structRes.numMembers > 1)
							{
								IrIndex slot = lir.backendData.stackLayout.addStackItem(context, type, StackSlotKind.local, 0);
								IrIndex slotType = lir.backendData.stackLayout[slot].type;
								genStore(slot, 0, instrHeader.arg(ir, 0), 0, type, lirBlockIndex, ir);
								return genLoad(slot, 0, slotType, lirBlockIndex);
							}
						}
						return getFixedIndex(instrHeader.arg(ir, 0));
					}

					if (numHiddenParams == 1) {
						// store struct into pointer, then return pointer
						IrIndex valType = context.types.getPointerBaseType(type);
						genStore(hiddenParameter, 0, instrHeader.arg(ir, 0), 0, valType, lirBlockIndex, ir);
						builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, hiddenParameter);
					} else {
						IrIndex itemToReturn = tryMovComplexStruct;
						builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, itemToReturn);
					}

					IrIndex instruction = builder.emitInstr!LirAmd64Instr_return(lirBlockIndex);
					break;

				default:
					context.internal_error("IrToLir unimplemented IR instr %s", cast(IrOpcode)instrHeader.op);
			}
		}
	}

	void fixInstrs(IrIndex blockIndex, ref IrBasicBlock lirBlock)
	{
		// replace old args with new args and add users
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
			foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(lir))
			{
				fixIndex(phiArg.value);
				builder.addUser(phiIndex, phiArg.value);
			}
		}
	}

	//dumpFunction(context, lir); // uncomment to see generated LIR before fixing

	foreach (IrIndex blockIndex, ref IrBasicBlock lirBlock; lir.blocks)
	{
		fixInstrs(blockIndex, lirBlock);
		fixPhis(blockIndex, lirBlock);
	}
}
