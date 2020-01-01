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

	mod.lirModule.addFunction(context, lirData);
	IrFunction* irData = context.getAst!IrFunction(func.backendData.irData);

	builder.beginLir(lirData, irData, context);
	processFunc(context, builder, irData, lirData);
	builder.finalizeIr;

	if (context.validateIr) validateIrFunction(context, lirData);
	if (context.printLir && context.printDumpOf(func)) dumpFunction(context, lirData);
}

/// Converts complex constants fitting in a single register into an integer constant
IrIndex simplifyConstant(IrIndex index, CompilationContext* c)
{
	union U {
		ulong bufferValue;
		ubyte[8] buffer;
	}
	U data;
	uint typeSize;
	if (index.isConstantZero)
	{
		typeSize = c.types.typeSize(index.constantZeroType);
	}
	else if (index.isConstantAggregate)
	{
		IrAggregateConstant* con = &c.constants.getAggregate(index);
		typeSize = c.types.typeSize(con.type);
	}
	else
	{
		return index;
	}

	constantToMem(data.buffer[0..typeSize], index, c);
	return c.constants.add(data.bufferValue, IsSigned.no);
}

void processFunc(CompilationContext* context, IrBuilder* builder, IrFunction* ir, IrFunction* lir)
{
	//writefln("IR to LIR %s", context.idString(ir.backendData.name));
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
			ExtraInstrArgs extra = { addUsers : false, type : ptrType };
			InstrWithResult movInstr = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, lirPtr);
			ptr = movInstr.result;
		} else {
			IrIndex offsetIndex = context.constants.add(offset, IsSigned.no);
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
		auto instr = builder.emitInstr!(Amd64Opcode.load)(lirBlockIndex, extra, ptr);
		return instr.result;
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

				IrIndex rvalue = getFixedIndex(irValue);
				bool isBigConstant = rvalue.isConstant && context.constants.get(rvalue).payloadSize(rvalue) == IrArgSize.size64;

				/// Cannot obtain address and store it in another address in one step
				if (rvalue.isGlobal || rvalue.isStackSlot || isBigConstant)
				{
					// copy to temp register
					ExtraInstrArgs extra = { addUsers : false, type : srcType };
					InstrWithResult movInstr = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, rvalue);
					rvalue = movInstr.result;
				}

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
						IrInstrHeader* instr = &ir.get!IrInstrHeader(origin);

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
								context.internal_error("%s", cast(IrOpcode)instr.op);
						}
						break;

					case constantZero:
						IrIndex zero = context.constants.add(0, IsSigned.no);
						foreach (i, IrTypeStructMember member; structType.members)
						{
							IrIndex ptr = genAddressOffset(lirPtr, offset + member.offset, ptrType, lirBlockIndex);
							genStore(ptr, 0, zero, fromOffset + member.offset, member.type, lirBlockIndex, ir);
						}
						return;

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

			case array:
				IrTypeArray* arrayType = &context.types.get!IrTypeArray(valueType);
				uint elemSize = context.types.typeSize(arrayType.elemType);
				IrIndex[] members;

				switch(irValue.kind) with(IrValueKind)
				{
					case virtualRegister:
						IrIndex origin = ir.getVirtReg(irValue).definition;
						IrInstrHeader* instr = &ir.get!IrInstrHeader(origin);

						switch (instr.op)
						{
							case IrOpcode.load_aggregate, IrOpcode.load:
								foreach (i; 0..arrayType.size)
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
								foreach (i; 0..arrayType.size)
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
						IrIndex zero = context.constants.add(0, IsSigned.no);
						foreach (i; 0..arrayType.size)
						{
							IrIndex ptr = genAddressOffset(lirPtr, offset + elemSize * i, ptrType, lirBlockIndex);
							genStore(ptr, 0, zero, fromOffset + elemSize * i, arrayType.elemType, lirBlockIndex, ir);
						}
						return;

					case constantAggregate:
						members = context.constants.getAggregate(irValue).members;
						break;

					default: context.internal_error("%s", irValue.kind); assert(false);
				}

				context.assertf(members.length == arrayType.size, "%s != %s", members.length, arrayType.size);

				foreach (i; 0..arrayType.size)
				{
					genStore(lirPtr, offset + elemSize * i, members[i], fromOffset, arrayType.elemType, lirBlockIndex, ir);
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
			InstrWithResult instr = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, paramReg);
			hiddenParameter = instr.result;
		}

		// Add instructions with old args
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; ir.getBlock(irBlockIndex).instructions(ir))
		{
			auto emitLirInstr(alias I)()
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
				builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, getFixedIndex(from));
			}

			switch(instrHeader.op)
			{
				case IrOpcode.parameter:
					// hack, TODO: hidden param is accounted for in frontend (ir_gen_function)
					uint paramIndex = ir.get!IrInstrHeader(instrIndex).extraPayload(ir, 1)[0].asUint;// + numHiddenParams;
					context.assertf(paramIndex < lir.backendData.getCallConv(context).paramsInRegs.length,
						"Only parameters passed through registers are implemented");

					IrIndex paramReg = lir.backendData.getCallConv(context).paramsInRegs[paramIndex];
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					paramReg.physRegSize = typeToRegSize(type, context);
					ExtraInstrArgs extra = { type : type, argSize : instrHeader.argSize };
					InstrWithResult instr = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, paramReg);
					recordIndex(instrHeader.result(ir), instr.result);
					break;

				case IrOpcode.create_aggregate:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;

					// pack values and constants into a register via `shift` and `binary or` instructions
					if (isPassByValue(type))
					{
						IrArgSize argSize = typeToIrArgSize(type, context);
						context.assertf(instrHeader.numArgs <= 8, "too much args %s", instrHeader.numArgs);
						ulong constant = 0;
						// how many non-constants are prepared in argBuffer
						uint numBufferedValues = 0;

						void insertNonConstant(IrIndex value, uint bit_offset, uint size)
						{
							ExtraInstrArgs extra = { addUsers : false, argSize : argSize, type : type };
							switch(size) { // zero extend 8 and 16 bit args to 32bit
								case 1: value = builder.emitInstr!(Amd64Opcode.movzx_btod)(lirBlockIndex, extra, value).result; break;
								case 2: value = builder.emitInstr!(Amd64Opcode.movzx_wtod)(lirBlockIndex, extra, value).result; break;
								default: break;
							}

							// shift
							if (bit_offset == 0)
								argBuffer[numBufferedValues] = value;
							else
							{

								IrIndex rightArg = context.constants.add(bit_offset, IsSigned.no);
								ExtraInstrArgs extra1 = { addUsers : false, argSize : argSize, type : type };
								IrIndex shiftRes = builder.emitInstr!(Amd64Opcode.shl)(lirBlockIndex, extra1, value, rightArg).result;
								argBuffer[numBufferedValues] = shiftRes;
							}
							++numBufferedValues;

							if (numBufferedValues == 2)
							{
								// or
								ExtraInstrArgs extra2 = { addUsers : false, argSize : argSize, type : type };
								argBuffer[0] = builder.emitInstr!(Amd64Opcode.or)(lirBlockIndex, extra2, argBuffer[0], argBuffer[1]).result;
								numBufferedValues = 1;
							}
						}

						void insertAt(IrIndex value, uint offset, uint size)
						{
							if (value.isConstant) {
								constant |= context.constants.get(value).i64 << (offset * 8);
							} else {
								insertNonConstant(value, offset * 8, size);
							}
						}

						switch(type.typeKind) with(IrTypeKind) {
							case struct_t:
								IrTypeStruct* structType = &context.types.get!IrTypeStruct(type);
								IrIndex[] args = instrHeader.args(ir);
								foreach_reverse (i, IrTypeStructMember member; structType.members)
								{
									uint memberSize = context.types.typeSize(member.type);
									insertAt(getFixedIndex(args[i]), member.offset, memberSize);
								}
								break;
							case array:
								IrTypeArray* arrayType = &context.types.get!IrTypeArray(type);
								uint elemSize = context.types.typeSize(arrayType.elemType);
								IrIndex[] args = instrHeader.args(ir);
								foreach_reverse (i; 0..arrayType.size)
								{
									insertAt(getFixedIndex(args[i]), i * elemSize, elemSize);
								}
								break;
							default: assert(false);
						}

						IrIndex constIndex = context.constants.add(constant, IsSigned.no, argSize);
						if (numBufferedValues == 1)
						{
							if (constant == 0)
							{
								recordIndex(instrHeader.result(ir), argBuffer[0]);
								break;
							}

							bool isBigConstant = context.constants.get(constIndex).payloadSize(constIndex) == IrArgSize.size64;

							if (isBigConstant)
							{
								// copy to temp register
								ExtraInstrArgs extra = { addUsers : false, argSize : argSize, type : type };
								constIndex = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, constIndex).result;
							}

							ExtraInstrArgs extra3 = { addUsers : false, argSize : argSize, type : type };
							IrIndex result = builder.emitInstr!(Amd64Opcode.or)(lirBlockIndex, extra3, argBuffer[0], constIndex).result;
							recordIndex(instrHeader.result(ir), result);
						}
						else
						{
							recordIndex(instrHeader.result(ir), constIndex);
						}
					}
					// skip
					break;

				case IrOpcode.load_aggregate:
					IrIndex resType = ir.getVirtReg(instrHeader.result(ir)).type;

					// pack values and constants into a register via `shift` and `binary or` instructions
					if (isPassByValue(resType))
					{
						IrIndex lirPtr = getFixedIndex(instrHeader.arg(ir, 0));
						IrArgSize argSize = typeToIrArgSize(resType, context);
						ExtraInstrArgs extra = { addUsers : false, type : resType, argSize : argSize };
						IrIndex result = builder.emitInstr!(Amd64Opcode.load)(lirBlockIndex, extra, lirPtr).result;
						recordIndex(instrHeader.result(ir), result);
					}
					// skip
					break;

				// TODO. Win64 call conv is hardcoded here
				case IrOpcode.call:

					lir.backendData.stackLayout.numCalls += 1;

					IrIndex callee = instrHeader.arg(ir, 0);
					IrIndex calleeTypeIndex = ir.getValueType(context, callee);
					if (calleeTypeIndex.isTypePointer)
						calleeTypeIndex = context.types.getPointerBaseType(calleeTypeIndex);
					IrTypeFunction* calleeType = &context.types.get!IrTypeFunction(calleeTypeIndex);

					CallConv* callConv = context.types.getCalleeCallConv(callee, ir, context);
					argBuffer[0] = getFixedIndex(callee);
					IrIndex[] args = instrHeader.args(ir)[1..$];
					IrIndex[] localArgBuffer = argBuffer[1..$];

					// allocate stack slot for big return value
					bool hasResultArg = false;
					if (calleeType.numResults == 1)
					{
						IrIndex resType = calleeType.resultTypes[0];
						if (!isPassByValue(resType))
						{
							argBuffer[1] = lir.backendData.stackLayout.addStackItem(context, resType, StackSlotKind.argument, 0);
							hasResultArg = true;
							recordIndex(instrHeader.result(ir), argBuffer[1]);
						}
					}

					enum STACK_ITEM_SIZE = 8;
					size_t numArgs = args.length + hasResultArg;
					size_t numParamsInRegs = callConv.paramsInRegs.length;
					// how many bytes are allocated on the stack before func call
					size_t stackReserve = max(numArgs, numParamsInRegs) * STACK_ITEM_SIZE;
					IrIndex stackPtrReg = callConv.stackPointer;

					// Copy args to stack if necessary (big structs or doesn't fit into regs)
					foreach (i, IrIndex irArg; args)
					{
						IrIndex type = ir.getValueType(context, irArg);

						if (isPassByValue(type)) {
							IrIndex fixedArg = getFixedIndex(irArg);
							fixedArg = simplifyConstant(fixedArg, context);
							if (fixedArg.isConstant && context.constants.get(fixedArg).payloadSize(fixedArg) == IrArgSize.size64)
							{
								// push cannot push 64bit constants
								// copy to temp register
								ExtraInstrArgs extra = { addUsers : false, type : type };
								fixedArg = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, fixedArg).result;
							}
							localArgBuffer[i+hasResultArg] = fixedArg;
						} else {
							//allocate stack slot, store value there and use slot pointer as argument
							localArgBuffer[i+hasResultArg] = lir.backendData.stackLayout.addStackItem(context, type, StackSlotKind.argument, 0);
							genStore(localArgBuffer[i+hasResultArg], 0, irArg, 0, type, lirBlockIndex, ir);
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
							builder.emitInstr!(Amd64Opcode.sub)(
								lirBlockIndex, extra, stackPtrReg, paddingSize);
						}

						// push args to stack
						foreach_reverse (IrIndex lirArg; localArgBuffer[numParamsInRegs..numArgs])
						{
							// TODO: push can use mem address
							if (lirArg.isStackSlot || lirArg.isFunction) {
								ExtraInstrArgs extra = { addUsers : false, type : lir.getValueType(context, lirArg) };
								InstrWithResult movInstr = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, lirArg);
								lirArg = movInstr.result;
							}

							ExtraInstrArgs extra = { addUsers : false };
							builder.emitInstr!(Amd64Opcode.push)(lirBlockIndex, extra, lirArg);
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
						builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, lirArg);
					}

					{	// Allocate shadow space for 4 physical registers
						IrIndex const_32 = context.constants.add(32, IsSigned.no);
						ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
						builder.emitInstr!(Amd64Opcode.sub)(
							lirBlockIndex, extra, stackPtrReg, const_32);
					}

					{
						bool hasResult = instrHeader.hasResult && (!hasResultArg);
						IrIndex returnReg = callConv.returnReg;
						if (hasResult) {
							returnReg.physRegSize = typeToIrArgSize(ir.getVirtReg(instrHeader.result(ir)).type, context);
						}
						// call
						ExtraInstrArgs callExtra = {
							addUsers : false,
							hasResult : hasResult,
							result : returnReg // will be used if function has result
						};

						InstrWithResult callInstr = builder.emitInstr!(Amd64Opcode.call)(
							lirBlockIndex, callExtra, argBuffer[0..numPhysRegs+1]); // include callee
						lir.get!IrInstrHeader(callInstr.instruction).extendFixedArgRange = true;
						if (hasResult) lir.get!IrInstrHeader(callInstr.instruction).extendFixedResultRange = true;

						{	// Deallocate stack after call
							IrIndex conReservedBytes = context.constants.add(stackReserve, IsSigned.no);
							ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
							builder.emitInstr!(Amd64Opcode.add)(
								lirBlockIndex, extra, stackPtrReg, conReservedBytes);
						}

						if (hasResult) {
							// mov result to virt reg
							ExtraInstrArgs extra = { type : ir.getVirtReg(instrHeader.result(ir)).type };
							InstrWithResult movInstr = builder.emitInstr!(Amd64Opcode.mov)(
								lirBlockIndex, extra, returnReg);
							recordIndex(instrHeader.result(ir), movInstr.result);
						}
					}

					break;

				case IrOpcode.add: emitLirInstr!(Amd64Opcode.add); break;
				case IrOpcode.sub: emitLirInstr!(Amd64Opcode.sub); break;
				case IrOpcode.umul, IrOpcode.smul: emitLirInstr!(Amd64Opcode.imul); break;
				case IrOpcode.not: emitLirInstr!(Amd64Opcode.not); break;
				case IrOpcode.neg: emitLirInstr!(Amd64Opcode.neg); break;
				case IrOpcode.and: emitLirInstr!(Amd64Opcode.and); break;
				case IrOpcode.or: emitLirInstr!(Amd64Opcode.or); break;
				case IrOpcode.xor: emitLirInstr!(Amd64Opcode.xor); break;

				case IrOpcode.udiv, IrOpcode.sdiv, IrOpcode.urem, IrOpcode.srem:
					//   v1 = div v2, v3
					// is converted into:
					//   mov ax, v2
					//   zx/sx dx:ax
					//   ax = div ax, dx, v3
					//   mov v1, ax

					bool isSigned = instrHeader.op == IrOpcode.sdiv || instrHeader.op == IrOpcode.srem;
					bool isDivision = instrHeader.op == IrOpcode.udiv || instrHeader.op == IrOpcode.sdiv;

					// divisor must be in register
					IrIndex divisor = instrHeader.arg(ir, 1);
					if (instrHeader.arg(ir, 1).isConstant) {
						ExtraInstrArgs extra = { addUsers : false, type : getValueType(divisor, ir, context) };
						divisor = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, divisor).result;
					}
					else fixIndex(divisor);

					IrIndex dividendTop = amd64_reg.dx;
					dividendTop.physRegSize = instrHeader.argSize;

					// copy bottom half of dividend
					IrIndex dividendBottom = amd64_reg.ax;
					dividendBottom.physRegSize = instrHeader.argSize;
					makeMov(dividendBottom, instrHeader.arg(ir, 0), instrHeader.argSize);

					if (isSigned) {
						// if dividend is 8bit we use movsx and only change ax
						// if it is bigger we use cwd/cdq/cqo that affects dx too
						// TODO: for now always say that we modify dx even if we dont (8bit arg doesn't touch dx, only ax)
						IrIndex divsxResult = amd64_reg.dx;
						// sign-extend top half of dividend
						ExtraInstrArgs extra2 = { argSize : instrHeader.argSize, result : divsxResult };
						builder.emitInstr!(Amd64Opcode.divsx)(lirBlockIndex, extra2);
					} else {
						// zero top half of dividend
						makeMov(dividendTop, context.constants.add(0, IsSigned.no), IrArgSize.size32);
					}

					// choose result
					IrIndex resultReg = dividendTop; // remainder
					if (isDivision) {
						resultReg = dividendBottom; // dividend
					}

					// divide
					ExtraInstrArgs extra3 = { addUsers : false, argSize : instrHeader.argSize, result : resultReg };
					InstrWithResult res;
					if (isSigned)
						res = builder.emitInstr!(Amd64Opcode.idiv)(lirBlockIndex, extra3, dividendBottom, dividendTop, divisor);
					else
						res = builder.emitInstr!(Amd64Opcode.div)(lirBlockIndex, extra3, dividendBottom, dividendTop, divisor);

					// copy result (quotient)
					ExtraInstrArgs extra4 = { addUsers : false, argSize : instrHeader.argSize, type : ir.getVirtReg(instrHeader.result(ir)).type };
					InstrWithResult movResult = builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra4, resultReg);
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
				case IrOpcode.load: emitLirInstr!(Amd64Opcode.load); break;
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
					emitLirInstr!(Amd64Opcode.mov);
					break;
				case IrOpcode.sext:
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrArgSize sizeFrom = getTypeArgSize(typeFrom, lir, context);
					IrArgSize sizeTo = instrHeader.argSize;
					final switch(sizeFrom) with(IrArgSize)
					{
						case size8:
							switch(sizeTo) with(IrArgSize)
							{
								case size16: emitLirInstr!(Amd64Opcode.movsx_btow); break;
								case size32: emitLirInstr!(Amd64Opcode.movsx_btod); break;
								case size64: emitLirInstr!(Amd64Opcode.movsx_btoq); break;
								default: context.internal_error("Invalid target size %s", sizeTo); break;
							}
							break;
						case size16:
							switch(sizeTo) with(IrArgSize)
							{
								case size32: emitLirInstr!(Amd64Opcode.movsx_wtod); break;
								case size64: emitLirInstr!(Amd64Opcode.movsx_wtoq); break;
								default: context.internal_error("Invalid target size %s", sizeTo); break;
							}
							break;
						case size32:
							context.assertf(sizeTo == size64, "Invalid target size %s", sizeTo);
							emitLirInstr!(Amd64Opcode.movsx_dtoq);
							break;
						case size64: context.internal_error("Invalid source size %s", sizeFrom); break;
					}
					break;
				case IrOpcode.zext:
					IrIndex typeFrom = getValueType(instrHeader.arg(ir, 0), ir, context);
					IrArgSize sizeFrom = getTypeArgSize(typeFrom, lir, context);
					IrArgSize sizeTo = instrHeader.argSize;
					final switch(sizeFrom) with(IrArgSize)
					{
						case size8:
							switch(sizeTo) with(IrArgSize)
							{
								case size16: emitLirInstr!(Amd64Opcode.movzx_btow); break;
								case size32: emitLirInstr!(Amd64Opcode.movzx_btod); break;
								case size64: emitLirInstr!(Amd64Opcode.movzx_btoq); break;
								default: context.internal_error("Invalid target size %s", sizeTo); break;
							}
							break;
						case size16:
							switch(sizeTo) with(IrArgSize)
							{
								case size32: emitLirInstr!(Amd64Opcode.movzx_wtod); break;
								case size64: emitLirInstr!(Amd64Opcode.movzx_wtoq); break;
								default: context.internal_error("Invalid target size %s", sizeTo); break;
							}
							break;
						case size32:
							context.assertf(sizeTo == size64, "Invalid target size %s", sizeTo);
							emitLirInstr!(Amd64Opcode.mov);
							break;
						case size64: context.internal_error("Invalid source size %s", sizeFrom); break;
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

				case IrOpcode.set_unary_cond:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize, type : type };
					InstrWithResult res = builder.emitInstr!(Amd64Opcode.set_unary_cond)(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)));
					recordIndex(instrHeader.result(ir), res.result);
					break;

				case IrOpcode.set_binary_cond:
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;
					ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize, type : type };
					InstrWithResult res = builder.emitInstr!(Amd64Opcode.set_binary_cond)(lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), getFixedIndex(instrHeader.arg(ir, 1)));
					recordIndex(instrHeader.result(ir), res.result);
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
					IrIndex instruction = builder.emitInstr!(Amd64Opcode.bin_branch)(
						lirBlockIndex, extra, getFixedIndex(instrHeader.arg(ir, 0)), getFixedIndex(instrHeader.arg(ir, 1)));
					break;

				case IrOpcode.ret:
					builder.emitInstr!(Amd64Opcode.ret)(lirBlockIndex);
					break;

				case IrOpcode.ret_val:
					IrIndex result = lir.backendData.getCallConv(context).returnReg;
					auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
					IrIndex type = lirFuncType.resultTypes[0];
					result.physRegSize = typeToRegSize(type, context);
					ExtraInstrArgs extra = { addUsers : false, result : result };

					if (numHiddenParams == 1) {
						// store struct into pointer, then return pointer
						IrIndex valType = context.types.getPointerBaseType(type);
						genStore(hiddenParameter, 0, instrHeader.arg(ir, 0), 0, valType, lirBlockIndex, ir);
						builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, hiddenParameter);
					} else {
						IrIndex itemToReturn = getFixedIndex(instrHeader.arg(ir, 0));
						itemToReturn = simplifyConstant(itemToReturn, context);
						builder.emitInstr!(Amd64Opcode.mov)(lirBlockIndex, extra, itemToReturn);
					}

					IrIndex instruction = builder.emitInstr!(Amd64Opcode.ret)(lirBlockIndex);
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
