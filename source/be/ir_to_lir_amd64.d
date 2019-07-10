/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.ir_to_lir_amd64;

import std.stdio;
import all;


void pass_ir_to_lir_amd64(ref CompilationContext context, ref ModuleDeclNode mod, ref FunctionDeclNode func)
{
	auto pass = IrToLir(&context);
	pass.run(mod, func);
}

struct IrToLir
{
	CompilationContext* context;
	IrBuilder builder;

	void run(ref ModuleDeclNode mod, ref FunctionDeclNode func)
	{
		if (func.isExternal) return;

		func.backendData.lirData = context.appendAst!IrFunction;
		func.backendData.lirData.backendData = &func.backendData;

		mod.lirModule.addFunction(*context, func.backendData.lirData);
		processFunc(*func.backendData.irData, *func.backendData.lirData);
		if (context.validateIr) validateIrFunction(*context, *func.backendData.lirData);
		if (context.printLir && context.printDumpOf(&func)) dumpFunction(*func.backendData.lirData, *context);
	}

	void processFunc(ref IrFunction ir, ref IrFunction lir)
	{
		//writefln("IR to LIR %s", context.idString(ir.name));
		lir.instructionSet = IrInstructionSet.lir_amd64;

		context.tempBuffer.clear;

		builder.beginLir(&lir, &ir, context);

		// Mirror of original IR, we will put the new IrIndex of copied entities there
		// and later use this info to rewire all connections between basic blocks
		IrMirror!IrIndex mirror;
		mirror.create(context, &ir);

		// save map from old index to new index
		void recordIndex(IrIndex oldIndex, IrIndex newIndex)
		{
			assert(oldIndex.isDefined);
			assert(newIndex.isDefined);
			mirror[oldIndex] = newIndex;
		}

		IrIndex getFixedIndex(IrIndex index)
		{
			assert(index.isDefined);
			if (index.isBasicBlock || index.isVirtReg || index.isPhi || index.isInstruction) {
				return mirror[index];
			}
			return index;
		}

		void fixIndex(ref IrIndex index)
		{
			assert(index.isDefined);
			if (index.isBasicBlock || index.isVirtReg || index.isPhi || index.isInstruction) {
				//writefln("%s -> %s", index, mirror[index]);
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
		void genStore(IrIndex lirPtr, uint offset, IrIndex irValue, uint fromOffset, IrIndex valueType, IrIndex lirBlockIndex, ref IrFunction ir)
		{
			//writefln("genStore %s %s %s %s %s", lirPtr, offset, irValue, fromOffset, IrIndexDump(valueType, *context, ir));
			IrIndex srcType = getValueType(irValue, ir, *context);
			IrIndex dstType = getValueType(lirPtr, lir, *context);
			context.assertf(dstType.isTypePointer, "%s", IrIndexDump(dstType, *context, lir));
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
										genStore(lirPtr, offset + member.offset, instr.args[0], fromOffset + member.offset, member.type, lirBlockIndex, ir);
									}
									return;

								case IrOpcode.create_aggregate:
									members = instr.args;
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

		IrIndex prevBlock;
		// dup basic blocks
		foreach (IrIndex blockIndex, ref IrBasicBlock irBlock; ir.blocks)
		{
			++lir.numBasicBlocks;
			IrIndex lirBlock = builder.append!IrBasicBlock;
			recordIndex(blockIndex, lirBlock);

			lir.getBlock(lirBlock).name = irBlock.name;
			lir.getBlock(lirBlock).isLoopHeader = irBlock.isLoopHeader;

			foreach(IrIndex pred; irBlock.predecessors.range(ir)) {
				lir.getBlock(lirBlock).predecessors.append(&builder, pred);
			}
			foreach(IrIndex succ; irBlock.successors.range(ir)) {
				lir.getBlock(lirBlock).successors.append(&builder, succ);
			}

			// temporarily store link to old block
			lir.getBlock(lirBlock).firstInstr = blockIndex;

			if (blockIndex == ir.entryBasicBlock) {
				lir.entryBasicBlock = lirBlock;
			} else if (blockIndex == ir.exitBasicBlock) {
				lir.exitBasicBlock = lirBlock;
				lir.getBlock(lirBlock).prevBlock = prevBlock;
				lir.getBlock(prevBlock).nextBlock = lirBlock;
			} else {
				lir.getBlock(lirBlock).prevBlock = prevBlock;
				lir.getBlock(prevBlock).nextBlock = lirBlock;
			}

			// Add phis with old args
			foreach(IrIndex phiIndex, ref IrPhi phi; irBlock.phis(ir))
			{
				IrVirtualRegister* oldReg = &ir.getVirtReg(phi.result);
				IrIndex newPhi = builder.addPhi(lirBlock, oldReg.type, phi.var);
				recordIndex(phiIndex, newPhi);
				IrIndex newResult = lir.getPhi(newPhi).result;
				IrVirtualRegister* newReg = &lir.getVirtReg(newResult);
				newReg.type = oldReg.type;

				recordIndex(phi.result, lir.getPhi(newPhi).result);
				foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
				{
					builder.addPhiArg(newPhi, phiArg.basicBlock, phiArg.value);
				}
			}

			prevBlock = lirBlock;
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
			lir.type = context.types.appendFuncSignature(0, irFuncType.numParameters);
			auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
			paramTypes = lirFuncType.parameterTypes;
		}
		else if (irFuncType.numResults == 1)
		{
			IrIndex resType = irFuncType.resultTypes[0];
			if (isPassByValue(resType))
			{
				lir.type = context.types.appendFuncSignature(1, irFuncType.numParameters);
				auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
				lirFuncType.resultTypes[0] = resType;
				paramTypes = lirFuncType.parameterTypes;
			}
			else
			{
				numHiddenParams = 1;
				lir.type = context.types.appendFuncSignature(1, irFuncType.numParameters + 1);
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

		//writefln("%s", IrIndexDump(lir.type, *context, lir));

		// buffer for call/instruction arguments
		enum MAX_ARGS = 255;
		IrIndex[MAX_ARGS] argBuffer;

		// fix successors predecessors links
		foreach (IrIndex lirBlockIndex, ref IrBasicBlock lirBlock; lir.blocks)
		{
			lirBlock.isSealed = true;
			lirBlock.isFinished = true;

			foreach(ref pred; lirBlock.predecessors.range(lir)) fixIndex(pred);
			foreach(ref succ; lirBlock.successors.range(lir)) fixIndex(succ);

			// get link to the old block and null it
			IrIndex irBlockIndex = lirBlock.firstInstr;
			lirBlock.firstInstr = IrIndex();

			// Add hidden parameter(s) to first block
			if (irBlockIndex == ir.entryBasicBlock && numHiddenParams == 1)
			{
				auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
				IrIndex type = lirFuncType.parameterTypes[0];
				IrIndex paramReg = lir.backendData.callingConvention.paramsInRegs[0];
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

					IrIndex[getInstrInfo!I.numArgs] fixedArgs = instrHeader.args;
					foreach(ref arg; fixedArgs) fixIndex(arg);

					static if (getInstrInfo!I.hasResult)
					{
						IrIndex type = ir.getVirtReg(instrHeader.result).type;
						ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize, type : type };
						InstrWithResult res = builder.emitInstr!I(lirBlockIndex, extra, fixedArgs);
						recordIndex(instrIndex, res.instruction);
						recordIndex(instrHeader.result, res.result);
					}
					else
					{
						ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize };
						IrIndex res = builder.emitInstr!I(lirBlockIndex, extra, fixedArgs);
						recordIndex(instrIndex, res);
					}
				}

				void makeMov(IrIndex to, IrIndex from, IrArgSize argSize) {
					ExtraInstrArgs extra = { addUsers : false, result : to, argSize : argSize };
					builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, getFixedIndex(from));
				}

				switch(instrHeader.op)
				{
					case IrOpcode.parameter:
						uint paramIndex = ir.get!IrInstr_parameter(instrIndex).index + numHiddenParams;
						context.assertf(paramIndex < lir.backendData.callingConvention.paramsInRegs.length,
							"Only parameters passed through registers are implemented");

						IrIndex paramReg = lir.backendData.callingConvention.paramsInRegs[paramIndex];
						IrIndex type = ir.getVirtReg(instrHeader.result).type;
						paramReg.physRegSize = typeToRegSize(type, context);
						ExtraInstrArgs extra = { type : type, argSize : instrHeader.argSize };
						InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, paramReg);
						recordIndex(instrIndex, instr.instruction);
						recordIndex(instrHeader.result, instr.result);
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

						enum STACK_ITEM_SIZE = 8;
						size_t numArgs = instrHeader.args.length;
						size_t numParamsInRegs = lir.backendData.callingConvention.paramsInRegs.length;
						// how many bytes are allocated on the stack before func call
						size_t stackReserve = max(numArgs, numParamsInRegs) * STACK_ITEM_SIZE;
						IrIndex stackPtrReg = lir.backendData.callingConvention.stackPointer;

						// Copy args to stack if necessary (big structs or doesn't fit into regs)
						foreach (i, IrIndex irArg; instrHeader.args)
						{
							IrIndex type = ir.getValueType(*context, irArg);

							if (isPassByValue(type)) {
								argBuffer[i] = getFixedIndex(irArg);
							} else {
								//allocate stack slot, store value there and use slot pointer as argument
								argBuffer[i] = lir.backendData.stackLayout.addStackItem(context, type, StackSlotKind.local, 0);
								genStore(argBuffer[i], 0, irArg, 0, type, lirBlockIndex, ir);
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
							foreach_reverse (IrIndex lirArg; argBuffer[numParamsInRegs..numArgs])
							{
								if (lirArg.isStackSlot) {
									ExtraInstrArgs extra = { addUsers : false, type : lir.getValueType(*context, lirArg) };
									InstrWithResult movInstr = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, lirArg);
									lirArg = movInstr.result;
								}

								ExtraInstrArgs extra = { addUsers : false };
								builder.emitInstr!LirAmd64Instr_push(lirBlockIndex, extra, lirArg);
							}
						}

						// move args to registers
						size_t numPhysRegs = min(numParamsInRegs, numArgs);
						foreach_reverse (i, IrIndex lirArg; argBuffer[0..numPhysRegs])
						{
							IrIndex type = lir.getValueType(*context, lirArg);
							IrIndex argRegister = lir.backendData.callingConvention.paramsInRegs[i];
							argRegister.physRegSize = typeToRegSize(type, context);
							argBuffer[i] = argRegister;
							ExtraInstrArgs extra = {addUsers : false, result : argRegister};
							builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, lirArg);
						}

						{	// Allocate shadow space for 4 physical registers
							IrIndex const_32 = context.constants.add(32, IsSigned.no);
							ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
							builder.emitInstr!LirAmd64Instr_sub(
								lirBlockIndex, extra, stackPtrReg, const_32);
						}

						{	// call
							ExtraInstrArgs callExtra = {
								addUsers : false,
								hasResult : instrHeader.hasResult,
								result : lir.backendData.callingConvention.returnReg // will be used if function has result
							};

							FunctionIndex calleeIndex = instrHeader.preheader!IrInstrPreheader_call.calleeIndex;
							builder.emitInstrPreheader(IrInstrPreheader_call(calleeIndex));
							InstrWithResult callInstr = builder.emitInstr!LirAmd64Instr_call(
								lirBlockIndex, callExtra, argBuffer[0..numPhysRegs]);
							recordIndex(instrIndex, callInstr.instruction);

							{	// Deallocate stack after call
								IrIndex conReservedBytes = context.constants.add(stackReserve, IsSigned.no);
								ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
								builder.emitInstr!LirAmd64Instr_add(
									lirBlockIndex, extra, stackPtrReg, conReservedBytes);
							}

							if (callExtra.hasResult) {
								// mov result to virt reg
								ExtraInstrArgs extra = { type : ir.getVirtReg(instrHeader.result).type };
								InstrWithResult movInstr = builder.emitInstr!LirAmd64Instr_mov(
									lirBlockIndex, extra, lir.backendData.callingConvention.returnReg);
								recordIndex(instrHeader.result, movInstr.result);
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
						makeMov(dividendBottom, instrHeader.args[0], instrHeader.argSize);

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
						IrIndex divisor = instrHeader.args[1];
						if (instrHeader.args[1].isConstant) {
							ExtraInstrArgs extra = { addUsers : false, type : getValueType(divisor, ir, *context) };
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
						recordIndex(instrIndex, res.instruction);

						// copy result (quotient)
						ExtraInstrArgs extra4 = { addUsers : false, argSize : instrHeader.argSize, type : ir.getVirtReg(instrHeader.result).type };
						InstrWithResult movResult = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra4, resultReg);
						recordIndex(instrHeader.result, movResult.result);
						break;

					case IrOpcode.shl, IrOpcode.lshr, IrOpcode.ashr:
						// TODO: check if right operand is constant and pass it as is. Recognize constants in codegen
						IrIndex rightArg = amd64_reg.cx;
						rightArg.physRegSize = ArgType.BYTE;
						makeMov(rightArg, instrHeader.args[1], instrHeader.argSize);
						IrIndex type = ir.getVirtReg(instrHeader.result).type;
						ExtraInstrArgs extra = { addUsers : false, argSize : instrHeader.argSize, type : type };
						InstrWithResult res;
						switch(instrHeader.op) {
							case IrOpcode.shl:
								res = builder.emitInstr!LirAmd64Instr_shl(lirBlockIndex, extra, getFixedIndex(instrHeader.args[0]), rightArg);
								break;
							case IrOpcode.lshr:
								res = builder.emitInstr!LirAmd64Instr_shr(lirBlockIndex, extra, getFixedIndex(instrHeader.args[0]), rightArg);
								break;
							case IrOpcode.ashr:
								res = builder.emitInstr!LirAmd64Instr_sar(lirBlockIndex, extra, getFixedIndex(instrHeader.args[0]), rightArg);
								break;
							default: assert(false);
						}
						recordIndex(instrIndex, res.instruction);
						recordIndex(instrHeader.result, res.result);
						break;
					case IrOpcode.load: emitLirInstr!LirAmd64Instr_load; break;
					case IrOpcode.store:
						IrIndex type = ir.getValueType(*context, instrHeader.args[1]);
						genStore(getFixedIndex(instrHeader.args[0]), 0, instrHeader.args[1], 0, type, lirBlockIndex, ir);
						break;
					case IrOpcode.conv:
						IrIndex typeFrom = getValueType(instrHeader.args[0], ir, *context);
						IrIndex typeTo = ir.getVirtReg(instrHeader.result).type;
						uint typeSizeFrom = context.types.typeSize(typeFrom);
						uint typeSizeTo = context.types.typeSize(typeTo);
						context.assertf(typeSizeTo <= typeSizeFrom,
							"Can't cast from %s bytes to %s bytes", typeSizeFrom, typeSizeTo);
						emitLirInstr!LirAmd64Instr_mov;
						break;

					case IrOpcode.set_unary_cond:
						IrIndex type = ir.getVirtReg(instrHeader.result).type;
						ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize, type : type };
						InstrWithResult res = builder.emitInstr!LirAmd64Instr_set_unary_cond(lirBlockIndex, extra, getFixedIndex(instrHeader.args[0]));
						recordIndex(instrIndex, res.instruction);
						recordIndex(instrHeader.result, res.result);
						break;

					case IrOpcode.set_binary_cond:
						IrIndex type = ir.getVirtReg(instrHeader.result).type;
						ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize, type : type };
						InstrWithResult res = builder.emitInstr!LirAmd64Instr_set_binary_cond(lirBlockIndex, extra, getFixedIndex(instrHeader.args[0]), getFixedIndex(instrHeader.args[1]));
						recordIndex(instrIndex, res.instruction);
						recordIndex(instrHeader.result, res.result);
						break;

					case IrOpcode.block_exit_jump:
						builder.emitInstr!LirAmd64Instr_jmp(lirBlockIndex);
						break;

					case IrOpcode.block_exit_unary_branch:
						ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize };
						IrIndex instruction = builder.emitInstr!LirAmd64Instr_un_branch(
							lirBlockIndex, extra, getFixedIndex(instrHeader.args[0]));
						recordIndex(instrIndex, instruction);
						break;

					case IrOpcode.block_exit_binary_branch:
						ExtraInstrArgs extra = { addUsers : false, cond : instrHeader.cond, argSize : instrHeader.argSize };
						IrIndex instruction = builder.emitInstr!LirAmd64Instr_bin_branch(
							lirBlockIndex, extra, getFixedIndex(instrHeader.args[0]), getFixedIndex(instrHeader.args[1]));
						recordIndex(instrIndex, instruction);
						break;

					case IrOpcode.block_exit_return_void:
						builder.emitInstr!LirAmd64Instr_return(lirBlockIndex);
						break;

					case IrOpcode.block_exit_return_value:
						IrIndex result = lir.backendData.callingConvention.returnReg;
						auto lirFuncType = &context.types.get!IrTypeFunction(lir.type);
						IrIndex type = lirFuncType.resultTypes[0];
						result.physRegSize = typeToRegSize(type, context);
						ExtraInstrArgs extra = { addUsers : false, result : result };

						if (numHiddenParams == 1) {
							// store struct into pointer, then return pointer
							IrIndex valType = context.types.getPointerBaseType(type);
							genStore(hiddenParameter, 0, instrHeader.args[0], 0, valType, lirBlockIndex, ir);
							builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, hiddenParameter);
						} else {
							builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, getFixedIndex(instrHeader.args[0]));
						}

						IrIndex instruction = builder.emitInstr!LirAmd64Instr_return(lirBlockIndex);
						recordIndex(instrIndex, instruction);
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
				foreach(ref IrIndex arg; instrHeader.args)
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
					fixIndex(phiArg.basicBlock);
					fixIndex(phiArg.value);
					builder.addUser(phiIndex, phiArg.value);
				}
			}
		}

		//dumpFunction(lir, *context); // uncomment to see generated LIR before fixing

		foreach (IrIndex blockIndex, ref IrBasicBlock lirBlock; lir.blocks)
		{
			fixInstrs(blockIndex, lirBlock);
			fixPhis(blockIndex, lirBlock);
		}
	}
}
