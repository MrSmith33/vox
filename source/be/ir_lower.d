/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.

// Missing case of System V ABI implementation:
// - Aggregates with misaligned members
// - Aggregates with alignment > 16 bytes
// - xmm/ymm/zmm register passing
// - x87
module be.ir_lower;

import std.bitmanip : bitfields;
import std.stdio;
import all;


void pass_ir_lower(CompilationContext* c, ModuleDeclNode* mod, FunctionDeclNode* func)
{
	IrFunction* optimizedIrData = c.getAst!IrFunction(func.backendData.optimizedIrData);
	func.backendData.loweredIrData = c.appendAst!IrFunction;
	IrFunction* loweredIrData = c.getAst!IrFunction(func.backendData.loweredIrData);
	*loweredIrData = *optimizedIrData; // copy

	scope(failure) {
		c.currentFunction = func;
	}

	IrBuilder builder;
	builder.beginDup(loweredIrData, c);
	IrIndex funcIndex = func.getIrIndex(c);

	void doPass(FuncPassIr pass) {
		pass(c, loweredIrData, funcIndex, builder);
		if (c.validateIr)
			validateIrFunction(c, loweredIrData);
		if (c.printIrLowerEach && c.printDumpOf(func)) dumpFunction(c, loweredIrData, "IR lowering each");
	}

	doPass(&func_pass_lower_abi);
	doPass(&func_pass_lower_aggregates);
	doPass(&func_pass_lower_gep);

	if (!c.printIrLowerEach && c.printIrLower && c.printDumpOf(func)) dumpFunction(c, loweredIrData, "IR lowering all");
	builder.finalizeIr;
}

bool fitsIntoRegister(IrIndex type, CompilationContext* c) {
	if (type.isTypeStruct || type.isTypeArray) {
		switch(c.types.typeSize(type)) {
			case 1: return true;
			case 2: return true;
			case 4: return true;
			case 8: return true;
			default: return false;
		}
	}
	return true;
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
	return c.constants.add(data.bufferValue, IsSigned.no, sizeToIrArgSize(typeSize, c));
}

IrIndex genAddressOffset(IrIndex ptr, uint offset, IrIndex ptrType, IrIndex beforeInstr, ref IrBuilder builder) {
	if (offset == 0) {
		ExtraInstrArgs extra = { type : ptrType };
		InstrWithResult movInstr = builder.emitInstrBefore!(IrOpcode.move)(beforeInstr, extra, ptr);
		return movInstr.result;
	} else {
		IrIndex offsetIndex = builder.context.constants.add(offset, IsSigned.no);
		ExtraInstrArgs extra = { type : ptrType };
		InstrWithResult addressInstr = builder.emitInstrBefore!(IrOpcode.add)(beforeInstr, extra, ptr, offsetIndex);
		return addressInstr.result;
	}
}

IrIndex genCopy(IrIndex dst, IrIndex src, IrIndex beforeInstr, ref IrBuilder builder) {
	if (src.isSomeConstant)
		return builder.emitInstrBefore!(IrOpcode.store)(beforeInstr, ExtraInstrArgs(), dst, src);
	else
		return builder.emitInstrBefore!(IrOpcode.copy)(beforeInstr, ExtraInstrArgs(), dst, src);
}

IrIndex genLoad(IrIndex ptr, uint offset, IrIndex ptrType, IrIndex beforeInstr, ref IrBuilder builder) {
	ptr = genAddressOffset(ptr, offset, ptrType, beforeInstr, builder);
	IrIndex valType = builder.context.types.getPointerBaseType(ptrType);
	IrArgSize argSize = typeToIrArgSize(valType, builder.context);
	ExtraInstrArgs extra = { type : valType, argSize : argSize };
	auto instr = builder.emitInstrBefore!(IrOpcode.load)(beforeInstr, extra, ptr);
	return instr.result;
}

struct LowerVreg
{
	IrIndex redirectTo;
}

void func_pass_lower_aggregates(CompilationContext* c, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	//writefln("lower_aggregates %s", c.idString(ir.name));

	// buffer for call/instruction arguments
	enum MAX_ARGS = 255;
	IrIndex[MAX_ARGS] argBuffer = void;

	LowerVreg[] vregInfos = makeParallelArray!LowerVreg(c, ir.numVirtualRegisters);

	foreach (IrIndex vregIndex, ref IrVirtualRegister vreg; ir.virtualRegisters)
	{
		if (vreg.type.isTypeStruct || vreg.type.isTypeArray)
		{
			//writefln("- vreg %s", vregIndex);

			IrInstrHeader* definition = ir.getInstr(vreg.definition);
			if (definition.op == IrOpcode.load_aggregate)
			{
				// we can omit stack allocation and reuse source memory
				if (definition.isUniqueLoad)
				{
					vregInfos[vregIndex.storageUintIndex].redirectTo = definition.arg(ir, 0);
					removeInstruction(ir, vreg.definition);
				}
			}
		}
	}

	// transforms instructions
	// gathers all registers to be promoted to pointer
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			IrIndex type = ir.getVirtReg(phi.result).type;
			if (!type.fitsIntoRegister(c)) {
				//writefln("- phi %s", phiIndex);
			}
			foreach(size_t arg_i, ref IrIndex phiArg; phi.args(ir))
			{
			}
		}

		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			switch(instrHeader.op)
			{
				case IrOpcode.store:
					IrIndex ptr = instrHeader.arg(ir, 0);
					IrIndex val = instrHeader.arg(ir, 1);
					if (ptr.isPhysReg || val.isPhysReg) break;

					//writefln("- store %s %s %s", instrIndex, ptr, val);
					IrIndex ptrType = ir.getValueType(c, ptr);
					IrIndex valType = ir.getValueType(c, val);

					// value will be replaced with pointer, replace store with copy
					if (!valType.fitsIntoRegister(c) && !val.isSomeConstant)
					{
						instrHeader.op = IrOpcode.copy;
					}
					break;

				case IrOpcode.load_aggregate:
					//writefln("- load_aggregate %s", instrIndex);
					IrIndex ptr = instrHeader.arg(ir, 0);
					IrIndex ptrType = ir.getValueType(c, ptr);
					IrIndex base = c.types.getPointerBaseType(ptrType);

					if (base.fitsIntoRegister(c))
					{
						IrArgSize argSize = typeToIrArgSize(base, c);
						ExtraInstrArgs extra = { result : instrHeader.result(ir), argSize : argSize };
						builder.emitInstrBefore!(IrOpcode.load)(instrIndex, extra, ptr);
					}
					else
					{
						IrIndex slot = builder.appendStackSlot(base, c.types.typeSizeAndAlignment(base), StackSlotKind.local);
						genCopy(slot, instrHeader.arg(ir, 0), instrIndex, builder);

						vregInfos[instrHeader.result(ir).storageUintIndex].redirectTo = slot;
					}
					removeInstruction(ir, instrIndex);
					break;

				case IrOpcode.create_aggregate:
					//writefln("- create_aggregate %s", instrIndex);
					IrIndex type = ir.getVirtReg(instrHeader.result(ir)).type;

					if (!type.fitsIntoRegister(c)) {
						IrTypeStruct* structType = &c.types.get!IrTypeStruct(type);
						IrIndex slot = builder.appendStackSlot(type, c.types.typeSizeAndAlignment(type), StackSlotKind.local);
						vregInfos[instrHeader.result(ir).storageUintIndex].redirectTo = slot;

						IrIndex[] members = instrHeader.args(ir);
						c.assertf(members.length == structType.numMembers, "%s != %s", members.length, structType.numMembers);

						foreach (i, IrTypeStructMember member; structType.members)
						{
							IrIndex ptrType = c.types.appendPtr(member.type);
							IrIndex ptr = genAddressOffset(slot, member.offset, ptrType, instrIndex, builder);
							if (member.type.fitsIntoRegister(c))
							{
								IrArgSize argSize = getTypeArgSize(member.type, c);
								ExtraInstrArgs extra = { argSize : argSize };
								builder.emitInstrBefore!(IrOpcode.store)(instrIndex, extra, ptr, members[i]);
							}
							else
							{
								builder.emitInstrBefore!(IrOpcode.copy)(instrIndex, ExtraInstrArgs(), ptr, members[i]);
							}
						}
						//convertAggregateVregToPointer(instrHeader.result(ir), ir, builder);
						removeInstruction(ir, instrIndex);
					}
					else
						createSmallAggregate(instrIndex, type, instrHeader, ir, builder);
					break;

				case IrOpcode.get_element:
					// if source is stored inside register - extract with bit manipulation, otherwise lower to GEP

					//writefln("- get_element %s", instrIndex);
					// instruction is reused

					IrIndex[] args = instrHeader.args(ir);
					IrIndex sourceType = getValueType(args[0], ir, c);
					IrTypeStructMember member = c.types.getAggregateMember(sourceType, c, args[1..$]);
					IrIndex resultType = member.type;
					uint resultSize = c.types.typeSize(resultType);

					if (sourceType.fitsIntoRegister(c))
					{
						// do simple variant where all indicies are constant
						IrIndex value = args[0];
						if (member.offset > 0)
						{
							// shift right
							IrIndex rightArg = c.constants.add(member.offset * 8, IsSigned.no);
							ExtraInstrArgs extra = { argSize : getTypeArgSize(sourceType, c), type : sourceType };
							value = builder.emitInstrBefore!(IrOpcode.lshr)(instrIndex, extra, value, rightArg).result;
						}

						// mask if not 1, 2, 4 or 8 bytes in size
						if (!resultType.fitsIntoRegister(c))
						{
							// and
							IrIndex mask = c.constants.add((1 << (resultSize * 8)) - 1, IsSigned.no);
							ExtraInstrArgs extra = { type : member.type };
							value = builder.emitInstrBefore!(IrOpcode.and)(instrIndex, extra, value, mask).result;
						}
						else
						{
							ExtraInstrArgs extra = { argSize : sizeToIrArgSize(resultSize, c), type : member.type };
							value = builder.emitInstrBefore!(IrOpcode.trunc)(instrIndex, extra, value).result;
						}

						vregInfos[instrHeader.result(ir).storageUintIndex].redirectTo = value;
						removeInstruction(ir, instrIndex);
						break;
					}

					// reuse the same indicies from get_element and perform GEP on them, then do load
					instrHeader.op = IrOpcode.get_element_ptr_0;

					if (resultType.fitsIntoRegister(c))
					{
						IrIndex loadResult = instrHeader.result(ir);
						IrIndex ptrType = c.types.appendPtr(resultType);
						IrIndex gepResult = builder.addVirtualRegister(instrIndex, ptrType);
						instrHeader.result(ir) = gepResult;

						ExtraInstrArgs extra2 = { argSize : getTypeArgSize(resultType, c), result : loadResult };
						IrIndex loadInstr = builder.emitInstr!(IrOpcode.load)(extra2, gepResult).instruction;
						builder.insertAfterInstr(instrIndex, loadInstr);
					}
					break;

				case IrOpcode.get_aggregate_slice:
					//writefln("- get_aggregate_slice %s", instrIndex);
					IrIndex[] args = instrHeader.args(ir);

					long indexVal = c.constants.get(args[1]).i64;
					IrIndex addr;
					if (indexVal == 0) {
						ExtraInstrArgs extra1 = { type : c.i64PtrType };
						InstrWithResult movInstr = builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra1, args[0]);
						addr = movInstr.result;
					} else {
						ExtraInstrArgs extra1 = { type : c.i64PtrType };
						InstrWithResult addressInstr = builder.emitInstrBefore!(IrOpcode.add)(instrIndex, extra1, args[0], args[1]);
						addr = addressInstr.result;
					}

					ExtraInstrArgs extra2 = { argSize : IrArgSize.size64, type : makeBasicTypeIndex(IrValueType.i64) };
					InstrWithResult loadInstr = builder.emitInstrBefore!(IrOpcode.load)(instrIndex, extra2, addr);

					instrHeader.numArgs = 1;
					instrHeader.op = IrOpcode.move;
					args[0] = loadInstr.result;
					builder.addUser(instrIndex, args[0]);
					break;
				case IrOpcode.insert_element:
					//writefln("- insert_element %s", instrIndex);
					break;

				case IrOpcode.branch_switch:
					// unroll switch into a chain of compare branches
					IrIndex[] args = instrHeader.args(ir);
					IrSmallArray successors = block.successors;
					block.successors = IrSmallArray.init;
					IrIndex[] succ = successors.data(ir);

					assert(args.length == succ.length);
					assert(args.length > 0);
					IrIndex value = args[0];
					IrIndex valueType = ir.getValueType(c, value);
					IrArgSize argSize = typeToIrArgSize(valueType, c);
					IrIndex defaultBlock = succ[0];

					// replace switch with branch to first case block
					ExtraInstrArgs extra = { cond : IrBinaryCondition.eq, argSize : argSize };
					IrIndex firstInstr = builder.emitInstr!(IrOpcode.branch_binary)(extra, value, args[1]);
					replaceInstruction(ir, instrIndex, firstInstr);
					block.successors.append(&builder, succ[1]);
					// predecessor is already correct for this block

					// build a chain
					IrIndex lastBlock = blockIndex;
					foreach(i; 2..args.length)
					{
						IrIndex branchBlockIndex = builder.addBasicBlock;
						IrBasicBlock* branchBlock = ir.getBlock(branchBlockIndex);

						builder.addBlockTarget(lastBlock, branchBlockIndex);
						ir.getBlock(succ[i]).predecessors[0, ir] = branchBlockIndex;
						branchBlock.successors.append(&builder, succ[i]);

						branchBlock.isSealed = true;
						branchBlock.isFinished = true;

						builder.emitInstr!(IrOpcode.branch_binary)(branchBlockIndex, extra, value, args[i]);
						moveBlockAfter(ir, branchBlockIndex, lastBlock);
						lastBlock = branchBlockIndex;
					}

					successors.free(ir);
					block.successors.append(&builder, succ[1]);

					ir.getBlock(lastBlock).successors.append(&builder, defaultBlock);
					ir.getBlock(defaultBlock).predecessors[0, ir] = lastBlock;
					break;

				default:
					//c.internal_error("IR lower unimplemented IR instr %s", cast(IrOpcode)instrHeader.op);
					break;
			}
		}
	}

	foreach(i, info; vregInfos)
	{
		if (info.redirectTo.isDefined)
		{
			builder.redirectVregUsersTo(IrIndex(cast(uint)i, IrValueKind.virtualRegister), info.redirectTo);
		}
	}
}

// pack values and constants into a register via `shift` and `binary or` instructions
void createSmallAggregate(IrIndex instrIndex, IrIndex type, ref IrInstrHeader instrHeader, IrFunction* ir, ref IrBuilder builder)
{
	CompilationContext* c = builder.context;

	uint targetTypeSize = c.types.typeSize(type);
	IrArgSize argSize = sizeToIrArgSize(targetTypeSize, c);
	c.assertf(instrHeader.numArgs <= 8, "too much args %s", instrHeader.numArgs);
	ulong constant = 0;
	// how many non-constants are prepared in argBuffer
	uint numBufferedValues = 0;

	IrIndex[2] argBuffer;

	void insertNonConstant(IrIndex value, uint bit_offset, uint size)
	{
		if (size < targetTypeSize) {
			ExtraInstrArgs extra = { argSize : argSize, type : type };
			switch(size) { // zero extend 8 and 16 bit args to 32bit
				case 1: value = builder.emitInstrBefore!(IrOpcode.zext)(instrIndex, extra, value).result; break;
				case 2: value = builder.emitInstrBefore!(IrOpcode.zext)(instrIndex, extra, value).result; break;
				default: break;
			}
		}

		// shift
		if (bit_offset == 0)
			argBuffer[numBufferedValues] = value;
		else
		{
			IrIndex rightArg = c.constants.add(bit_offset, IsSigned.no);
			ExtraInstrArgs extra1 = { argSize : argSize, type : type };
			IrIndex shiftRes = builder.emitInstrBefore!(IrOpcode.shl)(instrIndex, extra1, value, rightArg).result;
			argBuffer[numBufferedValues] = shiftRes;
		}
		++numBufferedValues;

		if (numBufferedValues == 2)
		{
			// or
			ExtraInstrArgs extra2 = { argSize : argSize, type : type };
			argBuffer[0] = builder.emitInstrBefore!(IrOpcode.or)(instrIndex, extra2, argBuffer[0], argBuffer[1]).result;
			numBufferedValues = 1;
		}
	}

	void insertAt(IrIndex value, uint offset, uint size)
	{
		if (value.isConstant) {
			constant |= c.constants.get(value).i64 << (offset * 8);
		} else {
			insertNonConstant(value, offset * 8, size);
		}
	}

	switch(type.typeKind) with(IrTypeKind) {
		case struct_t:
			IrTypeStruct* structType = &c.types.get!IrTypeStruct(type);
			IrIndex[] args = instrHeader.args(ir);
			foreach_reverse (i, IrTypeStructMember member; structType.members)
			{
				uint memberSize = c.types.typeSize(member.type);
				insertAt(args[i], member.offset, memberSize);
			}
			break;
		case array:
			IrTypeArray* arrayType = &c.types.get!IrTypeArray(type);
			uint elemSize = c.types.typeSize(arrayType.elemType);
			IrIndex[] args = instrHeader.args(ir);
			foreach_reverse (i; 0..arrayType.numElements)
			{
				insertAt(args[i], i * elemSize, elemSize);
			}
			break;
		default: assert(false);
	}

	IrIndex constIndex = c.constants.add(constant, IsSigned.no, argSize);
	IrIndex result;
	if (numBufferedValues == 1)
	{
		if (constant == 0)
		{
			result = argBuffer[0];
		}
		else
		{
			bool isBigConstant = c.constants.get(constIndex).payloadSize(constIndex) == IrArgSize.size64;

			if (isBigConstant)
			{
				// copy to temp register
				ExtraInstrArgs extra = { argSize : argSize, type : type };
				constIndex = builder.emitInstrBefore!(IrOpcode.move)(instrIndex, extra, constIndex).result;
			}

			ExtraInstrArgs extra3 = { argSize : argSize, type : type };
			result = builder.emitInstrBefore!(IrOpcode.or)(instrIndex, extra3, argBuffer[0], constIndex).result;
		}
	}
	else
	{
		result = constIndex;
	}
	builder.redirectVregUsersTo(instrHeader.result(ir), result);
	removeInstruction(ir, instrIndex);
}

void func_pass_lower_gep(CompilationContext* context, IrFunction* ir, IrIndex funcIndex, ref IrBuilder builder)
{
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			switch(cast(IrOpcode)instrHeader.op) with(IrOpcode)
			{
				case get_element_ptr, get_element_ptr_0:
					lowerGEP(context, builder, instrIndex, instrHeader);
					break;
				default: break;
			}
		}
	}
}

// TODO some typecasts are needed for correct typing
void lowerGEP(CompilationContext* context, ref IrBuilder builder, IrIndex instrIndex, ref IrInstrHeader instrHeader)
{
	IrIndex buildOffset(IrIndex basePtr, long offsetVal, IrIndex resultType) {
		if (offsetVal == 0) {
			// Shortcut for 0-th index
			IrIndex basePtrType = getValueType(basePtr, builder.ir, context);
			// TODO: prefer proper typing for now, until IR lowering is implemented
			if (basePtrType == resultType) return basePtr;

			ExtraInstrArgs extra = { type : resultType };
			InstrWithResult instr = builder.emitInstr!(IrOpcode.conv)(extra, basePtr);
			builder.insertBeforeInstr(instrIndex, instr.instruction);
			return instr.result;
		} else {
			IrIndex offset = context.constants.add(offsetVal, IsSigned.yes);

			ExtraInstrArgs extra = { type : resultType };
			InstrWithResult addressInstr = builder.emitInstr!(IrOpcode.add)(extra, basePtr, offset);
			builder.insertBeforeInstr(instrIndex, addressInstr.instruction);

			return addressInstr.result;
		}
	}

	IrIndex buildIndex(IrIndex basePtr, IrIndex index, uint elemSize, IrIndex resultType)
	{
		IrIndex scale = context.constants.add(elemSize, IsSigned.no);
		IrIndex indexVal = index;

		if (elemSize > 1) {
			ExtraInstrArgs extra1 = { type : makeBasicTypeIndex(IrValueType.i64) };
			InstrWithResult offsetInstr = builder.emitInstr!(IrOpcode.umul)(extra1, index, scale);
			builder.insertBeforeInstr(instrIndex, offsetInstr.instruction);
			indexVal = offsetInstr.result;
		}

		ExtraInstrArgs extra2 = { type : resultType };
		InstrWithResult addressInstr = builder.emitInstr!(IrOpcode.add)(extra2, basePtr, indexVal);
		builder.insertBeforeInstr(instrIndex, addressInstr.instruction);

		return addressInstr.result;
	}

	IrIndex aggrPtr = instrHeader.arg(builder.ir, 0); // aggregate ptr
	IrIndex aggrPtrType = getValueType(aggrPtr, builder.ir, context);

	context.assertf(aggrPtrType.isTypePointer,
		"First argument to GEP instruction must be pointer, not %s", aggrPtr.typeKind);

	IrIndex aggrType = context.types.getPointerBaseType(aggrPtrType);
	uint aggrSize = context.types.typeSize(aggrType);

	IrIndex[] args;

	// get_element_ptr_0 first index is zero, hence no op
	if (cast(IrOpcode)instrHeader.op == IrOpcode.get_element_ptr)
	{
		IrIndex firstIndex = instrHeader.arg(builder.ir, 1);

		if (firstIndex.isSimpleConstant) {
			long indexVal = context.constants.get(firstIndex).i64;
			long offset = indexVal * aggrSize;
			aggrPtr = buildOffset(aggrPtr, offset, aggrPtrType);
		} else {
			aggrPtr = buildIndex(aggrPtr, firstIndex, aggrSize, aggrPtrType);
		}

		args = instrHeader.args(builder.ir)[2..$]; // 0 is ptr, 1 is first index
	}
	else
	{
		args = instrHeader.args(builder.ir)[1..$]; // 0 is ptr
	}

	foreach(IrIndex memberIndex; args)
	{
		final switch(aggrType.typeKind)
		{
			case IrTypeKind.basic:
				context.internal_error("Cannot index basic type %s", aggrType.typeKind);
				break;

			case IrTypeKind.pointer:
				context.internal_error("Cannot index pointer with GEP instruction, use load first");
				break;

			case IrTypeKind.array:
				IrIndex elemType = context.types.getArrayElementType(aggrType);
				IrIndex elemPtrType = context.types.appendPtr(elemType);
				uint elemSize = context.types.typeSize(elemType);

				if (memberIndex.isSimpleConstant) {
					long indexVal = context.constants.get(memberIndex).i64;
					long offset = indexVal * elemSize;
					aggrPtr = buildOffset(aggrPtr, offset, elemPtrType);
				} else {
					aggrPtr = buildIndex(aggrPtr, memberIndex, elemSize, elemPtrType);
				}

				aggrType = elemType;
				break;

			case IrTypeKind.struct_t:
				context.assertf(memberIndex.isSimpleConstant, "Structs can only be indexed with constants, not with %s", memberIndex);

				long memberIndexVal = context.constants.get(memberIndex).i64;
				IrTypeStructMember[] members = context.types.get!IrTypeStruct(aggrType).members;

				context.assertf(memberIndexVal < members.length,
					"Indexing member %s of %s-member struct",
					memberIndexVal, members.length);

				IrTypeStructMember member = members[memberIndexVal];
				IrIndex memberPtrType = context.types.appendPtr(member.type);

				aggrPtr = buildOffset(aggrPtr, member.offset, memberPtrType);
				aggrType = member.type;
				break;

			case IrTypeKind.func_t:
				context.internal_error("Cannot index function type");
				break;
		}
	}

	builder.redirectVregUsersTo(instrHeader.result(builder.ir), aggrPtr);
	removeInstruction(builder.ir, instrIndex);
}
