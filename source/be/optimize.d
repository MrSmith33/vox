/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module be.optimize;

import std.stdio;
import all;

alias FuncPassIr = void function(ref CompilationContext, ref IrFunction, ref IrBuilder);
alias FuncPass = void function(ref CompilationContext, ref IrFunction);

void apply_lir_func_pass(ref CompilationContext context, FuncPass pass)
{
	foreach (ref SourceFileInfo file; context.files.data)
	foreach (IrFunction* lir; file.mod.lirModule.functions) {
		pass(context, *lir);
		if (context.validateIr)
			validateIrFunction(context, *lir);
	}
}

void pass_optimize_ir(ref CompilationContext context, ref ModuleDeclNode mod, ref FunctionDeclNode func)
{
	if (func.isExternal) return;

	FuncPassIr[] passes = [&func_pass_invert_conditions, &func_pass_remove_dead_code, &func_pass_lower_gep];
	IrBuilder builder;

	builder.beginDup(func.backendData.irData, &context);
	foreach (FuncPassIr pass; passes) {
		pass(context, *func.backendData.irData, builder);
		if (context.validateIr)
			validateIrFunction(context, *func.backendData.irData);
		if (context.printIrOpt) dumpFunction(*func.backendData.irData, context);
	}
}

void func_pass_invert_conditions(ref CompilationContext context, ref IrFunction ir, ref IrBuilder builder)
{
	ir.assignSequentialBlockIndices();

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		if (!block.lastInstr.isDefined) continue;

		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(block.lastInstr);
		ubyte invertedCond;

		switch(instrHeader.op) with(IrOpcode)
		{
			case block_exit_unary_branch:
				invertedCond = invertUnaryCond(cast(IrUnaryCondition)instrHeader.cond);
				break;
			case block_exit_binary_branch:
				invertedCond = invertBinaryCond(cast(IrBinaryCondition)instrHeader.cond);
				break;

			default: continue;
		}

		uint seqIndex0 = ir.getBlock(block.successors[0, ir]).seqIndex;
		uint seqIndex1 = ir.getBlock(block.successors[1, ir]).seqIndex;
		if (block.seqIndex + 1 == seqIndex0)
		{
			instrHeader.cond = invertedCond;
			IrIndex succIndex0 = block.successors[0, ir];
			IrIndex succIndex1 = block.successors[1, ir];
			block.successors[0, ir] = succIndex1;
			block.successors[1, ir] = succIndex0;
		}
	}
}

void func_pass_remove_dead_code(ref CompilationContext context, ref IrFunction ir, ref IrBuilder builder)
{
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(ir))
		{
			if (hasSideEffects(cast(IrOpcode)instrHeader.op)) continue;
			if (!instrHeader.hasResult) continue;

			context.assertf(instrHeader.result.isVirtReg, "instruction result must be virt reg");
			if (ir.getVirtReg(instrHeader.result).users.length > 0) continue;

			// we found some dead instruction, remove it
			foreach(ref IrIndex arg; instrHeader.args) {
				removeUser(context, ir, instrIndex, arg);
			}
			removeInstruction(ir, instrIndex);
			//writefln("remove dead %s", instrIndex);
		}
	}
}

// TODO some typecasts are needed for correct typing
void lowerGEP(ref CompilationContext context, ref IrBuilder builder, IrIndex instrIndex, ref IrInstrHeader instrHeader)
{
	IrIndex buildOffset(IrIndex basePtr, long offsetVal, IrIndex resultType) {
		if (offsetVal == 0) {
			// Shortcut for 0-th index
			IrIndex basePtrType = getValueType(basePtr, *builder.ir, context);
			if (basePtrType == resultType) return basePtr;

			ExtraInstrArgs extra = { type : resultType };
			InstrWithResult instr = builder.emitInstr!IrInstr_conv(extra, basePtr);
			builder.insertBeforeInstr(instrIndex, instr.instruction);
			return instr.result;
		} else {
			IrIndex offset = context.constants.add(offsetVal, IsSigned.yes);

			ExtraInstrArgs extra = { type : resultType };
			InstrWithResult addressInstr = builder.emitInstr!IrInstr_add(extra, basePtr, offset);
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
			InstrWithResult offsetInstr = builder.emitInstr!IrInstr_umul(extra1, index, scale);
			builder.insertBeforeInstr(instrIndex, offsetInstr.instruction);
			indexVal = offsetInstr.result;
		}

		ExtraInstrArgs extra2 = { type : resultType };
		InstrWithResult addressInstr = builder.emitInstr!IrInstr_add(extra2, basePtr, indexVal);
		builder.insertBeforeInstr(instrIndex, addressInstr.instruction);

		return addressInstr.result;
	}

	IrIndex aggrPtr = instrHeader.args[0]; // aggregate ptr
	IrIndex aggrPtrType = getValueType(aggrPtr, *builder.ir, context);

	context.assertf(aggrPtrType.isTypePointer,
		"First argument to GEP instruction must be pointer, not %s", aggrPtr.typeKind);

	IrIndex aggrType = context.types.getPointerBaseType(aggrPtrType);
	uint aggrSize = context.types.typeSize(aggrType);

	IrIndex firstIndex = instrHeader.args[1];

	if (firstIndex.isConstant) {
		long indexVal = context.constants.get(firstIndex).i64;
		long offset = indexVal * aggrSize;
		aggrPtr = buildOffset(aggrPtr, offset, aggrPtrType);
	} else {
		aggrPtr = buildIndex(aggrPtr, firstIndex, aggrSize, aggrPtrType);
	}

	foreach(IrIndex memberIndex; instrHeader.args[2..$])
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

				if (memberIndex.isConstant) {
					long indexVal = context.constants.get(memberIndex).i64;
					long offset = indexVal * elemSize;
					aggrPtr = buildOffset(aggrPtr, offset, elemPtrType);
				} else {
					aggrPtr = buildIndex(aggrPtr, memberIndex, elemSize, elemPtrType);
				}

				aggrType = elemType;
				break;

			case IrTypeKind.struct_t:
				context.assertf(memberIndex.isConstant, "Structs can only be indexed with constants, not with %s", memberIndex);

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

	builder.redirectVregUsersTo(instrHeader.result, aggrPtr);
	removeInstruction(*builder.ir, instrIndex);
}

void func_pass_lower_gep(ref CompilationContext context, ref IrFunction ir, ref IrBuilder builder)
{
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			if (cast(IrOpcode)instrHeader.op == IrOpcode.get_element_ptr) {
				lowerGEP(context, builder, instrIndex, instrHeader);
			}
		}
	}
}

/*
void lir_func_pass_simplify(ref CompilationContext context, ref IrFunction ir)
{
	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocksReverse)
	{
		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructionsReverse(ir))
		{
			switch(cast(Amd64Opcode)instrHeader.op) with(Amd64Opcode)
			{
				case mov:
					static assert(LirAmd64Instr_xor.sizeof == LirAmd64Instr_mov.sizeof);
					// replace 'mov reg, 0' with xor reg reg
					IrIndex dst = instrHeader.result;
					IrIndex src = instrHeader.args[0];
					if (src.isConstant && context.constants.get(src).i64 == 0)
					{

					}
				default: break;
			}
		}
	}
}
*/
void pass_optimize_lir(ref CompilationContext context)
{
	apply_lir_func_pass(context, &pass_optimize_lir_func);
}

void pass_optimize_lir_func(ref CompilationContext context, ref IrFunction ir)
{
	ir.assignSequentialBlockIndices();

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		if (!block.lastInstr.isDefined) continue;

		IrInstrHeader* instrHeader = &ir.get!IrInstrHeader(block.lastInstr);
		auto isJump = context.machineInfo.instrInfo[instrHeader.op].isJump;

		if (isJump)
		{
			uint seqIndex0 = ir.getBlock(block.successors[0, ir]).seqIndex;
			// successor is the next instruction after current block
			if (block.seqIndex + 1 == seqIndex0)
			{
				removeInstruction(ir, block.lastInstr);
			}
		}
	}
}
