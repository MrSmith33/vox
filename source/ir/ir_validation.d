///Copyright: Copyright (c) 2017-2019 Andrey Penechko.
///License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
///Authors: Andrey Penechko.

/// IR Validation routines
module ir.ir_validation;

import all;
import ir.ir_index;

///
void validateIrFunction(CompilationContext* context, IrFunction* ir)
{
	scope(failure) dumpFunction(context, ir);

	auto funcInstrInfos = allInstrInfos[ir.instructionSet];
	auto instrValidator = instrValidators[ir.instructionSet];

	foreach (IrIndex blockIndex, ref IrBasicBlock block; ir.blocks)
	{
		context.assertf(blockIndex.storageUintIndex < ir.numBasicBlocks, "basic block out of bounds %s", blockIndex);

		if (!block.isSealed)
		{
			context.internal_error("Unsealed basic block %s", blockIndex);
		}

		if (!block.isFinished)
		{
			context.internal_error("Unfinished basic block %s", blockIndex);
		}

		IrInstrHeader* firstInstr = &ir.get!IrInstrHeader(block.firstInstr);
		IrInstrHeader* lastInstr = &ir.get!IrInstrHeader(block.lastInstr);
		context.assertf(funcInstrInfos[lastInstr.op].isBlockExit,
			"Basic block %s does not end with jump, branch or return instruction",
			blockIndex);

		// Check that all users of virtual reg point to definition
		void checkArg(IrIndex argUser, IrIndex arg)
		{
			if (!arg.isVirtReg) return;

			IrVirtualRegister* vreg = &ir.getVirtReg(arg);

			// Check case when virtual register is in use,
			// but it's definition point is not set
			context.assertf(vreg.definition.isDefined,
				"Virtual register %s, invalid definition (%s)",
				arg, vreg.definition);

			// How many times 'argUser' is found in vreg.users
			uint numVregUses = 0;
			foreach (i, IrIndex user; vreg.users.range(ir))
				if (user == argUser)
					++numVregUses;

			// How many times 'args' is found in instr.args
			uint timesUsed = 0;

			if (argUser.isInstruction)
			{
				foreach (i, IrIndex instrArg; ir.get!IrInstrHeader(argUser).args(ir))
					if (instrArg == arg)
						++timesUsed;
			}
			else if (argUser.isPhi)
			{
				foreach(size_t arg_i, ref IrPhiArg phiArg; ir.getPhi(argUser).args(ir))
					if (phiArg.value == arg)
						++timesUsed;
			}
			else
			{
				context.internal_error("Virtual register cannot be used by %s", argUser.kind);
			}

			// For each use of arg by argUser there must one item in users list of vreg and in args list of user
			context.assertf(numVregUses == timesUsed,
				"Virtual register %s appears %s times as argument of %s, but instruction appears as user only %s times",
					arg, timesUsed, argUser, numVregUses);
		}

		void checkResult(IrIndex definition, IrIndex result)
		{
			if (!result.isVirtReg) return;

			IrVirtualRegister* vreg = &ir.getVirtReg(result);

			// Type must be set for every virtual register
			context.assertf(vreg.type.isType,
				"Virtual register %s, invalid type (%s)",
				result, vreg.type);

			// Check that all users of virtual reg point to definition
			context.assertf(vreg.definition == definition,
				"Virtual register %s definition %s doesn't match instruction %s",
				result, vreg.definition, definition);

			foreach (i, IrIndex user; vreg.users.range(ir))
				checkArg(user, result);
		}

		foreach(IrIndex phiIndex, ref IrPhi phi; block.phis(ir))
		{
			size_t numPhiArgs = 0;
			size_t numUniqueArgs = 0; // not an exact count, but precise in [0..2] range
			IrIndex uniqueValue;
			foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
			{
				++numPhiArgs;
				checkArg(phiIndex, phiArg.value);

				if (phiArg.value == uniqueValue || phiArg.value == phi.result) {
					continue;
				}
				// assignment will be done first time when uniqueValue is undefined and phiArg.value != phi.result
				// second time when phiArg.value != uniqueValue and phiArg.value != phi.result,
				// so, we are looking for numUniqueArgs > 1
				uniqueValue = phiArg.value;
				++numUniqueArgs;
			}

			// check that phi function is not redundant
			context.assertf(numUniqueArgs > 1, "%s is redundant", phiIndex);

			// TODO: check that all types of args match type of result

			// check that phi-function receives values from all predecessors
			size_t numPredecessors = 0;
			foreach(IrIndex predIndex; block.predecessors.range(ir))
			{
				context.assertf(predIndex.storageUintIndex < ir.numBasicBlocks, "basic block out of bounds %s", predIndex);
				++numPredecessors;
			}
			context.assertf(numPredecessors == block.predecessors.length,
				"Corrupted list of predecessors %s != %s",
				numPredecessors, block.predecessors.length);

			context.assertf(numPhiArgs == numPredecessors,
				"Number of predecessors: %s doesn't match number of phi arguments: %s",
				numPredecessors, numPhiArgs);

			checkResult(phiIndex, phi.result);
			//writefln("phi %s args %s preds %s", phiIndex, numPhiArgs, numPredecessors);
		}

		foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; block.instructions(ir))
		{
			foreach (i, IrIndex arg; instrHeader.args(ir))
			{
				checkArg(instrIndex, arg);
			}

			if (instrHeader.hasResult)
			{
				checkResult(instrIndex, instrHeader.result(ir));
			}

			if (funcInstrInfos[instrHeader.op].isBlockExit)
			{
				context.assertf(&instrHeader == lastInstr,
					"Branch %s is in the middle of basic block %s",
					instrIndex, blockIndex);
			}

			instrValidator(context, ir, instrIndex, instrHeader);
		}
	}
}

immutable void function(CompilationContext*, IrFunction*, IrIndex, ref IrInstrHeader)[] instrValidators = [
	&validateIrInstruction, // ir
	&validateIrInstruction_dummy, // lir_amd64
];

// TODO: check all instructions
// only checks create_aggregate for now
void validateIrInstruction(CompilationContext* c, IrFunction* ir, IrIndex instrIndex, ref IrInstrHeader instrHeader)
{
	switch(instrHeader.op)
	{
		case IrOpcode.load:
			IrIndex ptr = instrHeader.arg(ir, 0);
			IrIndex value = instrHeader.result(ir);
			IrIndex ptrType = getValueType(ptr, ir, c);
			c.assertf(ptrType.isTypePointer, "%s: first argument must be pointer, not: %s", instrIndex, ptrType.kind);
			IrIndex valueType = getValueType(value, ir, c);
			IrIndex baseType = c.types.getPointerBaseType(ptrType);
			c.assertf(c.types.isSameType(baseType, valueType), "%s: cannot load %s from %s",
				instrIndex, IrIndexDump(valueType, c, ir), IrIndexDump(ptrType, c, ir));
			break;

		case IrOpcode.store:
			IrIndex ptr = instrHeader.arg(ir, 0);
			IrIndex value = instrHeader.arg(ir, 1);
			IrIndex ptrType = getValueType(ptr, ir, c);
			c.assertf(ptrType.isTypePointer, "%s: first argument must be pointer, not: %s", instrIndex, ptrType.kind);
			IrIndex valueType = getValueType(value, ir, c);
			IrIndex baseType = c.types.getPointerBaseType(ptrType);
			c.assertf(c.types.isSameType(baseType, valueType), "%s: cannot store %s into %s",
				instrIndex, IrIndexDump(valueType, c, ir), IrIndexDump(ptrType, c, ir));
			break;

		case IrOpcode.create_aggregate:
			c.assertf(instrHeader.hasResult, "%s: create_aggregate has no result", instrIndex);

			IrIndex result = instrHeader.result(ir);
			c.assertf(result.isVirtReg, "%s: create_aggregate result is %s. virtualRegister expected", instrIndex, result.kind);

			IrVirtualRegister* vreg = &ir.getVirtReg(result);
			c.assertf(vreg.type.isType, "%s: result type is not a type: %s", instrIndex, vreg.type.kind);

			if (vreg.type.isTypeStruct)
			{
				IrTypeStruct* structType = &c.types.get!IrTypeStruct(vreg.type);
				c.assertf(instrHeader.numArgs == structType.numMembers,
					"%s: create_aggregate invalid number of arguments, got %s, expected %s",
					instrIndex, instrHeader.numArgs, structType.numMembers);

				IrTypeStructMember[] structMembers = structType.members;
				foreach (i, IrIndex arg; instrHeader.args(ir))
				{
					IrIndex memberType = structMembers[i].type;
					IrIndex argType = getValueType(arg, ir, c);
					bool sameType = c.types.isSameType(argType, memberType);
					c.assertf(sameType, "%s: create_aggregate type of arg %s mismatch. Expected %s, got %s",
						instrIndex, i+1, IrIndexDump(memberType, c, ir), IrIndexDump(argType, c, ir));
				}
			}
			else if (vreg.type.isTypeArray)
			{
				IrTypeArray* arrayType = &c.types.get!IrTypeArray(vreg.type);
				c.assertf(instrHeader.numArgs == arrayType.size,
					"%s: create_aggregate invalid number of arguments, got %s, expected %s",
					instrIndex, instrHeader.numArgs, arrayType.size);

				IrIndex memberType = arrayType.elemType;
				foreach (i, IrIndex arg; instrHeader.args(ir))
				{
					IrIndex argType = getValueType(arg, ir, c);
					bool sameType = c.types.isSameType(argType, memberType);
					c.assertf(sameType, "%s: create_aggregate type of arg %s mismatch. Expected %s, got %s",
						instrIndex, i+1, IrIndexDump(memberType, c, ir), IrIndexDump(argType, c, ir));
				}
			}
			else
				c.assertf(false, "%s: create_aggregate result type must be struct or array, got %s",
						instrIndex, vreg.type.kind);
			break;

		default: break;
	}
}

void validateIrInstruction_dummy(CompilationContext* context, IrFunction* ir, IrIndex instrIndex, ref IrInstrHeader instrHeader)
{

}
