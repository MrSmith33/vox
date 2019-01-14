/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ir_to_lir_amd64;

import std.stdio;
import all;


void pass_ir_to_lir_amd64(ref CompilationContext context)
{
	auto pass = IrToLir(&context);
	pass.run;
}

struct IrToLir
{
	CompilationContext* context;
	IrBuilder builder;

	void run()
	{
		foreach (i, FunctionDeclNode* fun; context.mod.functions)
		{
			if (fun.isExternal) continue;

			fun.backendData.lirData = new IrFunction;
			fun.backendData.lirData.backendData = &fun.backendData;

			//dumpFunction(*fun.backendData.irData, *context);

			context.mod.lirModule.addFunction(fun.backendData.lirData);
			processFunc(*fun.backendData.irData, *fun.backendData.lirData);
			if (context.validateIr) validateIrFunction(*context, *fun.backendData.lirData);
			//dumpFunction(*fun.backendData.lirData, *context);
		}
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

		IrIndex newIndexFromOldIndex(IrIndex oldIndex)
		{
			return mirror[oldIndex];
		}

		void fixIndex(ref IrIndex index)
		{
			assert(index.isDefined);
			if (index.isBasicBlock || index.isVirtReg || index.isPhi || index.isInstruction) {
				//writefln("%s -> %s", index, mirror[index]);
				index = mirror[index];
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
				IrIndex newPhi = builder.addPhi(lirBlock);
				recordIndex(phiIndex, newPhi);
				recordIndex(phi.result, lir.getPhi(newPhi).result);
				foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
				{
					builder.addPhiArg(newPhi, phiArg.basicBlock, phiArg.value);
				}
			}

			prevBlock = lirBlock;
		}

		// fix successors predecessors links
		foreach (IrIndex lirBlockIndex, ref IrBasicBlock lirBlock; lir.blocks)
		{
			lirBlock.isSealed = true;
			lirBlock.isFinished = true;

			foreach(ref pred; lirBlock.predecessors.range(lir)) fixIndex(pred);
			foreach(ref succ; lirBlock.successors.range(lir)) fixIndex(succ);

			// get the temp and null it
			IrIndex irBlockIndex = lirBlock.firstInstr;
			lirBlock.firstInstr = IrIndex();

			// Add instructions with old args
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; ir.getBlock(irBlockIndex).instructions(ir))
			{
				auto emitLirInstr(I)()
				{
					ExtraInstrArgs extra = {addUsers : false};
					static if (getInstrInfo!I.hasResult)
					{
						InstrWithResult res = builder.emitInstr!I(lirBlockIndex, extra, instrHeader.args);
						recordIndex(instrIndex, res.instruction);
						recordIndex(instrHeader.result, res.result);
					}
					else
					{
						IrIndex res = builder.emitInstr!I(lirBlockIndex, extra, instrHeader.args);
						recordIndex(instrIndex, res);
					}
				}

				switch(instrHeader.op)
				{
					case IrOpcode.parameter:
						uint paramIndex = ir.get!IrInstr_parameter(instrIndex).index;
						context.assertf(paramIndex < lir.backendData.callingConvention.paramsInRegs.length,
							"Only parameters passed through registers are implemented");

						InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(
							lirBlockIndex, lir.backendData.callingConvention.paramsInRegs[paramIndex]);
						recordIndex(instrIndex, instr.instruction);
						recordIndex(instrHeader.result, instr.result);
						break;

					// TODO. Win64 call conv is hardcoded here
					case IrOpcode.call:

						enum STACK_ITEM_SIZE = 8;
						size_t numArgs = instrHeader.args.length;
						size_t numParamsInRegs = lir.backendData.callingConvention.paramsInRegs.length;
						// how many bytes are allocated on the stack before func call
						size_t stackReserve = max(numArgs, numParamsInRegs) * STACK_ITEM_SIZE;
						IrIndex stackPtrReg = lir.backendData.callingConvention.stackPointer;

						if (numArgs > numParamsInRegs)
						{
							if (numArgs % 2 == 1)
							{	// align stack to 16 bytes
								stackReserve += STACK_ITEM_SIZE;
								IrIndex paddingSize = context.constants.add(IrConstant(STACK_ITEM_SIZE));
								ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
								builder.emitInstr!LirAmd64Instr_sub(
									lirBlockIndex, extra, stackPtrReg, paddingSize);
							}

							// push args to stack
							foreach_reverse (IrIndex arg; instrHeader.args[numParamsInRegs..$])
							{
								ExtraInstrArgs extra = {addUsers : false};
								builder.emitInstr!LirAmd64Instr_push(lirBlockIndex, extra, arg);
							}
						}

						// move args to registers
						enum MAX_PHYS_ARGS = 256;
						size_t numPhysRegs = min(numParamsInRegs, numArgs);
						IrIndex[MAX_PHYS_ARGS] physArgs;
						foreach_reverse (i, IrIndex arg; instrHeader.args[0..numPhysRegs])
						{
							physArgs[i] = lir.backendData.callingConvention.paramsInRegs[i];
							IrIndex argRegister = lir.backendData.callingConvention.paramsInRegs[i];
							ExtraInstrArgs extra = {addUsers : false, result : argRegister};
							builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, arg);
						}
						{	// Allocate shadow space for 4 physical registers
							IrIndex const_32 = context.constants.add(IrConstant(32));
							ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
							builder.emitInstr!LirAmd64Instr_sub(
								lirBlockIndex, extra, stackPtrReg, const_32);
						}

						{	// call
							ExtraInstrArgs extra = {
								addUsers : false,
								hasResult : instrHeader.hasResult,
								result : lir.backendData.callingConvention.returnReg};

							FunctionIndex calleeIndex = instrHeader.preheader!IrInstrPreheader_call.calleeIndex;
							context.assertf(calleeIndex < context.mod.functions.length, "Invalid callee index %s", calleeIndex);
							builder.emitInstrPreheader(IrInstrPreheader_call(calleeIndex));
							InstrWithResult callInstr = builder.emitInstr!LirAmd64Instr_call(
								lirBlockIndex, extra, physArgs[0..numPhysRegs]);
							recordIndex(instrIndex, callInstr.instruction);

							if (extra.hasResult) {
								// mov result to virt reg
								InstrWithResult movInstr = builder.emitInstr!LirAmd64Instr_mov(
									lirBlockIndex, lir.backendData.callingConvention.returnReg);
								recordIndex(instrHeader.result, movInstr.result);
							}
						}

						{	// Deallocate stack after call
							IrIndex conReservedBytes = context.constants.add(IrConstant(stackReserve));
							ExtraInstrArgs extra = {addUsers : false, result : stackPtrReg};
							builder.emitInstr!LirAmd64Instr_add(
								lirBlockIndex, extra, stackPtrReg, conReservedBytes);
						}
						break;

					case IrOpcode.add: emitLirInstr!LirAmd64Instr_add; break;
					case IrOpcode.sub: emitLirInstr!LirAmd64Instr_sub; break;
					case IrOpcode.mul: emitLirInstr!LirAmd64Instr_imul; break;
					case IrOpcode.load: emitLirInstr!LirAmd64Instr_load; break;
					case IrOpcode.store: emitLirInstr!LirAmd64Instr_store; break;

					case IrOpcode.block_exit_jump:
						builder.emitInstr!LirAmd64Instr_jmp(lirBlockIndex);
						break;

					case IrOpcode.block_exit_unary_branch:
						ExtraInstrArgs extra = {addUsers : false, cond : instrHeader.cond};
						IrIndex instruction = builder.emitInstr!LirAmd64Instr_un_branch(
							lirBlockIndex, extra, instrHeader.args);
						recordIndex(instrIndex, instruction);
						break;

					case IrOpcode.block_exit_binary_branch:
						ExtraInstrArgs extra = {addUsers : false, cond : instrHeader.cond};
						IrIndex instruction = builder.emitInstr!LirAmd64Instr_bin_branch(
							lirBlockIndex, extra, instrHeader.args);
						recordIndex(instrIndex, instruction);
						break;

					case IrOpcode.block_exit_return_void:
						builder.emitInstr!LirAmd64Instr_return(lirBlockIndex);
						break;

					case IrOpcode.block_exit_return_value:
						ExtraInstrArgs extra = {addUsers : false, result : lir.backendData.callingConvention.returnReg};
						builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, instrHeader.args);
						IrIndex instruction = builder.emitInstr!LirAmd64Instr_return(lirBlockIndex);
						recordIndex(instrIndex, instruction);
						break;

					default:
						context.internal_error("IrToLir unimplemented IR instr %s", cast(IrOpcode)instrHeader.op);
				}
			}
		}

		void fixArg(IrIndex instrIndex, ref IrIndex arg)
		{
			fixIndex(arg);
			builder.addUser(instrIndex, arg);
		}

		void fixInstrs(IrIndex blockIndex, ref IrBasicBlock lirBlock)
		{
			// replace old args with new args and add users
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; lirBlock.instructions(lir))
			{
				foreach(ref IrIndex arg; instrHeader.args)
				{
					fixArg(instrIndex, arg);
				}

				/// Cannot obtain address and store it in another address in one step
				if (instrHeader.op == Amd64Opcode.store && (instrHeader.args[1].isGlobal || instrHeader.args[1].isStackSlot))
				{
					// copy to temp register
					InstrWithResult movInstr = builder.emitInstr!LirAmd64Instr_mov(
						ExtraInstrArgs(), instrHeader.args[1]);
					builder.insertBeforeInstr(instrIndex, movInstr.instruction);
					// store temp to memory
					removeUser(*context, lir, instrIndex, instrHeader.args[1]);
					instrHeader.args[1] = movInstr.result;
					builder.addUser(instrIndex, movInstr.result);
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

		foreach (IrIndex blockIndex, ref IrBasicBlock lirBlock; lir.blocks)
		{
			fixInstrs(blockIndex, lirBlock);
			fixPhis(blockIndex, lirBlock);
		}
	}
}
