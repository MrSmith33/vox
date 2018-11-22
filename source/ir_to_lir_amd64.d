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

			fun.lirData = new IrFunction;
			context.mod.lirModule.addFunction(fun.lirData);
			processFunc(*fun.irData, *fun.lirData);
			if (context.validateIr) validateIrFunction(*context, *fun.lirData);
		}
	}

	void processFunc(ref IrFunction ir, ref IrFunction lir)
	{
		//writefln("IR to LIR %s", context.idString(ir.name));
		lir.returnType = ir.returnType;
		lir.callingConvention = ir.callingConvention;

		context.tempBuffer.clear;

		lir.name = ir.name;
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
			if (index.kind == IrValueKind.constant) return;
			if (index.kind == IrValueKind.physicalRegister) return;

			//writefln("%s -> %s", index, mirror[index.storageUintIndex]);
			index = mirror[index];
		}

		IrIndex prevBlock;
		// dup basic blocks
		foreach (IrIndex blockIndex, ref IrBasicBlock irBlock; ir.blocks)
		{
			IrIndex lirBlock = builder.append!IrBasicBlock;
			++lir.numBasicBlocks;
			recordIndex(blockIndex, lirBlock);
			lir.getBlock(lirBlock).name = irBlock.name;
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
						context.assertf(paramIndex < lir.callingConvention.paramsInRegs.length,
							"Only parameters passed through registers are implemented");

						InstrWithResult instr = builder.emitInstr!LirAmd64Instr_mov(
							lirBlockIndex,
							lir.callingConvention.paramsInRegs[paramIndex]);
						recordIndex(instrIndex, instr.instruction);
						recordIndex(instrHeader.result, instr.result);
						break;

					// TODO. Win64 call conv is hardcoded here
					case IrOpcode.call:
						size_t numArgs = instrHeader.args.length;

						if (numArgs > 4)
						{
							if (numArgs % 2 == 1)
							{
								IrIndex const_8 = context.addConstant(IrConstant(8));
								IrIndex[2] args = [lir.callingConvention.stackPointer, const_8];
								builder.emitInstr!LirAmd64Instr_sub(lirBlockIndex, args);
							}

							foreach_reverse (IrIndex arg; instrHeader.args[4..$])
							{
								ExtraInstrArgs extra = {addUsers : false};
								builder.emitInstr!LirAmd64Instr_push(lirBlockIndex, extra, arg);
							}
						}

						enum MAX_PHYS_ARGS = 255;
						size_t numPhysRegs = min(4, numArgs);
						IrIndex[MAX_PHYS_ARGS] physArgs;
						foreach_reverse (i, IrIndex arg; instrHeader.args[0..numPhysRegs])
						{
							physArgs[i] = lir.callingConvention.paramsInRegs[i];
							ExtraInstrArgs extra = {addUsers : false, result : lir.callingConvention.paramsInRegs[i]};
							builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, arg);
						}

						ExtraInstrArgs extra = {
							addUsers : false,
							hasResult : instrHeader.hasResult,
							result : lir.callingConvention.returnReg};

						InstrWithResult callInstr = builder.emitInstr!LirAmd64Instr_call(
							lirBlockIndex, extra, physArgs[0..numPhysRegs]);
						builder.emitInstrTail(instrHeader.tail!IrInstrTail_call);

						if (extra.hasResult) {
							// mov to virt reg
							InstrWithResult movInstr = builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, lir.callingConvention.returnReg);
							recordIndex(instrHeader.result, movInstr.result);
						}
						recordIndex(instrIndex, callInstr.instruction);
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
						ExtraInstrArgs extra = {addUsers : false, result : lir.callingConvention.returnReg};
						builder.emitInstr!LirAmd64Instr_mov(lirBlockIndex, extra, instrHeader.args);
						IrIndex instruction = builder.emitInstr!LirAmd64Instr_return(lirBlockIndex);
						recordIndex(instrIndex, instruction);
						break;

					default:
						writefln("inst %s", cast(IrOpcode)instrHeader.op);
						context.unreachable;
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
