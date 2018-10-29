module ir_to_lir_amd64;

import std.stdio;
import compiler1;
import lir_amd64;
import ir;

void pass_ir_to_lir_amd64(ref CompilationContext ctx)
{
	auto pass = IrToLir(&ctx);
	pass.visit(ctx.mod.irModule);
}

struct IrToLir
{
	CompilationContext* context;
	IrFunction* lir;
	IrBuilder builder;

	void visit(ref IrModule mod)
	{
		foreach (IrFunction* f; mod.functions)
		{
			visit(f);
		}
	}

	void visit(IrFunction* ir)
	{
		//writefln("IR to LIR %s", context.idString(ir.name));

		lir = new IrFunction;
		lir.returnType = IrValueType.i32;

		context.tempBuffer.clear;

		builder.beginLir(lir, ir, context);

		// Mirror of original IR, we will put the new IrIndex of copied entities there
		// and later use this info to rewire all connections between basic blocks
		uint[] irMirror = context.tempBuffer.voidPut(ir.storageLength);
		irMirror[] = 0;

		void recordIndex(IrIndex oldIndex, IrIndex newIndex)
		{
			assert(oldIndex.isDefined);
			assert(newIndex.isDefined);
			irMirror[oldIndex.storageUintIndex] = newIndex.asUint;
		}

		void fixIndex(ref IrIndex index)
		{
			assert(index.isDefined);
			if (index.kind == IrValueKind.constant) return;

			//writefln("%s -> %s", index, IrIndex.fromUint(irMirror[index.storageUintIndex]));
			index = IrIndex.fromUint(irMirror[index.storageUintIndex]);
		}

		IrIndex prevBlock;
		// dup basic blocks
		foreach (IrIndex blockIndex, ref IrBasicBlockInstr irBlock; ir.blocks)
		{
			IrIndex lirBlock = builder.append!IrBasicBlockInstr;
			//writefln("old %s is new %s", blockIndex.storageUintIndex, lirBlock.storageUintIndex);
			recordIndex(blockIndex, lirBlock);
			lir.getBlock(lirBlock).name = irBlock.name;
			foreach(IrIndex pred; irBlock.predecessors.range(ir)) lir.getBlock(lirBlock).predecessors.append(&builder, pred);
			foreach(IrIndex succ; irBlock.successors.range(ir)) lir.getBlock(lirBlock).successors.append(&builder, succ);

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
			foreach(IrIndex phiIndex, ref IrPhiInstr phi; irBlock.phis(ir))
			{
				IrIndex newPhi = builder.addPhi(lirBlock);
				recordIndex(phiIndex, newPhi);
				recordIndex(phi.result, lir.getPhi(newPhi).result);
				foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(ir))
				{
					builder.addPhiArg(newPhi, phiArg.basicBlock, phiArg.value);
				}
			}

			// Add instructions with old args
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; irBlock.instructions(ir))
			{
				switch(instrHeader.op)
				{
					case IrOpcode.parameter:
						IrIndex paramIndex = builder.addInstruction!IrInstrParameter(lirBlock);
						recordIndex(instrIndex, paramIndex);
						lir.get!IrInstrParameter(paramIndex).index = ir.get!IrInstrParameter(instrIndex).index;
						IrIndex paramValue = lir.get!IrInstrHeader(paramIndex).result;
						recordIndex(instrHeader.result, paramValue);
						break;

					case IrOpcode.block_exit_jump:
						builder.addJump(lirBlock);
						break;

					case IrOpcode.block_exit_unary_branch:
						IrIndex branchIndex = builder.addUnaryBranch(lirBlock, cast(IrUnaryCondition)instrHeader.cond, instrHeader.args[0]);
						recordIndex(instrIndex, branchIndex);
						break;

					case IrOpcode.block_exit_binary_branch:
						IrIndex branchIndex = builder.addBinBranch(lirBlock, cast(IrBinaryCondition)instrHeader.cond, instrHeader.args[0], instrHeader.args[1]);
						recordIndex(instrIndex, branchIndex);
						break;

					case IrOpcode.block_exit_return_void:
						builder.addInstruction!IrReturnVoidInstr(lirBlock);
						break;

					case IrOpcode.block_exit_return_value:
						IrIndex retIndex = builder.addInstruction!IrReturnValueInstr(lirBlock);
						lir.get!IrReturnValueInstr(retIndex).args[0] = instrHeader.args[0];
						recordIndex(instrIndex, retIndex);
						break;

					default:
						//writefln("inst %s", cast(IrOpcode)instrHeader.op);
				}
			}

			prevBlock = lirBlock;
		}

		foreach (IrIndex blockIndex, ref IrBasicBlockInstr lirBlock; lir.blocks)
		{
			lirBlock.isSealed = true;
			lirBlock.isFinished = true;

			//writefln("block %s", blockIndex);
			foreach(ref pred; lirBlock.predecessors.range(lir)) fixIndex(pred);
			foreach(ref succ; lirBlock.successors.range(lir)) fixIndex(succ);

			// replace old args with new args and add users
			foreach(IrIndex instrIndex, ref IrInstrHeader instrHeader; lirBlock.instructions(lir))
			{
				foreach(ref IrIndex arg; instrHeader.args)
				{
					fixIndex(arg);
					builder.addUser(instrIndex, arg);
				}
			}

			// fix phi args and add users
			foreach(IrIndex phiIndex, ref IrPhiInstr phi; lirBlock.phis(lir))
			{
				foreach(size_t arg_i, ref IrPhiArg phiArg; phi.args(lir))
				{
					fixIndex(phiArg.value);
					builder.addUser(phiIndex, phiArg.value);
				}
			}
		}

		//writeln("// LIR");
		//dumpFunction(lir, context);
	}
}
