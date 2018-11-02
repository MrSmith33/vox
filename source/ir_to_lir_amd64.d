/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module ir_to_lir_amd64;

import std.stdio;
import all;


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

		// save map from old index to new index
		void recordIndex(IrIndex oldIndex, IrIndex newIndex)
		{
			assert(oldIndex.isDefined);
			assert(newIndex.isDefined);
			irMirror[oldIndex.storageUintIndex] = newIndex.asUint;
		}

		IrIndex newIndexFromOldIndex(IrIndex oldIndex)
		{
			return IrIndex.fromUint(irMirror[oldIndex.storageUintIndex]);
		}

		void fixIndex(ref IrIndex index)
		{
			assert(index.isDefined);
			if (index.kind == IrValueKind.constant) return;
			if (index.kind == IrValueKind.physicalRegister) return;

			//writefln("%s -> %s", index, IrIndex.fromUint(irMirror[index.storageUintIndex]));
			index = IrIndex.fromUint(irMirror[index.storageUintIndex]);
		}

		IrIndex prevBlock;
		// dup basic blocks
		foreach (IrIndex blockIndex, ref IrBasicBlockInstr irBlock; ir.blocks)
		{
			IrIndex lirBlock = builder.append!IrBasicBlockInstr;
			recordIndex(blockIndex, lirBlock);
			lir.getBlock(lirBlock).name = irBlock.name;
			foreach(IrIndex pred; irBlock.predecessors.range(ir)) lir.getBlock(lirBlock).predecessors.append(&builder, pred);
			foreach(IrIndex succ; irBlock.successors.range(ir)) lir.getBlock(lirBlock).successors.append(&builder, succ);

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

			prevBlock = lirBlock;
		}

		// fix successors predecessors links
		foreach (IrIndex lirBlockIndex, ref IrBasicBlockInstr lirBlock; lir.blocks)
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
				switch(instrHeader.op)
				{
					case IrOpcode.parameter:
						IrIndex paramIndex = builder.appendVoidToBlock!LirAmd64Instr_mov(lirBlockIndex);
						recordIndex(instrIndex, paramIndex);

						IrIndex paramValue = builder.addVirtualRegister(paramIndex);
						lir.get!LirAmd64Instr_mov(paramIndex).initialize(paramValue, amd64_register_index.ax);
						recordIndex(instrHeader.result, paramValue);
						break;

					case IrOpcode.block_exit_jump:
						IrIndex jmp = builder.addInstruction!LirAmd64Instr_jmp(lirBlockIndex);
						lir.get!LirAmd64Instr_jmp(jmp).args[0] = ir.getBlock(irBlockIndex).successors[0, lir];
						break;

					case IrOpcode.block_exit_unary_branch:
						//IrIndex branchIndex = builder.addUnaryBranch(lirBlockIndex, cast(IrUnaryCondition)instrHeader.cond, instrHeader.args[0]);
						//recordIndex(instrIndex, branchIndex);
						assert(false);
						break;

					case IrOpcode.block_exit_binary_branch:
						IrIndex cmp = builder.addInstruction!LirAmd64Instr_cmp(lirBlockIndex);
						lir.get!LirAmd64Instr_cmp(cmp).args[0] = instrHeader.args[0];
						lir.get!LirAmd64Instr_cmp(cmp).args[1] = instrHeader.args[1];

						Amd64Condition cond = IrBinCondToAmd64Condition[instrHeader.cond];
						IrIndex jcc = builder.addInstruction!LirAmd64Instr_jcc(lirBlockIndex);
						lir.get!LirAmd64Instr_jcc(jcc).header.cond = cond;
						lir.get!LirAmd64Instr_jcc(jcc).args[0] = ir.getBlock(irBlockIndex).successors[0, ir];

						IrIndex jmp = builder.addInstruction!LirAmd64Instr_jmp(lirBlockIndex);
						lir.get!LirAmd64Instr_jmp(jmp).args[0] = ir.getBlock(irBlockIndex).successors[1, ir];

						recordIndex(instrIndex, cmp);
						break;

					case IrOpcode.block_exit_return_void:
						IrIndex ret = builder.addInstruction!LirAmd64Instr_return(lirBlockIndex);
						break;

					case IrOpcode.block_exit_return_value:
						IrIndex movIndex = builder.appendVoidToBlock!LirAmd64Instr_mov(lirBlockIndex);
						lir.get!LirAmd64Instr_mov(movIndex)
							.initialize(amd64_register_index.ax, instrHeader.args[0]);

						IrIndex retIndex = builder.addInstruction!LirAmd64Instr_return(lirBlockIndex);

						recordIndex(instrIndex, retIndex);
						break;

					default:
						writefln("inst %s", cast(IrOpcode)instrHeader.op);
				}
			}
		}

		foreach (IrIndex blockIndex, ref IrBasicBlockInstr lirBlock; lir.blocks)
		{
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

		writeln("// LIR");
		FuncDumpSettings dumpSettings;
		dumpSettings.dumper = &dumpAmd64Instr;
		dumpFunction(lir, context, dumpSettings);
	}
}

Amd64Condition[] IrBinCondToAmd64Condition = [
	Amd64Condition.E,  // eq
	Amd64Condition.NE, // ne
	Amd64Condition.G,  // g
	Amd64Condition.GE, // ge
	Amd64Condition.L,  // l
	Amd64Condition.LE, // le
];
