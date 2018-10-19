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
		writefln("IR to LIR %s", context.idString(ir.name));

		lir = new IrFunction;
		lir.returnType = IrValueType.i32;

		context.tempBuffer.clear;

		builder.beginLir(lir, ir, context);

		// Mirror of original IR, we will put the new IrIndex of copied entities there
		// and later use this info to rewire all connections between basic blocks
		uint[] irMirror = context.tempBuffer.voidPut(ir.storageLength);
		irMirror[] = 0;

		IrIndex prevBlock;
		// dup basic blocks
		foreach (IrIndex blockIndex, ref IrBasicBlockInstr irBlock; ir.blocks)
		{
			IrIndex lirBlock = builder.append!IrBasicBlockInstr;
			//writefln("old %s is new %s", blockIndex.storageUintIndex, lirBlock.storageUintIndex);
			irMirror[blockIndex.storageUintIndex] = lirBlock.asUint;
			lir.getBlock(lirBlock).name = irBlock.name;
			foreach(IrIndex pred; irBlock.predecessors.range(ir)) lir.getBlock(lirBlock).predecessors.append(&builder, pred);
			foreach(IrIndex succ; irBlock.successors.range(ir)) lir.getBlock(lirBlock).successors.append(&builder, succ);

			if (blockIndex == ir.entryBasicBlock)
			{
				lir.entryBasicBlock = lirBlock;
			}
			else if (blockIndex == ir.exitBasicBlock)
			{
				lir.exitBasicBlock = lirBlock;
				lir.getBlock(lirBlock).prevBlock = prevBlock;
				lir.getBlock(prevBlock).nextBlock = lirBlock;
			}
			else
			{
				lir.getBlock(lirBlock).prevBlock = prevBlock;
				lir.getBlock(prevBlock).nextBlock = lirBlock;
			}

			prevBlock = lirBlock;
		}

		void fixIndex(ref IrIndex index)
		{
			index = IrIndex.fromUint(irMirror[index.storageUintIndex]);
		}

		// fix references
		foreach (IrIndex blockIndex, ref IrBasicBlockInstr lirBlock; lir.blocks)
		{
			foreach(ref pred; lirBlock.predecessors.range(lir)) fixIndex(pred);
			foreach(ref succ; lirBlock.successors.range(lir)) fixIndex(succ);
			lirBlock.firstInstr = IrIndex();
			lirBlock.lastInstr = IrIndex();
			lirBlock.firstPhi = IrIndex();
		}

		dumpFunction(lir, context);
	}
}
