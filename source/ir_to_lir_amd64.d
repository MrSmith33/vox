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
	}
}
