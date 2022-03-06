/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// IR Module
module vox.ir.ir_module;

import vox.all;

struct IrModule
{
	Array!(IrFunction*) functions;

	void addFunction(CompilationContext* context, IrFunction* fun)
	{
		functions.put(context.arrayArena, fun);
	}

	// sink and settings may be null
	void dump(CompilationContext* context, FuncDumpSettings* settings = null, TextSink* sink = null)
	{
		IrDumpContext dumpCtx = { context : context, settings : settings, sink : sink };
		foreach (func; functions) {
			dumpCtx.ir = func;
			dumpFunction(&dumpCtx);
		}
	}
}
