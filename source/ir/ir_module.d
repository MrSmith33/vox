/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

/// IR Module
module ir.ir_module;

import all;

struct IrModule
{
	IrFunction*[] functions;

	void addFunction(IrFunction* fun)
	{
		functions ~= fun;
	}

	void dump(ref TextSink sink, ref CompilationContext context, ref FuncDumpSettings settings)
	{
		foreach (func; functions) dumpFunction(*func, sink, context, settings);
	}
}
