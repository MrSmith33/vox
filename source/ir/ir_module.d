/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/

module ir.ir_module;

import all;

struct IrModule
{
	IrFunction*[] functions;

	ubyte[] codeBuffer;
	ubyte[] code;

	void addFunction(IrFunction* fun)
	{
		functions ~= fun;
	}

	void dump(ref TextSink sink, CompilationContext* context, ref FuncDumpSettings settings)
	{
		foreach (func; functions) dumpFunction(func, sink, context, settings);
	}
}
