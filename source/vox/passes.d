/// Copyright: Copyright (c) 2017-2020 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module vox.passes;

import vox.all;

void pass_write_binary(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	import std.file : write;
	write(context.outputFilename, context.binaryBuffer.data);

	version(Posix) {
		import std.file : setAttributes, getAttributes;
		import std.conv : octal;
		setAttributes(context.outputFilename, getAttributes(context.outputFilename) | octal!111);
	}
}

void write_bundle(ref CompilationContext context)
{
	import std.file : write;
	write(context.outputFilename, context.bundleBuffer.data);
}

immutable CompilePassGlobal[] frontendPasses = [
	global_pass("Read source", &pass_read_source),
	global_pass("Lex", &pass_lexer),
	global_pass("Parse", &pass_parser),
	global_pass("Register names", &pass_names_register),
	global_pass("Lookup names", &pass_names_resolve),
	global_pass("Check types", &pass_type_check),
];

immutable CompilePassGlobal[] backendPasses = [
	global_pass("IR gen", &pass_ir_gen),

	global_pass("Optimize", &pass_optimize_ir),

	global_pass(null, &run_ir_to_lir_liveness_and_reg_alloc, [
		CompilePassPerFunction("IR lower", null),
		CompilePassPerFunction("IR to LIR AMD64", null),
		// IR liveness
		CompilePassPerFunction("Live intervals", null),
		// IR regalloc
		CompilePassPerFunction("Linear scan", null),
		// Stack layout
		CompilePassPerFunction("Stack layout", null),
	]),

	// LIR -> machine code
	global_pass("Code gen", &pass_emit_mc_amd64),
];

immutable CompilePassGlobal[] commonPasses = frontendPasses ~ backendPasses;



immutable CompilePassGlobal[] extraJitPasses = [
	CompilePassGlobal("Link JIT", &pass_link_jit),
];

immutable CompilePassGlobal[] extraExePasses = [
	CompilePassGlobal("Link executable", &pass_create_executable),
	CompilePassGlobal("Write binary", &pass_write_binary),
];

CompilePassGlobal[] frontendOnlyPasses = frontendPasses;
CompilePassGlobal[] jitPasses = commonPasses ~ extraJitPasses;
CompilePassGlobal[] exePasses = commonPasses ~ extraExePasses;
CompilePassGlobal[] bundlePasses = commonPasses ~ extraExePasses[0..$-1];

void run_global_pass(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	//context.printMemSize;
	foreach (ref SourceFileInfo file; context.files.data)
	{
		foreach(ref CompilePassPerModule subPass; subPasses)
		{
			auto time1 = currTime;

			// throws immediately on unrecoverable error or ICE
			subPass.run(context, *file.mod, subPass.subPasses);

			auto time2 = currTime;
			subPass.duration += time2-time1;

			// throws if there were recoverable error in the pass
			context.throwOnErrors;
		}
	}
}

void run_module_pass(ref CompilationContext context, ref ModuleDeclNode mod, CompilePassPerFunction[] subPasses)
{
	foreach (AstIndex funcIndex; mod.functions)
	{
		FunctionDeclNode* func = context.getAst!FunctionDeclNode(funcIndex);

		foreach(ref CompilePassPerFunction subPass; subPasses)
		{
			auto time1 = currTime;

			// throws immediately on unrecoverable error or ICE
			subPass.run(context, mod, *func);

			auto time2 = currTime;
			subPass.duration += time2-time1;

			// throws if there were recoverable error in the pass
			context.throwOnErrors;
		}
	}
}

void run_ir_to_lir_liveness_and_reg_alloc(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	CompilePassPerFunction* ir_lower_pass = &subPasses[0].subPasses[0];
	CompilePassPerFunction* ir_to_lir_pass = &subPasses[0].subPasses[1];
	CompilePassPerFunction* liveness_pass = &subPasses[0].subPasses[2];
	CompilePassPerFunction* ra_pass = &subPasses[0].subPasses[3];
	CompilePassPerFunction* stack_layout_pass = &subPasses[0].subPasses[4];

	// gets reused for all functions
	LivenessInfo liveness;
	LinearScan linearScan;
	linearScan.context = &context;
	linearScan.livePtr = &liveness;
	scope(exit) linearScan.freeMem;

	scope(exit) context.tempBuffer.clear;
	scope(exit) context.currentFunction = null;

	foreach (ref SourceFileInfo file; context.files.data)
	{
		foreach (AstIndex funcIndex; file.mod.functions)
		{
			FunctionDeclNode* func = context.getAst!FunctionDeclNode(funcIndex);

			if (func.isExternal) continue;

			context.currentFunction = func;
			context.tempBuffer.clear;


			{
				auto time1 = currTime;
				pass_ir_lower(&context, file.mod, func); // throws immediately on unrecoverable error or ICE
				auto time2 = currTime;
				ir_lower_pass.duration += time2-time1;
				context.throwOnErrors; // throws if there were recoverable error in the pass
			}

			IrBuilder lirBuilder;
			{
				auto time1 = currTime;
				pass_ir_to_lir_amd64(&context, &lirBuilder, file.mod, func); // throws immediately on unrecoverable error or ICE
				auto time2 = currTime;
				ir_to_lir_pass.duration += time2-time1;
				context.throwOnErrors; // throws if there were recoverable error in the pass
			}

			{
				auto time1 = currTime;
				pass_live_intervals(&context, file.mod, func, &liveness); // throws immediately on unrecoverable error or ICE
				auto time2 = currTime;
				liveness_pass.duration += time2-time1;
				context.throwOnErrors; // throws if there were recoverable error in the pass
			}

			{
				auto time1 = currTime;

				linearScan.builder = &lirBuilder;
				linearScan.fun = func;
				pass_linear_scan(&linearScan); // throws immediately on unrecoverable error or ICE

				auto time2 = currTime;
				ra_pass.duration += time2-time1;
				context.throwOnErrors; // throws if there were recoverable error in the pass
			}

			{
				auto time1 = currTime;
				pass_stack_layout(&context, func); // throws immediately on unrecoverable error or ICE
				auto time2 = currTime;
				stack_layout_pass.duration += time2-time1;
				context.throwOnErrors; // throws if there were recoverable error in the pass
			}
		}
	}
}

CompilePassGlobal global_pass(string name, GlobalPassFun run, CompilePassPerModule[] subPasses = null)
{
	return CompilePassGlobal(name, run, subPasses);
}

CompilePassGlobal global_pass(string name, ModulePassFun run, CompilePassPerFunction[] subPasses = null)
{
	return CompilePassGlobal(null, &run_global_pass, [CompilePassPerModule(null, run, subPasses)]);
}

CompilePassGlobal global_pass(string name, GlobalPassFun run, CompilePassPerFunction[] subPasses)
{
	return CompilePassGlobal(name, run, [CompilePassPerModule(null, &run_module_pass, subPasses)]);
}

CompilePassGlobal global_pass(string name, FunctionPassFun run)
{
	return CompilePassGlobal(null, &run_global_pass, [
		CompilePassPerModule(null, &run_module_pass, [
			CompilePassPerFunction(name, run)])]);
}

alias GlobalPassFun = void function(ref CompilationContext context, CompilePassPerModule[] subPasses);
alias ModulePassFun = void function(ref CompilationContext context, ref ModuleDeclNode mod, CompilePassPerFunction[] subPasses);
alias FunctionPassFun = void function(ref CompilationContext context, ref ModuleDeclNode mod, ref FunctionDeclNode func);

/// Must have either `run` or subPasses
struct CompilePassGlobal
{
	string name;
	GlobalPassFun run;
	CompilePassPerModule[] subPasses;
	Duration duration;
	void clear() {
		duration = Duration.init;
		foreach(ref subPass; subPasses) subPass.clear;
	}
}

struct CompilePassPerModule
{
	string name;
	void function(ref CompilationContext context, ref ModuleDeclNode mod, CompilePassPerFunction[] subPasses) run;
	CompilePassPerFunction[] subPasses;
	Duration duration;
	void clear() {
		duration = Duration.init;
		foreach(ref subPass; subPasses) subPass.clear;
	}
}

struct CompilePassPerFunction
{
	string name;
	void function(ref CompilationContext context, ref ModuleDeclNode mod, ref FunctionDeclNode func) run;
	Duration duration;
	void clear() {
		duration = Duration.init;
	}
}

struct PassMetaIterator
{
	CompilePassGlobal[] passes;
	int opApply(scope int delegate(size_t, string name, Duration duration) dg)
	{
		size_t i = 0;
		foreach(ref pass; passes)
		{
			if (auto res = dg(i, pass.name, pass.duration)) return res;
			++i;
			foreach(ref subPass; pass.subPasses)
			{
				if (auto res = dg(i, subPass.name, subPass.duration)) return res;
				++i;
				foreach(ref subPass2; subPass.subPasses)
				{
					if (auto res = dg(i, subPass2.name, subPass2.duration)) return res;
					++i;
				}
			}
		}
		return 0;
	}
}
