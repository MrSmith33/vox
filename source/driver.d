/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module driver;

import std.stdio : writeln, write, writef, writefln, stdout;
import std.path : baseName, stripExtension;
import all;

void pass_source(ref CompilationContext ctx, CompilePassPerModule[] subPasses)
{
	size_t start = ctx.sourceBuffer.length;
	foreach(ref file; ctx.files.data)
	{
		if (file.content)
		{
			ctx.sourceBuffer.put(SOI_CHAR);
			ctx.sourceBuffer.put(file.content);
			ctx.sourceBuffer.put(EOI_CHAR);

			file.length = cast(uint)(file.content.length + 2);
			file.start = cast(uint)start;
			start += file.length;
		}
		else
		{
			import std.file : exists;
			import std.path : absolutePath;
			import std.stdio : File;

			if (!exists(file.name))
			{
				ctx.error("File `%s` not found", absolutePath(file.name));
				return;
			}

			ctx.sourceBuffer.put(SOI_CHAR);
			auto f = File(file.name, "r");
			char[] sourceBuffer = ctx.sourceBuffer.voidPut(f.size);
			char[] result = f.rawRead(sourceBuffer);
			f.close();
			ctx.sourceBuffer.put(EOI_CHAR);

			file.content = cast(string)sourceBuffer;
			file.length = cast(uint)(result.length + 2);
			file.start = cast(uint)start;
			start += file.length;
		}

		if (ctx.printSource) {
			writefln("// Source `%s`", file.name);
			writeln(file.content);
		}
	}
}

void pass_write_exe(ref CompilationContext context, CompilePassPerModule[] subPasses)
{
	import std.file : write;
	write(context.outputFilename, context.binaryBuffer.data);
}

immutable CompilePassGlobal[] commonPasses = [
	global_pass("Read source", &pass_source),
	global_pass("Lex", &pass_lexer),
	global_pass("Parse", &pass_parser),
	global_pass("Semantic insert", &pass_names_register),
	global_pass("Semantic lookup", &pass_names_resolve),
	global_pass("Semantic types", &pass_type_check),
	global_pass("IR gen", &pass_ir_gen),

	global_pass("Optimize", &pass_optimize_ir),

	global_pass(null, &run_ir_to_lir_liveness_and_reg_alloc, [
		CompilePassPerFunction("IR to LIR AMD64", null),
		// IR liveness
		CompilePassPerFunction("Live intervals", null),
		// IR regalloc
		CompilePassPerFunction("Linear scan", null)
	]),
	// Stack layout
	global_pass("Stack layout", &pass_stack_layout),

	// LIR -> machine code
	global_pass("Code gen", &pass_emit_mc_amd64),
];




immutable CompilePassGlobal[] extraJitPasses = [
	CompilePassGlobal("Link JIT", &pass_link_jit),
];

immutable CompilePassGlobal[] extraExePasses = [
	CompilePassGlobal("Link executable", &pass_create_executable),
	CompilePassGlobal("Write executable", &pass_write_exe),
];

CompilePassGlobal[] jitPasses = commonPasses ~ extraJitPasses;
CompilePassGlobal[] exePasses = commonPasses ~ extraExePasses;

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
	CompilePassPerFunction* ir_to_lir_pass = &subPasses[0].subPasses[0];
	CompilePassPerFunction* liveness_pass = &subPasses[0].subPasses[1];
	CompilePassPerFunction* ra_pass = &subPasses[0].subPasses[2];

	// gets reused for all functions
	LivenessInfo liveness;
	LinearScan linearScan;
	linearScan.context = &context;
	linearScan.livePtr = &liveness;
	scope(exit) linearScan.freeMem;

	scope(exit) context.tempBuffer.clear;

	foreach (ref SourceFileInfo file; context.files.data)
	{
		foreach (AstIndex funcIndex; file.mod.functions)
		{
			FunctionDeclNode* func = context.getAst!FunctionDeclNode(funcIndex);

			if (func.isExternal) continue;

			context.tempBuffer.clear;

			IrBuilder builder;

			{
				auto time1 = currTime;
				pass_ir_to_lir_amd64(&context, &builder, file.mod, func); // throws immediately on unrecoverable error or ICE
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

				linearScan.builder = &builder;
				linearScan.fun = func;
				pass_linear_scan(&linearScan); // throws immediately on unrecoverable error or ICE

				auto time2 = currTime;
				ra_pass.duration += time2-time1;
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

/// To compile a set of modules do following steps:
/// 1. initialize(passes)
/// 2. beginCompilation()
/// 3. addHostSymbols(hostSymbols)  --+
/// 4. addDllModules(dllModules)      | In any order
/// foreach(module; modules)          |
/// 5. addModule(module)  ------------+
/// 6. compile()
/// 7. markCodeAsExecutable() if in JIT mode
/// 8. releaseMemory()
struct Driver
{
	CompilationContext context;
	CompilePassGlobal[] passes;

	ArenaPool arenaPool;

	static void funWithAddress(){}
	void initialize(CompilePassGlobal[] passes_)
	{
		passes = passes_;

		// IrIndex can address 2^28 * 4 bytes = 1GB
		size_t BYTES_TO_RESERVE = GiB*166;
		arenaPool.reserve(BYTES_TO_RESERVE);
		//writefln("arenaPool %X .. %X", arenaPool.buffer.ptr, arenaPool.buffer.ptr+arenaPool.buffer.length);

		/// Those 3 must be allocated in this order (or in inverse order)
		/// Code must be able to use RIP-relative addressing into static data (and into import data when in JIT mode)
		context.importBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.codeBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.staticDataBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.roStaticDataBuffer.setBuffer(arenaPool.take(GiB), 0);

		context.sourceBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.files.setBuffer(arenaPool.take(GiB), 0);
		context.tokenBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.tokenLocationBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.binaryBuffer.setBuffer(arenaPool.take(GiB), 0);

		context.irStorage.instrHeaderBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrPayloadBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrNextBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.instrPrevBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.vregBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.phiBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.basicBlockBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.irStorage.arrayBuffer.setBuffer(arenaPool.take(8*GiB), 0);

		context.types.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.tempBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.vmBuffer.setBuffer(arenaPool.take(4*GiB), 0);
		context.objSymTab.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.globals.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.globals.initializerBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.constants.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.constants.aggregateBuffer.setBuffer(arenaPool.take(4*GiB), 0);
		context.astBuffer.setBuffer(arenaPool.take(16*GiB), 0);
		context.arrayArena.setBuffer(arenaPool.take(13*4*GiB));

		context.idMap.strings.setBuffer(arenaPool.take(2*GiB), 0);
		context.idMap.stringDataBuffer.setBuffer(arenaPool.take(2*GiB), 0);

		// all arena sizes must sum up to predefined constant
		context.assertf(arenaPool.takenBytes == BYTES_TO_RESERVE,
			"%s bytes taken, %s bytes to take", arenaPool.takenBytes, BYTES_TO_RESERVE);

		context.initialize();
	}

	void releaseMemory()
	{
		arenaPool.decommitAll;
	}

	void beginCompilation()
	{
		markAsRW(context.codeBuffer.bufPtr, divCeil(context.codeBuffer.length, PAGE_SIZE));
		markAsRW(context.roStaticDataBuffer.bufPtr, divCeil(context.roStaticDataBuffer.length, PAGE_SIZE));
		context.beginCompilation;
		foreach(ref pass; passes) pass.clear;
		addSections();
	}

	void addModule(SourceFileInfo moduleFile)
	{
		uint fileIndex = cast(uint)context.files.length;
		context.files.put(moduleFile);
		SourceFileInfo* file = &context.files.back();

		Identifier id = context.idMap.getOrRegNoDup(file.name.baseName.stripExtension);
		ObjectModule localModule = {
			kind : ObjectModuleKind.isLocal,
			id : id
		};
		auto mod = context.appendAst!ModuleDeclNode();
		file.mod = context.getAst!ModuleDeclNode(mod);
		file.mod.moduleIndex = ModuleIndex(fileIndex);
		file.mod.id = id;
		file.mod.objectSymIndex = context.objSymTab.addModule(localModule);
	}

	void addHar(string harFilename, const(char)[] harData)
	{
		void onHarFile(SourceFileInfo fileInfo) {
			addModule(fileInfo);
		}
		parseHar(context, harFilename, harData, &onHarFile);
	}

	void compile()
	{
		foreach (ref pass; passes)
		{
			auto time1 = currTime;

			// throws immediately on unrecoverable error or ICE
			pass.run(context, pass.subPasses);

			auto time2 = currTime;
			pass.duration = time2-time1;

			// throws if there were recoverable error in the pass
			context.throwOnErrors;
		}
	}

	/// Must be called after compilation is finished and before execution
	/// Effect is reverted with the call to beginCompilation
	/// Marks code pages as read-execute, and readonly data pages as read-only
	/// Clears zero-initialized data
	/// Only needed in JIT mode, not needed in AOT mode
	void markCodeAsExecutable()
	{
		markAsExecutable(context.codeBuffer.bufPtr, divCeil(context.codeBuffer.length, PAGE_SIZE));
		markAsRO(context.roStaticDataBuffer.bufPtr, divCeil(context.roStaticDataBuffer.length, PAGE_SIZE));
		// we cannot have a separate section for zeroinitialized data (would require 2 smaller arenas)
		// because it needs to occupy the same GiB as initialized data
		// to be RIP addressable in JIT mode
		context.staticDataBuffer.voidPut(context.zeroDataLength)[] = 0; // zero initialize
	}

	private bool canReferenceFromCode(void* hostSym)
	{
		void* start = context.codeBuffer.bufPtr;
		void* end = context.codeBuffer.bufPtr + context.codeBuffer.length;
		bool reachesFromStart = (hostSym - start) == cast(int)(hostSym - start);
		bool reachesFromEnd = (hostSym - end) == cast(int)(hostSym - end);
		return reachesFromStart && reachesFromEnd;
	}

	void addSections()
	{
		ObjectSection hostSection = {
			sectionAddress : 0,
			length : 0,
			alignment : 1,
			id : context.idMap.getOrRegNoDup(":host")
		};
		context.hostSectionIndex = context.objSymTab.addSection(hostSection);

		ObjectSection importSection = {
			sectionAddress : 0,
			length : 0,
			alignment : 1,
			id : context.idMap.getOrRegNoDup(".idata"),
			buffer : &context.importBuffer,
		};
		context.importSectionIndex = context.objSymTab.addSection(importSection);

		ObjectSection dataSection = {
			sectionAddress : 0,
			sectionData : context.staticDataBuffer.bufPtr,
			length : 0,
			alignment : 1,
			id : context.idMap.getOrRegNoDup(".data"),
			buffer : &context.staticDataBuffer,
		};
		context.dataSectionIndex = context.objSymTab.addSection(dataSection);

		ObjectSection rdataSection = {
			sectionAddress : 0,
			sectionData : context.roStaticDataBuffer.bufPtr,
			length : 0,
			alignment : 1,
			id : context.idMap.getOrRegNoDup(".rdata"),
			buffer : &context.roStaticDataBuffer,
		};
		context.rdataSectionIndex = context.objSymTab.addSection(rdataSection);

		ObjectSection textSection = {
			sectionAddress : 0,
			sectionData : context.codeBuffer.bufPtr,
			length : 0,
			alignment : 1,
			id : context.idMap.getOrRegNoDup(".text"),
			buffer : &context.codeBuffer,
		};
		context.textSectionIndex = context.objSymTab.addSection(textSection);
	}

	void addHostSymbols(HostSymbol[] hostSymbols)
	{
		if (hostSymbols.length > 0)
			context.assertf(context.buildType == BuildType.jit, "Can only add host symbols in JIT mode");

		ObjectModule hostModule = {
			kind : ObjectModuleKind.isHost,
			id : context.idMap.getOrRegNoDup(":host")
		};
		LinkIndex hostModuleIndex = context.objSymTab.addModule(hostModule);

		foreach (HostSymbol hostSym; hostSymbols)
		{
			Identifier symId = context.idMap.getOrRegNoDup(hostSym.name);

			if (canReferenceFromCode(hostSym.ptr))
			{
				ObjectSymbol importedSymbol = {
					kind : ObjectSymbolKind.isHost,
					id : symId,
					dataPtr : cast(ubyte*)hostSym.ptr,
					sectionOffset : cast(ulong)hostSym.ptr,
					sectionIndex : context.hostSectionIndex,
					moduleIndex : hostModuleIndex,
				};
				LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);
				context.externalSymbols[symId] = importedSymbolIndex;
			}
			else
			{
				ulong sectionOffset = context.importBuffer.length;
				ulong ptr = cast(ulong)hostSym.ptr;
				context.importBuffer.put(*cast(ubyte[8]*)&ptr);

				ObjectSymbol importedSymbol = {
					kind : ObjectSymbolKind.isHost,
					flags : ObjectSymbolFlags.isIndirect,
					id : symId,
					sectionOffset : sectionOffset,
					sectionIndex : context.importSectionIndex,
					moduleIndex : hostModuleIndex,
				};
				LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);
				context.externalSymbols[symId] = importedSymbolIndex;
			}
		}
	}

	/// Returns index of imported module
	LinkIndex addDllModule(string libName)
	{
		ObjectModule importedModule = {
			kind : ObjectModuleKind.isImported,
			id : context.idMap.getOrRegNoDup(libName)
		};
		return context.objSymTab.addModule(importedModule);
	}

	void addDllModuleSymbol(LinkIndex dllModuleIndex, string symName)
	{
		Identifier symId = context.idMap.getOrReg(symName);
		ObjectSymbol importedSymbol = {
			kind : ObjectSymbolKind.isImported,
			flags : ObjectSymbolFlags.isIndirect,
			id : symId,
			alignment : 8, // pointer size
			sectionIndex : context.importSectionIndex,
			moduleIndex : dllModuleIndex,
		};
		LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);
		context.externalSymbols[symId] = importedSymbolIndex;
	}

	void addDllModules(DllModule[] dllModules)
	{
		if (dllModules.length > 0)
			context.assertf(context.buildType == BuildType.exe, "Can only use dll symbols in exe mode");

		foreach (ref DllModule dllModule; dllModules)
		{
			LinkIndex importedModuleIndex = addDllModule(dllModule.libName);

			foreach (string symName; dllModule.importedSymbols)
			{
				addDllModuleSymbol(importedModuleIndex, symName);
			}
		}
	}
}

struct PerPassTimeMeasurements
{
	TimeMeasurements totalTimes;
	TimeMeasurements[] passTimes;
	CompilePassGlobal[] passes;
	size_t numPasses; // number of passes in the tree of passes

	this(size_t numIters, CompilePassGlobal[] passes)
	{
		this.passes = passes;
		totalTimes = TimeMeasurements(numIters);
		foreach (passIndex, string name, Duration dur; PassMetaIterator(passes)) ++numPasses;
		passTimes = new TimeMeasurements[numPasses];
		foreach (ref times; passTimes) times = TimeMeasurements(numIters);
	}

	void onIteration(size_t iterIndex, Duration iterTime)
	{
		totalTimes.onIteration(iterIndex, iterTime);
		foreach (passIndex, string name, Duration dur; PassMetaIterator(passes))
			passTimes[passIndex].onIteration(iterIndex, dur);
	}

	void print()
	{
		void printRow(string name, ref TimeMeasurements times)
		{
			writef("%- 20s", name);
			times.print;
			writeln;
		}

		writef("Iterations % 5.0s    ", scaledNumberFmt(totalTimes.numIters));
		totalTimes.printHeader; writeln;
		printRow("Total", totalTimes);
		foreach (passIndex, string name, Duration dur; PassMetaIterator(passes))
			if (name)
				printRow(name, passTimes[passIndex]);
	}

	void printTsv()
	{
		void printRow(string name, ref TimeMeasurements times)
		{
			writef("%s", name);
			times.printTsv;
			writeln;
		}

		writef("Iterations %s", scaledNumberFmt(totalTimes.numIters));
		totalTimes.printHeaderTsv; writeln;
		printRow("Total", totalTimes);
		foreach (passIndex, string name, Duration dur; PassMetaIterator(passes))
			if (name)
				printRow(name, passTimes[passIndex]);
	}
}

struct TimeMeasurements
{
	size_t numIters;
	Duration[] iterTimes;
	Duration totalTime;
	Duration avgTime() { return totalTime/numIters; }
	Duration minTime = Duration.max;
	Duration maxTime = Duration.min;

	this(size_t numIters)
	{
		this.numIters = numIters;
		iterTimes = new Duration[numIters];
	}

	void onIteration(size_t iterIndex, Duration iterTime)
	{
		iterTimes[iterIndex] = iterTime;
		totalTime += iterTime;
		minTime = min(iterTime, minTime);
		maxTime = max(iterTime, maxTime);
	}

	enum showNumFirstIters = 3;

	void printHeader()
	{
		if (numIters == 1) {
			write("    time");
		} else {
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef("  iter %s", i);
			write("   total     avg     min     max");
		}
	}

	void print()
	{
		if (numIters == 1) {
			writef(" % 6.1ss", scaledNumberFmt(iterTimes[0]));
		} else {
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef(" % 6.1ss", scaledNumberFmt(iterTimes[i]));
			writef(" % 6.1ss", scaledNumberFmt(totalTime));
			writef(" % 6.1ss", scaledNumberFmt(avgTime));
			writef(" % 6.1ss", scaledNumberFmt(minTime));
			writef(" % 6.1ss", scaledNumberFmt(maxTime));
		}
	}

	void printHeaderTsv()
	{
		if (numIters == 1) {
			write("\ttime");
		} else {
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef("\titer %s", i);
			write("\ttotal\tavg\tmin\tmax");
		}
	}

	void printTsv()
	{
		if (numIters == 1) {
			writef("\t%ss", scaledNumberFmt(iterTimes[0]));
		} else {
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef("\t%#ss", scaledNumberFmt(iterTimes[i]));
			writef("\t%#ss", scaledNumberFmt(totalTime));
			writef("\t%#ss", scaledNumberFmt(avgTime));
			writef("\t%#ss", scaledNumberFmt(minTime));
			writef("\t%#ss", scaledNumberFmt(maxTime));
		}
	}
}
