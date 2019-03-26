/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module driver;

import std.stdio : writeln, write, writef, writefln, stdout;
import std.path : stripExtension;
import all;

void pass_source(ref CompilationContext ctx)
{
	size_t start = 0;
	foreach(ref file; ctx.files.data)
	{
		file.mod.id = ctx.idMap.getOrRegNoDup(stripExtension(file.name));

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

void pass_write_exe(ref CompilationContext ctx)
{
	import std.file : write;
	write(ctx.outputFilename, ctx.binaryBuffer.data);
}

immutable CompilePass[] commonPasses = [
	CompilePass("Read source", &pass_source),
	CompilePass("Lex", &pass_lexer),
	CompilePass("Parse", &pass_parser),
	CompilePass("Semantic insert", &pass_semantic_decl),
	CompilePass("Semantic lookup", &pass_semantic_lookup),
	CompilePass("Semantic types", &pass_semantic_type),
	CompilePass("IR gen", &pass_ir_gen),
	CompilePass("Optimize", &pass_optimize_ir),
	CompilePass("IR to LIR AMD64", &pass_ir_to_lir_amd64),

	// IR liveness
	CompilePass("Live intervals", &pass_live_intervals),
	// IR regalloc
	CompilePass("Linear scan", &pass_linear_scan),
	// Stack layout
	CompilePass("Stack layout", &pass_stack_layout),
	// LIR -> machine code
	CompilePass("Code gen", &pass_emit_mc_amd64),
];

immutable CompilePass[] extraJitPasses = [
	CompilePass("Link JIT", &pass_link_jit),
];

immutable CompilePass[] extraExePasses = [
	CompilePass("Exe", &pass_create_executable),
	CompilePass("Write exe", &pass_write_exe),
];

CompilePass[] jitPasses = commonPasses ~ extraJitPasses;
CompilePass[] exePasses = commonPasses ~ extraExePasses;

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
	CompilePass[] passes;

	ArenaPool arenaPool;

	static void funWithAddress(){}
	void initialize(CompilePass[] passes_)
	{
		passes = passes_;

		// IrIndex can address 2^28 * 4 bytes = 1GB
		enum ulong GiB = 1024UL*1024*1024;

		size_t irMemSize = GiB*53;
		arenaPool.reserve(irMemSize);

		/// Those 3 must be allocated in this order (or in inverse order)
		/// Code must be able to use RIP-relative addressing into static data (and into import data when in JIT mode)
		context.importBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.codeBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.staticDataBuffer.setBuffer(arenaPool.take(GiB), 0);

		context.sourceBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.files.setBuffer(arenaPool.take(GiB), 0);
		context.tokenBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.tokenLocationBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.binaryBuffer.setBuffer(arenaPool.take(GiB), 0);
		context.irBuffer.setBuffer(arenaPool.take(16*GiB), 0);
		context.types.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.tempBuffer.setBuffer(arenaPool.take(8*GiB), 0);
		context.objSymTab.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.globals.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.constants.buffer.setBuffer(arenaPool.take(GiB), 0);
		context.astBuffer.setBuffer(arenaPool.take(16*GiB), 0);
	}

	void releaseMemory()
	{
		arenaPool.decommitAll;
	}

	void beginCompilation()
	{
		markAsRW(context.codeBuffer.bufPtr, divCeil(context.codeBuffer.length, PAGE_SIZE));
		context.sourceBuffer.clear;
		context.files.clear;
		context.codeBuffer.clear;
		context.importBuffer.clear;
		context.tokenBuffer.clear;
		context.tokenLocationBuffer.clear;
		context.binaryBuffer.clear;
		context.irBuffer.clear;
		context.types.buffer.clear;
		context.tempBuffer.clear;
		context.staticDataBuffer.clear;
		context.objSymTab.buffer.clear;
		context.objSymTab.firstModule = LinkIndex();
		context.globals.buffer.clear;
		context.constants.buffer.clear;
		context.astBuffer.clear;
		context.entryPoint = null;

		context.externalSymbols.clear();

		addSections();
	}

	void addModule(SourceFileInfo moduleFile)
	{
		context.files.put(moduleFile);
		import core.memory : GC;
		SourceFileInfo* file = &context.files.back();
		GC.addRange(cast(void*)file, SourceFileInfo.sizeof, typeid(SourceFileInfo));

		ObjectModule localModule = {
			kind : ObjectModuleKind.isLocal,
			id : context.idMap.getOrRegNoDup(":local")
		};
		file.mod = context.appendAst!ModuleDeclNode();
		file.mod.moduleIndex = ModuleIndex(0);
		file.mod.objectSymIndex = context.objSymTab.addModule(localModule);
	}

	void compile()
	{
		foreach (ref pass; passes)
		{
			auto time1 = currTime;

			// throws immediately on unrecoverable error or ICE
			pass.run(context);

			auto time2 = currTime;
			pass.duration = time2-time1;

			// throws if there were recoverable error in the pass
			context.throwOnErrors;
		}
	}

	/// Must be called after compilation is finished and before execution
	void markCodeAsExecutable()
	{
		markAsExecutable(context.codeBuffer.bufPtr, divCeil(context.codeBuffer.length, PAGE_SIZE));
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
			id : context.idMap.getOrRegNoDup(".idata")
		};
		context.importSectionIndex = context.objSymTab.addSection(importSection);

		ObjectSection dataSection = {
			sectionAddress : 0,
			sectionData : context.staticDataBuffer.bufPtr,
			length : 0,
			alignment : 1,
			id : context.idMap.getOrRegNoDup(".data")
		};
		context.dataSectionIndex = context.objSymTab.addSection(dataSection);

		ObjectSection textSection = {
			sectionAddress : 0,
			sectionData : context.codeBuffer.bufPtr,
			length : 0,
			alignment : 1,
			id : context.idMap.getOrRegNoDup(".text")
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

struct CompilePass
{
	string name;
	void function(ref CompilationContext context) run;

	Duration duration;
}

struct PerPassTimeMeasurements
{
	TimeMeasurements totalTimes;
	TimeMeasurements[] passTimes;
	CompilePass[] passes;

	this(size_t numIters, CompilePass[] passes)
	{
		this.passes = passes;
		totalTimes = TimeMeasurements(numIters);
		passTimes = new TimeMeasurements[passes.length];
		foreach (ref times; passTimes) times = TimeMeasurements(numIters);
	}

	void onIteration(size_t iterIndex, Duration iterTime)
	{
		totalTimes.onIteration(iterIndex, iterTime);
		foreach (passIndex, ref pass; passes)
			passTimes[passIndex].onIteration(iterIndex, pass.duration);
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
		foreach (passIndex, ref times; passTimes)
			printRow(passes[passIndex].name, times);
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
		if (numIters == 1)
		{
			write("    time");
		}
		else
		{
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef("  iter %s", i);
			write("   total     avg     min     max");
		}
	}

	void print()
	{
		if (numIters == 1)
		{
			writef(" % 6.1ss", scaledNumberFmt(iterTimes[0]));
		}
		else
		{
			foreach (i; 0..min(numIters, showNumFirstIters))
				writef(" % 6.1ss", scaledNumberFmt(iterTimes[i]));
			writef(" % 6.1ss", scaledNumberFmt(totalTime));
			writef(" % 6.1ss", scaledNumberFmt(avgTime));
			writef(" % 6.1ss", scaledNumberFmt(minTime));
			writef(" % 6.1ss", scaledNumberFmt(maxTime));
		}
	}
}
