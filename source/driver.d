/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module driver;

import std.stdio : writeln, write, writef, writefln, stdout;
import all;

void pass_source(ref CompilationContext ctx)
{
	size_t start = 0;
	foreach(ref file; ctx.files.data)
	{
		if (file.content)
		{
			ctx.sourceBuffer[start] = SOI_CHAR;
			ctx.sourceBuffer[start+1..start+1+file.content.length] = file.content;
			ctx.sourceBuffer[start+1+file.content.length] = EOI_CHAR;

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

			ctx.sourceBuffer[start] = SOI_CHAR;
			auto f = File(file.name, "r");
			char[] result = f.rawRead(ctx.sourceBuffer[start+1..$]);
			f.close();
			ctx.sourceBuffer[start+1+result.length] = EOI_CHAR;

			file.length = cast(uint)(result.length + 2);
			file.start = cast(uint)start;
			start += file.length;
		}
	}

	//if (ctx.printSource) {
	//	writeln("// Source");
	//	writeln(ctx.input);
	//}
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

struct Driver
{
	CompilationContext context;
	CompilePass[] passes;

	Win32Allocator allocator;
	ubyte[] codeAndDataBuffer;

	ubyte[] sourceBuffer;
	ubyte[] fileBuffer;
	ubyte[] codeBuffer;
	ubyte[] staticBuffer;
	ubyte[] tokenBuffer;
	ubyte[] tokenLocationBuffer;
	ubyte[] linkBuffer;
	ubyte[] importBuffer;
	ubyte[] binaryBuffer;

	ubyte[] irBuffer;
	ubyte[] modIrBuffer;
	ubyte[] tempBuffer;

	static void funWithAddress(){}
	void initialize(CompilePass[] passes_)
	{
		passes = passes_;

		// Try to allocate code buffer closer to host code pages,
		// so that 32-bit offset can be used for calls
		size_t thisAddr = cast(size_t)(&funWithAddress); // take address of function in memory
		size_t step = 0x10_000_000;
		size_t aligned = alignValue(thisAddr, step) + step*5;

		// Ideally we would allocate 2GB for code and data, split in 2 1-gig parts
		enum BUF_SIZE = PAGE_SIZE * 32;
		enum NUM_BUFS = 2;
		///codeAndDataBuffer = allocate(BUF_SIZE * NUM_BUFS, cast(void*)aligned, MemType.RW);
		ubyte[] take(size_t size) {
			ubyte[] temp = codeAndDataBuffer[0..size];
			codeAndDataBuffer = codeAndDataBuffer[size..$];
			return temp;
		}

		//writefln("thisAddr h%X, data h%X..h%X, code h%X..h%X", thisAddr,
		//	staticBuffer.ptr, staticBuffer.ptr+staticBuffer.length,
		//	codeBuffer.ptr, codeBuffer.ptr+codeBuffer.length);

		// IrIndex can address 2^28 * 4 bytes = 1GB
		enum GiB = 1024UL*1024*1024;
		enum MiB = 1024UL*1024;
		enum BIG_BUF_SIZE = MiB*20;
		enum SMALL_BUF_SIZE = MiB*1;
		//enum BIG_BUF_SIZE = MiB;
		size_t irMemSize = MiB*300;
		bool success = allocator.reserve(irMemSize);
		context.assertf(success, "allocator failed");

		codeBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);
		staticBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);
		sourceBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);
		fileBuffer = cast(ubyte[])allocator.allocate(1024);
		tokenBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);
		tokenLocationBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);
		linkBuffer = cast(ubyte[])allocator.allocate(SMALL_BUF_SIZE);
		importBuffer = cast(ubyte[])allocator.allocate(SMALL_BUF_SIZE);
		binaryBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);

		irBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);
		modIrBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);
		tempBuffer = cast(ubyte[])allocator.allocate(BIG_BUF_SIZE);
	}

	void releaseMemory()
	{
		allocator.releaseMemory();
		deallocate(codeAndDataBuffer);
	}

	ModuleDeclNode* compileModule(SourceFileInfo moduleFile, HostSymbol[] hostSymbols, DllModule[] dllModules)
	{
		markAsRW(codeBuffer.ptr, codeBuffer.length / PAGE_SIZE);
		context.sourceBuffer = cast(char[])sourceBuffer;
		context.files.setBuffer(fileBuffer);
		context.codeBuffer = codeBuffer;
		context.importBuffer = importBuffer;
		context.tokenBuffer = cast(TokenType[])tokenBuffer;
		context.tokenLocationBuffer = cast(SourceLocation[])tokenLocationBuffer;
		context.binaryBuffer.setBuffer(binaryBuffer);
		context.irBuffer.setBuffer(irBuffer);
		context.types.buffer.setBuffer(modIrBuffer);
		context.tempBuffer.setBuffer(tempBuffer);
		context.staticDataBuffer.setBuffer(staticBuffer);
		context.objSymTab.buffer.setBuffer(linkBuffer);
		context.objSymTab.firstModule = LinkIndex();
		context.globals.array.length = 0;
		context.entryPoint = null;

		context.files.put(moduleFile);
		context.externalSymbols.clear();

		addSections();
		addHostSymbols(hostSymbols);
		addDllModules(dllModules);

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

		return context.mod;
	}

	/// Must be called after compilation is finished and before execution
	void markCodeAsExecutable()
	{
		markAsExecutable(codeBuffer.ptr, codeBuffer.length / PAGE_SIZE);
	}

	private bool canReferenceFromCode(void* hostSym)
	{
		void* start = context.codeBuffer.ptr;
		void* end = context.codeBuffer.ptr + context.codeBuffer.length;
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
			sectionData : context.codeBuffer.ptr,
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

		FixedBuffer!ulong jitImports;
		jitImports.setBuffer(context.importBuffer);

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
				ulong sectionOffset = jitImports.byteLength;
				jitImports.put(cast(ulong)hostSym.ptr);

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

	void addDllModules(DllModule[] dllModules)
	{
		if (dllModules.length > 0)
			context.assertf(context.buildType == BuildType.exe, "Can only use dll symbols in exe mode");

		foreach (ref DllModule dllModule; dllModules)
		{
			ObjectModule importedModule = {
				kind : ObjectModuleKind.isImported,
				id : context.idMap.getOrRegNoDup(dllModule.libName)
			};
			LinkIndex importedModuleIndex = context.objSymTab.addModule(importedModule);

			foreach (string symName; dllModule.importedSymbols)
			{
				Identifier symId = context.idMap.getOrRegNoDup(symName);
				ObjectSymbol importedSymbol = {
					kind : ObjectSymbolKind.isImported,
					flags : ObjectSymbolFlags.isIndirect,
					id : symId,
					alignment : 8, // pointer size
					sectionIndex : context.importSectionIndex,
					moduleIndex : importedModuleIndex,
				};
				LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);
				context.externalSymbols[symId] = importedSymbolIndex;
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
