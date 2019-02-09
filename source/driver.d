/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module driver;

import std.stdio : writeln, write, writef, writefln, stdout;
import all;


immutable CompilePass[] commonPasses = [
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

CompilePass[] jitPasses = commonPasses ~ CompilePass("Link JIT", &pass_link_jit);
CompilePass[] exePasses = commonPasses ~ CompilePass("Exe", &pass_create_executable);

struct Driver
{
	CompilationContext context;
	CompilePass[] passes;

	Win32Allocator allocator;
	ubyte[] codeBuffer;
	ubyte[] staticBuffer;
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
		enum bufSize = PAGE_SIZE * 32;
		enum numBufs = 5;
		ubyte[] codeAndData = allocate(bufSize * numBufs, cast(void*)aligned, MemType.RW);
		ubyte[] take() {
			ubyte[] temp = codeAndData[0..bufSize];
			codeAndData = codeAndData[bufSize..$];
			return temp;
		}
		codeBuffer = take();
		staticBuffer = take();
		linkBuffer = take();
		importBuffer = take();
		binaryBuffer = take();
		//writefln("thisAddr h%X, data h%X..h%X, code h%X..h%X", thisAddr,
		//	staticBuffer.ptr, staticBuffer.ptr+staticBuffer.length,
		//	codeBuffer.ptr, codeBuffer.ptr+codeBuffer.length);

		// IrIndex can address 2^28 * 4 bytes = 1GB
		enum GiB = 1024UL*1024*1024;
		size_t irMemSize = GiB*3;
		bool success = allocator.reserve(irMemSize);
		context.assertf(success, "allocator failed");

		irBuffer = cast(ubyte[])allocator.allocate(GiB);
		modIrBuffer = cast(ubyte[])allocator.allocate(GiB);
		tempBuffer = cast(ubyte[])allocator.allocate(GiB);
	}

	void releaseMemory()
	{
		allocator.releaseMemory();
	}

	ModuleDeclNode* compileModule(string moduleSource, HostSymbol[] hostSymbols, DllModule[] dllModules)
	{
		markAsRW(codeBuffer.ptr, codeBuffer.length / PAGE_SIZE);
		context.codeBuffer = codeBuffer;
		context.importBuffer = importBuffer;
		context.binaryBuffer = binaryBuffer;
		context.irBuffer.setBuffer(irBuffer);
		context.types.buffer.setBuffer(modIrBuffer);
		context.tempBuffer.setBuffer(tempBuffer);
		context.staticDataBuffer.setBuffer(staticBuffer);
		context.objSymTab.buffer.setBuffer(linkBuffer);
		context.objSymTab.firstModule = LinkIndex();
		context.globals.array.length = 0;
		context.entryPoint = null;

		context.input = moduleSource;
		context.externalSymbols.clear();

		ObjectModule hostModule = {
			kind : ObjectModuleKind.isHost,
			id : context.idMap.getOrRegNoDup(":host")
		};
		LinkIndex hostModuleIndex = context.objSymTab.addModule(hostModule);

		foreach (HostSymbol hostSym; hostSymbols)
		{
			Identifier symId = context.idMap.getOrRegNoDup(hostSym.name);
			ushort symFlags;
			if (!canReferenceFromCode(hostSym.ptr)) symFlags |= ObjectSymbolFlags.isIndirect;
			ObjectSymbol importedSymbol = {
				kind : ObjectSymbolKind.isHost,
				flags : symFlags,
				id : symId,
				dataPtr : cast(ubyte*)hostSym.ptr,
				moduleIndex : hostModuleIndex,
			};
			LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);

			context.externalSymbols[symId] = importedSymbolIndex;
		}

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
					moduleIndex : importedModuleIndex,
				};
				LinkIndex importedSymbolIndex = context.objSymTab.addSymbol(importedSymbol);
				context.externalSymbols[symId] = importedSymbolIndex;
			}
		}

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

	bool canReferenceFromCode(void* hostSym)
	{
		// TODO
		return true;
	}

	/// Must be called after compilation is finished and before execution
	void markCodeAsExecutable()
	{
		markAsExecutable(codeBuffer.ptr, codeBuffer.length / PAGE_SIZE);
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
