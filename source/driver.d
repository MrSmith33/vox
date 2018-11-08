/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module driver;

import std.stdio : writeln, write, writef, writefln, stdout;
import all;


CompilePass[] compilerPasses = [
	CompilePass("Parse", &pass_parser),
	CompilePass("Semantic insert", &pass_semantic_decl),
	CompilePass("Semantic lookup", &pass_semantic_lookup),
	CompilePass("Semantic types", &pass_semantic_type),
	CompilePass("IR gen", &pass_ir_gen),
	CompilePass("IR to LIR AMD64", &pass_ir_to_lir_amd64),
	//// IR liveness
	CompilePass("Live intervals", &pass_live_intervals),
	//// IR regalloc
	CompilePass("Linear scan", &pass_linear_scan),
	//// Stack layout
	//CompilePass("Stack layout", &pass_stack_layout),
	//// LIR -> machine code
	//CompilePass("Code gen", &pass_code_gen),
];

struct Driver
{
	CompilationContext context;
	CompilePass[] passes;

	Win32Allocator allocator;
	ubyte[] codeBuffer;
	ubyte[] irBuffer;
	ubyte[] tempBuffer;

	void initialize(CompilePass[] passes_)
	{
		passes = passes_;

		// Try to allocate code buffer closer to host code pages,
		// so that 32-bit offset can be used for calls
		size_t thisAddr = cast(size_t)((&initialize).ptr); // take address of function in memory
		size_t step = 0x10_000_000;
		size_t aligned = alignValue(thisAddr, step) - step*20;
		//codeBuffer = allocate(PAGE_SIZE * 8, cast(void*)aligned, MemType.RWX);

		// IrIndex can address 2^28 * 4 bytes = 1GB
		size_t irMemSize = 1024UL*1024*1024*2;
		size_t arrSizes = 1024UL*1024*1024;
		bool success = allocator.reserve(irMemSize);
		context.assertf(success, "allocator failed");

		irBuffer = cast(ubyte[])allocator.allocate(arrSizes);
		tempBuffer = cast(ubyte[])allocator.allocate(arrSizes);

		context.codeBuffer = codeBuffer;
		context.irBuffer.setBuffer(cast(uint[])irBuffer);
		context.tempBuffer.setBuffer(cast(uint[])tempBuffer);
	}

	void releaseMemory()
	{
		allocator.releaseMemory();
	}

	ModuleDeclNode* compileModule(string moduleSource, ExternalSymbol[] externalSymbols)
	{
		context.input = moduleSource;
		foreach (ref extSym; externalSymbols)
			context.externalSymbols[context.idMap.getOrReg(extSym.name)] = extSym;

		try foreach (ref pass; passes)
		{
			auto time1 = currTime;

			pass.run(context);

			auto time2 = currTime;
			pass.duration = time2-time1;

			context.throwOnErrors;
		}
		catch(CompilationException e)
		{
			writeln(context.sink.text);
			//if (e.isICE) // always show stacktrace
				writeln(e);
		}
		return context.mod;
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
