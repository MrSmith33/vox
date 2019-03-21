/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module cli;

import std.stdio;
import all;

/// exe path is stripped from args
void tryRunCli(string[] args)
{
	try {
		runCli(args);
	} catch(Throwable t) {
		writeln(t);
	}
}

void runCli(string[] args)
{
	import std.path;
	import std.getopt;

	auto time1 = currTime;
	auto startInitTime = currTime;

	Driver driver;

	bool printTime;
	bool printMem;
	string[] files;
	string outputFilename;
	string filterFuncName;

	bool printHelp;

	auto optResult = getopt(
		args,
		"of", "Write output to file.", &outputFilename,
		"print-time", "Print time of compilation.", &printTime,
		"print-source", "Print source code.", &driver.context.printSource,
		"print-lexemes", "Print lexemes.", &driver.context.printLexemes,
		"print-ast-fresh", "Print AST after parsing.", &driver.context.printAstFresh,
		"print-ast-sema", "Print AST after semantic analisys.", &driver.context.printAstSema,
		"print-ir", "Print IR after AST to IR pass.", &driver.context.printIr,
		"print-lir", "Print Print LIR after IR to LIR pass.", &driver.context.printLir,
		"print-lir-ra", "Print LIR after register allocation pass.", &driver.context.printLirRA,
		"print-liveness", "Print liveness analisys info.", &driver.context.printLiveIntervals,
		"print-stack-layout", "Print stack layout.", &driver.context.printStackLayout,
		"print-symbols", "Print symbols.", &driver.context.printSymbols,
		"print-mem", "Print memory consumtion.", &printMem,
		"print-filter", "Print only info about <function name>.", &filterFuncName,
		);

	args = args[1..$]; // skip program name

	if (args.length < 1) printHelp = true;
	if (optResult.helpWanted) printHelp = true;

	if (printHelp)
	{
		writeln("Usage: tiny_jit [options...] infile(s)...");

		size_t ls, ll;
		foreach (it; optResult.options)
		{
			ls = max(ls, it.optShort.length);
			ll = max(ll, it.optLong.length);
		}

		foreach (it; optResult.options)
		{
			writefln("%-*s %-*s  %s", ls, it.optShort, ll, it.optLong, it.help);
		}

		return;
	}

	string[] filenames = args;

	driver.initialize(exePasses);
	driver.context.buildType = BuildType.exe;
	if (outputFilename) driver.context.outputFilename = outputFilename;
	else driver.context.outputFilename = setExtension(filenames[0], ".exe");
	if (filterFuncName) driver.context.printOnlyFun = driver.context.idMap.getOrRegNoDup(filterFuncName);
	auto times = PerPassTimeMeasurements(1, driver.passes);
	auto endInitTime = currTime;

	try
	{
		driver.beginCompilation();
		//driver.addHostSymbols();
		//driver.addDllModules();
		foreach(filename; filenames)
			driver.addModule(SourceFileInfo(filename));
		driver.compile();
	}
	catch(CompilationException e) {
		writeln(driver.context.sink.text);
		if (e.isICE)
			writeln(e);
		return;
	}
	catch(Throwable t) {
		writeln(driver.context.sink.text);
		writeln(t);
		driver.context.printMemSize;
		return;
	}

	if (printMem) driver.context.printMemSize;

	auto startReleaseTime = currTime;
		// releasing memory is not necessary when running in standalone mode
		driver.releaseMemory;
	auto endReleaseTime = currTime;

	auto time2 = currTime;
	Duration duration = time2-time1;

	times.onIteration(0, duration);

	if (printTime)
	{
		times.print;

		writefln("Finished in %ss, init %ss, release %ss",
			scaledNumberFmt(duration),
			scaledNumberFmt(endInitTime-startInitTime),
			scaledNumberFmt(endReleaseTime-startReleaseTime));
	}
}
