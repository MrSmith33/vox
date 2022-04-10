/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module cli;

import std.stdio;
import std.file : exists;
import std.path : absolutePath, extension, baseName, setExtension;
import vox.all;

enum WindowsSubsystemCli : ushort {
	CUI,
	GUI
}

int runCli(string[] args)
{
	import std.getopt;

	auto time1 = currTime;
	auto startInitTime = currTime;

	Driver driver;
	WindowsSubsystemCli subSystem;

	bool printTime;
	bool printMem;
	bool printStats;
	bool checkOnly;
	string outputFilename;
	string outputTarget = TARGET_OS_STRING[driver.context.hostOS];
	string filterFuncName;
	string targetHelp;

	version(Windows) {
		targetHelp = "Choose target. [windows-x64(default), linux-x64, macos-x64]";
	}
	else version(linux) {
		targetHelp = "Choose target. [windows-x64, linux-x64(default), macos-x64]";
	}
	else version(OSX) {
		targetHelp = "Choose target. [windows-x64, linux-x64, macos-x64(default)]";
	}
	else static assert(false, "Unnhandled OS");

	bool printHelp;
	GetoptResult optResult;

	// Look for tool usage
	if (args.length > 1)
	{
		switch(args[1])
		{
			case "pdb-dump":
				if (args.length == 1) {
					writeln("Usage: vox pdb-dump file.pdb");
					return 1;
				}
				string filename = absolutePath(args[2]);
				if (!exists(filename))
				{
					writefln("File `%s` not found", absolutePath(filename));
					return 1;
				}
				import vox.be.debug_info.pdb;
				PdbReader.fromFile(filename);
				return 0;

			default:
				break; // regular compiler invocation
		}
	}

	// Regular compiler invocation
	retry_parse_opts:
	try
	{
		// GC
		optResult = getopt(
			args,
			"of", "Write output to file.", &outputFilename,
			"target", targetHelp, &outputTarget,
			"subsystem", "Select windows subsystem. [CUI(default), GUI]", &subSystem,
			"check-only", "Disable backend passes, leaving only error checking", &checkOnly,
			"bundle", "Emit .har file containing all of the input files and CLI arguments. No other output will be generated", &driver.context.bundleInputs,

			"no-dce", "Disable Dead Code Elimination", &driver.context.disableDCE,
			"no-inline", "Disable Inlining", &driver.context.disableInline,

			"print-time", "Print time of compilation.", &printTime,
			"print-mem", "Print memory consumtion.", &printMem,
			"print-stats", "Print general statistics", &printStats,

			"print-source", "Print source code.", &driver.context.printSource,
			"print-lexemes", "Print lexemes.", &driver.context.printLexemes,
			"print-ast-fresh", "Print AST after parsing.", &driver.context.printAstFresh,
			"print-ast-sema", "Print AST after semantic analisys.", &driver.context.printAstSema,
			"print-ir", "Print IR after AST to IR pass.", &driver.context.printIr,
			"print-ir-opt-each", "Print IR after each optimization pass.", &driver.context.printIrOptEach,
			"print-ir-opt", "Print IR after all optimization passes.", &driver.context.printIrOpt,
			"print-ir-lower-each", "Print IR after each lowering pass.", &driver.context.printIrLowerEach,
			"print-ir-lower", "Print IR after all lowering passes.", &driver.context.printIrLower,
			"print-lir", "Print Print LIR after IR to LIR pass.", &driver.context.printLir,
			"print-liveness", "Print liveness analisys info.", &driver.context.printLiveIntervals,
			"print-lir-ra", "Print LIR after register allocation pass.", &driver.context.printLirRA,
			"print-stack-layout", "Print stack layout.", &driver.context.printStackLayout,
			"print-code-hex", "Print code hex.", &driver.context.printCodeHex,
			"print-symbols", "Print symbols.", &driver.context.printSymbols,
			"print-filter", "Print only info about <function name>.", &filterFuncName,
			"print-error-trace", "Print stack trace for every error", &driver.context.printTraceOnError,
		);
	}
	catch(GetOptException e)
	{
		import std.algorithm.mutation : remove;

		writeln(e.msg);
		printHelp = true;
		args = args.remove(1);
		goto retry_parse_opts;
	}

	switch(outputTarget) {
		case "windows-x64": driver.context.targetOs = TargetOs.windows; break;
		case "linux-x64":   driver.context.targetOs = TargetOs.linux;   break;
		case "macos-x64":   driver.context.targetOs = TargetOs.macos;   break;
		//case "linux-arm64": driver.context.targetOs = TargetOs.linux; driver.context.targetArch = TargetArch.arm64; break;
		default:
			writefln("Unknown target: %s", outputTarget);
			printHelp = true;
	}

	if (args.length < 2) printHelp = true;
	if (args.length > 0) args = args[1..$]; // skip program name

	if (optResult.helpWanted) printHelp = true;

	if (printHelp)
	{
		writeln("Usage: vox [tool] [options]... [.vx|.har]...");
		writeln(" tools:");
		writeln("   pdb-dump - dumps the contents of a .pdb file");
		writeln(" options:");

		size_t maxShortLength;
		size_t maxLongLength;
		foreach (it; optResult.options)
		{
			maxShortLength = max(maxShortLength, it.optShort.length);
			maxLongLength = max(maxLongLength, it.optLong.length);
		}

		foreach (it; optResult.options)
		{
			writefln("%-*s %-*s  %s", maxShortLength, it.optShort, maxLongLength, it.optLong, it.help);
		}

		return 0;
	}

	final switch (subSystem) {
		case WindowsSubsystemCli.CUI: driver.context.windowsSubsystem = WindowsSubsystem.WINDOWS_CUI; break;
		case WindowsSubsystemCli.GUI: driver.context.windowsSubsystem = WindowsSubsystem.WINDOWS_GUI; break;
	}

	string[] filenames = args;

	auto passes = exePasses;
	// Disable backend for check-only
	if (checkOnly) passes = frontendOnlyPasses;
	if (driver.context.bundleInputs) passes = bundlePasses;

	driver.context.buildType = BuildType.exe;

	driver.initialize(passes);
	driver.beginCompilation();

	if (outputFilename) driver.context.outputFilename = outputFilename;
	else {
		string ext;
		if (driver.context.targetOs == TargetOs.windows) ext = ".exe";
		driver.context.outputFilename = filenames[0].baseName.setExtension(ext); // GC
	}
	if (driver.context.bundleInputs) {
		driver.context.outputFilename = driver.context.outputFilename.setExtension(".har");
	}

	if (filterFuncName) driver.context.setDumpFilter(filterFuncName);

	auto times = PerPassTimeMeasurements(1, driver.passes);
	auto endInitTime = currTime;

	try
	{
		foreach(filename; filenames)
		{
			string ext = filename.extension;
			switch(ext)
			{
				case ".har":
					if (!exists(filename))
					{
						driver.context.error("File `%s` not found", absolutePath(filename));
						break;
					}

					auto file = File(filename, "r");
					char[] sourceBuffer = driver.context.sourceBuffer.voidPut(file.size);
					char[] harData = file.rawRead(sourceBuffer);
					file.close();

					void onHarFile(SourceFileInfo fileInfo)
					{
						if (fileInfo.name == "<args>") {
							// skip
						} else {
							driver.addModule(fileInfo);
						}
					}

					parseHar(driver.context, filename, harData, &onHarFile);
					break;
				default:
					driver.addModule(SourceFileInfo(filename));
					break;
			}
		}

		scope(exit) {
			// In case of an error write the output
			if (driver.context.bundleInputs) write_bundle(driver.context);
		}

		driver.compile();
	}
	catch(CompilationException e) {
		writeln(driver.context.sink.text);
		if (e.isICE) {
			writeln(e);
		}
		return 1;
	}
	catch(Throwable t) {
		writeln(driver.context.sink.text);
		writeln(t);
		return 1;
	}

	TextSink sink;
	if (printMem) driver.context.printMemSize(sink);

	auto startReleaseTime = currTime;
		// releasing memory is not necessary when running in standalone mode
		driver.releaseMemory;
	auto endReleaseTime = currTime;

	auto time2 = currTime;
	Duration duration = time2-time1;

	times.onIteration(0, duration);

	if (printMem) write(cast(string)sink.data.data);

	if (printStats) {
		writefln("Lexed %s lines", driver.context.numLinesLexed);
	}

	if (printTime) {
		times.print;
	}

	if (printStats || printTime) {
		writefln("Finished in %ss, init %ss, release %ss",
			scaledNumberFmt(duration),
			scaledNumberFmt(endInitTime-startInitTime),
			scaledNumberFmt(endReleaseTime-startReleaseTime));
	}

	return 0;
}
