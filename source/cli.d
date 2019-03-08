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

	bool printTime = true;

	if (args.length < 1)
	{
		writefln("Usage: tiny_jit [options...] infile(s)...", args);
		return;
	}
	string filename = args[0];

	auto time1 = currTime;
	auto startInitTime = currTime;
		Driver driver;
		driver.initialize(exePasses);
		driver.context.buildType = BuildType.exe;
		driver.context.outputFilename = setExtension(filename, ".exe");
		auto times = PerPassTimeMeasurements(1, driver.passes);
	auto endInitTime = currTime;

	try
	{
		driver.compileModule(SourceFileInfo(filename), null, null);
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
		return;
	}

	auto startReleaseTime = currTime;
		// releasing memory is not necessary when in runningstandalone mode
		//driver.releaseMemory;
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
