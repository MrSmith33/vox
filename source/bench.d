/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module bench;

import all;
import tester;
import std.stdio;
import std.string : stripLeft;
import tests.passing;

/// --tsv  output in tsv format with greek µ instead of u
void runBench(string[] args)
{
	bool outputTsv;
	foreach(arg; args) if (arg == "--tsv") outputTsv = true;

	Test curTest = test8;
	//Test curTest = test31;

	Driver driver;
	driver.initialize(jitPasses);
	//driver.context.validateIr = true;
	scope(exit) driver.releaseMemory;

	enum iters = 100_000;
	auto times = PerPassTimeMeasurements(iters, driver.passes);

	foreach (iteration; 0..times.totalTimes.numIters)
	{
		auto time1 = currTime;
		driver.beginCompilation();
		driver.addHostSymbols(curTest.hostSymbols);
		driver.addDllModules(curTest.dllModules);
		string strippedHar = curTest.harData.stripLeft;
		driver.addHar("test.har", strippedHar);
		driver.compile();
		auto time2 = currTime;

		times.onIteration(iteration, time2-time1);
	}

	driver.context.printMemSize;

	if (outputTsv) times.printTsv;
	else times.print;
}
