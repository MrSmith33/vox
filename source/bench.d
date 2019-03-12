/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module bench;

import all;
import tests;
import std.stdio;

void runBench()
{
	ModuleDeclNode* mod;
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
		mod = driver.compileModule(SourceFileInfo("test", curTest.source), curTest.hostSymbols, curTest.dllModules);
		auto time2 = currTime;

		times.onIteration(iteration, time2-time1);
	}

	driver.context.printMemSize;
	times.print;
}
