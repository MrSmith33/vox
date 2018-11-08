/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module bench;

import all;

void runBench()
{
	ModuleDeclNode* mod;
	string input =
	q{i32 sign(i32 number)
	{
		i32 result;
		if (number < 0) result = 0-1;
		else if (number > 0) result = 1;
		else result = 0;
		return result;
	}};

	Driver driver;
	driver.initialize(compilerPasses);
	scope(exit) driver.releaseMemory;

	enum iters = 100_000;
	auto times = PerPassTimeMeasurements(iters, driver.passes);

	foreach (iteration; 0..times.totalTimes.numIters)
	{
		auto time1 = currTime;
		mod = driver.compileModule(input, null);
		auto time2 = currTime;

		times.onIteration(iteration, time2-time1);
	}

	times.print;
}
