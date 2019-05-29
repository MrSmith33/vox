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

	Test curTest = test21;
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

immutable input = q{--- fibonacci
	// test fibonacci. while loop. func call
	void print(i32){}
	i32 fibonacci(i32 max) {
		i32 lo = 0;
		i32 hi = 1;
		while (hi < max) {
			hi = hi + lo;
			lo = hi - lo;
			print(lo);
		}
		return lo;
	}
};

void benchSpeed()
{
	auto test = Test("Fib", input, "fibonacci");

	Driver driver;
	driver.initialize(jitPasses);
	scope(exit) driver.releaseMemory;

	driver.beginCompilation();
	driver.addHostSymbols(test.hostSymbols);
	driver.addDllModules(test.dllModules);
	string strippedHar = test.harData.stripLeft;
	driver.addHar("test.har", strippedHar);
	driver.compile();
	driver.markCodeAsExecutable();
	FunctionDeclNode* funDecl;
	foreach (ref SourceFileInfo file; driver.context.files.data)
	{
		FunctionDeclNode* fun = file.mod.findFunction(test.funcName, &driver.context);
		if (fun !is null)
		{
			if (funDecl !is null)
				driver.context.internal_error("test function %s is found in 2 places", test.funcName);
			funDecl = fun;
		}
	}

	if (funDecl is null)
		driver.context.internal_error("test function `%s` is not found in %s modules", test.funcName, driver.context.files.length);

	alias FuncT = extern(C) int function(int);
	FuncT f;
	if (funDecl != null && funDecl.backendData.funcPtr != null)
	{
		f = cast(FuncT)funDecl.backendData.funcPtr;
	}

	auto time1 = currTime;
	int res;
	foreach (iteration; 0..100_000)
	{
		res = f(1073741824);
	}
	auto time2 = currTime;

	writefln("run time %ss", scaledNumberFmt(time2-time1));
	writefln("res %s", res);
}
