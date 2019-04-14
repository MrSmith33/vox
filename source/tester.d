/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tester;

import all;
import tests.passing;
import tests.failing;
import tests.exe;
public import all : HostSymbol, DllModule;

public import std.format : formattedWrite, format;
import std.string : stripLeft;

void runDevTests()
{
	Driver driver;
	driver.initialize(jitPasses);
	driver.context.buildType = BuildType.jit;
	driver.context.validateIr = true;
	driver.context.printTraceOnError = true;
	driver.context.printTodos = true;
	scope(exit) driver.releaseMemory;

	FuncDumpSettings dumpSettings;
	dumpSettings.printBlockFlags = true;

	//driver.context.printSource = true;
	//driver.context.printLexemes = true;
	//driver.context.printAstFresh = true;
	//driver.context.printAstSema = true;
	//driver.context.runTesters = false;

	//driver.context.printIr = true;
	//driver.context.printIrOpt = true;
	//driver.context.printLir = true;
	//driver.context.printLirRA = true;
	//driver.context.printLiveIntervals = true;
	//driver.context.printStaticData = true;
	//driver.context.printCodeHex = true;
	//driver.context.printTimings = true;

	tryRunSingleTest(driver, dumpSettings, DumpTest.yes, test36);

	//driver.context.buildType = BuildType.exe;
	//driver.passes = exePasses;
	//driver.context.windowsSubsystem = WindowsSubsystem.WINDOWS_GUI;
	//tryRunSingleTest(driver, dumpSettings, DumpTest.yes, test30);
}

enum StopOnFirstFail : bool { no = false, yes = true }

void runAllTests(StopOnFirstFail stopOnFirstFail)
{
	auto startInitTime = currTime;
	Driver driver;
	driver.initialize(jitPasses);
	driver.context.buildType = BuildType.jit;
	driver.context.buildDebug = false;
	driver.context.validateIr = true;
	driver.context.printTraceOnError = true;
	auto endInitTime = currTime;

	FuncDumpSettings dumpSettings;
	dumpSettings.printBlockFlags = true;

	Test[] jitTests = tests.passing.passingTests();
	Test[] exeTests = tests.exe.exeTests();

	size_t numTests = jitTests.length + exeTests.length;
	size_t numSuccessfulTests;
	writefln("Running %s tests", numTests);
	bool failed = false;

	void runTests(size_t indexOffset, Test[] tests)
	{
		foreach(size_t i, ref Test test; tests)
		{
			if (stopOnFirstFail && failed) break;

			TestResult res = tryRunSingleTest(driver, dumpSettings, DumpTest.no, test);

			if (res == TestResult.failure)
			{
				writefln("%s/%s %s %s", indexOffset+i+1, numTests, test.testName, res);
				failed = true;
				if (stopOnFirstFail) writeln("Stopping on first fail");
			}
			else
			{
				//writefln("%s `%s` success", indexOffset+i+1, test.testName);
				++numSuccessfulTests;
			}
		}
	}

	auto time1 = currTime;

	runTests(0, passingTests);

	auto time2 = currTime;
	driver.context.buildType = BuildType.exe;
	driver.passes = exePasses;
	runTests(jitTests.length, exeTests);

	auto time3 = currTime;
	Duration durationJit = time2-time1;
	Duration durationExe = time3-time2;
	Duration durationTotal = time3-time1;

	auto startReleaseTime = currTime;
	driver.releaseMemory;
	auto endReleaseTime = currTime;

	writefln("jit(%s tests) %ss",
		jitTests.length,
		scaledNumberFmt(durationJit),
		);
	writefln("exe(%s tests) %ss",
		exeTests.length,
		scaledNumberFmt(durationExe),
		);
	writefln("Done %s/%s successful in %ss, init %ss, release %ss",
		numSuccessfulTests,
		numTests,
		scaledNumberFmt(durationTotal),
		scaledNumberFmt(endInitTime-startInitTime),
		scaledNumberFmt(endReleaseTime-startReleaseTime),
		);
}

enum DumpTest : bool { no = false, yes = true }
enum TestResult { failure, success }
struct Test
{
	string testName;
	string harData;
	string funcName;
	alias Tester = void function(void* funcPtr);
	void function(void* funcPtr) tester;
	HostSymbol[] hostSymbols;
	DllModule[] dllModules;
}

/// Global test output for jit tests
TextSink testSink;

TestResult tryRunSingleTest(ref Driver driver, ref FuncDumpSettings dumpSettings, DumpTest dumpTest, Test curTest)
{
	try
	{
		runSingleTest(driver, dumpSettings, dumpTest, curTest);
	}
	catch(CompilationException e) {
		writeln(driver.context.sink.text);
		if (e.isICE)
			writeln(e);
		return TestResult.failure;
	}
	catch(Throwable t) {
		writeln(driver.context.sink.text);
		writeln(t);
		return TestResult.failure;
	}
	return TestResult.success;
}

void runSingleTest(ref Driver driver, ref FuncDumpSettings dumpSettings, DumpTest dumpTest, Test curTest)
{
	enum NUM_ITERS = 1;
	auto times = PerPassTimeMeasurements(NUM_ITERS, driver.passes);
	auto time1 = currTime;
		driver.beginCompilation();
		driver.addHostSymbols(curTest.hostSymbols);
		driver.addDllModules(curTest.dllModules);
		string strippedHar = curTest.harData.stripLeft;
		driver.addHar("test.har", strippedHar);
		driver.compile();
		driver.markCodeAsExecutable();
	auto time2 = currTime;
	times.onIteration(0, time2-time1);

	if (dumpTest && driver.context.printTimings) times.print;

	if (!driver.context.runTesters) return;

	final switch (driver.context.buildType)
	{
		case BuildType.jit:
			if (curTest.funcName is null) return;

			FunctionDeclNode* funDecl;
			foreach (ref SourceFileInfo file; driver.context.files.data)
			{
				FunctionDeclNode* fun = file.mod.findFunction(curTest.funcName, &driver.context);
				if (fun !is null)
				{
					if (funDecl !is null)
						driver.context.internal_error("test function %s is found in 2 places", curTest.funcName);
					funDecl = fun;
				}
			}

			if (funDecl is null)
				driver.context.internal_error("test function `%s` is not found in %s modules", curTest.funcName, driver.context.files.length);

			if (funDecl != null && funDecl.backendData.funcPtr != null)
			{
				if (dumpTest) writefln("Running: %s %s()", curTest.testName, curTest.funcName);
				curTest.tester(funDecl.backendData.funcPtr);
			}
			break;

		case BuildType.exe:
			import std.process;
			import std.file : exists;
			import std.path;
			if(exists(driver.context.outputFilename))
			{
				if (dumpTest) writef("Running: %s", driver.context.outputFilename.absolutePath);
				auto result = execute(driver.context.outputFilename);
				if (dumpTest) writefln(", status %s, output '%s'", result.status, result.output);
			}
			else
			{
				writefln("No executable produced '%s'", driver.context.outputFilename);
			}
			break;
	}
}