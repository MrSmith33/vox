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
import std.string : stripLeft, strip;

void runDevTests()
{
	Test test = makeTest!(tests.passing.test48);
	string filterFuncName;

	Driver driver;
	driver.initialize(jitPasses);
	driver.context.buildType = BuildType.jit;
	driver.context.validateIr = true;
	driver.context.printTraceOnError = true;
	driver.context.printTodos = true;
	scope(exit) {
		//driver.context.printMemSize;
		driver.releaseMemory;
	}

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

	driver.context.printOnlyFun = Identifier.max;
	if (filterFuncName) driver.context.printOnlyFun = driver.context.idMap.getOrRegNoDup(filterFuncName);

	tryRunSingleTest(driver, dumpSettings, DumpTest.yes, test);

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
	// Is slow when doing failing tests
	//driver.context.printTraceOnError = true;
	auto endInitTime = currTime;

	FuncDumpSettings dumpSettings;
	dumpSettings.printBlockFlags = true;

	Test[] jitTests = tests.passing.passingTests ~ tests.failing.failingTests;
	Test[] exeTests = tests.exe.exeTests;

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
				writefln("%s/%s test `%s` %s", indexOffset+i+1, numTests, test.testName, res);
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

	runTests(0, jitTests);

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
	void function(ref TestContext) tester;
	HostSymbol[] hostSymbols;
	DllModule[] dllModules;
}

Test makeTest(alias test)() {
	static assert (__traits(getAttributes, test).length > 0);
	TestInfo info = __traits(getAttributes, test)[0];
	return Test(__traits(identifier, test), test, info.tester, info.hostSymbols, info.dllModules);
}

Test[] collectTests(alias M)()
{
	Test[] tests;
	import std.traits;
	foreach(m; __traits(allMembers, M))
	{
		alias member = __traits(getMember, M, m);
		static if (is(typeof(member) == immutable string)) {
			static if (__traits(getAttributes, member).length > 0) {
				tests ~= makeTest!member;
			}
		}
	}
	return tests;
}

struct TestInfo
{
	void function(ref TestContext) tester;
	HostSymbol[] hostSymbols;
	DllModule[] dllModules;
}

struct TestContext
{
	Driver* driver;

	auto getFunctionPtr(ResultType, ParamTypes...)(string funcName) {
		return driver.context.getFunctionPtr!(ResultType, ParamTypes)(funcName);
	}
}

/// Global test output for jit tests
TextSink testSink;

TestResult tryRunSingleTest(ref Driver driver, ref FuncDumpSettings dumpSettings, DumpTest dumpTest, Test curTest)
{
	try
	{
		TestResult res = runSingleTest(driver, dumpSettings, dumpTest, curTest);
		return res;
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

TestResult runSingleTest(ref Driver driver, ref FuncDumpSettings dumpSettings, DumpTest dumpTest, Test curTest)
{
	bool isFailingTest;
	string expectedError;

	enum NUM_ITERS = 1;
	auto times = PerPassTimeMeasurements(NUM_ITERS, driver.passes);
	auto time1 = currTime;

		// setup modules
		driver.beginCompilation();
		driver.addHostSymbols(curTest.hostSymbols);
		driver.addDllModules(curTest.dllModules);

		void onHarFile(SourceFileInfo fileInfo)
		{
			if (fileInfo.name == "<error>") {
				assert(!isFailingTest, format("Multiple <error> files in test `%s`", curTest.testName));
				isFailingTest = true;
				expectedError = cast(string)fileInfo.content.strip;
			} else {
				driver.addModule(fileInfo);
			}
		}

		string strippedHar = curTest.harData.stripLeft;
		parseHar(driver.context, "test.har", strippedHar, &onHarFile);

		// compile
		try
		{
			driver.compile();
		}
		catch(CompilationException e)
		{
			if (!isFailingTest) throw e;
			if (e.isICE) throw e;

			// successfully matched the error message(s)
			// TODO: we skip `times.print` below for failing tests, move it upward
			if (driver.context.errorSink.text == expectedError) {
				return TestResult.success;
			}

			writefln("Test `%s` failed", curTest.testName);
			writefln("Expected error:");
			writeln(expectedError);
			writefln("Received error:");
			writeln(driver.context.sink.text);
			writefln("Stack trace:");
			writeln(e.info);
			return TestResult.failure;
		}

		// Compiled successfully, but expected to fail
		if (isFailingTest) {
			writefln("Test `%s` compiled successfully, but expected to fail", curTest.testName);
			writefln("Expected error:");
			writeln(expectedError);
			return TestResult.failure;
		}

		// finalize code pages
		driver.markCodeAsExecutable();

	auto time2 = currTime;
	times.onIteration(0, time2-time1);

	if (dumpTest && driver.context.printTimings) times.print;

	if (!driver.context.runTesters) return TestResult.success;

	final switch (driver.context.buildType)
	{
		case BuildType.jit:
			if (curTest.tester) {
				auto testContext = TestContext(&driver);
				if (dumpTest) writefln("Running: %s tester", curTest.testName);
				curTest.tester(testContext);
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

	return TestResult.success;
}
