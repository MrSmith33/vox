/// Copyright: Copyright (c) 2017-2019 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module tester;

import all;
import tests.aggregates;
import tests.ctfe;
import tests.passing;
import tests.failing;
import tests.exe;
import tests.reg_alloc;
public import all : HostSymbol, Slice, SliceString;

public import std.format : formattedWrite, format;
import std.string : stripLeft, strip;

int runDevTests()
{
	Test test = makeTest!(test243);

	Driver driver;
	driver.initialize(jitPasses);
	driver.context.buildType = BuildType.jit;
	driver.context.validateIr = true;
	driver.context.printTraceOnError = true;
	driver.context.printTodos = true;
	//driver.context.disableDCE = true;
	//driver.context.runTesters = false;
	//driver.context.debugRegAlloc = true;
	//driver.context.buildType = BuildType.exe;
	//driver.passes = exePasses;
	//driver.context.targetOs = TargetOs.linux;
	//driver.context.windowsSubsystem = WindowsSubsystem.WINDOWS_GUI;
	//test.printFilter = "i32_to_str";

	scope(exit) {
		//driver.context.printMemSize;
		driver.releaseMemory;
	}

	FuncDumpSettings dumpSettings;
	dumpSettings.printBlockFlags = true;
	//dumpSettings.printBlockOuts = true;

	//driver.context.printSource = true;
	//driver.context.printLexemes = true;
	//driver.context.printAstFresh = true;
	//driver.context.printAstSema = true;
	//driver.context.printIr = true;
	//driver.context.printIrOptEach = true;
	//driver.context.printIrOpt = true;
	//driver.context.printIrLowerEach = true;
	//driver.context.printIrLower = true;
	//driver.context.printLir = true;
	//driver.context.printLiveIntervals = true;
	//driver.context.printLirRA = true;
	//driver.context.printStackLayout = true;
	//driver.context.printStaticData = true;
	//driver.context.printCodeHex = true;
	//driver.context.printTimings = true;

	TestResult res = tryRunSingleTest(driver, dumpSettings, DumpTest.yes, test);
	//writefln("%s", driver.context.numCtfeRuns);

	return cast(int)(res == TestResult.failure);
}

enum StopOnFirstFail : bool { no = false, yes = true }

// Returns number of failed tests
int runAllTests(StopOnFirstFail stopOnFirstFail)
{
	auto startInitTime = currTime;
	Driver driver;
	driver.initialize(jitPasses);
	driver.context.buildType = BuildType.jit;
	driver.context.buildDebug = false;
	driver.context.validateIr = true;
	//driver.context.debugRegAlloc = true;
	//driver.context.printIr = true;

	//driver.context.printAstSema = true;
	// Is slow when doing failing tests
	//driver.context.printTraceOnError = true;
	auto endInitTime = currTime;

	FuncDumpSettings dumpSettings;
	dumpSettings.printBlockFlags = true;

	Test[] jitTests =
		tests.passing.passingTests ~
		tests.aggregates.aggregatesTests ~
		tests.ctfe.ctfeTests ~
		tests.failing.failingTests;
	Test[] regAllocTests = tests.reg_alloc.regAllocTests;
	Test[] exeTests = tests.exe.exeTests;

	size_t numTests = jitTests.length + regAllocTests.length;
	version(Windows) numTests += exeTests.length;
	size_t numSuccessfulTests;
	writefln("Running %s tests", numTests);

	size_t indexOffset = 0;
	bool failed = false;

	void runTests(Test[] tests)
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
				writeln;
			}
			else
			{
				//writefln("%s `%s` success", indexOffset+i+1, test.testName);
				++numSuccessfulTests;
			}
		}
		indexOffset += tests.length;
	}

	auto time1 = currTime;

	runTests(jitTests);
	auto time2 = currTime;

	driver.context.debugRegAlloc = true;
	runTests(regAllocTests);
	driver.context.debugRegAlloc = false;
	auto time3 = currTime;

	driver.context.buildType = BuildType.exe;
	driver.passes = exePasses;
	version(Windows) runTests(exeTests);
	auto time4 = currTime;

	Duration durationJit = time2-time1;
	Duration durationRA = time3-time2;
	Duration durationExe = time4-time3;
	Duration durationTotal = time4-time1;

	auto startReleaseTime = currTime;
	driver.releaseMemory;
	auto endReleaseTime = currTime;

	writefln("jit(%s tests) %ss",
		jitTests.length,
		scaledNumberFmt(durationJit),
		);
	writefln("RA(%s tests) %ss",
		regAllocTests.length,
		scaledNumberFmt(durationRA),
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

	return cast(int)(numTests - numSuccessfulTests);
}

enum DumpTest : bool { no = false, yes = true }
enum TestResult { failure, success }
struct Test
{
	string testName;
	string harData;
	void function(ref TestContext) tester;
	HostSymbol[] hostSymbols;
	string printFilter;
}

Test makeTest(alias test)() {
	static assert (__traits(getAttributes, test).length > 0);
	TestInfo info = __traits(getAttributes, test)[0];
	return Test(__traits(identifier, test), test, info.tester, info.hostSymbols);
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
}

struct TestContext
{
	Driver* driver;

	auto getFunctionPtr(ResultType, ParamTypes...)(string funcName) {
		return driver.context.getFunctionPtr!(ResultType, ParamTypes)(funcName);
	}
}

/// Global test output for jit tests
/// Cleared automatically after each test
TextSink testSink;

extern(C) void external_noop() {}

TestResult tryRunSingleTest(ref Driver driver, ref FuncDumpSettings dumpSettings, DumpTest dumpTest, Test curTest)
{
	scope(exit) testSink.clear;
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
}

TestResult runSingleTest(ref Driver driver, ref FuncDumpSettings dumpSettings, DumpTest dumpTest, Test curTest)
{
	import std.string : lineSplitter;
	import std.algorithm : joiner, equal;
	bool isFailingTest;
	string expectedError;

	enum NUM_ITERS = 1;
	auto time1 = currTime;

		// setup modules
		driver.beginCompilation();

		if (curTest.printFilter.length) driver.context.setDumpFilter(curTest.printFilter);

		driver.addHostSymbols(curTest.hostSymbols);

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

		// We strip beginning to allow newline between `q{` and `--- <filename>` for better readability
		string strippedHar = curTest.harData.stripLeft;
		parseHar(driver.context, "test.har", strippedHar, &onHarFile);

		// Splitting lines gets rid of \r\n on windows, which will cause mismatch with error message
		auto expectedErrorRange = expectedError.lineSplitter.joiner("\n");

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
			if (equal(driver.context.errorSink.text, expectedErrorRange)) {
				return TestResult.success;
			}

			writefln("Test `%s` failed", curTest.testName);
			writefln("Expected error:");
			writeln(expectedErrorRange);
			writefln("Received error:");
			writeln(driver.context.errorSink.text);
			writefln("Stack trace:");
			writeln(driver.context.sink.text);
			writeln(e.info);
			return TestResult.failure;
		}

		// Compiled successfully, but expected to fail
		if (isFailingTest) {
			writefln("Test `%s` compiled successfully, but expected to fail", curTest.testName);
			writefln("Expected error:");
			writeln(expectedErrorRange);
			return TestResult.failure;
		}

		// finalize code pages
		driver.markCodeAsExecutable();

	auto time2 = currTime;
	if (dumpTest && driver.context.printTimings)
		writefln("Compiled in %ss", scaledNumberFmt(time2-time1));

	if (!driver.context.runTesters) return TestResult.success;
	if (driver.context.targetOs != driver.context.hostOS) return TestResult.success;

	final switch (driver.context.buildType)
	{
		case BuildType.jit:
			if (curTest.tester) {
				auto testContext = TestContext(&driver);
				if (dumpTest) writefln("Running: %s tester", curTest.testName);
				auto time3 = currTime;
				curTest.tester(testContext);
				auto time4 = currTime;
				if (dumpTest && driver.context.printTimings)
					writefln("Run in %ss", scaledNumberFmt(time4-time3));
			}
			break;

		case BuildType.exe:
			import std.process;
			import std.file : exists;
			import std.path;
			if(exists(driver.context.outputFilename))
			{
				if (dumpTest) writef("Running: %s", driver.context.outputFilename.absolutePath);
				auto time3 = currTime;
				auto result = execute(driver.context.outputFilename);
				auto time4 = currTime;
				if (dumpTest && driver.context.printTimings)
					writefln("Run in %ss", scaledNumberFmt(time4-time3));
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
