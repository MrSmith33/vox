/// Copyright: Copyright (c) 2017-2021 Andrey Penechko.
/// License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
/// Authors: Andrey Penechko.
module main;

version = standalone;
//version = cli;
//version = bench;
//version = asmtest;
//version = devtest;
//version = test;

version(standalone) int main(string[] args)
{
	scope(exit) {
		import std.stdio;
		stdout.flush;
	}

	version(cli) {
		import cli;
		return runCli(args);
	} else version(bench) {
		import bench;
		return runBench(args);
	} else version(asmtest) {
		import tests.amd64asm_tests;
		return testAmd64Asm();
	} else version(devtest) {
		import tester;
		return runDevTests();
	} else version(test) {
		import tester;
		return runAllTests(StopOnFirstFail.no);
	} else return 0;
}
