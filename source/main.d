/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module main;

import std.stdio;
import bench;
import cli;
import tests;

version = standalone;
//version = cli;
//version = bench;
//version = test;
version(standalone) void main(string[] args)
{
	scope(exit) stdout.flush;
	version(cli) runCli(args);
	version(bench) runBench();
	version(test) runAllTests(StopOnFirstFail.yes);
	//runDevTests();
}

unittest
{
	runAllTests(StopOnFirstFail.no);
}
