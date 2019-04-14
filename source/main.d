/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module main;

import std.stdio;
import bench;
import cli;
import tester;

version = standalone;
//version = cli;
//version = bench;
version = test;
version(standalone) void main(string[] args)
{
	scope(exit) stdout.flush;
	version(cli) runCli(args);
	version(bench) runBench();
	version(test) runAllTests(StopOnFirstFail.yes);
	//runDevTests();
	//runCli([args[0], "sdl_test.har"]);
	//runCli([args[0], "--print-mem", "--subsystem=GUI", "sdl_window.d", "SDL2.dll", `C:\Windows\System32\kernel32.dll`]);
}

unittest
{
	runAllTests(StopOnFirstFail.no);
}
