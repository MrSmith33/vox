/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module main;

import bench;
import tests;

version = standalone;
version(standalone) void main()
{
	//runBench();
	runAllTests(StopOnFirstFail.yes);
	//runDevTests();
}

unittest
{
	runAllTests(StopOnFirstFail.no);
}
