/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module benchutils;

import std.datetime : MonoTime, Duration, usecs, dur;

MonoTime currTime() {
	return MonoTime.currTime();
}
