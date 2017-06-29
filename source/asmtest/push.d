/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module asmtest.push;

void testPush()
{
	import asmtest.utils;

	//push reg16
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.pushw(reg);
	assertHexAndReset("66506651665266536654665566566657664150664151664152664153664154664155664156664157");

	//push reg64
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.pushq(reg);
	assertHexAndReset("505152535455565741504151415241534154415541564157");

	//push WORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.pushw(memAddrBase(reg));
	assertHexAndReset("66FF3066FF3166FF3266FF3366FF342466FF750066FF3666FF376641FF306641FF316641FF326641FF336641FF34246641FF75006641FF366641FF37");

	//push QWORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.pushq(memAddrBase(reg));
	assertHexAndReset("FF30FF31FF32FF33FF3424FF7500FF36FF3741FF3041FF3141FF3241FF3341FF342441FF750041FF3641FF37");

	//push Imm8/16/32
	testCodeGen.pushb(Imm8(0x11));
	testCodeGen.pushw(Imm16(0x1122));
	testCodeGen.pushd(Imm32(0x11223344));
	assertHexAndReset("6A11666822116844332211");
}
