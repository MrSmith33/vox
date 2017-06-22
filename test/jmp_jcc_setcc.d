/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module test.jmp_jcc_setcc;

void testJmpJccSetcc()
{
	import test.utils;

	//jmp
	testCodeGen.jmp(Imm8(0xAA));
	testCodeGen.jmp(Imm32(0xAABBCCDD));
	assertHexAndReset("EBAAE9DDCCBBAA");

	// je
	testCodeGen.je(Imm8(0xAA));
	testCodeGen.je(Imm32(0xAABBCCDD));
	assertHexAndReset("74AA0F84DDCCBBAA");

	//jne
	testCodeGen.jne(Imm8(0xAA));
	testCodeGen.jne(Imm32(0xAABBCCDD));
	assertHexAndReset("75AA0F85DDCCBBAA");

	//sete
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.sete(reg);
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.sete(memAddrBase(reg));
	assertHexAndReset("0F94C00F94C10F94C20F94C3400F94C4400F94C5400F94C6400F94C7410F94C0410F94C1410F94C2410F94C3410F94C4410F94C5410F94C6410F94C70F94000F94010F94020F94030F9404240F9445000F94060F9407410F9400410F9401410F9402410F9403410F940424410F944500410F9406410F9407");

	//setne
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.setne(reg);
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.setne(memAddrBase(reg));
	assertHexAndReset("0F95C00F95C10F95C20F95C3400F95C4400F95C5400F95C6400F95C7410F95C0410F95C1410F95C2410F95C3410F95C4410F95C5410F95C6410F95C70F95000F95010F95020F95030F9504240F9545000F95060F9507410F9500410F9501410F9502410F9503410F950424410F954500410F9506410F9507");
}
