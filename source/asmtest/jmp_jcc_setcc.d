/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module asmtest.jmp_jcc_setcc;

import asmtest.utils;

void testJmpJccSetcc(CodegenTester tester)
{
	//jmp
	tester.jmp(Imm8(0xAA));
	tester.jmp(Imm32(0xAABBCCDD));
	tester.assertHexAndReset("EBAAE9DDCCBBAA");

	// je
	tester.jcc(Condition.E, Imm8(0xAA));
	tester.jcc(Condition.E, Imm32(0xAABBCCDD));
	tester.assertHexAndReset("74AA0F84DDCCBBAA");

	//jne
	tester.jcc(Condition.NE, Imm8(0xAA));
	tester.jcc(Condition.NE, Imm32(0xAABBCCDD));
	tester.assertHexAndReset("75AA0F85DDCCBBAA");

	//sete
	foreach (Register reg; Register.min..RegisterMax) tester.setcc(Condition.E, reg);
	foreach (Register reg; Register.min..RegisterMax) tester.setcc(Condition.E, memAddrBase(reg));
	tester.assertHexAndReset("0F94C00F94C10F94C20F94C3400F94C4400F94C5400F94C6400F94C7410F94C0410F94C1410F94C2410F94C3410F94C4410F94C5410F94C6410F94C70F94000F94010F94020F94030F9404240F9445000F94060F9407410F9400410F9401410F9402410F9403410F940424410F944500410F9406410F9407");

	//setne
	foreach (Register reg; Register.min..RegisterMax) tester.setcc(Condition.NE, reg);
	foreach (Register reg; Register.min..RegisterMax) tester.setcc(Condition.NE, memAddrBase(reg));
	tester.assertHexAndReset("0F95C00F95C10F95C20F95C3400F95C4400F95C5400F95C6400F95C7410F95C0410F95C1410F95C2410F95C3410F95C4410F95C5410F95C6410F95C70F95000F95010F95020F95030F9504240F9545000F95060F9507410F9500410F9501410F9502410F9503410F950424410F954500410F9506410F9507");

	//setg
	foreach (Register reg; Register.min..RegisterMax) tester.setcc(Condition.G, reg);
	foreach (Register reg; Register.min..RegisterMax) tester.setcc(Condition.G, memAddrBase(reg));
	tester.assertHexAndReset("0F9FC00F9FC10F9FC20F9FC3400F9FC4400F9FC5400F9FC6400F9FC7410F9FC0410F9FC1410F9FC2410F9FC3410F9FC4410F9FC5410F9FC6410F9FC70F9F000F9F010F9F020F9F030F9F04240F9F45000F9F060F9F07410F9F00410F9F01410F9F02410F9F03410F9F0424410F9F4500410F9F06410F9F07");
}
