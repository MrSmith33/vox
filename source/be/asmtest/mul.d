/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.asmtest.mul;

import be.asmtest.utils;

void testMul(CodegenTester tester)
{
	//mul reg8
	foreach (Register reg; Register.min..RegisterMax) tester.mulb(reg);
	tester.assertHexAndReset("F6E0F6E1F6E2F6E340F6E440F6E540F6E640F6E741F6E041F6E141F6E241F6E341F6E441F6E541F6E641F6E7");

	//mul reg16
	foreach (Register reg; Register.min..RegisterMax) tester.mulw(reg);
	tester.assertHexAndReset("66F7E066F7E166F7E266F7E366F7E466F7E566F7E666F7E76641F7E06641F7E16641F7E26641F7E36641F7E46641F7E56641F7E66641F7E7");

	//mul reg32
	foreach (Register reg; Register.min..RegisterMax) tester.muld(reg);
	tester.assertHexAndReset("F7E0F7E1F7E2F7E3F7E4F7E5F7E6F7E741F7E041F7E141F7E241F7E341F7E441F7E541F7E641F7E7");

	//mul reg64
	foreach (Register reg; Register.min..RegisterMax) tester.mulq(reg);
	tester.assertHexAndReset("48F7E048F7E148F7E248F7E348F7E448F7E548F7E648F7E749F7E049F7E149F7E249F7E349F7E449F7E549F7E649F7E7");

	//mul BYTE PTR "reg"
	foreach (Register reg; Register.min..RegisterMax) tester.mulb(memAddrBase(reg));
	tester.assertHexAndReset("F620F621F622F623F62424F66500F626F62741F62041F62141F62241F62341F6242441F6650041F62641F627");

	//mul WORD PTR "reg"
	foreach (Register reg; Register.min..RegisterMax) tester.mulw(memAddrBase(reg));
	tester.assertHexAndReset("66F72066F72166F72266F72366F7242466F7650066F72666F7276641F7206641F7216641F7226641F7236641F724246641F765006641F7266641F727");

	//mul DWORD PTR "reg"
	foreach (Register reg; Register.min..RegisterMax) tester.muld(memAddrBase(reg));
	tester.assertHexAndReset("F720F721F722F723F72424F76500F726F72741F72041F72141F72241F72341F7242441F7650041F72641F727");

	//mul QWORD PTR "reg"
	foreach (Register reg; Register.min..RegisterMax) tester.mulq(memAddrBase(reg));
	tester.assertHexAndReset("48F72048F72148F72248F72348F7242448F7650048F72648F72749F72049F72149F72249F72349F7242449F7650049F72649F727");
}
