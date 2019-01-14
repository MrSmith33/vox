/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module asmtest.not;

import asmtest.utils;

void testNot(CodegenTester tester)
{
	//not BYTE PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) tester.notb(memAddrBase(reg));
	tester.assertHexAndReset("F610F611F612F613F61424F65500F616F61741F61041F61141F61241F61341F6142441F6550041F61641F617");

	//not WORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) tester.notw(memAddrBase(reg));
	tester.assertHexAndReset("66F71066F71166F71266F71366F7142466F7550066F71666F7176641F7106641F7116641F7126641F7136641F714246641F755006641F7166641F717");

	//not DWORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) tester.notd(memAddrBase(reg));
	tester.assertHexAndReset("F710F711F712F713F71424F75500F716F71741F71041F71141F71241F71341F7142441F7550041F71641F717");

	//not QWORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) tester.notq(memAddrBase(reg));
	tester.assertHexAndReset("48F71048F71148F71248F71348F7142448F7550048F71648F71749F71049F71149F71249F71349F7142449F7550049F71649F717");

	//not reg8
	foreach (Register reg; Register.min..RegisterMax) tester.notb(reg);
	tester.assertHexAndReset("F6D0F6D1F6D2F6D340F6D440F6D540F6D640F6D741F6D041F6D141F6D241F6D341F6D441F6D541F6D641F6D7");

	//not reg16
	foreach (Register reg; Register.min..RegisterMax) tester.notw(reg);
	tester.assertHexAndReset("66F7D066F7D166F7D266F7D366F7D466F7D566F7D666F7D76641F7D06641F7D16641F7D26641F7D36641F7D46641F7D56641F7D66641F7D7");

	//not reg32
	foreach (Register reg; Register.min..RegisterMax) tester.notd(reg);
	tester.assertHexAndReset("F7D0F7D1F7D2F7D3F7D4F7D5F7D6F7D741F7D041F7D141F7D241F7D341F7D441F7D541F7D641F7D7");

	//not reg64
	foreach (Register reg; Register.min..RegisterMax) tester.notq(reg);
	tester.assertHexAndReset("48F7D048F7D148F7D248F7D348F7D448F7D548F7D648F7D749F7D049F7D149F7D249F7D349F7D449F7D549F7D649F7D7");
}
