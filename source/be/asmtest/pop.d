/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.asmtest.pop;

import be.asmtest.utils;

void testPop(CodegenTester tester)
{
	//pop reg16
	foreach (Register reg; Register.min..RegisterMax) tester.popw(reg);
	tester.assertHexAndReset("66586659665A665B665C665D665E665F66415866415966415A66415B66415C66415D66415E66415F");

	//pop reg64
	foreach (Register reg; Register.min..RegisterMax) tester.popq(reg);
	tester.assertHexAndReset("58595A5B5C5D5E5F41584159415A415B415C415D415E415F");

	//pop WORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) tester.popw(memAddrBase(reg));
	tester.assertHexAndReset("668F00668F01668F02668F03668F0424668F4500668F06668F0766418F0066418F0166418F0266418F0366418F042466418F450066418F0666418F07");

	//pop QWORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) tester.popq(memAddrBase(reg));
	tester.assertHexAndReset("8F008F018F028F038F04248F45008F068F07418F00418F01418F02418F03418F0424418F4500418F06418F07");
}
