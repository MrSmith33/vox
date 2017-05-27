/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module test.inc;

void testInc()
{
	import test.utils;

	//inc BYTE PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.incb(memAddrBase(reg));
	assertHexAndReset("FE00FE01FE02FE03FE0424FE4500FE06FE0741FE0041FE0141FE0241FE0341FE042441FE450041FE0641FE07");

	//inc WORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.incw(memAddrBase(reg));
	assertHexAndReset("66FF0066FF0166FF0266FF0366FF042466FF450066FF0666FF076641FF006641FF016641FF026641FF036641FF04246641FF45006641FF066641FF07");

	//inc DWORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.incd(memAddrBase(reg));
	assertHexAndReset("FF00FF01FF02FF03FF0424FF4500FF06FF0741FF0041FF0141FF0241FF0341FF042441FF450041FF0641FF07");

	//inc QWORD PTR [reg]
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.incq(memAddrBase(reg));
	assertHexAndReset("48FF0048FF0148FF0248FF0348FF042448FF450048FF0648FF0749FF0049FF0149FF0249FF0349FF042449FF450049FF0649FF07");

	//inc reg8
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.incb(reg);
	assertHexAndReset("FEC0FEC1FEC2FEC340FEC440FEC540FEC640FEC741FEC041FEC141FEC241FEC341FEC441FEC541FEC641FEC7");

	//inc reg16
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.incw(reg);
	assertHexAndReset("66FFC066FFC166FFC266FFC366FFC466FFC566FFC666FFC76641FFC06641FFC16641FFC26641FFC36641FFC46641FFC56641FFC66641FFC7");

	//inc reg32
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.incd(reg);
	assertHexAndReset("FFC0FFC1FFC2FFC3FFC4FFC5FFC6FFC741FFC041FFC141FFC241FFC341FFC441FFC541FFC641FFC7");

	//inc reg64
	foreach (Register reg; Register.min..RegisterMax) testCodeGen.incq(reg);
	assertHexAndReset("48FFC048FFC148FFC248FFC348FFC448FFC548FFC648FFC749FFC049FFC149FFC249FFC349FFC449FFC549FFC649FFC7");
}
