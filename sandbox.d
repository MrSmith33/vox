/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module sandbox;

import std.stdio;
import amd64asm;
import utils;
import test.utils;

enum PAGE_SIZE = 4096;

// for printing
enum Reg8  : ubyte {AL, CL, DL, BL, SPL,BPL,SIL,DIL,R8B,R9B,R10B,R11B,R12B,R13B,R14B,R15B}
enum Reg16 : ubyte {AX, CX, DX, BX, SP, BP, SI, DI, R8W,R9W,R10W,R11W,R12W,R13W,R14W,R15W}
enum Reg32 : ubyte {EAX,ECX,EDX,EBX,ESP,EBP,ESI,EDI,R8D,R9D,R10D,R11D,R12D,R13D,R14D,R15D}
enum Reg64 : ubyte {RAX,RCX,RDX,RBX,RSP,RBP,RSI,RDI,R8, R9, R10, R11, R12, R13, R14, R15 }

void main()
{
	run_from_rwx();
	//testPrintMemAddress();

	CodeGen_x86_64 codeGen;
	codeGen.encoder.setBuffer(alloc_executable_memory(PAGE_SIZE * 1024));
	//writefln("MOV byte ptr %s, 0x%X", memAddrDisp32(0x55667788), 0xAA);

	alias R = Reg64;
	enum regMax = cast(R)(R.max+1);
	foreach (R regB; R.min..regMax)
	{
		//codeGen.addq(memAddrBase(cast(Register)regB), Imm8(1));
		//writefln("mov %s, qword ptr %s", regB, memAddrBaseIndexDisp8(cast(Register)regB, cast(Register)regB, SibScale(3), 0xFE));
		//codeGen.movq(cast(Register)regB, Imm64(0x24364758AABBCCDD));
	}

	//printHex(codeGen.encoder.sink.data, 10);
	testAll();
}

void testAll()
{
	testCodeGen.encoder.setBuffer(alloc_executable_memory(PAGE_SIZE * 1024));

	import test.add;
	import test.mov;
	import test.not;
	import test.mul;
	import test.inc;
	import test.pop;
	import test.push;
	import test.cmp;
	import test.jmp_jcc_setcc;
	testAdd();
	testMov();
	testNot();
	testMul();
	testInc();
	testPop();
	testPush();
	testCmp();
	testJmpJccSetcc();
}

void testPrintMemAddress()
{
	writeln(memAddrDisp32(0x11223344));
	writeln(memAddrIndexDisp32(Register.AX, SibScale(0), 0x11223344));
	writeln(memAddrBase(Register.AX));
	writeln(memAddrBaseDisp32(Register.AX, 0x11223344));
	writeln(memAddrBaseIndex(Register.AX, Register.BX, SibScale(1)));
	writeln(memAddrBaseIndexDisp32(Register.AX, Register.BX, SibScale(2), 0x11223344));
	writeln(memAddrBaseDisp8(Register.AX, 0xFE));
	writeln(memAddrBaseIndexDisp8(Register.AX, Register.BX, SibScale(3), 0xFE));
}

void run_from_rwx()
{
	const size_t SIZE = 4096;
	ubyte[] mem = alloc_executable_memory(SIZE);
	writefln("alloc %s bytes at %s", mem.length, mem.ptr);

	emit_code_into_memory(mem);

	alias JittedFunc = long function(long);
	JittedFunc func = cast(JittedFunc)mem.ptr;

	long result = func(2);

	writefln("func(2) == %s", result);
}

void emit_code_into_memory(ubyte[] mem)
{
	CodeGen_x86_64 codeGen;
	codeGen.encoder.setBuffer(mem);

	version(Windows)
	{
		// main
		codeGen.beginFunction();
		auto sub_call = codeGen.saveFixup();
		codeGen.call(PC(null));
		codeGen.endFunction();

		sub_call.call(codeGen.pc);

		// sub_fun
		codeGen.beginFunction();
		codeGen.movq(Register.AX, Imm32(42));
		codeGen.endFunction();
	}
	else version(Posix)
	{
		codeGen.movq(Register.AX, Register.DI);
		codeGen.addq(Register.AX, Imm8(4));
		codeGen.movq(memAddrBaseDisp32(Register.AX, 0x55), Imm32(0xAABBCCDD));
		codeGen.ret();
	}
	printHex(codeGen.encoder.code, 16);
}
