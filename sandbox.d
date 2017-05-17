/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module sandbox;

import std.stdio;
import amd64asm;
import test.mov;
import test.not;
import utils;

enum Reg8  : ubyte {AL, CL, DL, BL, SPL,BPL,SIL,DIL,R8B,R9B,R10B,R11B,R12B,R13B,R14B,R15B}
enum Reg16 : ubyte {AX, CX, DX, BX, SP, BP, SI, DI, R8W,R9W,R10W,R11W,R12W,R13W,R14W,R15W}
enum Reg32 : ubyte {EAX,ECX,EDX,EBX,ESP,EBP,ESI,EDI,R8D,R9D,R10D,R11D,R12D,R13D,R14D,R15D}
enum Reg64 : ubyte {RAX,RCX,RDX,RBX,RSP,RBP,RSI,RDI,R8, R9, R10, R11, R12, R13, R14, R15 }

void main()
{
	testAll();

	//run_from_rwx();
	//testPrintMemAddress();

	CodeGen_x86_64!ArraySink codeGen;
	//writefln("MOV byte ptr %s, 0x%X", memAddrDisp32(0x55667788), 0xAA);

	//writefln("%08b", Register.SP);
	//writefln("%08b", Register.BP);
	//writefln("%08b", Register.R12);
	//writefln("%08b", Register.R13);
/*
	alias R = Reg8;
	enum regMax = cast(R)(R.max+1);
	foreach (R regB; R.min..regMax)
	{
		//writefln("NOT %s", memAddrDisp32(0xAABBAABB));
		writefln("NOT %s", regB);
		codeGen.notb(cast(Register)regB);
	}*/
	codeGen.notb(memAddrBase(Register.DI));

	//codeGen.movq(Register.AX, Register.CX);
	//codeGen.addb(Register.AX, Imm8(4));
	//codeGen.ret();
	printHex(codeGen.sink.data, 0);
}

void testAll()
{
	testMov();
	testNot();
}

void printHex(ubyte[] buffer, size_t lineLength)
{
	size_t index = 0;
	if (lineLength)
		while (index + lineLength <= buffer.length)
	{
		writefln("%(%02X %)", buffer[index..index+lineLength]);
		index += lineLength;
	}

	if (index < buffer.length)
		writefln("%(%02X %)", buffer[index..buffer.length]);
}

void testPrintMemAddress()
{
	writeln(memAddrDisp32(0x11223344));
	writeln(memAddrIndexDisp32(Register.AX, SibScale(0), 0x11223344));
	writeln(memAddrBase(Register.AX));
	writeln(memAddrBaseDisp32(Register.AX, 0x11223344));
	writeln(memAddrBaseIndex(Register.AX, Register.BX, SibScale(2)));
	writeln(memAddrBaseIndexDisp32(Register.AX, Register.BX, SibScale(2), 0x11223344));
	writeln(memAddrBaseDisp8(Register.AX, 0xFE));
	writeln(memAddrBaseIndexDisp8(Register.AX, Register.BX, SibScale(3), 0xFE));
}

void emit_code_into_memory(ubyte[] m)
{
	m[0..code.length] = code;
}

void run_from_rwx()
{
	const size_t SIZE = 4096;
	ubyte[] m = alloc_executable_memory(SIZE);
	writefln("alloc %s %s", m.ptr, m.length);

	emit_code_into_memory(m);

	alias JittedFunc = long function(long);
	JittedFunc func = cast(JittedFunc)m.ptr;

	long result = func(2);

	writefln("result = %s", result);
}

version(Windows)
{
	ubyte[] code = [
		0x48, 0x89, 0xC8,       // mov    rax,rcx
		0x48, 0x83, 0xC0, 0x04, // add    rax,4
		0xC3,                   // ret
	];
}
else version(Posix)
{
	ubyte[] code = [
		0x48, 0x89, 0xf8,       // mov    rax,rdi
		0x48, 0x83, 0xc0, 0x04, // add    rax,0x4
		0xC7, 0x44, 0x20, 0x0, 0x22, 0x11, 0xFF, 0xEE, 0xDD, 0xCC, 0xBB, 0xAA, // mov [rax+0x55], 0xAABBCCDDEEFF1122
		0xc3,                   // ret
	];
}
