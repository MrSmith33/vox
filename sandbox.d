/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module sandbox;

import std.stdio;
import amd64asm;
import test.mov;
import utils;

void main()
{
	testMov();
	//run_from_rwx();
	//testPrintMemAddress();

	CodeGen_x86_64!ArraySink codeGen;
	//writefln("MOV byte ptr %s, 0x%X", memAddrDisp32(0x55667788), 0xAA);

/*
	alias R = Reg64;
	enum Rmax = Reg64Max;
	foreach (R regB; R.min..Rmax)
	{
		writefln("MOV %s, %s", R.min, regB);
		codeGen.mov(R.min, regB);
	}
*/
	//codeGen.mov(Reg64.RAX, Reg64.RCX);
	//codeGen.ret();
	//codeGen.mov(Reg64.RAX, Reg64.RSP);
	//printHex(codeGen.sink.data, 3);
}

void printHex(ubyte[] buffer, size_t lineLength)
{
	size_t index = 0;
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
	writeln(memAddrIndexDisp32(Reg32.EAX, SibScale(0), 0x11223344));
	writeln(memAddrBase(Reg32.EAX));
	writeln(memAddrBaseDisp32(Reg32.EAX, 0x11223344));
	writeln(memAddrBaseIndex(Reg32.EAX, Reg32.EBX, SibScale(2)));
	writeln(memAddrBaseIndexDisp32(Reg32.EAX, Reg32.EBX, SibScale(2), 0x11223344));
	writeln(memAddrBaseDisp8(Reg32.EAX, 0xFE));
	writeln(memAddrBaseIndexDisp8(Reg32.EAX, Reg32.EBX, SibScale(3), 0xFE));
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
