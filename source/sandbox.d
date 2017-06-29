/**
Copyright: Copyright (c) 2017 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module sandbox;

import std.stdio;
import amd64asm;
import utils;

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

	testVMs();
	testLang();

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
	import asmtest.utils;
	testCodeGen.encoder.setBuffer(alloc_executable_memory(PAGE_SIZE * 1024));

	import asmtest.add;
	import asmtest.mov;
	import asmtest.not;
	import asmtest.mul;
	import asmtest.inc;
	import asmtest.pop;
	import asmtest.push;
	import asmtest.cmp;
	import asmtest.jmp_jcc_setcc;
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
	//writefln("alloc %s bytes at %s", mem.length, mem.ptr);

	emit_code_into_memory(mem);

	alias JittedFunc = long function(long);
	JittedFunc func = cast(JittedFunc)mem.ptr;

	long result = func(2);
	assert(result == 42);

	//writefln("func(2) == %s", result);
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
		//codeGen.movq(Register.AX, Register.DI);
		//codeGen.addq(Register.AX, Imm8(4));
		//codeGen.movq(memAddrBaseDisp32(Register.AX, 0x55), Imm32(0xAABBCCDD));
		codeGen.ret();
	}
	//printHex(codeGen.encoder.code, 16);
}

/*---------------------------------------------------------------------------*/
// Tiny C

string[] testSources = [
`{ i=1; while (i<100) { while (j < 100) j=j+1; i=i+1;} }`,
`{ i=1; while (i<100) { j=0; while (j < 100) {j=j+1;a=a+1;} i=i+1;} }`,
`{ i=i+1; }`,
`{ i;i;i; }`,
`{ i=125; j=100; while (i-j) if (i<j) j=j-i; else i=i-j; }`,
`{ if (i) j=1; else j=2; }`,
"a=b=c=2<3;",
"{ i=1; do i=i+10; while (i<50); }",
"{ i=1; while ((i=i+10)<50) ; }",
"{ i=7; if (i<5) n=1; if (i<10) y=2; }",
];

struct Source
{
	const(char)[] slice;

	char stdinGetter() {
		return cast(char)getchar();
	}

	void reset()
	{
		slice = testSources[3];
	}

	char testGetter() {
		if (slice.length == 0) return 255;

		char ch = slice[0];
		slice = slice[1..$];
		return ch;
	}
}
import benchutils;

void testVMs()
{
	import tinyc;
	import utils;

	auto time0 = currTime;
	enum times = 1_000;

	Source source;
	Lexer lexer;
	Parser parser = Parser(&lexer);
	Node* rootNode;
	foreach (_; 0..times)
	{
		source.reset();
		lexer = Lexer(&source.testGetter);
		rootNode = parser.program();
	}

	auto time1 = currTime;

	byte[1000] _object; // executable

	CodeGenerator codeGen;
	foreach (_; 0..times)
	{
		codeGen = CodeGenerator(_object.ptr);
		codeGen.compile(rootNode);
	}

	auto time2 = currTime;

	VM vm;
	foreach (_; 0..times)
	{
		vm = VM();
		vm.run(_object);
	}

	auto time3 = currTime;

	foreach(i, v; vm.globals)
		if (v != 0) writefln("%s = %s", cast(char)('a'+i), v);

	writefln("Parse: %ss, compile: %ss, run: %ss",
		scaledNumberFmt(time1 - time0, 1.0/times),
		scaledNumberFmt(time2 - time1, 1.0/times),
		scaledNumberFmt(time3 - time2, 1.0/times));

	JitVM jit_vm;
	time1 = currTime;
	foreach (_; 0..times) {
		jit_vm.compile(rootNode);
	}
	time2 = currTime;
	foreach (_; 0..times) {
		jit_vm.reset();
		jit_vm.run();
	}
	time3 = currTime;
	foreach(i, v; jit_vm.globals)
		if (v != 0) writefln("%s = %s", cast(char)('a'+i), v);

	writefln("Compile: %ss, run: %ss",
		scaledNumberFmt(time2 - time1, 1.0/times),
		scaledNumberFmt(time3 - time2, 1.0/times));

	//printHex(jit_vm.code, 8);
	printAST(rootNode);

	writefln("Total %ss", scaledNumberFmt(time3 - time0));
}



string input = q{func main(param1){ a = 42; return param1 + a + 10; }};

void testLang()
{
	import lang.codegen;
	import lang.parse;
	import lang.lex2;
	import lang.ast;
	import lang.semantics;

	enum times = 100_000;
	auto time0 = currTime;

	auto idMap = new IdentifierMap();
	Lexer2 lexer = Lexer2(input);

	Parser parser = Parser(&lexer, idMap);
	Module moduleDecl;
	try
	{
		foreach (_; 0..times)
		{
			parser.setup();
			lexer = Lexer2(input);
			moduleDecl = parser.parseModule();
		}
	}
	catch(ParsingException e)
	{
		writefln("line %s col %s: [ERROR] %s", e.token.line, e.token.col, e.msg);
		writeln(e);
		return;
	}

	auto time1 = currTime;

	ModuleSemantics moduleSemantics;
	foreach (_; 0..times)
	{
		moduleSemantics = analyzeModule(moduleDecl, idMap);
		if (!moduleSemantics.globalFunctions[0].valid) return;
	}


	auto time2 = currTime;

	CodeGen codeGen;
	alias JittedFunc = int function(int);
	JittedFunc func;
	foreach (_; 0..times)
	{
		codeGen.setup();
		func = cast(JittedFunc)codeGen.compileFunction(moduleSemantics.globalFunctions[0]);
	}

	auto time3 = currTime;

	int res;
	foreach (_; 0..times)
	{
		res = func(42);
	}

	auto time4 = currTime;

	writefln("Lang: parse %ss, semantics %ss, compile %ss, run %ss",
		scaledNumberFmt(time1 - time0, 1.0/times),
		scaledNumberFmt(time2 - time1, 1.0/times),
		scaledNumberFmt(time3 - time2, 1.0/times),
		scaledNumberFmt(time4 - time3, 1.0/times));
	writeln(input);
	//printAST(moduleDecl, idMap);
	printHex(codeGen.code, 8);
	writefln("func() == %s", res);
}
