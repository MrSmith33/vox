/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
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

extern(C) int printNum(int param){ write(param); return 0; }
extern(C) int printNumLn(int param){ writeln(param); return 0; }

void main()
{
	//run_from_rwx();
	//testPrintMemAddress();
	//testVMs();
	//testLang();
	//testLang2();

	{
		CodeGen_x86_64 codeGen;
		codeGen.encoder.setBuffer(alloc_executable_memory(PAGE_SIZE * 1024));
		scope(exit) free_executable_memory(codeGen.encoder.freeBuffer);

		codeGen.addq(Register.CX, Register.AX);
		printHex(codeGen.encoder.code, 10);
	}
	testAll();
	/*
	writefln("main() == %s", runScript(input, "main"));
	writefln("main() == %s", runScript(q{i32 main(){return 42;}}, "main"));
	writefln("main(20) == %s", runScript(q{i32 main(i32 par){return par;}}, "main", 20));
	writefln("main(20) == %s", runScript(q{i32 main(i32 par){return par+par;}}, "main", 20));
	writefln("main(20) == %s", runScript(q{i32 main(i32 par){return sub(par)+10;} i32 sub(i32 a){return a+a;}}, "main", 20));

	{
		LangVM vm;
		vm.setup();
		scope(exit) vm.free;

		writefln("%s", cast(void*)&printNum);
		vm.registerFunction("print", &printNum);
		vm.registerFunction("println", &printNumLn);
		vm.compileModule(q{ void test(i32 i) { print(i); println(i+i); } });
		if (vm.valid)
		{
			printHex(vm.codeGen.code, 16);
			writefln("fun table %s", vm.codeGen.functionTable);
			writefln("%s", vm.run!int("test", __LINE__, __FILE__, 20));
		}
	}

	{
		CodeGen_x86_64 codeGen;
		codeGen.encoder.setBuffer(alloc_executable_memory(PAGE_SIZE * 1024));
		scope(exit) free_executable_memory(codeGen.encoder.freeBuffer);

		//writefln("MOV byte ptr %s, 0x%X", memAddrDisp32(0x55667788), 0xAA);

		alias R = Reg64;
		enum regMax = cast(R)(R.max+1);
		foreach (R regB; R.min..regMax)
		{
			//codeGen.addq(memAddrBase(cast(Register)regB), Imm8(1));
			//writefln("mov %s, qword ptr %s", regB, memAddrBaseIndexDisp8(cast(Register)regB, cast(Register)regB, SibScale(3), 0xFE));
			//codeGen.movq(cast(Register)regB, Imm64(0x24364758AABBCCDD));
		}

		codeGen.call(memAddrRipDisp32(100));
		codeGen.call(codeGen.stubPC+100);
	}

	//printHex(codeGen.encoder.code, 10);
	*/
}

void testAll()
{
	import asmtest.utils;
	CodegenTester tester;

	tester.setup();
	scope(exit) tester.free();

	import asmtest.add;
	import asmtest.mov;
	import asmtest.not;
	import asmtest.mul;
	import asmtest.inc;
	import asmtest.pop;
	import asmtest.push;
	import asmtest.cmp;
	import asmtest.jmp_jcc_setcc;
	import asmtest.imul;

	testAdd(tester);
	testMov(tester);
	testNot(tester);
	testMul(tester);
	testInc(tester);
	testPop(tester);
	testPush(tester);
	testCmp(tester);
	testJmpJccSetcc(tester);
	testImul(tester);
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
	scope(exit) free_executable_memory(mem);
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
		codeGen.call(codeGen.pc);
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
	scope(exit) jit_vm.free;
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
	//printAST(rootNode);

	writefln("Total %ss", scaledNumberFmt(time3 - time0));
}

auto runScript(int line = __LINE__, string file = __FILE__, Args...)(string input, string funcName, Args args)
{
	LangVM vm; vm.setup;
	scope(exit) vm.free;
	vm.compileModule(input);
	return vm.run!(int)(funcName, line, file, args);
}

struct LangVM
{
	import lang;

	private IdentifierMap idMap;
	private Lexer2 lexer;
	private Parser parser;
	private LangCodeGen codeGen;
	private Module moduleDecl;
	private ModuleSemantics moduleSemantics;
	private NativeFunction[] nativeFunctions;
	private bool valid;

	void setup()
	{
		idMap = new IdentifierMap();
		parser = Parser(&lexer, idMap);
		parser.setup();
		codeGen.setup();
	}

	void free()
	{
		codeGen.free;
	}

	void registerFunction(string name, NativeFunPtr fun)
	{
		nativeFunctions ~= NativeFunction(idMap.getOrReg(name), 1, fun);
	}

	void compileModule(string source)
	{
		valid = true;
		try {
			lexer = Lexer2(source);
			moduleDecl = parser.parseModule();
			moduleSemantics = analyzeModule(moduleDecl, idMap, nativeFunctions);
			codeGen.compileModule(moduleSemantics);
		} catch(CompilationException e) {
			writefln("[ERROR] %s: %s", e.loc, e.msg);
			writeln(e);
			valid = false;
		}
	}

	ResultType run(ResultType, Args...)(string funcName, int line, string file, Args args)
	{
		if (!valid) throw runtime_error("Cannot start '%s'. Module compiled with errors", funcName, line, file);

		foreach(arg; Args)
		{
			static assert(is(arg == int), "parameter must be int");
		}
		static assert(is(ResultType == int), "return type must be int");
		alias JittedFunc = extern(C) ResultType function(Args);
		auto id = idMap.find(funcName);
		if (id == Identifier.max) throw runtime_error("Unknown function name '%s'", funcName, line, file);
		auto fun = moduleSemantics.tryGetFunction(id);
		if (fun is null) throw runtime_error("'%s' is not a function name", funcName, line, file);

		auto numArgs = Args.length;
		auto numParams = fun.node.parameters.length;
		if (numArgs < numParams)
			throw runtime_error("Insufficient parameters to '%s', got %s, expected %s",
				funcName, numArgs, numParams, line, file);
		else if (numArgs > numParams)
			throw runtime_error("Too much parameters to '%s', got %s, expected %s",
				funcName, numArgs, numParams, line, file);

		JittedFunc func = cast(JittedFunc)fun.funcPtr;
		//assert(false);
		auto result = func(args);
		return result;
	}
}

string input = q{
	i32 main() {
		i32 localVar;
		struct nestedStruct {}
		//fn i32 nestedFunc(i32 a) { return a + 1; }
		return localVar;
		//return sub(1, 2, 3, 4, 5, 6); // returns 21 as expected
	}
	//fn i32 sub(i32 a, i32 b, i32 c, i32 d, i32 e, i32 f) {
	//	return a + b + c + d + e + f;
	//}
	i32 globalVar;
	struct structWIP {}
};

void testLang()
{
	import lang;

	enum times = 10_000;
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
		auto loc = e.loc;
		writefln("%s: [ERROR] %s", loc, e.msg);
		return;
	}

	auto time1 = currTime;

	ModuleSemantics moduleSemantics;
	try
	{
		foreach (_; 0..times)
		{
			moduleSemantics = analyzeModule(moduleDecl, idMap);
		}
	}
	catch(SemanticsException e)
	{
		auto loc = e.loc;
		writefln("[ERROR] %s: %s", loc, e.msg);
		return;
	}

	auto time2 = currTime;

	LangCodeGen codeGen;
	scope(exit) codeGen.free;
	foreach (_; 0..times)
	{
		codeGen.setup();
		codeGen.compileModule(moduleSemantics);
	}

	auto time3 = currTime;

	int res;
	foreach (_; 0..times)
	{
		alias JittedFunc = extern(C) int function(int, int, int, int, int, int);
		JittedFunc func = cast(JittedFunc)moduleSemantics.functions[0].funcPtr; // main
		res = func(1, 2, 3, 4, 5, 6);
	}

	auto time4 = currTime;

	writefln("Lang: parse %ss, semantics %ss, compile %ss, run %ss",
		scaledNumberFmt(time1 - time0, 1.0/times),
		scaledNumberFmt(time2 - time1, 1.0/times),
		scaledNumberFmt(time3 - time2, 1.0/times),
		scaledNumberFmt(time4 - time3, 1.0/times));
	writeln(input);
	printAST(moduleDecl, idMap);
	printHex(codeGen.code, 16);
	writefln("func() == %s", res);
}

string input2 = q{
	i32 isNegative(i32 number) {
		i32 result;
		if (number < 0) result = 1;
		else result = 0;
		return result;
	}
};

string input3 = q{
i32 sign(i32 number) {
	i32 result;
	if (number < 0) result = 0-1;
	else if (number < 0) result = 1;
	else result = 0
	return result;
}
};

void testLang2()
{
	LangVM vm;
	vm.setup();
	scope(exit) vm.free;

	writefln("%s", cast(void*)&printNum);
	vm.compileModule(input3);
	if (vm.valid)
	{
		printHex(vm.codeGen.code, 16);
		writefln("fun table %s", vm.codeGen.functionTable);
		writefln("%s", vm.run!int("sign", __LINE__, __FILE__, 10));
	}
}
