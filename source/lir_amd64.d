/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lir_amd64;

import std.bitmanip : bitfields;
import std.stdio;

import all;

//version = standalone;
version (standalone) void main()
{
	// i32 sign(i32 number) {
	//     i32 result;
	//     if (number < 0) result = 0-1;
	//     else if (number > 0) result = 1;
	//     else result = 0;
	//     return result;
	// }

	// function sign() {
	//   @0
	//     v43 = parameter0 users [i62, i93]
	//     jmp @1
	//   @1
	//     if v43 < c1 then @2 else @3
	//   @2
	//     jmp @6
	//   @3
	//     if v43 > c5 then @4 else @5
	//   @4
	//     jmp @6
	//   @5
	//     jmp @6
	//   @6
	//     v157 = phi152(c7 @5, c6 @4, c4 @2) users [i26]
	//     jmp @7
	//   @7
	//     return v157
	// }

	// function sign() {
	//   @0
	//     mov vreg.0, ecx
	//     jmp @1
	//   @1
	//     cmp v43, c1
	//     jl @2
	//     jmp @3
	//   @2
	//     jmp @6
	//   @3
	//     cmp v43, c5
	//     jg @4
	//     jmp @5
	//   @4
	//     jmp @6
	//   @5
	//     jmp @6
	//   @6
	//     v157 = phi152(c7 @5, c6 @4, c4 @2)
	//     jmp @7
	//   @7
	//     return v157
	// }

	writefln("ir to lir");
	Driver driver;
	driver.initialize(null);
	scope(exit) driver.releaseMemory;

	IrBuilder builder;
	IrFunction lir;

	builder.context = &driver.context;
	builder.ir = &lir;
	lir.name = driver.context.idMap.getOrReg("sign");

	lir.storage = driver.context.irBuffer.freePart;
	lir.storageLength = 0;
	builder.setupEntryExitBlocks;

	IrIndex block1 = builder.addBasicBlock();
	IrIndex block2 = builder.addBasicBlock();
	IrIndex block3 = builder.addBasicBlock();
	IrIndex block4 = builder.addBasicBlock();
	IrIndex block5 = builder.addBasicBlock();
	IrIndex block6 = builder.addBasicBlock();

	builder.addBlockTarget(lir.entryBasicBlock, block1);
	builder.addBlockTarget(block1, block2);
	builder.addBlockTarget(block1, block3);
	builder.addBlockTarget(block2, block6);
	builder.addBlockTarget(block3, block4);
	builder.addBlockTarget(block3, block5);
	builder.addBlockTarget(block4, block6);
	builder.addBlockTarget(block5, block6);
	builder.addBlockTarget(block6, lir.exitBasicBlock);

	IrIndex const_0 = driver.context.addConstant(IrConstant(0));
	IrIndex const_1 = driver.context.addConstant(IrConstant(1));
	IrIndex const_m1 = driver.context.addConstant(IrConstant(-1));

	//     mov vreg.0, ecx
	IrIndex param0Index = builder.appendVoidToBlock!LirAmd64Instr_mov(lir.entryBasicBlock);
	lir.get!LirAmd64Instr_mov(param0Index)
		.initialize(builder.addVirtualRegister(param0Index), amd64_register_index.ax);
	IrIndex param0Value = lir.get!LirAmd64Instr_mov(param0Index).result;

	//     jmp @1
	IrIndex jmp0 = builder.addInstruction!LirAmd64Instr_jmp(lir.entryBasicBlock);
	lir.get!LirAmd64Instr_jmp(jmp0).args[0] = block1;

	//   @1
	//     cmp v43, c1
	IrIndex cmp1 = builder.addInstruction!LirAmd64Instr_cmp(block1);
	lir.get!LirAmd64Instr_cmp(cmp1).args[0] = param0Value;
	lir.get!LirAmd64Instr_cmp(cmp1).args[1] = const_0;
	builder.addUser(cmp1, param0Value);
	builder.addUser(cmp1, const_0);

	//     jl @2
	IrIndex jl = builder.addInstruction!LirAmd64Instr_jcc(block1);
	lir.get!LirAmd64Instr_jcc(jl).header.cond = Amd64Condition.L;
	lir.get!LirAmd64Instr_jcc(jl).args[0] = block2;

	//     jmp @3
	IrIndex jmp1 = builder.addInstruction!LirAmd64Instr_jmp(block1);
	lir.get!LirAmd64Instr_jmp(jmp1).args[0] = block3;

	//   @2
	//     jmp @6
	IrIndex jmp2 = builder.addInstruction!LirAmd64Instr_jmp(block2);
	lir.get!LirAmd64Instr_jmp(jmp2).args[0] = block6;

	//   @3
	//     cmp v43, c5
	IrIndex cmp3 = builder.addInstruction!LirAmd64Instr_cmp(block3);
	lir.get!LirAmd64Instr_cmp(cmp3).args[0] = param0Value;
	lir.get!LirAmd64Instr_cmp(cmp3).args[1] = const_0;
	builder.addUser(cmp3, param0Value);
	builder.addUser(cmp3, const_0);

	//     jg @4
	IrIndex jg = builder.addInstruction!LirAmd64Instr_jcc(block3);
	lir.get!LirAmd64Instr_jcc(jg).header.cond = Amd64Condition.G;
	lir.get!LirAmd64Instr_jcc(jg).args[0] = block4;

	//     jmp @5
	IrIndex jmp3 = builder.addInstruction!LirAmd64Instr_jmp(block3);
	lir.get!LirAmd64Instr_jmp(jmp3).args[0] = block5;

	//   @4
	//     jmp @6
	IrIndex jmp4 = builder.addInstruction!LirAmd64Instr_jmp(block4);
	lir.get!LirAmd64Instr_jmp(jmp4).args[0] = block6;

	//   @5
	//     jmp @6
	IrIndex jmp5 = builder.addInstruction!LirAmd64Instr_jmp(block5);
	lir.get!LirAmd64Instr_jmp(jmp5).args[0] = block6;

	//   @6
	//     v157 = phi152(c7 @5, c6 @4, c4 @2)
	IrIndex phi = builder.addPhi(block6);
	IrIndex phiValue = lir.getPhi(phi).result;
	builder.addPhiArg(phi, block5, const_0);
	builder.addPhiArg(phi, block4, const_1);
	builder.addPhiArg(phi, block2, const_m1);
	builder.addUser(phi, const_0);
	builder.addUser(phi, const_1);
	builder.addUser(phi, const_m1);

	//     jmp @7
	IrIndex jmp6 = builder.addInstruction!LirAmd64Instr_jmp(block6);
	lir.get!LirAmd64Instr_jmp(jmp6).args[0] = lir.exitBasicBlock;

	//   @7
	//     return v157
	IrIndex movIndex = builder.appendVoidToBlock!LirAmd64Instr_mov(lir.exitBasicBlock);
	lir.get!LirAmd64Instr_mov(movIndex)
		.initialize(amd64_register_index.ax, phiValue);
	IrIndex ret = builder.addInstruction!LirAmd64Instr_return(lir.exitBasicBlock);


	FuncDumpSettings dumpSettings;
	dumpSettings.dumper = &dumpAmd64Instr;
	dumpFunction(&lir, &driver.context, dumpSettings);
}

struct PhysicalRegister
{
	string name;
}

/// IrIndex.storageUintIndex points into this array when its kind is IrValueKind.physicalRegister
PhysicalRegister[] amd64_registers = [
	PhysicalRegister("ax"),
	PhysicalRegister("cx"),
	PhysicalRegister("dx"),
	PhysicalRegister("bx"),
	PhysicalRegister("sp"),
	PhysicalRegister("bp"),
	PhysicalRegister("si"),
	PhysicalRegister("di"),
	PhysicalRegister("r8"),
	PhysicalRegister("r9"),
	PhysicalRegister("r10"),
	PhysicalRegister("r11"),
	PhysicalRegister("r12"),
	PhysicalRegister("r13"),
	PhysicalRegister("r14"),
	PhysicalRegister("r15"),
	];

enum amd64_register_index : IrIndex {
	ax = IrIndex(0, IrValueKind.physicalRegister),
	cx = IrIndex(1, IrValueKind.physicalRegister),
	dx = IrIndex(2, IrValueKind.physicalRegister),
	bx = IrIndex(3, IrValueKind.physicalRegister),
	sp = IrIndex(4, IrValueKind.physicalRegister),
	bp = IrIndex(5, IrValueKind.physicalRegister),
	si = IrIndex(6, IrValueKind.physicalRegister),
	di = IrIndex(7, IrValueKind.physicalRegister),
	r8 = IrIndex(8, IrValueKind.physicalRegister),
	r9 = IrIndex(9, IrValueKind.physicalRegister),
	r10 = IrIndex(10, IrValueKind.physicalRegister),
	r11 = IrIndex(11, IrValueKind.physicalRegister),
	r12 = IrIndex(12, IrValueKind.physicalRegister),
	r13 = IrIndex(13, IrValueKind.physicalRegister),
	r14 = IrIndex(14, IrValueKind.physicalRegister),
	r15 = IrIndex(15, IrValueKind.physicalRegister),
}

///
enum Amd64Opcode : ushort {
	add,
	sub,
	imul,
	or,
	and,
	xor,
	mul,
	div,
	lea,

	mov,
	movsx,
	movzx,

	not,
	neg,

	cmp,
	test,

	jmp,
	jcc,

	setcc,

	call,
	ret,

	pop,
	push,
}

alias LirAmd64Instr_add = IrGenericInstr!(Amd64Opcode.add, 2, HasResult.yes);
alias LirAmd64Instr_sub = IrGenericInstr!(Amd64Opcode.sub, 2, HasResult.yes);
alias LirAmd64Instr_cmp = IrGenericInstr!(Amd64Opcode.cmp, 2, HasResult.no);
alias LirAmd64Instr_jcc = IrGenericInstr!(Amd64Opcode.jcc, 1, HasResult.no);
alias LirAmd64Instr_jmp = IrGenericInstr!(Amd64Opcode.jmp, 1, HasResult.no);
alias LirAmd64Instr_test = IrGenericInstr!(Amd64Opcode.test, 1, HasResult.no);
alias LirAmd64Instr_return = IrGenericInstr!(Amd64Opcode.ret, 0, HasResult.no);
alias LirAmd64Instr_mov = IrGenericInstr!(Amd64Opcode.mov, 1, HasResult.yes);

// call layout
// - header
// - result (if callee is non-void)
// - arg0
// - arg1
// - ...
// - argN
///
struct LirAmd64Instr_call
{
	IrInstrHeader header;
}

/// The terms "less" and "greater" are used for comparisons of signed integers.
/// The terms "above" and "below" are used for unsigned integers.
enum Amd64Condition : ubyte {
	O   = 0x0, /// overflow (OF=1).
	NO  = 0x1, /// not overflow (OF=0).
	B   = 0x2, /// below (CF=1).
	C   = 0x2, /// carry (CF=1).
	NAE = 0x2, /// not above or equal (CF=1).
	AE  = 0x3, /// above or equal (CF=0).
	NB  = 0x3, /// not below (CF=0).
	NC  = 0x3, /// not carry (CF=0).
	E   = 0x4, /// equal (ZF=1).
	Z   = 0x4, /// zero (ZF = 1).
	NE  = 0x5, /// not equal (ZF=0).
	NZ  = 0x5, /// not zero (ZF=0).
	BE  = 0x6, /// below or equal (CF=1 or ZF=1).
	NA  = 0x6, /// not above (CF=1 or ZF=1).
	A   = 0x7, /// above (CF=0 and ZF=0).
	NBE = 0x7, /// not below or equal (CF=0 andZF=0).
	S   = 0x8, /// sign (SF=1).
	NS  = 0x9, /// not sign (SF=0).
	P   = 0xA, /// parity (PF=1).
	PE  = 0xA, /// parity even (PF=1).
	NP  = 0xB, /// not parity (PF=0).
	PO  = 0xB, /// parity odd (PF=0).
	L   = 0xC, /// less (SF≠ OF).
	NGE = 0xC, /// not greater or equal (SF≠ OF).
	GE  = 0xD, /// greater or equal (SF=OF).
	NL  = 0xD, /// not less (SF=OF).
	LE  = 0xE, /// less or equal (ZF=1 or SF≠ OF).
	NG  = 0xE, /// not greater (ZF=1 or SF≠ OF).
	G   = 0xF, /// greater (ZF=0 and SF=OF).
	NLE = 0xF, /// not less or equal (ZF=0 andSF=OF).
}

void dumpAmd64Instr(ref InstrPrintInfo p)
{
	switch(p.instrHeader.op)
	{
		case Amd64Opcode.jcc:
			p.sink.putf("    j%s %s", cast(Amd64Condition)p.instrHeader.cond, p.instrHeader.args[0]);
			break;
		default:
			if (p.instrHeader.hasResult)
				p.sink.putf("    %s = %s", p.instrHeader.result, cast(Amd64Opcode)p.instrHeader.op);
			else  p.sink.putf("    %s", cast(Amd64Opcode)p.instrHeader.op);
			foreach (i, IrIndex arg; p.instrHeader.args)
			{
				if (i > 0) p.sink.put(",");
				p.sink.putf(" %s", arg);
			}
			break;
	}
}

///
struct LirBuilder
{
	CompilationContext* context;
	IrFunction* ir;
	IrFunction* lir;

	/// Must be called before LIR gen pass
	void begin(IrFunction* lir, IrFunction* ir, CompilationContext* context) {
		this.context = context;
		this.lir = lir;
		this.ir = ir;

		lir.storage = context.irBuffer.freePart;
		lir.storageLength = 0;
	}
}
