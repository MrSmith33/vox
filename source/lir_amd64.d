/**
Copyright: Copyright (c) 2017-2018 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module lir_amd64;

import std.bitmanip : bitfields;
import std.stdio;
import std.format;

import all;
import amd64asm;

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
	IrIndex index;
}

struct MachineInfo
{
	PhysicalRegister[] registers;
	InstrInfo[] instrInfo;
}

__gshared MachineInfo mach_info_amd64 = MachineInfo(
	[
		PhysicalRegister("ax", amd64_reg.ax),
		PhysicalRegister("cx", amd64_reg.cx),
		PhysicalRegister("dx", amd64_reg.dx),
		PhysicalRegister("bx", amd64_reg.bx),
		PhysicalRegister("sp", amd64_reg.sp),
		PhysicalRegister("bp", amd64_reg.bp),
		PhysicalRegister("si", amd64_reg.si),
		PhysicalRegister("di", amd64_reg.di),
		PhysicalRegister("r8", amd64_reg.r8),
		PhysicalRegister("r9", amd64_reg.r9),
		PhysicalRegister("r10", amd64_reg.r10),
		PhysicalRegister("r11", amd64_reg.r11),
		PhysicalRegister("r12", amd64_reg.r12),
		PhysicalRegister("r13", amd64_reg.r13),
		PhysicalRegister("r14", amd64_reg.r14),
		PhysicalRegister("r15", amd64_reg.r15),
	],
	gatherInfos()
);

enum amd64_reg : IrIndex {
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

struct CallConv
{
	IrIndex[] paramsInRegs;
	IrIndex returnReg;
	IrIndex[] volatileRegs;
	IrIndex[] allocatableRegs;
	/// Not included into allocatableRegs
	/// Can be used as frame pointer when
	/// frame pointer is enable for the function, or
	/// can be used as allocatable register if not
	IrIndex framePointer;

	bool isParamOnStack(size_t parIndex) {
		return parIndex >= paramsInRegs.length;
	}
}

__gshared CallConv win64_call_conv = CallConv
(
	// parameters in registers
	[amd64_reg.cx, amd64_reg.dx, amd64_reg.r8, amd64_reg.r9],

	amd64_reg.ax,  // return reg

	[amd64_reg.ax, // volatile regs
	amd64_reg.cx,
	amd64_reg.dx,
	amd64_reg.r8,
	amd64_reg.r9,
	amd64_reg.r10,
	amd64_reg.r11],

	[amd64_reg.ax, // avaliable for allocation
	amd64_reg.cx,
	amd64_reg.dx,
	amd64_reg.bx,
	amd64_reg.si,
	amd64_reg.di,
	amd64_reg.r8,
	amd64_reg.r9,
	amd64_reg.r10,
	amd64_reg.r11,
	amd64_reg.r12,
	amd64_reg.r13,
	amd64_reg.r14,
	amd64_reg.r15],

	amd64_reg.bp, // frame pointer
);

private alias _ii = InstrInfo;
///
enum Amd64Opcode : ushort {
	@_ii() add,
	@_ii() sub,
	@_ii() imul,
	@_ii() or,
	@_ii() and,
	@_ii() xor,
	@_ii() mul,
	@_ii() div,
	@_ii() lea,

	@_ii(0,1,IFLG.isMov) mov, // rr, ri
	@_ii(0,1,IFLG.isLoad) load,
	@_ii(0,2,IFLG.isStore) store,
	@_ii() movsx,
	@_ii() movzx,

	@_ii() not,
	@_ii() neg,

	@_ii() cmp,
	@_ii() test,

	// machine specific branches
	@_ii(0,0,IFLG.isJump) jmp,
	@_ii() jcc,
	// high-level branches
	@_ii(0,2,IFLG.isBranch) bin_branch,
	@_ii(0,2,IFLG.isBranch) un_branch,

	@_ii() setcc,

	@_ii() call,
	@_ii() ret,

	@_ii() pop,
	@_ii() push,
}

InstrInfo[] gatherInfos()
{
	InstrInfo[] res = new InstrInfo[__traits(allMembers, Amd64Opcode).length];
	foreach (i, m; __traits(allMembers, Amd64Opcode))
	{
		res[i] = __traits(getAttributes, mixin("Amd64Opcode."~m))[0];
	}
	return res;
}

alias LirAmd64Instr_add = IrGenericInstr!(Amd64Opcode.add, 2, IFLG.hasResult);
alias LirAmd64Instr_sub = IrGenericInstr!(Amd64Opcode.sub, 2, IFLG.hasResult);
alias LirAmd64Instr_mul = IrGenericInstr!(Amd64Opcode.mul, 2, IFLG.hasResult);
alias LirAmd64Instr_xor = IrGenericInstr!(Amd64Opcode.xor, 2, IFLG.hasResult);
alias LirAmd64Instr_cmp = IrGenericInstr!(Amd64Opcode.cmp, 2);
alias LirAmd64Instr_jcc = IrGenericInstr!(Amd64Opcode.jcc, 1);
alias LirAmd64Instr_jmp = IrGenericInstr!(Amd64Opcode.jmp, 0);
alias LirAmd64Instr_bin_branch = IrGenericInstr!(Amd64Opcode.bin_branch, 2, IFLG.hasCondition);
alias LirAmd64Instr_un_branch = IrGenericInstr!(Amd64Opcode.un_branch, 1, IFLG.hasCondition);
alias LirAmd64Instr_test = IrGenericInstr!(Amd64Opcode.test, 1);
alias LirAmd64Instr_return = IrGenericInstr!(Amd64Opcode.ret, 0);
alias LirAmd64Instr_mov = IrGenericInstr!(Amd64Opcode.mov, 1, IFLG.hasResult); // mov rr/ri
alias LirAmd64Instr_load = IrGenericInstr!(Amd64Opcode.load, 1, IFLG.hasResult); // mov rm
alias LirAmd64Instr_store = IrGenericInstr!(Amd64Opcode.store, 2); // mov mr/mi

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

Condition[] IrBinCondToAmd64Condition = [
	Condition.E,  // eq
	Condition.NE, // ne
	Condition.G,  // g
	Condition.GE, // ge
	Condition.L,  // l
	Condition.LE, // le
];

Condition[] IrUnCondToAmd64Condition = [
	Condition.Z,  // zero
	Condition.NZ, // not_zero
];

void dumpAmd64Instr(ref InstrPrintInfo p)
{
	void printIndex(IrIndex i)
	{
		final switch(i.kind) with(IrValueKind) {
			case none: p.sink.put("<null>"); break;
			case listItem: p.sink.putf("l.%s", i.storageUintIndex); break;
			case instruction: p.sink.putf("i.%s", i.storageUintIndex); break;
			case basicBlock: p.sink.putf("@%s", i.storageUintIndex); break;
			case constant: p.sink.putf("%s", p.context.constants[i.storageUintIndex].i64); break;
			case phi: p.sink.putf("phi.%s", i.storageUintIndex); break;
			case memoryAddress: p.sink.putf("m.%s", i.storageUintIndex); break;
			case stackSlot: p.sink.putf("s.%s", i.storageUintIndex); break;
			case virtualRegister: p.sink.putf("v.%s", i.storageUintIndex); break;
			// TODO, HACK: 32-bit version of register is hardcoded here
			case physicalRegister: p.sink.put("e"); p.sink.put(mach_info_amd64.registers[i.storageUintIndex].name); break;
		}
	}

	switch(p.instrHeader.op)
	{
		case Amd64Opcode.bin_branch:
			dumpBinBranch(p);
			break;
		case Amd64Opcode.un_branch:
			dumpUnBranch(p);
			break;
		case Amd64Opcode.jcc:
			p.sink.putf("    j%s ", cast(Condition)p.instrHeader.cond);
			printIndex(p.instrHeader.args[0]);
			break;
		default:
			if (p.instrHeader.hasResult)
			{
				p.sink.put("    ");
				printIndex(p.instrHeader.result);
				p.sink.putf(" = %s", cast(Amd64Opcode)p.instrHeader.op);
			}
			else  p.sink.putf("    %s", cast(Amd64Opcode)p.instrHeader.op);

			if (p.instrHeader.args.length > 0) p.sink.put(" ");
			foreach (i, IrIndex arg; p.instrHeader.args)
			{
				if (i > 0) p.sink.put(", ");
				printIndex(arg);
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
