/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module be.lir_amd64;

import std.bitmanip : bitfields;
import std.stdio;
import std.format;

import all;
import be.amd64asm;

// usage reg_names[physRegClass][physRegSize][physRegIndex]
immutable string[][][] reg_names = [[
	["al", "cl", "dl", "bl", "spl","bpl","sil","dil","r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b"],
	["ax", "cx", "dx", "bx", "sp", "bp", "si", "di", "r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w"],
	["eax","ecx","edx","ebx","esp","ebp","esi","edi","r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d"],
	["rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi","r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15" ],
]];

/// Creates physicalRegister index
IrIndex amd64Reg(uint index, uint regSize)
{
	IrIndex result;
	result.storageUintIndex = regSize << 12 | (index & ((1 << 12) - 1));
	result.kind = IrValueKind.physicalRegister;
	return result;
}

uint typeToRegSize(IrIndex type, CompilationContext* context) {
	uint typeSize = context.types.typeSize(type);
	switch (typeSize) {
		case 1: return ArgType.BYTE;
		case 2: return ArgType.WORD;
		case 4: return ArgType.DWORD;
		case 8: return ArgType.QWORD;
		default:
			context.internal_error("Type %s of size %s cannot be stored in a register",
				IrTypeDump(type, *context), typeSize);
			assert(false);
	}
}

struct PhysicalRegister
{
	string name;
	IrIndex index;
}

struct MachineInfo
{
	immutable string[][][] reg_names;
	PhysicalRegister[] registers;
	InstrInfo[] instrInfo;

	string regName(IrIndex reg) {
		return reg_names[reg.physRegClass][reg.physRegSize][reg.physRegIndex];
	}
}

__gshared MachineInfo mach_info_amd64 = MachineInfo(
	reg_names,
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
	amd64InstrInfos.dup
);

enum amd64_reg : IrIndex {
	ax = amd64Reg(0, ArgType.QWORD),
	cx = amd64Reg(1, ArgType.QWORD),
	dx = amd64Reg(2, ArgType.QWORD),
	bx = amd64Reg(3, ArgType.QWORD),
	sp = amd64Reg(4, ArgType.QWORD),
	bp = amd64Reg(5, ArgType.QWORD),
	si = amd64Reg(6, ArgType.QWORD),
	di = amd64Reg(7, ArgType.QWORD),
	r8 = amd64Reg(8, ArgType.QWORD),
	r9 = amd64Reg(9, ArgType.QWORD),
	r10 = amd64Reg(10, ArgType.QWORD),
	r11 = amd64Reg(11, ArgType.QWORD),
	r12 = amd64Reg(12, ArgType.QWORD),
	r13 = amd64Reg(13, ArgType.QWORD),
	r14 = amd64Reg(14, ArgType.QWORD),
	r15 = amd64Reg(15, ArgType.QWORD),
}

struct CallConv
{
	IrIndex[] paramsInRegs;
	IrIndex returnReg;
	IrIndex[] volatileRegs;
	IrIndex[] allocatableRegs;
	IrIndex[] calleeSaved;
	/// Not included into allocatableRegs
	/// Can be used as frame pointer when
	/// frame pointer is enabled for the function, or
	/// can be used as allocatable register if not
	IrIndex framePointer;
	IrIndex stackPointer;

	bool isParamOnStack(size_t parIndex) {
		return parIndex >= paramsInRegs.length;
	}
}

enum CallConvention : ubyte {
	win64
}

__gshared CallConv*[] callConventions = [
	&win64_call_conv,
];

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

	// avaliable for allocation
	[amd64_reg.ax, // volatile regs, zero cost
	amd64_reg.cx,
	amd64_reg.dx,
	amd64_reg.r8,
	amd64_reg.r9,
	amd64_reg.r10,
	amd64_reg.r11,

	// callee saved
	amd64_reg.bx, // need to save/restore to use
	amd64_reg.si,
	amd64_reg.di,
	amd64_reg.r12,
	amd64_reg.r13,
	amd64_reg.r14,
	amd64_reg.r15],

	[amd64_reg.bx, // callee saved regs
	amd64_reg.si,
	amd64_reg.di,
	amd64_reg.r12,
	amd64_reg.r13,
	amd64_reg.r14,
	amd64_reg.r15],

	amd64_reg.bp, // frame pointer
	amd64_reg.sp, // stack pointer
);

immutable InstrInfo[] amd64InstrInfos = gatherInstrInfos!Amd64Opcode;

private alias _ii = InstrInfo;
///
enum Amd64Opcode : ushort {
	@_ii(0,2,IFLG.isResultInDst) add,
	@_ii(0,2,IFLG.isResultInDst) sub,
	@_ii(0,2,IFLG.isResultInDst) mul,
	@_ii(0,2,IFLG.isResultInDst) imul,
	@_ii(0,2) div,
	@_ii(0,2) idiv,
	@_ii(0,0) divsx,
	@_ii(0,2,IFLG.isResultInDst) or,
	@_ii(0,2,IFLG.isResultInDst) and,
	@_ii(0,2,IFLG.isResultInDst) xor,
	@_ii(0,1,IFLG.isResultInDst) shl,
	@_ii(0,1,IFLG.isResultInDst) shr,
	@_ii(0,1,IFLG.isResultInDst) sar,
	@_ii() lea,

	@_ii(0,1,IFLG.isMov) mov, // rr, ri
	@_ii(0,1,IFLG.isLoad) load,
	@_ii(0,2,IFLG.isStore) store,
	@_ii() movsx,
	@_ii() movzx,
	@_ii() xchg,

	@_ii(0,1,IFLG.isResultInDst) not,
	@_ii(0,1,IFLG.isResultInDst) neg,

	@_ii() cmp,
	@_ii() test,

	// machine specific branches
	@_ii(0,0,IFLG.isJump | IFLG.isBlockExit) jmp,
	@_ii(0,0,IFLG.isBlockExit) jcc,
	// high-level branches
	@_ii(0,2,IFLG.isBranch | IFLG.isBlockExit) bin_branch,
	@_ii(0,2,IFLG.isBranch | IFLG.isBlockExit) un_branch,
	@_ii(0,1) set_unary_cond,
	@_ii(0,2) set_binary_cond,

	@_ii() setcc,

	@_ii(0,0,IFLG.isCall) call,
	@_ii(0,0,IFLG.isBlockExit) ret,

	@_ii() pop,
	@_ii() push,
}

alias LirAmd64Instr_add = IrGenericInstr!(Amd64Opcode.add, 2, IFLG.hasResult | IFLG.isResultInDst); // arg0 = arg0 + arg1
alias LirAmd64Instr_sub = IrGenericInstr!(Amd64Opcode.sub, 2, IFLG.hasResult | IFLG.isResultInDst); // arg0 = arg0 - arg1
alias LirAmd64Instr_mul = IrGenericInstr!(Amd64Opcode.mul, 2, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_imul = IrGenericInstr!(Amd64Opcode.imul, 2, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_div = IrGenericInstr!(Amd64Opcode.div, 3, IFLG.hasResult); // (dx, ax) = div (dx, ax) / v2
alias LirAmd64Instr_idiv = IrGenericInstr!(Amd64Opcode.idiv, 3, IFLG.hasResult); // (dx, ax) = div (dx, ax) / v2
alias LirAmd64Instr_divsx = IrGenericInstr!(Amd64Opcode.divsx, 0); // CWD/CDQ/CQO
alias LirAmd64Instr_and = IrGenericInstr!(Amd64Opcode.and, 2, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_or  = IrGenericInstr!(Amd64Opcode.or,  2, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_xor = IrGenericInstr!(Amd64Opcode.xor, 2, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_shl = IrGenericInstr!(Amd64Opcode.shl, 2, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_shr = IrGenericInstr!(Amd64Opcode.shr, 2, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_sar = IrGenericInstr!(Amd64Opcode.sar, 2, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_cmp = IrGenericInstr!(Amd64Opcode.cmp, 2);
alias LirAmd64Instr_jcc = IrGenericInstr!(Amd64Opcode.jcc, 1);
alias LirAmd64Instr_jmp = IrGenericInstr!(Amd64Opcode.jmp, 0);
alias LirAmd64Instr_bin_branch = IrGenericInstr!(Amd64Opcode.bin_branch, 2, IFLG.hasCondition);
alias LirAmd64Instr_un_branch = IrGenericInstr!(Amd64Opcode.un_branch, 1, IFLG.hasCondition);
alias LirAmd64Instr_set_unary_cond = IrGenericInstr!(Amd64Opcode.set_unary_cond, 1, IFLG.hasResult | IFLG.hasCondition);
alias LirAmd64Instr_set_binary_cond = IrGenericInstr!(Amd64Opcode.set_binary_cond, 2, IFLG.hasResult | IFLG.hasCondition);
alias LirAmd64Instr_test = IrGenericInstr!(Amd64Opcode.test, 1);
alias LirAmd64Instr_push = IrGenericInstr!(Amd64Opcode.push, 1);
alias LirAmd64Instr_return = IrGenericInstr!(Amd64Opcode.ret, 0);
alias LirAmd64Instr_mov = IrGenericInstr!(Amd64Opcode.mov, 1, IFLG.hasResult); // mov rr/ri
alias LirAmd64Instr_load = IrGenericInstr!(Amd64Opcode.load, 1, IFLG.hasResult); // mov rm
alias LirAmd64Instr_store = IrGenericInstr!(Amd64Opcode.store, 2); // mov mr/mi
alias LirAmd64Instr_xchg = IrGenericInstr!(Amd64Opcode.xchg, 2); // xchg mr/mr
alias LirAmd64Instr_not = IrGenericInstr!(Amd64Opcode.not, 1, IFLG.hasResult | IFLG.isResultInDst);
alias LirAmd64Instr_neg = IrGenericInstr!(Amd64Opcode.neg, 1, IFLG.hasResult | IFLG.isResultInDst);
// call layout
// - header
// - result (if callee is non-void)
// - arg0 (function or function pointer)
// - arg1
// - ...
// - argN
///
alias LirAmd64Instr_call = IrGenericInstr!(Amd64Opcode.call, 0, IFLG.hasVariadicArgs | IFLG.hasVariadicResult);

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
	switch(p.instrHeader.op)
	{
		case Amd64Opcode.bin_branch:
			dumpBinBranch(p);
			break;
		case Amd64Opcode.un_branch:
			dumpUnBranch(p);
			break;
		case Amd64Opcode.jmp: dumpJmp(p); break;
		case Amd64Opcode.jcc:
			p.sink.putf("    j%s %s",
				cast(Condition)p.instrHeader.cond,
				IrIndexDump(p.instrHeader.args[0], p));
			break;
		default:
			dumpOptionalResult(p);
			p.sink.putf("%s", cast(Amd64Opcode)p.instrHeader.op);
			dumpArgs(p);
			break;
	}
}

void dumpLirAmd64Index(scope void delegate(const(char)[]) sink, ref CompilationContext context, IrIndex i)
{
	if (!i.isDefined) {
		sink("<null>");
		return;
	}

	final switch(i.kind) with(IrValueKind) {
		case none: sink.formattedWrite("0x%X", i.asUint); break;
		case listItem: sink.formattedWrite("l.%s", i.storageUintIndex); break;
		case instruction: sink.formattedWrite("i.%s", i.storageUintIndex); break;
		case basicBlock: sink.formattedWrite("@%s", i.storageUintIndex); break;
		case constant: sink.formattedWrite("%s", context.constants.get(i).i64); break;
		case constantAggregate: sink.formattedWrite("cagg.%s", i.storageUintIndex); break;
		case global: sink.formattedWrite("g.%s", i.storageUintIndex); break;
		case phi: sink.formattedWrite("phi.%s", i.storageUintIndex); break;
		case stackSlot: sink.formattedWrite("s.%s", i.storageUintIndex); break;
		case virtualRegister: sink.formattedWrite("v.%s", i.storageUintIndex); break;
		case physicalRegister: sink(reg_names[i.physRegClass][i.physRegSize][i.physRegIndex]); break;
		case type: dumpIrType(sink, context, i); break;
		case variable: assert(false);
		case func: sink.formattedWrite("f.%s", i.storageUintIndex); break;
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

		lir.storage = context.irBuffer.nextPtr[0..0];
	}
}
