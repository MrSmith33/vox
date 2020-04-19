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
	@_ii() invalid,
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst|IFLG.isCommutative) add, // arg0 = arg0 + arg1
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst) sub, // arg0 = arg0 - arg1
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst) mul,
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst|IFLG.isCommutative) imul,
	@_ii(3,IFLG.hasResult) div, // (dx, ax) = div (dx, ax) / v2
	@_ii(3,IFLG.hasResult) idiv, // (dx, ax) = div (dx, ax) / v2
	@_ii(0,IFLG.hasResult) divsx, // CWD/CDQ/CQO
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst|IFLG.isCommutative) and,
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst|IFLG.isCommutative) or,
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst|IFLG.isCommutative) xor,
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst) shl,
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst) shr,
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst) sar,
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst) lea,

	@_ii(1,IFLG.hasResult|IFLG.isMov) mov, // mov rr/ri
	@_ii(1,IFLG.hasResult|IFLG.isLoad) load, // mov rm
	@_ii(2,IFLG.isStore) store, // mov mr/mi


	@_ii(1,IFLG.hasResult) movzx_btow,
	@_ii(1,IFLG.hasResult) movzx_btod,
	@_ii(1,IFLG.hasResult) movzx_btoq,
	@_ii(1,IFLG.hasResult) movzx_wtod,
	@_ii(1,IFLG.hasResult) movzx_wtoq,

	@_ii(1,IFLG.hasResult) movsx_btow,
	@_ii(1,IFLG.hasResult) movsx_btod,
	@_ii(1,IFLG.hasResult) movsx_btoq,
	@_ii(1,IFLG.hasResult) movsx_wtod,
	@_ii(1,IFLG.hasResult) movsx_wtoq,
	@_ii(1,IFLG.hasResult) movsx_dtoq,

	@_ii(2) xchg, // xchg mr/mr

	@_ii(1,IFLG.hasResult|IFLG.isResultInDst) not,
	@_ii(1,IFLG.hasResult|IFLG.isResultInDst) neg,

	@_ii(2) cmp,
	@_ii(1) test,

	// machine specific branches
	@_ii(0,IFLG.isJump | IFLG.isBlockExit) jmp,
	@_ii(1,IFLG.isBlockExit) jcc,
	// high-level branches
	@_ii(2,IFLG.hasCondition | IFLG.isBranch | IFLG.isBlockExit) bin_branch,
	@_ii(1,IFLG.hasCondition | IFLG.isBranch | IFLG.isBlockExit) un_branch,
	@_ii(1,IFLG.hasResult | IFLG.hasCondition) set_unary_cond,
	@_ii(2,IFLG.hasResult | IFLG.hasCondition) set_binary_cond,

	@_ii(1,IFLG.hasCondition) setcc,

	@_ii(0,IFLG.hasVariadicArgs | IFLG.hasVariadicResult | IFLG.isCall) call,
	@_ii(0,IFLG.isBlockExit) ret,

	@_ii(0,IFLG.hasResult) pop,
	@_ii(1) push,

	@_ii(0,IFLG.isBlockExit) ud2,
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
				IrIndexDump(p.instrHeader.arg(p.ir, 0), p));
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
		case array: sink.formattedWrite("arr%s", i.storageUintIndex); break;
		case instruction: sink.formattedWrite("i%s", i.storageUintIndex); break;
		case basicBlock: sink.formattedWrite("@%s", i.storageUintIndex); break;
		case constant: sink.formattedWrite("%s", context.constants.get(i).i64); break;
		case constantAggregate: sink.formattedWrite("cagg%s", i.storageUintIndex); break;
		case constantZero:
			if (i.typeKind == IrTypeKind.basic)
				sink("0");
			else
				sink("zeroinit");
			break;
		case global: sink.formattedWrite("g%s", i.storageUintIndex); break;
		case phi: sink.formattedWrite("phi%s", i.storageUintIndex); break;
		case stackSlot: sink.formattedWrite("s%s", i.storageUintIndex); break;
		case virtualRegister: sink.formattedWrite("v%s", i.storageUintIndex); break;
		case physicalRegister: sink(reg_names[i.physRegClass][i.physRegSize][i.physRegIndex]); break;
		case type: dumpIrType(sink, context, i); break;
		case variable: assert(false);
		case func: sink.formattedWrite("f%s", i.storageUintIndex); break;
	}
}
