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
	],
	[
		["xmm0b","xmm1b","xmm2b","xmm3b","xmm4b","xmm5b","xmm6b","xmm7b","xmm8b","xmm9b","xmm10b","xmm11b","xmm12b","xmm13b","xmm14b","xmm15b"], // 8
		["xmm0w","xmm1w","xmm2w","xmm3w","xmm4w","xmm5w","xmm6w","xmm7w","xmm8w","xmm9w","xmm10w","xmm11w","xmm12w","xmm13w","xmm14w","xmm15w"], // 16
		["xmm0d","xmm1d","xmm2d","xmm3d","xmm4d","xmm5d","xmm6d","xmm7d","xmm8d","xmm9d","xmm10d","xmm11d","xmm12d","xmm13d","xmm14d","xmm15d"], // 32
		["xmm0q","xmm1q","xmm2q","xmm3q","xmm4q","xmm5q","xmm6q","xmm7q","xmm8q","xmm9q","xmm10q","xmm11q","xmm12q","xmm13q","xmm14q","xmm15q"], // 64
		["xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7","xmm8","xmm9","xmm10","xmm11","xmm12","xmm13","xmm14","xmm15"], // 128
		["ymm0","ymm1","ymm2","ymm3","ymm4","ymm5","ymm6","ymm7","ymm8","ymm9","ymm10","ymm11","ymm12","ymm13","ymm14","ymm15"], // 256
		["zmm0","zmm1","zmm2","zmm3","zmm4","zmm5","zmm6","zmm7","zmm8","zmm9","zmm10","zmm11","zmm12","zmm13","zmm14","zmm15"], // 512
	],
];

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
	}
}

struct PhysReg {
	this(ubyte _regClass, ubyte _regIndex) {
		regClass = _regClass;
		regIndex = _regIndex;
	}

	mixin(bitfields!(
		uint, "regIndex", 5,
		uint, "regClass", 3,
	));

	void toString(scope void delegate(const(char)[]) sink) {
		sink.formattedWrite("%s %s %s", regClass, regIndex, regClass*(1<<5) + regIndex);
	}
}

enum NUM_REG_CLASSES = 2;
enum NUM_REGS_PER_CLASS = 32;
enum NUM_TOTAL_REGS = NUM_REG_CLASSES * NUM_REGS_PER_CLASS;

enum AMD64_REG_CLASS : ubyte {
	GPR = 0,
	XMM = 1,
}

// Register set of registers of all classes
struct FullRegSet {
	ClassRegSet[NUM_REG_CLASSES] classes;

	ubyte length() {
		ubyte result;
		foreach(i, cl; classes)
			result += classes[i].length;
		return result;
	}

	FullRegSet opBinary(string op)(FullRegSet rhs)
		if (op == "|" || op == "^" || op == "&")
	{
		FullRegSet result;
		foreach(i, cl; classes)
			result.classes[i] = mixin("classes[i] "~op~" rhs.classes[i]");
		return result;
	}

	FullRegSet opBinary(string op)(PhysReg rhs)
		if (op == "|" || op == "^" || op == "&")
	{
		FullRegSet result = this;
		mixin("result.classes[rhs.regClass].bits "~op~"= 1 << rhs.regIndex;");
		return result;
	}

	void opOpAssign(string op)(FullRegSet rhs)
		if (op == "|" || op == "^" || op == "&")
	{
		foreach(i, cl; classes)
			classes[i] = mixin("classes[i] "~op~" rhs.classes[i]");
	}

	void opOpAssign(string op)(PhysReg rhs)
		if (op == "|" || op == "^" || op == "&")
	{
		mixin("classes[rhs.regClass].bits "~op~"= 1 << rhs.regIndex;");
	}

	FullRegSet lowest(int n) {
		FullRegSet result;
		foreach(i, cl; classes)
			result.classes[i] = classes[i].lowest(n);
		return result;
	}

	int opApply(scope int delegate(PhysReg) dg)
	{
		foreach(ubyte regClass; 0..NUM_REG_CLASSES) {
			uint[1] bits = classes[regClass].bits;
			foreach(regIndex; bitsSet(bits[]))
				if (int res = dg(PhysReg(regClass, cast(ubyte)regIndex))) return res;
		}
		return 0;
	}

	int opApply(scope int delegate(uint index, PhysReg) dg)
	{
		uint index;
		foreach(ubyte regClass; 0..NUM_REG_CLASSES) {
			uint[1] bits = classes[regClass].bits;
			foreach(regIndex; bitsSet(bits[])) {
				if (int res = dg(index, PhysReg(regClass, cast(ubyte)regIndex))) return res;
				++index;
			}
		}
		return 0;
	}
}

/// Register set of a single register class
struct ClassRegSet {
	// Assume at most 32 registers per register class
	uint bits;

	ubyte length() { return cast(ubyte)popcnt(bits); }

	ClassRegSet opBinary(string op)(ClassRegSet rhs)
		if (op == "|" || op == "^" || op == "&")
	{
		return ClassRegSet(mixin("bits "~op~" rhs.bits"));
	}

	// returns set with lowest n registers
	ClassRegSet lowest(int n) {
		uint slotBits = bits;
		uint result;
		while (slotBits != 0 && n) {
			// Extract lowest set isolated bit
			// 111000 -> 001000; 0 -> 0
			uint lowestSetBit = slotBits & -slotBits;
			result |= lowestSetBit;

			// Disable lowest set isolated bit
			// 111000 -> 110000
			slotBits ^= lowestSetBit;
			--n;
		}
		return ClassRegSet(result);
	}

	int opApply(scope int delegate(ubyte) dg)
	{
		uint[1] bitsCopy = bits;
		foreach(regIndex; bitsSet(bitsCopy[]))
			if (int res = dg(cast(ubyte)regIndex)) return res;
		return 0;
	}
}

struct MachineInfo
{
	// total number of registers
	ubyte numRegisters;
	// index is register class. Each entry specifies number of registers in this class
	ubyte[NUM_REG_CLASSES] numRegsPerClass;
	// first index is register class
	// second index is register size
	// third index is register index
	// Sizes of name arrays must match corresponding regsPerClass
	immutable string[][][] reg_names;
	//PhysicalRegister[] registers;
	InstrInfo[] instrInfo;

	string regName(IrIndex reg) {
		return reg_names[reg.physRegClass][reg.physRegSize][reg.physRegIndex];
	}
}

__gshared MachineInfo mach_info_amd64 = MachineInfo(
	32,
	[16, 16],
	reg_names,
	amd64InstrInfos.dup
);

enum amd64_reg : PhysReg {
	ax    = PhysReg(0,  0),
	cx    = PhysReg(0,  1),
	dx    = PhysReg(0,  2),
	bx    = PhysReg(0,  3),
	sp    = PhysReg(0,  4),
	bp    = PhysReg(0,  5),
	si    = PhysReg(0,  6),
	di    = PhysReg(0,  7),
	r8    = PhysReg(0,  8),
	r9    = PhysReg(0,  9),
	r10   = PhysReg(0, 10),
	r11   = PhysReg(0, 11),
	r12   = PhysReg(0, 12),
	r13   = PhysReg(0, 13),
	r14   = PhysReg(0, 14),
	r15   = PhysReg(0, 15),

	xmm0  = PhysReg(1,  0),
	xmm1  = PhysReg(1,  1),
	xmm2  = PhysReg(1,  2),
	xmm3  = PhysReg(1,  3),
	xmm4  = PhysReg(1,  4),
	xmm5  = PhysReg(1,  5),
	xmm6  = PhysReg(1,  6),
	xmm7  = PhysReg(1,  7),
	xmm8  = PhysReg(1,  8),
	xmm9  = PhysReg(1,  9),
	xmm10 = PhysReg(1, 10),
	xmm11 = PhysReg(1, 11),
	xmm12 = PhysReg(1, 12),
	xmm13 = PhysReg(1, 13),
	xmm14 = PhysReg(1, 14),
	xmm15 = PhysReg(1, 15),
}

struct CallConv
{
	PhysReg[] gprParamRegs;
	PhysReg[] sseParamRegs;
	FullRegSet volatileRegs;
	FullRegSet calleeSaved;
	// Size of the register preserved by the callee
	IrArgSize[] calleeSavedSizePerClass;
	/// Not included into allocatableRegs
	/// Can be used as frame pointer when
	/// frame pointer is enabled for the function, or
	/// can be used as allocatable register if not
	PhysReg framePointer;
	PhysReg stackPointer;

	ubyte minStackAlignmentPower;

	uint flags;

	bool hasShadowSpace() { return cast(bool)(flags & CallConvFlags.hasShadowSpace); }
	bool hasRedZone() { return cast(bool)(flags & CallConvFlags.hasRedZone); }
	bool hasReverseStackOrder() { return cast(bool)(flags & CallConvFlags.hasReverseStackOrder); }
}

enum CallConvention : ubyte {
	win64,
	sysv64,
	sysv64_syscall,
}

enum CallConvFlags : uint {
	hasShadowSpace = 1 << 0,
	hasRedZone     = 1 << 1,
	/// By default parameters that are passed via stack are passed in left-to-right order
	/// This means that leftmost parameter has the smallest memory address, and rightmost parameter has biggest address
	/// If set, parameters are passed right-to-left on the stack
	hasReverseStackOrder = 1 << 2,
}

__gshared CallConv*[] callConventions = [
	&win64_call_conv,
	&sysv64_call_conv,
	&sysv64_syscall_call_conv,
];

__gshared CallConv win64_call_conv = CallConv
(
	// parameters in registers
	[amd64_reg.cx, amd64_reg.dx, amd64_reg.r8, amd64_reg.r9],
	[amd64_reg.xmm0, amd64_reg.xmm1, amd64_reg.xmm2, amd64_reg.xmm3],

	// volatile regs, zero cost allocation
	// ax cx dx r8 r9 r10 r11
	// xmm0 xmm1 xmm2 xmm3 xmm4 xmm5
	FullRegSet([
		//            1111 11
		//            5432 1098 7654 3210
		ClassRegSet(0b0000_1111_0000_0111),
		ClassRegSet(0b0000_0000_0011_1111)]),

	// callee saved regs, need to save/restore to use
	//   bp bx si di r12 r13 r14 r15
	FullRegSet([
		//            1111 11
		//            5432 1098 7654 3210
		ClassRegSet(0b1111_0000_1110_1000),
		ClassRegSet(0b1111_1111_1100_0000)]),

	[IrArgSize.size64, IrArgSize.size128],

	amd64_reg.bp, // frame pointer
	amd64_reg.sp, // stack pointer

	4,

	CallConvFlags.hasShadowSpace,
);

__gshared CallConv sysv64_call_conv = CallConv
(
	// parameters in registers
	[amd64_reg.di, amd64_reg.si, amd64_reg.dx, amd64_reg.cx, amd64_reg.r8, amd64_reg.r9],
	[amd64_reg.xmm0, amd64_reg.xmm1, amd64_reg.xmm2, amd64_reg.xmm3, amd64_reg.xmm4, amd64_reg.xmm5, amd64_reg.xmm6, amd64_reg.xmm7],

	// volatile regs, zero cost allocation
	//   ax cx dx si di r8 r9 r10 r11
	//   xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7
	FullRegSet([
		//            1111 11
		//            5432 1098 7654 3210
		ClassRegSet(0b0000_1111_1100_0111),
		ClassRegSet(0b1111_1111_1111_1111)]),

	// callee saved regs, need to save/restore to use
	//   bp bx r12 r13 r14 r15
	FullRegSet([
		//            1111 11
		//            5432 1098 7654 3210
		ClassRegSet(0b1111_0000_0010_1000),
		ClassRegSet(0b0000_0000_0000_0000)]),

	[IrArgSize.size64, IrArgSize.size128],

	amd64_reg.bp, // frame pointer
	amd64_reg.sp, // stack pointer

	4,
	CallConvFlags.hasRedZone,
);

__gshared CallConv sysv64_syscall_call_conv = CallConv
(
	// parameters in registers
	[amd64_reg.di, amd64_reg.si, amd64_reg.dx, amd64_reg.r10, amd64_reg.r8, amd64_reg.r9],
	[amd64_reg.xmm0, amd64_reg.xmm1, amd64_reg.xmm2, amd64_reg.xmm3, amd64_reg.xmm4, amd64_reg.xmm5, amd64_reg.xmm6, amd64_reg.xmm7],

	// volatile regs, zero cost allocation
	// cx and r11 are clobbered by syscall
	  // ax cx dx si di r8 r9 r10 r11
	  // xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7
	FullRegSet([
		//            1111 11
		//            5432 1098 7654 3210
		ClassRegSet(0b0000_1111_1100_0111),
		ClassRegSet(0b1111_1111_1111_1111)]),

	// callee saved regs, need to save/restore to use
	//   bp bx r12 r13 r14 r15
	FullRegSet([
		//            1111 11
		//            5432 1098 7654 3210
		ClassRegSet(0b1111_0000_0010_1000),
		ClassRegSet(0b0000_0000_0000_0000)]),

	[IrArgSize.size64, IrArgSize.size128],

	amd64_reg.bp, // frame pointer
	amd64_reg.sp, // stack pointer

	4,
	0,
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

	@_ii(2,IFLG.hasResult|IFLG.isResultInDst|IFLG.isCommutative) fadd, // arg0 = arg0 + arg1
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst) fsub, // arg0 = arg0 - arg1
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst|IFLG.isCommutative) fmul, // arg0 = arg0 * arg1
	@_ii(2,IFLG.hasResult|IFLG.isResultInDst) fdiv, // arg0 = arg0 / arg1

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

	@_ii(1,IFLG.hasResult) f32_to_f64,
	@_ii(1,IFLG.hasResult) f64_to_f32,

	@_ii(1,IFLG.hasResult) i32_to_f32,
	@_ii(1,IFLG.hasResult) i64_to_f32,
	@_ii(1,IFLG.hasResult) i32_to_f64,
	@_ii(1,IFLG.hasResult) i64_to_f64,
	@_ii(1,IFLG.hasResult) f32_to_i32_trunc,
	@_ii(1,IFLG.hasResult) f32_to_i64_trunc,
	@_ii(1,IFLG.hasResult) f64_to_i32_trunc,
	@_ii(1,IFLG.hasResult) f64_to_i64_trunc,

	@_ii(1,IFLG.hasResult) f32_to_i32_round,
	@_ii(1,IFLG.hasResult) f32_to_i64_round,
	@_ii(1,IFLG.hasResult) f64_to_i32_round,
	@_ii(1,IFLG.hasResult) f64_to_i64_round,

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

	@_ii(1,IFLG.hasVariadicArgs | IFLG.hasVariadicResult | IFLG.isCall) call,
	@_ii(1,IFLG.hasVariadicArgs | IFLG.hasVariadicResult | IFLG.isCall) syscall,
	@_ii(0,IFLG.isBlockExit) ret,

	@_ii(0,IFLG.hasResult) pop,
	@_ii(1) push,

	@_ii(3) rep_stos,

	@_ii(0,IFLG.isBlockExit) ud2,
}

Condition[] IrBinCondToAmd64Condition = [
	Condition.E,  // eq
	Condition.NE, // ne
	Condition.A,  // ugt
	Condition.AE, // uge
	Condition.B,  // ult
	Condition.BE, // ule
	Condition.G,  // sgt
	Condition.GE, // sge
	Condition.L,  // slt
	Condition.LE, // sle
	Condition.A,  // fgt
	Condition.AE, // fge
	Condition.B,  // flt
	Condition.BE, // fle
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
		case constant:
			final switch(i.constantKind) with(IrConstantKind) {
				case smallZx: sink.formattedWrite("%s", i.constantIndex); break;
				case smallSx: sink.formattedWrite("%s", (cast(int)i.constantIndex << 8) >> 8); break;
				case big: sink.formattedWrite("%s", context.constants.get(i).i64); break;
			}
			break;
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
