/**
Copyright: Copyright (c) 2017-2019 Andrey Penechko.
License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors: Andrey Penechko.
*/
module vox.be.amd64asm;

import vox.utils : Arena;

enum Register : ubyte {AX, CX, DX, BX, SP, BP, SI, DI, R8, R9, R10, R11, R12, R13, R14, R15}
enum RegisterMax = cast(Register)(Register.max+1);

bool is_SP_or_R12(Register reg) { return (reg & 0b111) == 0b100; }
bool is_BP_or_R13(Register reg) { return (reg & 0b111) == 0b101; }

enum ArgType : ubyte { BYTE, WORD, DWORD, QWORD }

import std.string : format;
struct Imm8  { ubyte  value; enum argT = ArgType.BYTE;  string toString(){ return format("0X%02X", value); } }
struct Imm16 { ushort value; enum argT = ArgType.WORD;  string toString(){ return format("0X%02X", value); } }
struct Imm32 { uint   value; enum argT = ArgType.DWORD; string toString(){ return format("0X%02X", value); } }
struct Imm64 { ulong  value; enum argT = ArgType.QWORD; string toString(){ return format("0X%02X", value); } }
enum bool isAnyImm(I) = is(I == Imm64) || is(I == Imm32) || is(I == Imm16) || is(I == Imm8);


enum ubyte REX_PREFIX = 0b0100_0000;
enum ubyte REX_W      = 0b0000_1000;
enum ubyte REX_R      = 0b0000_0100;
enum ubyte REX_X      = 0b0000_0010;
enum ubyte REX_B      = 0b0000_0001;

enum LegacyPrefix : ubyte {
	// Prefix group 1
	LOCK = 0xF0, // LOCK prefix
	REPN = 0xF2, // REPNE/REPNZ prefix
	REP  = 0xF3, // REP or REPE/REPZ prefix
	// Prefix group 2
	CS = 0x2E, // CS segment override
	SS = 0x36, // SS segment override
	DS = 0x3E, // DS segment override
	ES = 0x26, // ES segment override
	FS = 0x64, // FS segment override
	GS = 0x65, // GS segment override
	BNT = 0x2E, // Branch not taken
	BT = 0x3E, // Branch taken
	// Prefix group 3
	OPERAND_SIZE = 0x66, // Operand-size override prefix
	// Prefix group 4
	ADDRESS_SIZE = 0x67, // Address-size override prefix
}

/// The terms "less" and "greater" are used for comparisons of signed integers.
/// The terms "above" and "below" are used for unsigned integers.
enum Condition : ubyte {
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

// place 1 MSB of register into appropriate bit field of REX prefix
ubyte regTo_Rex_W(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 0; } // 1000 WRXB
ubyte regTo_Rex_R(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 1; } // 0100 WRXB
ubyte regTo_Rex_X(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 2; } // 0010 WRXB
ubyte regTo_Rex_B(Register reg) pure nothrow @nogc { return (reg & 0b1000) >> 3; } // 0001 WRXB

// place 3 LSB of register into appropriate bit field of ModR/M byte
ubyte regTo_ModRm_Reg(Register reg) pure nothrow @nogc { return (reg & 0b0111) << 3; }
ubyte regTo_ModRm_Rm(Register reg) pure nothrow @nogc { return (reg & 0b0111) << 0; }

struct SibScale { ubyte bits; ubyte value() { return cast(ubyte)(1 << bits); } }
struct ModRmMod { ubyte bits; }

ubyte encodeSibByte(SibScale ss, Register index, Register base) pure nothrow @nogc {
	return cast(ubyte)(ss.bits << 6) | (index & 0b0111) << 3 | (base & 0b0111);
}

ubyte encodeModRegRmByte(ModRmMod mod, Register reg, Register rm) pure nothrow @nogc {
	return cast(ubyte)(mod.bits << 6) | (reg & 0b0111) << 3 | (rm & 0b0111);
}

enum MemAddrType : ubyte {
	disp32,           // [                     disp32]
	indexDisp32,      // [       (index * s) + disp32]
	base,             // [base                       ]
	baseDisp32,       // [base +             + disp32]
	baseIndex,        // [base + (index * s)         ]
	baseIndexDisp32,  // [base + (index * s) + disp32]
	baseDisp8,        // [base +             + disp8 ]
	baseIndexDisp8,   // [base + (index * s) + disp8 ]
	ripDisp32         // [RIP  +             + disp32]
}
ubyte sibAddrType(MemAddrType type) { return 0b1_0000 | type; }

ubyte[9] memAddrType_to_mod = [0,0,0,2,0,2,1,1,0];
ubyte[9] memAddrType_to_dispType = [1,1,0,1,0,1,2,2,1]; // 0 - none, 1 - disp32, 2 - disp8

// memory location that can be passed to assembly instructions
struct MemAddress {
	ubyte typeStorage; // MemAddrType | 0b1_0000;
	Register indexReg = Register.SP;
	Register baseReg  = Register.BP;
	SibScale scale;
	uint disp; // disp8 is stored here too

	MemAddrType type() { return cast(MemAddrType)(typeStorage & 0b1111); }
	Imm32 disp32() @property { return Imm32(disp); }
	Imm8 disp8() @property { return Imm8(cast(ubyte)(disp & 0xFF)); }

	ubyte rexBits() { return regTo_Rex_X(indexReg) | regTo_Rex_B(baseReg); }
	ubyte modRmByte(ubyte reg = 0) {
		return encodeModRegRmByte(ModRmMod(memAddrType_to_mod[type]), cast(Register)reg, hasSibByte ? Register.SP : baseReg);
	}
	ModRmMod mod() { return ModRmMod(memAddrType_to_mod[type]); }
	ubyte sibByte() { return encodeSibByte(scale, indexReg, baseReg); }
	bool hasDisp32() { return memAddrType_to_dispType[type] == 1; }
	bool hasDisp8 () { return memAddrType_to_dispType[type] == 2; }
	bool hasSibByte() { return cast(bool)(typeStorage & 0b1_0000); }

	string toString() {
		final switch(type) {
			case MemAddrType.disp32: return format("[0x%x]", disp32.value);
			case MemAddrType.indexDisp32: return format("[(%s*%s) + 0x%x]", indexReg, scale.value, disp32.value);
			case MemAddrType.base: return format("[%s]", baseReg);
			case MemAddrType.baseDisp32: return format("[%s + 0x%x]", baseReg, disp32.value);
			case MemAddrType.baseIndex: return format("[%s + (%s*%s)]", baseReg, indexReg, scale.value);
			case MemAddrType.baseIndexDisp32: return format("[%s + (%s*%s) + 0x%x]", baseReg, indexReg, scale.value, disp32.value);
			case MemAddrType.baseDisp8: return format("[%s + 0x%x]", baseReg, disp8.value);
			case MemAddrType.baseIndexDisp8: return format("[%s + (%s*%s) + 0x%x]", baseReg, indexReg, scale.value, disp8.value);
			case MemAddrType.ripDisp32: return format("[RIP + 0x%x]", disp32.value);
		}
	}
}

// variant 1  [disp32]
MemAddress memAddrDisp32(int disp32) {
	return MemAddress(sibAddrType(MemAddrType.disp32), Register.SP, Register.BP, SibScale(), disp32); // with SIB
}
// variant 2  [(index * s) + disp32]
MemAddress memAddrIndexDisp32(Register indexReg, SibScale scale, int disp32) {
	assert(indexReg != Register.SP, "Cannot encode [RSP * scale + disp32]");
	return MemAddress(sibAddrType(MemAddrType.indexDisp32), indexReg, Register.BP, scale, disp32); // with SIB
}
// variant 3  [base]
MemAddress memAddrBase(Register baseReg) {
	if (is_BP_or_R13(baseReg)) // fallback to variant 7 [base + 0x0]
		return memAddrBaseDisp8(baseReg, 0); // with or without SIB
	else if (is_SP_or_R12(baseReg)) // cannot encode SP,R12 without SIB
		return MemAddress(sibAddrType(MemAddrType.base), Register.SP, baseReg); // with SIB
	else
		return MemAddress(MemAddrType.base, Register.SP, baseReg); // no SIB
}
// variant 4  [base + disp32]
MemAddress memAddrBaseDisp32(Register baseReg, int disp32) {
	if (is_SP_or_R12(baseReg))
		return MemAddress(sibAddrType(MemAddrType.baseDisp32), Register.SP, baseReg, SibScale(), disp32); // with SIB
	else
		return MemAddress(MemAddrType.baseDisp32, Register.SP, baseReg, SibScale(), disp32); // no SIB
}
// variant 5  [base + index * s]
MemAddress memAddrBaseIndex(Register baseReg, Register indexReg, SibScale scale) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale]");
	if (is_BP_or_R13(baseReg)) // fallback to variant 8 [base + (index * s) + disp8]
		return memAddrBaseIndexDisp8(baseReg, indexReg, scale, 0); // with SIB
	else
		return MemAddress(sibAddrType(MemAddrType.baseIndex), indexReg, baseReg, scale); // with SIB
}
// variant 6  [base + index * s + disp32]
MemAddress memAddrBaseIndexDisp32(Register baseReg, Register indexReg, SibScale scale, int disp32) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale + disp32]");
	return MemAddress(sibAddrType(MemAddrType.baseIndexDisp32), indexReg, baseReg, scale, disp32); // with SIB
}
// variant 7  [base + disp8]
MemAddress memAddrBaseDisp8(Register baseReg, byte disp8) {
	if (is_SP_or_R12(baseReg)) // cannot encode SP,R12 without SIB
		return MemAddress(sibAddrType(MemAddrType.baseDisp8), Register.SP, baseReg, SibScale(), disp8); // with SIB
	else
		return MemAddress(MemAddrType.baseDisp8, Register.SP, baseReg, SibScale(), disp8); // no SIB
}
// variant 8  [base + (index * s) + disp8]
MemAddress memAddrBaseIndexDisp8(Register baseReg, Register indexReg, SibScale scale, byte disp8) {
	assert(indexReg != Register.SP, "Cannot encode [base + RSP * scale + disp8]");
	return MemAddress(sibAddrType(MemAddrType.baseIndexDisp8), indexReg, baseReg, scale, disp8); // with SIB
}

// variant 9  [RIP + disp32]
MemAddress memAddrRipDisp32(int disp32) {
	return MemAddress(MemAddrType.ripDisp32, Register.SP, Register.BP, SibScale(), disp32); // with SIB
}

// Shortcut for memAddrBaseDisp32 and memAddrBaseDisp8. memAddrBaseDisp8 is used when possible.
MemAddress minMemAddrBaseDisp(Register baseReg, int displacement)
{
	if (displacement < byte.min || displacement > byte.max)
		return memAddrBaseDisp32(baseReg, displacement);
	else
		return memAddrBaseDisp8(baseReg, cast(byte)displacement);
}

// Opcode structures for 1-byte and 2-byte encodings
struct OP1 { enum size = 1; ubyte op0; }
struct OP2 { enum size = 2; ubyte op0; ubyte op1; }
enum bool isAnyOpcode(O) = is(O == OP1) || is(O == OP2);

alias PC = ubyte*;

enum EncFlg : ubyte {
	// Forces REX prefix when one of SP, BP, SI, DI are encoded.
	// Without REX prefix byte sized instructions get SPL, BPL, SIL or DIL encoded instead of SP, BP, SI, DI
	REX_HIGH  = 1 << 0,
	// Forces REX.W prefix to be added. Used for 64bit instructions
	REXW_FORCE = 1 << 1,
	// Adds Operand-size override prefix (0x66)
	OP_SIZE   = 1 << 2,
}

// ensures REX prefix for ah ch dh bh
bool regNeedsRexPrefix(ubyte flags)(Register reg) {
	static if (flags & EncFlg.REX_HIGH) return reg >= 4;
	else return false;
}

struct Encoder
{
	private Arena!ubyte* arena;
	private PC pc() { return arena.nextPtr; }

	uint pcOffset() { return cast(uint)arena.length; }
	void setBuffer(Arena!ubyte* arena) { this.arena = arena; }
	ubyte[] code() { return arena.data; }

	void sink_put(T)(T value)
	{
		arena.put(value);
	}

	void putRexByteChecked(ubyte flags)(ubyte bits, bool forceRex = false) {
		static if (flags & EncFlg.REXW_FORCE)
			sink_put!ubyte(REX_PREFIX | REX_W | bits);
		else
			if (bits || forceRex) sink_put!ubyte(REX_PREFIX | bits);
	}
	void putRexByte_RB(ubyte flags)(Register reg, Register rm) { // reg reg
		putRexByteChecked!flags(regTo_Rex_R(reg) | regTo_Rex_B(rm), regNeedsRexPrefix!flags(reg) || regNeedsRexPrefix!flags(rm)); }
	void putRexByte_regB(ubyte flags)(Register rm) { // R.R/M reg
		putRexByteChecked!flags(regTo_Rex_B(rm), regNeedsRexPrefix!flags(rm)); }
	void putRexByte_B(ubyte flags)(Register base) { // base
		putRexByteChecked!flags(regTo_Rex_B(base)); }
	void putRexByte_RXB(ubyte flags)(Register r, Register index, Register base) { // reg index base
		putRexByteChecked!flags(regTo_Rex_R(r) | regTo_Rex_X(index) | regTo_Rex_B(base), regNeedsRexPrefix!flags(r)); }
	void putRexByte_XB(ubyte flags)(Register index, Register base) { // index base
		putRexByteChecked!flags(regTo_Rex_X(index) | regTo_Rex_B(base)); }

	void putInstrBinaryRegReg(ubyte flags, O)(O opcode, Register dst_rm, Register src_reg) if (isAnyOpcode!O) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE); // 16 bit operand prefix
		putRexByte_RB!flags(src_reg, dst_rm);                                   // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), src_reg, dst_rm));          // ModR/r
	}
	void putInstrBinaryRegRegImm(ubyte flags, O, I)(O opcode, Register dst_rm, Register src_reg, I src_imm) if (isAnyOpcode!O && isAnyImm!I) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE); // 16 bit operand prefix
		putRexByte_RB!flags(src_reg, dst_rm);                                   // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), src_reg, dst_rm));          // ModR/r
		sink_put(src_imm);                                                      // Imm8/16/32/64
	}
	// PUSH, POP, MOV, XCHG, BSWAP
	void putInstrBinaryRegImm1(ubyte flags, I)(OP1 opcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE); // 16 bit operand prefix
		putRexByte_regB!flags(dst_rm);                                          // REX
		sink_put!ubyte(opcode.op0 | (dst_rm & 0b0111));                         // Opcode + reg
		sink_put(src_imm);                                                      // Imm8/16/32/64
	}
	void putInstrBinaryRegImm2(ubyte flags, I)(OP1 opcode, ubyte regOpcode, Register dst_rm, I src_imm) if (isAnyImm!I) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE); // 16 bit operand prefix
		putRexByte_regB!flags(dst_rm);                                          // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), cast(Register)regOpcode, dst_rm));  // ModO/R
		sink_put(src_imm);                                                      // Imm8/16/32/64
	}
	// if isReg == true then dst_r is register, otherwise it is extra opcode
	void putInstrBinaryRegMem(ubyte flags, bool isReg = true, O)(O opcode, Register reg_or_opcode, MemAddress src_mem) if (isAnyOpcode!O) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE); // 16 bit operand prefix
		static if (isReg) putRexByte_RXB!flags(reg_or_opcode, src_mem.indexReg, src_mem.baseReg); // REX
		else putRexByte_XB!flags(src_mem.indexReg, src_mem.baseReg);            // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(src_mem.modRmByte(reg_or_opcode));                             // ModR/M
		if (src_mem.hasSibByte)	   sink_put(src_mem.sibByte);                   // SIB
		if (src_mem.hasDisp32)     sink_put(src_mem.disp32);                    // disp32
		else if (src_mem.hasDisp8) sink_put(src_mem.disp8);                     // disp8
	}
	void putInstrBinaryRegMemImm(ubyte flags, O, I)(O opcode, Register reg, MemAddress src_mem, I src_imm) if (isAnyOpcode!O && isAnyImm!I) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE); // 16 bit operand prefix
		putRexByte_RXB!flags(reg, src_mem.indexReg, src_mem.baseReg);           // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(src_mem.modRmByte(reg));                                       // ModR/M
		if (src_mem.hasSibByte)	   sink_put(src_mem.sibByte);                   // SIB
		if (src_mem.hasDisp32)     sink_put(src_mem.disp32);                    // disp32
		else if (src_mem.hasDisp8) sink_put(src_mem.disp8);                     // disp8
		sink_put(src_imm);                                                      // Imm8/16/32
	}
	void putInstrBinaryMemImm(ubyte flags, O, I)(O opcode, ubyte regOpcode, MemAddress dst_mem, I src_imm) if (isAnyOpcode!O && isAnyImm!I) {
		putInstrBinaryRegMem!(flags, false)(opcode, cast(Register)regOpcode, dst_mem);
		sink_put(src_imm);                                                      // Imm8/16/32
	}

	void prefix(ubyte val) {
		sink_put(val);
	}
	void putInstrNullary(O)(O opcode) if(isAnyOpcode!O) {
		sink_put(opcode);                                                       // Opcode
	}
	void putInstrNullaryImm(O, I)(O opcode, I imm) if(isAnyOpcode!O && isAnyImm!I) {
		sink_put(opcode);                                                       // Opcode
		sink_put(imm);                                                          // Imm8/16/32/64
	}
	// <opcode> /regOpcode
	void putInstrUnaryReg1(ubyte flags, O)(O opcode, ubyte regOpcode, Register dst_rm) if (isAnyOpcode!O) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!flags(dst_rm);                                        // REX
		sink_put(opcode);                                                       // Opcode
		sink_put(encodeModRegRmByte(ModRmMod(0b11), cast(Register)regOpcode, dst_rm));// ModO/R
	}
	void putInstrUnaryReg2(ubyte flags)(ubyte opcode, Register dst_rm) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		putRexByte_regB!flags(dst_rm);                                        // REX
		sink_put!ubyte(opcode | (dst_rm & 0b0111));                             // Opcode
	}
	void putInstrUnaryMem(ubyte flags, O)(O opcode, ubyte regOpcode, MemAddress dst_mem) if (isAnyOpcode!O) {
		putInstrBinaryRegMem!(flags, false)(opcode, cast(Register)regOpcode, dst_mem);
	}
	void putInstrUnaryImm(ubyte flags, O, I)(O opcode, I imm) if (isAnyOpcode!O && isAnyImm!I) {
		static if (flags & EncFlg.OP_SIZE) sink_put(LegacyPrefix.OPERAND_SIZE);// 16 bit operand prefix
		sink_put(opcode);                                                       // Opcode
		sink_put(imm);                                                          // Imm8/16/32
	}
}

struct Fixup
{
	private CodeGen_x86_64* codeGen;
	private PC fixupPC;

	template opDispatch(string member)
	{
		import std.traits : Parameters;
		static foreach(Over; __traits(getOverloads, CodeGen_x86_64, member))
		{
			auto opDispatch(Parameters!(Over) args) {
				auto tempPC = codeGen.encoder.pc;
				codeGen.encoder.pc = fixupPC;
				scope(exit)codeGen.encoder.pc = tempPC;
				mixin("return codeGen."~member~"(args);");
			}
		}
	}
}

struct Fixup32
{
	uint fixupOffset;
	uint extraOffset;
}

Imm32 jumpOffset(PC from, PC to) {
	assert(to - from == cast(int)(to - from), format("offset from %s to %s is %X and is not representible as int", from, to, to-from));
	return Imm32(cast(int)(to - from));
}

enum AsmArgKind : ubyte { REG, IMM, MEM }
enum AsmArgKindProduct : ubyte {
//	REG      IMM      MEM         left
	REG_REG, IMM_REG, MEM_REG, // REG  right
	REG_IMM, IMM_IMM, MEM_IMM, // IMM
	REG_MEM, IMM_MEM, MEM_MEM, // MEM
}
AsmArgKindProduct asmArgKindProduct(AsmArgKind left, AsmArgKind right) {
	return cast(AsmArgKindProduct)(left + 3 * right);
}
union AsmArg
{
	Imm8 imm8;
	Imm16 imm16;
	Imm32 imm32;
	Imm64 imm64;
	Register reg;
	MemAddress memAddress;
}

enum AMD64OpRegular : ubyte {
	add,
	or,
	and,
	sub,
	xor,
	cmp
}

struct AsmOpParam
{
	AsmArgKind dstKind;
	AsmArgKind srcKind;
	AMD64OpRegular op;
	ArgType argType;
	ArgType immType;
}

// Sink defines put(T) for ubyte, ubyte[], Imm8, Imm16, Imm32, Imm64
struct CodeGen_x86_64
{
	Encoder encoder;

	Fixup fixupAt(PC at) return { return Fixup(&this, at); }
	Fixup saveFixup() return { return Fixup(&this, encoder.pc); }
	PC pc() { return encoder.pc; }

	/// Used for versions of instructions without argument size suffix.
	/// mov, add, sub, instead of movq, addb, subd.
	/// mov(Register.AX, Register.DI, ArgType.QWORD); instead of movq(Register.AX, Register.DI);
	void opDispatch(string s, Arg1, Arg2)(Arg1 dst, Arg2 src, ArgType argType) {
		switch(argType) {
			static if (__traits(compiles, mixin(s~"b(dst, src)"))) { case ArgType.BYTE:  mixin(s~"b(dst, src);"); break; }
			static if (__traits(compiles, mixin(s~"w(dst, src)"))) { case ArgType.WORD:  mixin(s~"w(dst, src);"); break; }
			static if (__traits(compiles, mixin(s~"d(dst, src)"))) { case ArgType.DWORD: mixin(s~"d(dst, src);"); break; }
			static if (__traits(compiles, mixin(s~"q(dst, src)"))) { case ArgType.QWORD: mixin(s~"q(dst, src);"); break; }
			default: assert(false, format("Cannot encode %s(%s, %s, ArgType.%s)", s, dst, src, argType));
		}
	}

	/// ditto
	void opDispatch(string s, Arg1)(Arg1 dst, ArgType argType) {
		switch(argType) {
			static if (__traits(compiles, mixin(s~"b(dst)"))) { case ArgType.BYTE:  mixin(s~"b(dst);"); break; }
			static if (__traits(compiles, mixin(s~"w(dst)"))) { case ArgType.WORD:  mixin(s~"w(dst);"); break; }
			static if (__traits(compiles, mixin(s~"d(dst)"))) { case ArgType.DWORD: mixin(s~"d(dst);"); break; }
			static if (__traits(compiles, mixin(s~"q(dst)"))) { case ArgType.QWORD: mixin(s~"q(dst);"); break; }
			default: assert(false, format("Cannot encode %s(%s, ArgType.%s)", s, dst, argType));
		}
	}

	mixin binaryInstr_RMtoR_RtoRM!("add", [0x00, 0x01], [0x02, 0x03]);
	mixin binaryInstr_RM_Imm!("add", 0);

	mixin instrMOV!();
	mixin binaryInstr_RMtoR_RtoRM!("mov", [0x88, 0x89], [0x8A, 0x8B]);

	mixin binaryInstr_RMtoR_RtoRM!("sub", [0x28, 0x29], [0x2A, 0x2B]);
	mixin binaryInstr_RM_Imm!("sub", 5);

	mixin binaryInstr_RMtoR_RtoRM!("and", [0x20, 0x21], [0x22, 0x23]);
	mixin binaryInstr_RM_Imm!("and", 4);

	mixin binaryInstr_RMtoR_RtoRM!("or", [0x08, 0x09], [0x0A, 0x0B]);
	mixin binaryInstr_RM_Imm!("or", 1);

	mixin binaryInstr_RMtoR_RtoRM!("xor", [0x30, 0x31], [0x32, 0x33]);
	mixin binaryInstr_RM_Imm!("xor", 6);

	mixin binaryInstr_RMtoR_RtoRM!("cmp", [0x38, 0x39], [0x3A, 0x3B]);
	mixin binaryInstr_RM_Imm!("cmp", 7);

	mixin binaryInstr_RMtoR_RtoRM!("xchg", [0x86, 0x87], [0x86, 0x87]);

	void leaw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE) (OP1(0x8D), dst, src); }
	void lead(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(0)(OP1(0x8D), dst, src); }
	void leaq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP1(0x8D), dst, src); }

	mixin unaryInstr_RM!("inc", [0xFE,0xFF], 0);
	mixin unaryInstr_RM!("dec", [0xFE,0xFF], 1);
	mixin unaryInstr_RM!("neg", [0xF6,0xF7], 3); // Two's Complement Negation
	mixin unaryInstr_RM!("mul", [0xF6,0xF7], 4);
	mixin unaryInstr_RM!("div", [0xF6,0xF7], 6);
	mixin unaryInstr_RM!("idiv", [0xF6,0xF7], 7);
	mixin unaryInstr_RM!("not", [0xF6,0xF7], 2); // One's Complement Negation

	mixin shift_RM_Imm8!("shli", [0xC0,0xC1], 4); // shl dst, imm8
	mixin shift_RM_Imm8!("shri", [0xC0,0xC1], 5); // shr dst, imm8
	mixin shift_RM_Imm8!("sari", [0xC0,0xC1], 7); // sar dst, imm8
	mixin unaryInstr_RM!("shl1", [0xD0,0xD1], 4); // shl dst, 1
	mixin unaryInstr_RM!("shr1", [0xD0,0xD1], 5); // shr dst, 1
	mixin unaryInstr_RM!("sar1", [0xD0,0xD1], 7); // sar dst, 1
	mixin unaryInstr_RM!("shl", [0xD2,0xD3], 4); // shl dst, cl
	mixin unaryInstr_RM!("shr", [0xD2,0xD3], 5); // shr dst, cl
	mixin unaryInstr_RM!("sar", [0xD2,0xD3], 7); // sar dst, cl

	void nop() { encoder.putInstrNullary(OP1(0x90)); }
	void ud2() { encoder.putInstrNullary(OP2(0x0F, 0x0B)); }

	/// relative call to target virtual address.
	void call(Imm32 targetOffset) { encoder.putInstrNullaryImm(OP1(0xE8), targetOffset); } // relative to next instr
	void call(PC target) { encoder.putInstrNullaryImm(OP1(0xE8), jumpOffset(encoder.pc + 5, target)); } // relative to next instr
	void call(Register target) { encoder.putInstrUnaryReg1!(EncFlg.REXW_FORCE)(OP1(0xFF), 2, target); } // absolute address
	void call(MemAddress target) { encoder.putInstrUnaryMem!(0)(OP1(0xFF), 2, target); } // absolute address, use DWORD to omit REX.W

	/// Generate fixup for last 32 bits of last instruction.
	Fixup32 getAddressFixup() { return Fixup32(encoder.pcOffset - 4, 4); }
	Fixup32 getDataFixup() { return Fixup32(encoder.pcOffset - 4, 0); }

	/// jump relative to next instr.
	void jmp(Imm8 offset ) { encoder.putInstrNullaryImm(OP1(0xEB), offset); }
	void jmp(Imm32 offset) { encoder.putInstrNullaryImm(OP1(0xE9), offset); }
	void jmpAbs(PC target) { encoder.putInstrNullaryImm(OP1(0xE9), jumpOffset(encoder.pc + 5, target) ); }

	/// jump relative to next instr.
	void jcc(Condition condition, Imm8  offset) { encoder.putInstrNullaryImm(OP1(0x70 | condition), offset); }
	void jcc(Condition condition, Imm32 offset) { encoder.putInstrNullaryImm(OP2(0x0F, 0x80 | condition), offset); }
	void jccAbs(Condition condition, PC target) { encoder.putInstrNullaryImm(OP2(0x0F, 0x80 | condition), jumpOffset(encoder.pc + 6, target) ); }

	void setcc(Condition condition, Register dst)   { encoder.putInstrUnaryReg1!(EncFlg.REX_HIGH)(OP2(0x0F, 0x90 | condition), 0, dst); }
	void setcc(Condition condition, MemAddress dst) { encoder.putInstrUnaryMem !(EncFlg.REX_HIGH)(OP2(0x0F, 0x90 | condition), 0, dst); }

	void test(Register dst, Register src, ArgType argType) {
		final switch(argType) {
			case ArgType.BYTE:  testb(dst, src); break;
			case ArgType.WORD:  testw(dst, src); break;
			case ArgType.DWORD: testd(dst, src); break;
			case ArgType.QWORD: testq(dst, src); break;
		}
	}

	void testb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH) (OP1(0x84), dst, src); }
	void testw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE) (OP1(0x85), dst, src); }
	void testd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(0)(OP1(0x85), dst, src); }
	void testq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP1(0x85), dst, src); }

	void imulw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE) (OP2(0x0F, 0xAF), src, dst); }
	void imuld(Register dst, Register src){ encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0xAF), src, dst); }
	void imulq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0xAF), src, dst); }
	void imulw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE) (OP2(0x0F, 0xAF), dst, src); }
	void imuld(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0xAF), dst, src); }
	void imulq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0xAF), dst, src); }

	void imulw(Register dst, Register src1, Imm8 src2) { encoder.putInstrBinaryRegRegImm!(EncFlg.OP_SIZE) (OP1(0x6B), src1, dst, src2); }
	void imuld(Register dst, Register src1, Imm8 src2) { encoder.putInstrBinaryRegRegImm!(0)(OP1(0x6B), src1, dst, src2); }
	void imulq(Register dst, Register src1, Imm8 src2) { encoder.putInstrBinaryRegRegImm!(EncFlg.REXW_FORCE)(OP1(0x6B), src1, dst, src2); }
	void imulw(Register dst, Register src1, Imm16 src2){ encoder.putInstrBinaryRegRegImm!(EncFlg.OP_SIZE) (OP1(0x69), src1, dst, src2); }
	void imuld(Register dst, Register src1, Imm32 src2){ encoder.putInstrBinaryRegRegImm!(0)(OP1(0x69), src1, dst, src2); }
	void imulq(Register dst, Register src1, Imm32 src2){ encoder.putInstrBinaryRegRegImm!(EncFlg.REXW_FORCE)(OP1(0x69), src1, dst, src2); }

	void imulw(Register dst, MemAddress src1, Imm8 src2) { encoder.putInstrBinaryRegMemImm!(EncFlg.OP_SIZE) (OP1(0x6B), dst, src1, src2); }
	void imuld(Register dst, MemAddress src1, Imm8 src2) { encoder.putInstrBinaryRegMemImm!(0)(OP1(0x6B), dst, src1, src2); }
	void imulq(Register dst, MemAddress src1, Imm8 src2) { encoder.putInstrBinaryRegMemImm!(EncFlg.REXW_FORCE)(OP1(0x6B), dst, src1, src2); }
	void imulw(Register dst, MemAddress src1, Imm16 src2){ encoder.putInstrBinaryRegMemImm!(EncFlg.OP_SIZE) (OP1(0x69), dst, src1, src2); }
	void imuld(Register dst, MemAddress src1, Imm32 src2){ encoder.putInstrBinaryRegMemImm!(0)(OP1(0x69), dst, src1, src2); }
	void imulq(Register dst, MemAddress src1, Imm32 src2){ encoder.putInstrBinaryRegMemImm!(EncFlg.REXW_FORCE)(OP1(0x69), dst, src1, src2); }

	void movzx_btow(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH | EncFlg.OP_SIZE) (OP2(0x0F, 0xB6), src, dst); }
	void movzx_btod(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH)(OP2(0x0F, 0xB6), src, dst); }
	void movzx_btoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH | EncFlg.REXW_FORCE)(OP2(0x0F, 0xB6), src, dst); }
	void movzx_wtod(Register dst, Register src){ encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0xB7), src, dst); }
	void movzx_wtoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0xB7), src, dst); }

	void movsx_btow(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH | EncFlg.OP_SIZE) (OP2(0x0F, 0xBE), src, dst); }
	void movsx_btod(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH)(OP2(0x0F, 0xBE), src, dst); }
	void movsx_btoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH | EncFlg.REXW_FORCE)(OP2(0x0F, 0xBE), src, dst); }
	void movsx_wtod(Register dst, Register src){ encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0xBF), src, dst); }
	void movsx_wtoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0xBF), src, dst); }
	void movsx_dtoq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP1(0x63), src, dst); }

	void movzx_btow(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH | EncFlg.OP_SIZE) (OP2(0x0F, 0xB6), dst, src); }
	void movzx_btod(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH)(OP2(0x0F, 0xB6), dst, src); }
	void movzx_btoq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH | EncFlg.REXW_FORCE)(OP2(0x0F, 0xB6), dst, src); }
	void movzx_wtod(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0xB7), dst, src); }
	void movzx_wtoq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0xB7), dst, src); }

	void movsx_btow(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH | EncFlg.OP_SIZE) (OP2(0x0F, 0xBE), dst, src); }
	void movsx_btod(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH)(OP2(0x0F, 0xBE), dst, src); }
	void movsx_btoq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH | EncFlg.REXW_FORCE)(OP2(0x0F, 0xBE), dst, src); }
	void movsx_wtod(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0xBF), dst, src); }
	void movsx_wtoq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0xBF), dst, src); }
	void movsx_dtoq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP1(0x63), dst, src); }

	void cwd() { encoder.putInstrNullary(OP2(0x66, 0x99)); }
	void cdq() { encoder.putInstrNullary(OP1(0x99)); }
	void cqo() { encoder.putInstrNullary(OP2(0x48, 0x99)); }

	void popw(Register dst)   { encoder.putInstrUnaryReg2!(EncFlg.OP_SIZE)(0x58, dst); }
	void popq(Register dst)   { encoder.putInstrUnaryReg2!(0)(0x58, dst); } // use DWORD to omit REX.W
	void popw(MemAddress dst) { encoder.putInstrUnaryMem!(EncFlg.OP_SIZE)(OP1(0x8F), 0, dst); }
	void popq(MemAddress dst) { encoder.putInstrUnaryMem!(0)(OP1(0x8F), 0, dst); } // use DWORD to omit REX.W

	void pushw(Register dst)   { encoder.putInstrUnaryReg2!(EncFlg.OP_SIZE)(0x50, dst); }
	void pushq(Register dst)   { encoder.putInstrUnaryReg2!(0)(0x50, dst); } // use DWORD to omit REX.W
	void pushw(MemAddress dst) { encoder.putInstrUnaryMem!(EncFlg.OP_SIZE)(OP1(0xFF), 6, dst); }
	void pushq(MemAddress dst) { encoder.putInstrUnaryMem!(0)(OP1(0xFF), 6, dst); } // use DWORD to omit REX.W

	void pushb(Imm8  src) { encoder.putInstrUnaryImm!(EncFlg.REX_HIGH )(OP1(0x6A), src); }
	void pushw(Imm16 src) { encoder.putInstrUnaryImm!(EncFlg.OP_SIZE )(OP1(0x68), src); }
	void pushd(Imm32 src) { encoder.putInstrUnaryImm!(0)(OP1(0x68), src); }

	void ret() { encoder.putInstrNullary(OP1(0xC3)); }
	void ret(Imm16 bytesToPop) { encoder.putInstrNullaryImm(OP1(0xC2), bytesToPop); }

	void int3() { encoder.putInstrNullary(OP1(0xCC)); }
	void syscall() { encoder.putInstrNullary(OP2(0x0F, 0x05)); }

	void andps(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x54), src, dst); }
	void andps(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x54), dst, src); }
	void andpd(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x54), src, dst); }
	void andpd(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x54), dst, src); }
	void orps(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x56), src, dst); }
	void orps(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x56), dst, src); }
	void orpd(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x56), src, dst); }
	void orpd(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x56), dst, src); }
	void xorps(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x57), src, dst); }
	void xorps(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x57), dst, src); }
	void xorpd(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x57), src, dst); }
	void xorpd(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x57), dst, src); }
	void addps(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x58), src, dst); }
	void addps(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x58), dst, src); }
	void addpd(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x58), src, dst); }
	void addpd(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x58), dst, src); }
	void mulps(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x59), src, dst); }
	void mulps(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x59), dst, src); }
	void mulpd(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x59), src, dst); }
	void mulpd(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x59), dst, src); }
	void subps(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x5C), src, dst); }
	void subps(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x5C), dst, src); }
	void subpd(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x5C), src, dst); }
	void subpd(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x5C), dst, src); }
	void divps(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x5E), src, dst); }
	void divps(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x5E), dst, src); }
	void divpd(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x5E), src, dst); }
	void divpd(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x5E), dst, src); }

	void cmpss(Register dst, Register src, Imm8 pred) { encoder.prefix(0xF3); encoder.putInstrBinaryRegRegImm!(0)(OP2(0x0F, 0xC2), src, dst, pred); }
	void cmpss(Register dst, MemAddress src, Imm8 pred) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMemImm!(0)(OP2(0x0F, 0xC2), dst, src, pred); }
	void cmpsd(Register dst, Register src, Imm8 pred) { encoder.prefix(0xF2); encoder.putInstrBinaryRegRegImm!(0)(OP2(0x0F, 0xC2), src, dst, pred); }
	void cmpsd(Register dst, MemAddress src, Imm8 pred) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMemImm!(0)(OP2(0x0F, 0xC2), dst, src, pred); }

	void ucomiss(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x2E), src, dst); }
	void ucomiss(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x2E), dst, src); }
	void ucomisd(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x2E), src, dst); }
	void ucomisd(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x2E), dst, src); }

	void addss(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x58), src, dst); }
	void addss(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x58), dst, src); }
	void addsd(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x58), src, dst); }
	void addsd(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x58), dst, src); }
	void subss(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x5C), src, dst); }
	void subss(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x5C), dst, src); }
	void subsd(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x5C), src, dst); }
	void subsd(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x5C), dst, src); }
	void mulss(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x59), src, dst); }
	void mulss(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x59), dst, src); }
	void mulsd(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x59), src, dst); }
	void mulsd(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x59), dst, src); }
	void divss(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x5E), src, dst); }
	void divss(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x5E), dst, src); }
	void divsd(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x5E), src, dst); }
	void divsd(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x5E), dst, src); }

	void movss(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x10), src, dst); }
	void movss(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x10), dst, src); }
	void movss(MemAddress dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x11), src, dst); }

	void movsd(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x10), src, dst); }
	void movsd(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x10), dst, src); }
	void movsd(MemAddress dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x11), src, dst); }

	void movaps(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x28), src, dst); }
	void movaps(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x28), dst, src); }
	void movaps(MemAddress dst, Register src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x29), src, dst); }

	void movups(Register dst, Register src) { encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x10), src, dst); }
	void movups(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x10), dst, src); }
	void movups(MemAddress dst, Register src) { encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x11), src, dst); }

	// x stands for xmm
	void movd_xr(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x6E), src, dst); }
	void movd_xr(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x6E), dst, src); }
	void movq_xr(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE|EncFlg.REXW_FORCE)(OP2(0x0F, 0x6E), src, dst); }
	void movq_xr(Register dst, MemAddress src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE|EncFlg.REXW_FORCE)(OP2(0x0F, 0x6E), dst, src); }

	void movd_rx(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE)(OP2(0x0F, 0x7E), dst, src); }
	void movd_rx(MemAddress dst, Register src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE)(OP2(0x0F, 0x7E), src, dst); }
	void movq_rx(Register dst, Register src) { encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE|EncFlg.REXW_FORCE)(OP2(0x0F, 0x7E), dst, src); }
	void movq_rx(MemAddress dst, Register src) { encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE|EncFlg.REXW_FORCE)(OP2(0x0F, 0x7E), src, dst); }

	void cvtss2sd(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x5A), src, dst); }
	void cvtss2sd(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x5A), dst, src); }
	void cvtsd2ss(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x5A), src, dst); }
	void cvtsd2ss(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x5A), dst, src); }

	void cvtss2sid(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x2D), src, dst); }
	void cvtss2sid(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x2D), dst, src); }
	void cvtss2siq(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2D), src, dst); }
	void cvtss2siq(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2D), dst, src); }

	void cvtsid2ss(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x2A), src, dst); }
	void cvtsid2ss(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x2A), dst, src); }
	void cvtsiq2ss(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2A), src, dst); }
	void cvtsiq2ss(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2A), dst, src); }

	void cvttss2sid(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x2C), src, dst); }
	void cvttss2sid(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x2C), dst, src); }
	void cvttss2siq(Register dst, Register src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2C), src, dst); }
	void cvttss2siq(Register dst, MemAddress src) { encoder.prefix(0xF3); encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2C), dst, src); }

	void cvtsd2sid(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x2D), src, dst); }
	void cvtsd2sid(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x2D), dst, src); }
	void cvtsd2siq(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2D), src, dst); }
	void cvtsd2siq(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2D), dst, src); }

	void cvtsid2sd(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x2A), src, dst); }
	void cvtsid2sd(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x2A), dst, src); }
	void cvtsiq2sd(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2A), src, dst); }
	void cvtsiq2sd(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2A), dst, src); }

	void cvttsd2sid(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(0)(OP2(0x0F, 0x2C), src, dst); }
	void cvttsd2sid(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(0)(OP2(0x0F, 0x2C), dst, src); }
	void cvttsd2siq(Register dst, Register src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2C), src, dst); }
	void cvttsd2siq(Register dst, MemAddress src) { encoder.prefix(0xF2); encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP2(0x0F, 0x2C), dst, src); }

	void rep_prefix() { encoder.prefix(0xF3); }
	void stos() { encoder.putInstrNullary(OP1(0xAA)); }

	void encodeRegular(AsmArg dst, AsmArg src, AsmOpParam param)
	{
		static immutable ubyte[] op_tbl_bin = [
			0x00, // add,
			0x08, // or,
			0x20, // and,
			0x28, // sub,
			0x30, // xor,
			0x38, // cmp,
		];

		static immutable ubyte[] op_tbl_un = [
			0, // add,
			1, // or,
			4, // and,
			5, // sub,
			6, // xor,
			7, // cmp
		];

		AsmArgKindProduct prod = asmArgKindProduct(param.dstKind, param.srcKind);
		final switch(prod) with(AsmArgKindProduct)
		{
			case REG_REG:
				final switch(param.argType) {
					case ArgType.BYTE:  encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH) (OP1(cast(ubyte)(op_tbl_bin[param.op]+0)), dst.reg, src.reg); break;
					case ArgType.WORD:  encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE) (OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), dst.reg, src.reg); break;
					case ArgType.DWORD: encoder.putInstrBinaryRegReg!(0)(OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), dst.reg, src.reg); break;
					case ArgType.QWORD: encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), dst.reg, src.reg); break;
				} break;

			case MEM_REG:
				final switch(param.argType) {
					case ArgType.BYTE:  encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH) (OP1(cast(ubyte)(op_tbl_bin[param.op]+0)), src.reg, dst.memAddress); break;
					case ArgType.WORD:  encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE) (OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), src.reg, dst.memAddress); break;
					case ArgType.DWORD: encoder.putInstrBinaryRegMem!(0)(OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), src.reg, dst.memAddress); break;
					case ArgType.QWORD: encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP1(cast(ubyte)(op_tbl_bin[param.op]+1)), src.reg, dst.memAddress); break;
				} break;

			case REG_MEM:
				final switch(param.argType) {
					case ArgType.BYTE:  encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH) (OP1(cast(ubyte)(op_tbl_bin[param.op]+2)), dst.reg, src.memAddress); break;
					case ArgType.WORD:  encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE) (OP1(cast(ubyte)(op_tbl_bin[param.op]+3)), dst.reg, src.memAddress); break;
					case ArgType.DWORD: encoder.putInstrBinaryRegMem!(0)(OP1(cast(ubyte)(op_tbl_bin[param.op]+3)), dst.reg, src.memAddress); break;
					case ArgType.QWORD: encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP1(cast(ubyte)(op_tbl_bin[param.op]+3)), dst.reg, src.memAddress); break;
				} break;

			case REG_IMM:
				bool valid1 = param.argType == param.immType;
				bool valid2 = param.immType == ArgType.BYTE;
				bool valid3 = param.argType == ArgType.QWORD && param.immType == ArgType.DWORD;
				assert(valid1 || valid2 || valid3, format("%s %s", param.argType, param.immType));

				final switch(param.immType)
				{
					case ArgType.BYTE:
						final switch(param.argType) {
							case ArgType.BYTE:  encoder.putInstrBinaryRegImm2!(EncFlg.REX_HIGH)  (OP1(0x80), op_tbl_un[param.op], dst.reg, src.imm8); break;
							case ArgType.WORD:  encoder.putInstrBinaryRegImm2!(EncFlg.OP_SIZE)  (OP1(0x83), op_tbl_un[param.op], dst.reg, src.imm8); break;
							case ArgType.DWORD: encoder.putInstrBinaryRegImm2!(0) (OP1(0x83), op_tbl_un[param.op], dst.reg, src.imm8); break;
							case ArgType.QWORD: encoder.putInstrBinaryRegImm2!(EncFlg.REXW_FORCE) (OP1(0x83), op_tbl_un[param.op], dst.reg, src.imm8); break;
						} break;

					case ArgType.WORD:  encoder.putInstrBinaryRegImm2!(EncFlg.OP_SIZE)  (OP1(0x81), op_tbl_un[param.op], dst.reg, src.imm16); break;
					case ArgType.DWORD:
						if (param.argType == ArgType.QWORD) goto case ArgType.QWORD;
						encoder.putInstrBinaryRegImm2!(0) (OP1(0x81), op_tbl_un[param.op], dst.reg, src.imm32);
						break;
					case ArgType.QWORD: encoder.putInstrBinaryRegImm2!(EncFlg.REXW_FORCE) (OP1(0x81), op_tbl_un[param.op], dst.reg, src.imm32); break;
				} break;

			case MEM_IMM:
				assert(param.argType == param.immType || param.immType == ArgType.BYTE);
				final switch(param.immType)
				{
					case ArgType.BYTE:
						final switch(param.argType) {
							case ArgType.BYTE:  encoder.putInstrBinaryMemImm!(EncFlg.REX_HIGH)  (OP1(0x80), op_tbl_un[param.op], dst.memAddress, src.imm8); break;
							case ArgType.WORD:  encoder.putInstrBinaryMemImm!(EncFlg.OP_SIZE)  (OP1(0x83), op_tbl_un[param.op], dst.memAddress, src.imm8); break;
							case ArgType.DWORD: encoder.putInstrBinaryMemImm!(0) (OP1(0x83), op_tbl_un[param.op], dst.memAddress, src.imm8); break;
							case ArgType.QWORD: encoder.putInstrBinaryMemImm!(EncFlg.REXW_FORCE) (OP1(0x83), op_tbl_un[param.op], dst.memAddress, src.imm8); break;
						} break;

					case ArgType.WORD:  encoder.putInstrBinaryMemImm!(EncFlg.OP_SIZE)  (OP1(0x81), op_tbl_un[param.op], dst.memAddress, src.imm16); break;
					case ArgType.DWORD: encoder.putInstrBinaryMemImm!(0) (OP1(0x81), op_tbl_un[param.op], dst.memAddress, src.imm32); break;
					case ArgType.QWORD: encoder.putInstrBinaryMemImm!(EncFlg.REXW_FORCE) (OP1(0x81), op_tbl_un[param.op], dst.memAddress, src.imm32); break;
				} break;

			case IMM_REG, IMM_IMM, IMM_MEM, MEM_MEM:
				assert(false);
		}
	}
}

mixin template instrMOV() {
	void movb(Register dst, Imm8  src){ encoder.putInstrBinaryRegImm1!(EncFlg.REX_HIGH) (OP1(0xB0), dst, src); }
	void movw(Register dst, Imm16 src){ encoder.putInstrBinaryRegImm1!(EncFlg.OP_SIZE) (OP1(0xB8), dst, src); }
	void movd(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm1!(0)(OP1(0xB8), dst, src); }
	void movq(Register dst, Imm32 src){ encoder.putInstrBinaryRegImm2!(EncFlg.REXW_FORCE)(OP1(0xC7), 0, dst, src); }
	void movq(Register dst, Imm64 src){ encoder.putInstrBinaryRegImm1!(EncFlg.REXW_FORCE)(OP1(0xB8), dst, src); }

	void movb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(EncFlg.REX_HIGH) (OP1(0xC6), 0, dst, src); }
	void movw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(EncFlg.OP_SIZE) (OP1(0xC7), 0, dst, src); }
	void movd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(0)(OP1(0xC7), 0, dst, src); }
	void movq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(EncFlg.REXW_FORCE)(OP1(0xC7), 0, dst, src); }
}

mixin template binaryInstr_RMtoR_RtoRM(string name, ubyte[2] rm_r, ubyte[2] r_rm) {
	mixin(format("void %sb(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH) (OP1(%s), src, dst); }", name, rm_r[0]));
	mixin(format("void %sw(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE) (OP1(%s), src, dst); }", name, rm_r[1]));
	mixin(format("void %sd(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(0)(OP1(%s), src, dst); }", name, rm_r[1]));
	mixin(format("void %sq(MemAddress dst, Register src){ encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP1(%s), src, dst); }", name, rm_r[1]));

	mixin(format("void %sb(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REX_HIGH) (OP1(%s), dst, src); }", name, rm_r[0]));
	mixin(format("void %sw(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.OP_SIZE) (OP1(%s), dst, src); }", name, rm_r[1]));
	mixin(format("void %sd(Register dst, Register src){ encoder.putInstrBinaryRegReg!(0)(OP1(%s), dst, src); }", name, rm_r[1]));
	mixin(format("void %sq(Register dst, Register src){ encoder.putInstrBinaryRegReg!(EncFlg.REXW_FORCE)(OP1(%s), dst, src); }", name, rm_r[1]));

	mixin(format("void %sb(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REX_HIGH) (OP1(%s), dst, src); }", name, r_rm[0]));
	mixin(format("void %sw(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.OP_SIZE) (OP1(%s), dst, src); }", name, r_rm[1]));
	mixin(format("void %sd(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(0)(OP1(%s), dst, src); }", name, r_rm[1]));
	mixin(format("void %sq(Register dst, MemAddress src){ encoder.putInstrBinaryRegMem!(EncFlg.REXW_FORCE)(OP1(%s), dst, src); }", name, r_rm[1]));
}

mixin template binaryInstr_RM_Imm(string name, ubyte extraOpcode) {
	mixin(format("void %sb(Register dst,   Imm8  src){ encoder.putInstrBinaryRegImm2!(EncFlg.REX_HIGH) (OP1(0x80), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sw(Register dst,   Imm16 src){ encoder.putInstrBinaryRegImm2!(EncFlg.OP_SIZE) (OP1(0x81), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sd(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(0)(OP1(0x81), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sq(Register dst,   Imm32 src){ encoder.putInstrBinaryRegImm2!(EncFlg.REXW_FORCE)(OP1(0x81), %s, dst, src); }", name, extraOpcode));

	mixin(format("void %sw(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(EncFlg.OP_SIZE) (OP1(0x83), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sd(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(0)(OP1(0x83), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sq(Register dst,   Imm8 src){ encoder.putInstrBinaryRegImm2!(EncFlg.REXW_FORCE)(OP1(0x83), %s, dst, src); }", name, extraOpcode));

	mixin(format("void %sb(MemAddress dst, Imm8  src){ encoder.putInstrBinaryMemImm!(EncFlg.REX_HIGH) (OP1(0x80), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sw(MemAddress dst, Imm16 src){ encoder.putInstrBinaryMemImm!(EncFlg.OP_SIZE) (OP1(0x81), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sd(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(0)(OP1(0x81), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sq(MemAddress dst, Imm32 src){ encoder.putInstrBinaryMemImm!(EncFlg.REXW_FORCE)(OP1(0x81), %s, dst, src); }", name, extraOpcode));

	mixin(format("void %sw(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(EncFlg.OP_SIZE) (OP1(0x83), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sd(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(0)(OP1(0x83), %s, dst, src); }", name, extraOpcode));
	mixin(format("void %sq(MemAddress dst, Imm8 src){ encoder.putInstrBinaryMemImm!(EncFlg.REXW_FORCE)(OP1(0x83), %s, dst, src); }", name, extraOpcode));
}

mixin template unaryInstr_RM(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(Register dst) { encoder.putInstrUnaryReg1!(EncFlg.REX_HIGH) (OP1(%s), %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(Register dst) { encoder.putInstrUnaryReg1!(EncFlg.OP_SIZE) (OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(Register dst) { encoder.putInstrUnaryReg1!(0)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(Register dst) { encoder.putInstrUnaryReg1!(EncFlg.REXW_FORCE)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));

	mixin(format("void %sb(MemAddress dst) { encoder.putInstrUnaryMem!(EncFlg.REX_HIGH) (OP1(%s), %s, dst); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(MemAddress dst) { encoder.putInstrUnaryMem!(EncFlg.OP_SIZE) (OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(MemAddress dst) { encoder.putInstrUnaryMem!(0)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(MemAddress dst) { encoder.putInstrUnaryMem!(EncFlg.REXW_FORCE)(OP1(%s), %s, dst); }", name, opcodes[1], extraOpcode));
}

// TODO: duplicate with binaryInstr_RM_Imm
mixin template shift_RM_Imm8(string name, ubyte[2] opcodes, ubyte extraOpcode) {
	mixin(format("void %sb(Register dst, Imm8 src) { encoder.putInstrBinaryRegImm2!(EncFlg.REX_HIGH) (OP1(%s), %s, dst, src); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(Register dst, Imm8 src) { encoder.putInstrBinaryRegImm2!(EncFlg.OP_SIZE) (OP1(%s), %s, dst, src); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(Register dst, Imm8 src) { encoder.putInstrBinaryRegImm2!(0)(OP1(%s), %s, dst, src); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(Register dst, Imm8 src) { encoder.putInstrBinaryRegImm2!(EncFlg.REXW_FORCE)(OP1(%s), %s, dst, src); }", name, opcodes[1], extraOpcode));

	mixin(format("void %sb(MemAddress dst, Imm8 src) { encoder.putInstrBinaryMemImm!(EncFlg.REX_HIGH) (OP1(%s), %s, dst, src); }", name, opcodes[0], extraOpcode));
	mixin(format("void %sw(MemAddress dst, Imm8 src) { encoder.putInstrBinaryMemImm!(EncFlg.OP_SIZE) (OP1(%s), %s, dst, src); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sd(MemAddress dst, Imm8 src) { encoder.putInstrBinaryMemImm!(0)(OP1(%s), %s, dst, src); }", name, opcodes[1], extraOpcode));
	mixin(format("void %sq(MemAddress dst, Imm8 src) { encoder.putInstrBinaryMemImm!(EncFlg.REX_HIGH)(OP1(%s), %s, dst, src); }", name, opcodes[1], extraOpcode));
}
